## ---------------------------------------------------------------------------
## Local Ollama client for the IMPC Disease Models Portal
##
## Responsibilities of this file:
##   1. Hold all Ollama connection settings in one place (model, host, timeout).
##   2. Build a data-grounded system prompt so the assistant can answer
##      questions about the portal's actual contents instead of guessing.
##   3. Expose a small, safe API for the chat module:
##         ollama_settings()            -> current settings (list)
##         ollama_set_model(model)      -> change model at runtime
##         ollama_health()              -> TRUE / FALSE + diagnostic message
##         build_portal_context()       -> short factual summary of the data
##         call_ollama_chat(...)        -> send a chat turn, get reply text
## ---------------------------------------------------------------------------

OLLAMA_DEFAULTS <- list(
  model     = Sys.getenv("OLLAMA_MODEL",   unset = "qwen2.5-coder:3b"),
  host      = Sys.getenv("OLLAMA_HOST",    unset = "http://localhost:11434"),
  timeout_s = as.numeric(Sys.getenv("OLLAMA_TIMEOUT", unset = "90")),
  # Context window for LOCAL (Ollama) models only -- hosted models manage
  # their own context. 35,000 was chosen by measurement, not convention:
  #   * fixed overhead (system prompt + tool declarations) is ~3,500 tokens
  #   * the largest tool result in the test set is ~1,600 tokens
  #   * a realistic worst case with tool chaining and history is ~7,500
  #   * the KV cache for Llama 3.1 8B costs ~128 KiB per token, so 35,000
  #     tokens uses ~4.3 GB on top of ~5 GB of weights -- about 9.3 GB,
  #     which fits comfortably in 16 GB without swapping.
  # The earlier default of 4,096 left only ~600 tokens of headroom once the
  # tool declarations were added, causing silent truncation of the prompt
  # and question on questions returning long lists.
  # Lower this if running on a machine with less memory.
  num_ctx   = as.integer(Sys.getenv("OLLAMA_NUM_CTX", unset = "35000")),
  # Temperature: randomness. 0 = deterministic (best for benchmarking, since
  # the same question gives the same answer every time). Override with
  # LLM_TEMPERATURE. Default 0 for reproducible factual answers.
  temperature = as.numeric(Sys.getenv("LLM_TEMPERATURE", unset = "0")),
  max_history = 12,
  # Toggle tool-calling mode. Set OLLAMA_USE_TOOLS=1 to enable.
  # FALSE (default) = the original pre-fetch / regex-RAG behaviour (baseline).
  # TRUE             = expose tools to the model; let it choose what to call.
  use_tools   = isTRUE(Sys.getenv("OLLAMA_USE_TOOLS", unset = "0") %in%
                       c("1", "true", "TRUE", "yes")),
  tool_max_iter = 4,  # safety cap on consecutive tool-call rounds per turn

  # --- Provider switch: "ollama" (local) or "openai" (cloud benchmark) -----
  # Set LLM_PROVIDER=openai and OPENAI_API_KEY=sk-... to use the cloud model.
  # Both retrieval modes (pre-fetch and tool calling) work with either provider
  # because OpenAI's chat API and Ollama's are almost identical.
  provider    = tolower(Sys.getenv("LLM_PROVIDER", unset = "ollama")),
  openai_key  = Sys.getenv("OPENAI_API_KEY", unset = ""),
  openai_model = Sys.getenv("OPENAI_MODEL",  unset = "gpt-4o-mini"),
  openai_base = Sys.getenv("OPENAI_BASE",    unset = "https://api.openai.com")
)

.ollama_state <- new.env(parent = emptyenv())
.ollama_state$settings <- OLLAMA_DEFAULTS
.ollama_state$portal_context <- NULL

ollama_settings <- function() .ollama_state$settings

ollama_set_model <- function(model) {
  stopifnot(is.character(model), length(model) == 1, nzchar(model))
  .ollama_state$settings$model <- model
  invisible(model)
}

## --- Provider helpers ------------------------------------------------------
## "ollama" = local model server; "openai" = cloud benchmark via the API.
active_provider <- function() ollama_settings()$provider

## The model name that is actually in use, depending on the provider.
active_model <- function() {
  s <- ollama_settings()
  if (identical(s$provider, "openai")) s$openai_model else s$model
}

ollama_set_provider <- function(provider) {
  provider <- tolower(provider)
  stopifnot(provider %in% c("ollama", "openai"))
  .ollama_state$settings$provider <- provider
  invisible(provider)
}

ollama_set_openai_key <- function(key) {
  stopifnot(is.character(key), length(key) == 1, nzchar(key))
  .ollama_state$settings$openai_key <- key
  invisible(TRUE)
}

## Side-effects a tool can produce that the CHAT UI needs (not the model).
## A bar-chart spec, and a downloadable result table. Cleared at the start of
## every chat turn; read by the chat module after the reply.
.llm_side_effects <- new.env(parent = emptyenv())
.llm_side_effects$chart <- NULL
.llm_side_effects$table <- NULL
## Token usage accumulated over the current turn (a turn may involve several
## API round-trips when tools are called, so these ADD UP across the turn).
.llm_side_effects$tokens_in  <- 0
.llm_side_effects$tokens_out <- 0
.llm_side_effects$api_calls  <- 0
.llm_side_effects$searched   <- FALSE
.llm_side_effects$openai_tokens_in <- 0
.llm_side_effects$openai_cached_tokens <- 0
.llm_side_effects$openai_tokens_out <- 0
.llm_side_effects$openai_api_calls <- 0
.llm_side_effects$openai_model_cost_usd <- 0
.llm_side_effects$openai_web_search_cost_usd <- 0
.llm_side_effects$openai_cost_known <- TRUE
.llm_side_effects$web_search_calls <- 0

ollama_reset_side_effects <- function() {
  .llm_side_effects$chart <- NULL
  .llm_side_effects$table <- NULL
  .llm_side_effects$tokens_in  <- 0
  .llm_side_effects$tokens_out <- 0
  .llm_side_effects$api_calls  <- 0
  .llm_side_effects$searched   <- FALSE
  .llm_side_effects$openai_tokens_in <- 0
  .llm_side_effects$openai_cached_tokens <- 0
  .llm_side_effects$openai_tokens_out <- 0
  .llm_side_effects$openai_api_calls <- 0
  .llm_side_effects$openai_model_cost_usd <- 0
  .llm_side_effects$openai_web_search_cost_usd <- 0
  .llm_side_effects$openai_cost_known <- TRUE
  .llm_side_effects$web_search_calls <- 0
  invisible(TRUE)
}

## Token usage for the last turn: prompt (in), completion (out), total, and
## how many API round-trips it took. Used for benchmarking and cost estimates.
ollama_last_usage <- function() {
  token_cost <- .llm_side_effects$openai_model_cost_usd
  search_cost <- .llm_side_effects$openai_web_search_cost_usd
  total_cost <- if (isTRUE(.llm_side_effects$openai_cost_known) &&
                    is.finite(token_cost) && is.finite(search_cost)) {
    token_cost + search_cost
  } else {
    NA_real_
  }
  list(
    tokens_in  = .llm_side_effects$tokens_in,
    tokens_out = .llm_side_effects$tokens_out,
    tokens_total = .llm_side_effects$tokens_in + .llm_side_effects$tokens_out,
    api_calls  = .llm_side_effects$api_calls,
    searched   = .llm_side_effects$searched,
    openai_tokens_in = .llm_side_effects$openai_tokens_in,
    openai_cached_tokens = .llm_side_effects$openai_cached_tokens,
    openai_tokens_out = .llm_side_effects$openai_tokens_out,
    openai_api_calls = .llm_side_effects$openai_api_calls,
    web_search_calls = .llm_side_effects$web_search_calls,
    openai_model_cost_usd = token_cost,
    openai_web_search_cost_usd = search_cost,
    estimated_openai_cost_usd = total_cost,
    openai_cost_known = .llm_side_effects$openai_cost_known
  )
}

## Text-token prices in US dollars per one million tokens. GPT-4o and GPT-4o
## mini are configured below. For another model, set all three environment
## variables using that model's current official OpenAI prices.
.openai_prices_per_million <- function(model) {
  custom <- suppressWarnings(c(
    input = as.numeric(Sys.getenv("OPENAI_INPUT_PRICE_PER_1M", unset = NA)),
    cached = as.numeric(Sys.getenv("OPENAI_CACHED_INPUT_PRICE_PER_1M", unset = NA)),
    output = as.numeric(Sys.getenv("OPENAI_OUTPUT_PRICE_PER_1M", unset = NA))
  ))
  if (all(is.finite(custom)) && all(custom >= 0)) return(custom)
  if (grepl("^gpt-4o-mini($|-)", model)) {
    return(c(input = 0.15, cached = 0.075, output = 0.60))
  }
  if (grepl("^gpt-4o($|-)", model)) {
    return(c(input = 2.50, cached = 1.25, output = 10.00))
  }
  NULL
}

.usage_number <- function(value, fallback = 0) {
  if (is.null(value) || length(value) == 0) return(fallback)
  number <- suppressWarnings(as.numeric(value[[1]]))
  if (!is.finite(number)) fallback else number
}

## A Responses API result can contain more than one hosted web-search action.
## Count the actual search actions because each one has a separate tool fee.
.count_web_search_calls <- function(parsed) {
  if (is.null(parsed$output) || length(parsed$output) == 0) return(0)
  sum(vapply(parsed$output, function(item) {
    type <- if (is.null(item$type)) "" else as.character(item$type[[1]])
    action_type <- if (is.null(item$action) || is.null(item$action$type)) {
      ""
    } else {
      as.character(item$action$type[[1]])
    }
    identical(type, "web_search_call") &&
      action_type %in% c("", "search")
  }, logical(1)))
}

## Record usage from one API response. Handles both OpenAI's naming
## (prompt_tokens/completion_tokens or input_tokens/output_tokens) and Ollama's
## (prompt_eval_count/eval_count). OpenAI costs are calculated per call, so a
## turn that mixes local Ollama with an OpenAI web search is still costed
## correctly.
.record_usage <- function(parsed, provider = active_provider(),
                          model = active_model(), web_search = FALSE) {
  .llm_side_effects$api_calls <- .llm_side_effects$api_calls + 1
  u <- parsed$usage
  tin <- tout <- 0
  if (!is.null(u)) {
    tin <- .usage_number(u$prompt_tokens %||% u$input_tokens)
    tout <- .usage_number(u$completion_tokens %||% u$output_tokens)
  }
  # Ollama reports counts at the top level instead of a usage object
  if (tin == 0 && !is.null(parsed$prompt_eval_count)) {
    tin <- as.numeric(parsed$prompt_eval_count)
  }
  if (tout == 0 && !is.null(parsed$eval_count)) {
    tout <- as.numeric(parsed$eval_count)
  }
  .llm_side_effects$tokens_in  <- .llm_side_effects$tokens_in  + tin
  .llm_side_effects$tokens_out <- .llm_side_effects$tokens_out + tout

  if (identical(provider, "openai")) {
    cached <- 0
    if (!is.null(u)) {
      details <- u$prompt_tokens_details %||% u$input_tokens_details
      if (!is.null(details)) cached <- .usage_number(details$cached_tokens)
    }
    cached <- min(max(cached, 0), tin)

    .llm_side_effects$openai_api_calls <-
      .llm_side_effects$openai_api_calls + 1
    .llm_side_effects$openai_tokens_in <-
      .llm_side_effects$openai_tokens_in + tin
    .llm_side_effects$openai_cached_tokens <-
      .llm_side_effects$openai_cached_tokens + cached
    .llm_side_effects$openai_tokens_out <-
      .llm_side_effects$openai_tokens_out + tout

    prices <- .openai_prices_per_million(model)
    if (is.null(prices)) {
      .llm_side_effects$openai_model_cost_usd <- NA_real_
      .llm_side_effects$openai_cost_known <- FALSE
    } else if (is.finite(.llm_side_effects$openai_model_cost_usd)) {
      uncached <- tin - cached
      call_cost <- (
        uncached * prices[["input"]] +
        cached  * prices[["cached"]] +
        tout    * prices[["output"]]
      ) / 1e6
      .llm_side_effects$openai_model_cost_usd <-
        .llm_side_effects$openai_model_cost_usd + call_cost
    }

    if (isTRUE(web_search)) {
      search_calls <- .count_web_search_calls(parsed)
      .llm_side_effects$web_search_calls <-
        .llm_side_effects$web_search_calls + search_calls
      price_per_1k <- suppressWarnings(as.numeric(Sys.getenv(
        "OPENAI_WEB_SEARCH_PRICE_PER_1K", unset = "10")))
      if (is.finite(price_per_1k) && price_per_1k >= 0) {
        .llm_side_effects$openai_web_search_cost_usd <-
          .llm_side_effects$openai_web_search_cost_usd +
          search_calls * price_per_1k / 1000
      } else {
        .llm_side_effects$openai_web_search_cost_usd <- NA_real_
        .llm_side_effects$openai_cost_known <- FALSE
      }
    }
  }
  invisible(TRUE)
}

## Null-coalescing helper used above.
`%||%` <- function(a, b) if (is.null(a)) b else a
## Returns the bar-chart spec produced during the last turn, or NULL.
ollama_last_chart <- function() .llm_side_effects$chart
## Returns list(name=, data=<data.frame>) of the last result table, or NULL.
ollama_last_table <- function() .llm_side_effects$table

## Called by data tools to make their full result downloadable as CSV.
## The model still receives only its (possibly capped) text summary; the
## FULL table is stored here for the download button.
.stash_result_table <- function(name, df) {
  if (is.data.frame(df) && nrow(df) > 0) {
    .llm_side_effects$table <- list(name = name, data = df)
  }
  invisible(TRUE)
}

## Normalise an assistant message from EITHER provider into one shape:
##   list(content = <string|NULL>,
##        tool_calls = list(list(id=, name=, arguments=<R list>), ...),
##        raw = <the provider's own message, to echo back into history>)
.normalise_assistant <- function(asst) {
  raw_tcs <- asst$tool_calls
  norm <- list()
  if (!is.null(raw_tcs) && length(raw_tcs) > 0) {
    norm <- lapply(raw_tcs, function(tc) {
      fn   <- tc[["function"]]
      args <- fn$arguments
      # OpenAI sends arguments as a JSON STRING; Ollama as an object.
      if (is.character(args) && length(args) == 1) {
        args <- tryCatch(jsonlite::fromJSON(args, simplifyVector = FALSE),
                         error = function(e) list())
      }
      list(id        = if (!is.null(tc$id)) tc$id else NULL,
           name      = fn$name,
           arguments = args)
    })
  }
  list(content = asst$content, tool_calls = norm, raw = asst)
}

## ONE function that sends a chat request to whichever provider is active and
## returns a normalised assistant message. `tools` is optional (NULL = no
## tools, used by the pre-fetch path).
.llm_chat_post <- function(messages, tools = NULL,
                           timeout_s = ollama_settings()$timeout_s) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Missing required packages: install 'httr' and 'jsonlite'.")
  }
  s <- ollama_settings()

  if (identical(s$provider, "openai")) {
    if (!nzchar(s$openai_key)) {
      stop("OpenAI provider selected but OPENAI_API_KEY is not set.")
    }
    body <- list(model = s$openai_model, messages = messages,
                 temperature = s$temperature)
    if (!is.null(tools)) body$tools <- tools
    resp <- tryCatch(
      httr::POST(
        url = paste0(s$openai_base, "/v1/chat/completions"),
        body = body, encode = "json",
        httr::add_headers(Authorization = paste("Bearer", s$openai_key)),
        httr::content_type_json(), httr::timeout(timeout_s)),
      error = function(e) stop(sprintf("Cannot reach OpenAI (%s).",
                                       conditionMessage(e))))
    if (httr::status_code(resp) >= 300) {
      b <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                    error = function(e) "")
      stop(sprintf("OpenAI returned HTTP %d. %s",
                   httr::status_code(resp), substr(b, 1, 300)))
    }
    parsed <- jsonlite::fromJSON(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE)
    .record_usage(parsed, provider = "openai", model = s$openai_model)
    return(.normalise_assistant(parsed$choices[[1]]$message))
  }

  # ---- default: Ollama ----
  body <- list(model = s$model, messages = messages, stream = FALSE,
               options = list(temperature = s$temperature, num_ctx = s$num_ctx))
  if (!is.null(tools)) body$tools <- tools
  resp <- tryCatch(
    httr::POST(url = paste0(s$host, "/api/chat"),
               body = body, encode = "json",
               httr::content_type_json(), httr::timeout(timeout_s)),
    error = function(e) stop(sprintf("Cannot reach Ollama at %s (%s).",
                                     s$host, conditionMessage(e))))
  if (httr::status_code(resp) >= 300) {
    b <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                  error = function(e) "")
    stop(sprintf("Ollama returned HTTP %d. %s",
                 httr::status_code(resp), substr(b, 1, 300)))
  }
  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE)
  .record_usage(parsed, provider = "ollama", model = s$model)
  .normalise_assistant(parsed$message)
}

## Append a tool RESULT message in the format the active provider expects.
.append_tool_result <- function(messages, tool_call, result) {
  if (identical(active_provider(), "openai")) {
    c(messages, list(list(role = "tool",
                          tool_call_id = tool_call$id,
                          content = result)))
  } else {
    c(messages, list(list(role = "tool",
                          name = tool_call$name,
                          content = result)))
  }
}

OLLAMA_BASE_SYSTEM_PROMPT <- paste(
  "You are the AI Assistant embedded inside the IMPC Disease Models Portal,",
  "a Shiny application that surfaces phenotypic similarity between human",
  "disease genes and their mouse knockout orthologs (computed with the",
  "PhenoDigm algorithm).",
  "",
  "Behaviour rules:",
  "- Answer concisely and in plain English suitable for non-technical users.",
  "- When the user asks about portal pages or navigation, refer to the tab",
  "  names exactly as shown in the sidebar: Home, PhenoDigm Scores,",
  "  Gene Summary, PhenoDigm Other, Publication, AI Assistant.",
  "- When the user asks about the data itself (counts, columns, example genes),",
  "  use ONLY the figures provided in the 'Portal data context' block below.",
  "  Never invent numbers. If the answer is not in that block, say so and",
  "  point the user at the relevant tab.",
  "- When the user asks about a specific gene by symbol (e.g. SPATA16, ZPR1),",
  "  ALWAYS check the 'Verified gene facts' block FIRST. That block contains",
  "  the exact row from the gene_summary table for that gene, including",
  "  disorder IDs (OMIM / Orphanet), disorder names, IMPC pipeline status,",
  "  whether the gene has phenotypes, the PhenoDigm match flag, and the",
  "  max PhenoDigm score. Quote the IDs and names verbatim. If a gene's",
  "  fact line is present, do NOT say 'data not provided'.",
  "- If 'PhenoDigm match = no' in the verified facts, the gene will NOT",
  "  appear in the PhenoDigm Scores or PhenoDigm Other tables. State this",
  "  directly rather than telling the user to 'go and check'.",
  "- CRITICAL: 'PhenoDigm match = no' applies ONLY to those two tables. If a",
  "  gene has a verified fact line at all, it IS present in the portal, in",
  "  the Gene Summary table. NEVER say a gene is 'not included in the app',",
  "  'not in the portal' or 'not present' on the basis of a PhenoDigm match",
  "  of no. Say instead that it is in the Gene Summary table but has no",
  "  PhenoDigm match, so it does not appear in the two PhenoDigm tables.",
  "- When the user mentions a disorder ID (e.g. OMIM:104200, ORPHA:404466),",
  "  ALWAYS check the 'Verified disorder facts' block. It lists every",
  "  matched mouse model with PhenoDigm score, HP (human phenotype) terms,",
  "  MP (mouse phenotype) terms, and which table the match comes from.",
  "  Quote IDs and scores verbatim. If the block says 'Not present in",
  "  PhenoDigm Scores or PhenoDigm Other tables', state that directly.",
  "- When the user mentions phenotype terms (HP:NNNNNNN or MP:NNNNNNN),",
  "  ALWAYS check the 'Verified phenotype facts' block. It already contains",
  "  per-term counts AND the union ('at least one') and intersection ('all of",
  "  them') counts across the supplied terms. Do NOT recompute these counts",
  "  yourself -- just quote the numbers from the block.",
  "- When the user asks aggregate questions about score thresholds (e.g.",
  "  'how many disorders/genes have a similarity score > 90'), read the",
  "  numbers from the 'PhenoDigm Scores table - counts by score threshold'",
  "  or the 'PhenoDigm Other table - counts by score threshold' sections.",
  "  Specify which table you are quoting from in your reply.",
  "- Disorder names mentioned without an OMIM/Orphanet ID (e.g. 'ADHD',",
  "  'Alzheimer disease') will not trigger a disorder fact lookup. Ask the",
  "  user to provide the OMIM or Orphanet identifier in that case.",
  "- Do not claim to run live queries against the database.",
  "- COUNTS: 'how many mouse models / genes / disorders' means DISTINCT",
  "  entities, not rows. When a fact block gives both a row count and a",
  "  distinct count, quote the DISTINCT one for a 'how many' question",
  "  (you may add the row count in brackets).",
  "  A distinct mouse model means a distinct model description (the",
  "  line/allele, zygosity and life-stage label), NOT a distinct gene or",
  "  MGI gene identifier.",
  "- If a question does not say which table and the answer differs between",
  "  PhenoDigm Scores and PhenoDigm Other, give both, each clearly labelled.",
  "- If the answer is a list of 3 or more models/genes/disorders, format it",
  "  as a short Markdown table.",
  "- Keep prose replies concise; tables may be longer as needed.",
  sep = "\n"
)

# Build a short factual summary of the loaded data. We compute this once per
# R session because the .fst files are read-only release data; recomputing on
## every chat turn would be wasteful.
build_portal_context <- function(force = FALSE) {
  if (!force && !is.null(.ollama_state$portal_context)) {
    return(.ollama_state$portal_context)
  }

  ctx <- tryCatch({
    if (!exists("load_gene_summary", mode = "function")) {
      source("read_data.R", local = FALSE)
    }
    gene_summary <- load_gene_summary()
    phenodigm    <- load_phenodigm()
    phenodigm_o  <- load_phenodigm_other()

    n_genes        <- nrow(gene_summary)
    n_disease      <- sum(gene_summary$disorder_id != "-", na.rm = TRUE)
    n_in_pipeline  <- sum(gene_summary$IMPC_pipeline == "yes", na.rm = TRUE)
    n_with_pheno   <- sum(gene_summary$IMPC_phenotypes == "yes", na.rm = TRUE)
    n_pdigm_match  <- sum(gene_summary$PhenoDigm_match == "yes", na.rm = TRUE)
    max_score      <- suppressWarnings(max(gene_summary$max_score, na.rm = TRUE))
    n_disorders    <- length(unique(phenodigm$disorder_id))
    sample_genes   <- head(unique(gene_summary$gene_symbol[
      gene_summary$PhenoDigm_match == "yes"]), 10)

    # Score-threshold aggregates against the PhenoDigm Scores table
    score_thr <- function(df, t, col = "score") {
      v <- suppressWarnings(as.numeric(df[[col]]))
      list(
        rows      = sum(v > t, na.rm = TRUE),
        disorders = length(unique(df$disorder_id[v > t & !is.na(v)])),
        genes     = length(unique(df$gene_symbol [v > t & !is.na(v)]))
      )
    }
    thr_main_40 <- score_thr(phenodigm, 40)
    thr_main_60 <- score_thr(phenodigm, 60)
    thr_main_80 <- score_thr(phenodigm, 80)
    thr_main_90 <- score_thr(phenodigm, 90)
    thr_main_95 <- score_thr(phenodigm, 95)
    # phenodigm_other uses `query` instead of disorder_id for the disorder column
    score_thr_other <- function(df, t) {
      v <- suppressWarnings(as.numeric(df$score))
      list(
        rows      = sum(v > t, na.rm = TRUE),
        disorders = length(unique(df$query[v > t & !is.na(v)])),
        genes     = length(unique(df$gene_symbol[v > t & !is.na(v)]))
      )
    }
    thr_other_40 <- score_thr_other(phenodigm_o, 40)
    thr_other_60 <- score_thr_other(phenodigm_o, 60)
    thr_other_80 <- score_thr_other(phenodigm_o, 80)
    thr_other_90 <- score_thr_other(phenodigm_o, 90)
    thr_other_95 <- score_thr_other(phenodigm_o, 95)
    n_pdo_rows <- nrow(phenodigm_o)

    paste(
      "Portal data context (IMPC Data Release 20.1):",
      "",
      "Aggregate counts:",
      sprintf("- Total genes tracked: %d", n_genes),
      sprintf("- Genes with a human disease association: %d", n_disease),
      sprintf("- Genes that entered the IMPC phenotyping pipeline: %d", n_in_pipeline),
      sprintf("- Genes with associated abnormal mouse phenotypes: %d", n_with_pheno),
      sprintf("- Genes with at least one PhenoDigm match: %d", n_pdigm_match),
      sprintf("- Distinct OMIM/Orphanet disorders covered: %d", n_disorders),
      sprintf("- Example matched genes: %s",
              paste(sample_genes, collapse = ", ")),
      "",
      "Range of the PhenoDigm score column:",
      sprintf("- Maximum value observed in the dataset: %.2f (out of 100).",
              max_score),
      "- IMPORTANT: this is the max across rows. The 'score' column itself",
      "  holds the per-row match score, NOT a fixed maximum. Do not describe",
      "  the 'score' column as 'the maximum observed score'.",
      "",
      "PhenoDigm Scores table — counts by score threshold:",
      sprintf("- rows with score > 40 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_main_40$rows, thr_main_40$disorders, thr_main_40$genes),
      sprintf("- rows with score > 60 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_main_60$rows, thr_main_60$disorders, thr_main_60$genes),
      sprintf("- rows with score > 80 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_main_80$rows, thr_main_80$disorders, thr_main_80$genes),
      sprintf("- rows with score > 90 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_main_90$rows, thr_main_90$disorders, thr_main_90$genes),
      sprintf("- rows with score > 95 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_main_95$rows, thr_main_95$disorders, thr_main_95$genes),
      "",
      sprintf("PhenoDigm Other table — counts by score threshold (total rows: %d):",
              n_pdo_rows),
      sprintf("- rows with score > 40 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_other_40$rows, thr_other_40$disorders, thr_other_40$genes),
      sprintf("- rows with score > 60 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_other_60$rows, thr_other_60$disorders, thr_other_60$genes),
      sprintf("- rows with score > 80 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_other_80$rows, thr_other_80$disorders, thr_other_80$genes),
      sprintf("- rows with score > 90 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_other_90$rows, thr_other_90$disorders, thr_other_90$genes),
      sprintf("- rows with score > 95 : %d  (distinct disorders=%d, distinct genes=%d)",
              thr_other_95$rows, thr_other_95$disorders, thr_other_95$genes),
      "",
      "Gene Summary table — columns and what each one is:",
      "- gene_symbol        : human gene symbol (e.g. SPATA16)",
      "- hgnc_id            : HGNC identifier (e.g. HGNC:13189)",
      "- mgi_id             : mouse ortholog MGI identifier",
      "- disorder_id        : OMIM/Orphanet ID(s), pipe-separated if multiple",
      "- disorder_name      : disorder name(s), pipe-separated if multiple",
      "- IMPC_pipeline      : 'yes' or 'no' flag",
      "- IMPC_phenotypes    : 'yes' or 'no' flag (abnormal mouse phenotypes)",
      "- HPO_phenotypes     : 'yes' or 'no' flag (whether HPO terms exist)",
      "- PhenoDigm_match    : 'yes' or 'no' flag",
      "- max_score          : highest PhenoDigm score for this gene (or NA)",
      "",
      "PhenoDigm Scores table — columns and what each one is:",
      "- disorder_id        : OMIM/Orphanet ID for this row",
      "- disorder_name      : disorder name for this row",
      "- gene_symbol        : human gene symbol",
      "- description        : mouse-model description (line + zygosity)",
      "- score              : PhenoDigm similarity score for this row",
      "- query_phenotype    : comma-separated HP terms (human phenotypes)",
      "- match_phenotype    : comma-separated MP terms (mouse phenotypes)",
      "",
      "PhenoDigm Other table — columns and what each one is:",
      "- description        : mouse-model description",
      "- mgi_id             : mouse gene MGI id",
      "- gene_symbol        : human ortholog gene symbol",
      "- query              : the disorder ID being queried (OMIM/Orphanet)",
      "- disorder_name      : disorder name",
      "- score              : PhenoDigm similarity score",
      "- query_phenotype    : comma-separated HP terms (human phenotypes)",
      "- match_phenotype    : comma-separated MP terms (mouse phenotypes)",
      "  (This table contains additional matches that did not meet the",
      "  primary PhenoDigm Scores criteria but are still informative.)",
      sep = "\n"
    )
  }, error = function(e) {
    paste("Portal data context unavailable:", conditionMessage(e))
  })

  .ollama_state$portal_context <- ctx
  ctx
}

## Small helper: look up a gene by symbol in the loaded data and return a
## factual multi-line summary. Used to inject *specific* facts into the prompt
## when the user mentions a known gene -- keeps the model honest without
## needing full tool-calling.
##
## Disorder IDs and names in the data may contain multiple values joined by
## a pipe character (e.g. "OMIM:617712|ORPHA:404466"). We split them so the
## model sees each disorder as its own line.
lookup_gene_fact <- function(gene_symbol) {
  if (!nzchar(gene_symbol)) return(NULL)
  out <- tryCatch({
    if (!exists("load_gene_summary", mode = "function")) source("read_data.R")
    df <- load_gene_summary()
    row <- df[toupper(df$gene_symbol) == toupper(gene_symbol), , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row <- row[1, ]

    # Build the disorders block (zero, one, or many disorders)
    if (is.na(row$disorder_id) || row$disorder_id == "-") {
      disorders_text <- "  - none"
      n_disorders <- 0
    } else {
      ids   <- strsplit(row$disorder_id,   "\\|", fixed = FALSE)[[1]]
      names <- strsplit(row$disorder_name, "\\|", fixed = FALSE)[[1]]
      n_disorders <- length(ids)
      k <- min(length(ids), length(names))
      disorders_text <- paste0(
        sprintf("  - %s  (%s)", names[seq_len(k)], ids[seq_len(k)]),
        collapse = "\n"
      )
    }

    max_score_str <- ifelse(
      is.na(row$max_score), "NA", as.character(round(row$max_score, 2))
    )

    # --- PhenoDigm row-level matches for this gene ---------------------
    # Pulls all rows where this gene appears in either PhenoDigm table,
    # and either lists them in full (if few) or summarises by score
    # threshold (if many). Keeps prompts bounded for genes like GPR173.
    pdm <- load_phenodigm()
    pdo <- load_phenodigm_other()
    m_main  <- pdm[pdm$gene_symbol == row$gene_symbol, , drop = FALSE]
    m_other <- pdo[pdo$gene_symbol == row$gene_symbol, , drop = FALSE]

    .matches_block <- function(df, source_label) {
      if (nrow(df) == 0) {
        return(sprintf("    (no rows in %s)", source_label))
      }
      # Sort by score descending
      df <- df[order(-as.numeric(df$score)), , drop = FALSE]
      n <- nrow(df)
      n_distinct <- length(unique(df$disorder_name))
      thr_counts <- sapply(c(40, 60, 80, 90),
        function(t) sum(as.numeric(df$score) > t))
      header <- sprintf(
        "    rows=%d, distinct disorders=%d, scores >40=%d, >60=%d, >80=%d, >90=%d",
        n, n_distinct, thr_counts[1], thr_counts[2], thr_counts[3], thr_counts[4])
      if (n <= 10) {
        body <- paste(sprintf("    - %s | %s | score=%.2f",
          df$disorder_name, df$disorder_id %||% df$query,
          as.numeric(df$score)), collapse = "\n")
      } else {
        top <- head(df, 10)
        body <- paste(
          "    Top 10 by score:",
          paste(sprintf("    - %s | %s | score=%.2f",
            top$disorder_name,
            if (!is.null(top$disorder_id)) top$disorder_id else top$query,
            as.numeric(top$score)), collapse = "\n"),
          sep = "\n")
      }
      paste(header, body, sep = "\n")
    }

    `%||%` <- function(a, b) if (is.null(a)) b else a

    paste(
      sprintf("Lookup for %s:", row$gene_symbol),
      sprintf("  HGNC id:          %s", row$hgnc_id),
      sprintf("  MGI id:           %s", row$mgi_id),
      sprintf("  Disorders (%d):",     n_disorders),
      disorders_text,
      sprintf("  In IMPC pipeline: %s", row$IMPC_pipeline),
      sprintf("  Mouse phenotypes: %s", row$IMPC_phenotypes),
      sprintf("  PhenoDigm match:  %s", row$PhenoDigm_match),
      sprintf("  Max PhenoDigm:    %s", max_score_str),
      "  PhenoDigm Scores matches for this gene:",
      .matches_block(m_main,  "PhenoDigm Scores"),
      "  PhenoDigm Other matches for this gene:",
      .matches_block(m_other, "PhenoDigm Other"),
      sep = "\n"
    )
  }, error = function(e) NULL)
  out
}

## Look up one or more phenotype terms (HP:NNNNNNN / MP:NNNNNNN) across the
## phenodigm tables. Returns per-term counts plus union/intersection across
## ALL of the provided terms when there is more than one -- this avoids
## asking the model to do set arithmetic over long ID lists.
lookup_phenotype_facts <- function(terms) {
  terms <- unique(terms[nzchar(terms)])
  if (length(terms) == 0) return(NULL)
  tryCatch({
    if (!exists("load_phenodigm",       mode = "function")) source("read_data.R")
    if (!exists("load_phenodigm_other", mode = "function")) source("read_data.R")
    pdm <- load_phenodigm()
    pdo <- load_phenodigm_other()

    is_mp <- grepl("^MP:", terms)
    # MP terms live in match_phenotype, HP terms in query_phenotype
    col_main  <- ifelse(is_mp, "match_phenotype", "query_phenotype")
    col_other <- col_main  # phenodigm_other uses the same column names

    # For each term: which rows mention it? Return a logical vector per table.
    row_hits_main  <- lapply(seq_along(terms), function(i) {
      grepl(terms[i], pdm[[col_main[i]]], fixed = TRUE)
    })
    row_hits_other <- lapply(seq_along(terms), function(i) {
      grepl(terms[i], pdo[[col_other[i]]], fixed = TRUE)
    })

    per_term <- vapply(seq_along(terms), function(i) {
      sprintf("  - %s : %d rows in PhenoDigm Scores, %d rows in PhenoDigm Other",
              terms[i],
              sum(row_hits_main[[i]]),
              sum(row_hits_other[[i]]))
    }, character(1))

    # Union: rows where AT LEAST ONE term matches (per table)
    union_main  <- Reduce("|", row_hits_main)
    union_other <- Reduce("|", row_hits_other)
    # Intersection: rows where ALL terms match (per table)
    inter_main  <- Reduce("&", row_hits_main)
    inter_other <- Reduce("&", row_hits_other)

    # Distinct mouse models = distinct `description` values within hits
    n_union_models_main  <- length(unique(pdm$description[union_main]))
    n_union_models_other <- length(unique(pdo$description[union_other]))
    n_inter_models_main  <- length(unique(pdm$description[inter_main]))
    n_inter_models_other <- length(unique(pdo$description[inter_other]))

    # A few example models for the intersection (top by score, capped at 5)
    inter_examples <- character(0)
    if (any(inter_main)) {
      sub <- pdm[inter_main, c("gene_symbol", "description", "score")]
      sub <- sub[order(-sub$score), , drop = FALSE]
      inter_examples <- head(sprintf(
        "    %s | %s | score=%.2f", sub$gene_symbol, sub$description, sub$score
      ), 5)
    }

    paste(
      sprintf("Phenotype-term lookup for: %s", paste(terms, collapse = ", ")),
      "  Per-term counts:",
      paste(per_term, collapse = "\n"),
      "",
      sprintf("  UNION (at least ONE of the terms above):"),
      sprintf("    rows in PhenoDigm Scores: %d  (distinct mouse models: %d)",
              sum(union_main), n_union_models_main),
      sprintf("    rows in PhenoDigm Other:  %d  (distinct mouse models: %d)",
              sum(union_other), n_union_models_other),
      "",
      sprintf("  INTERSECTION (ALL %d terms present in the same row):", length(terms)),
      sprintf("    rows in PhenoDigm Scores: %d  (distinct mouse models: %d)",
              sum(inter_main), n_inter_models_main),
      sprintf("    rows in PhenoDigm Other:  %d  (distinct mouse models: %d)",
              sum(inter_other), n_inter_models_other),
      if (length(inter_examples) > 0)
        paste("  Intersection examples (PhenoDigm Scores, top by score):",
              paste(inter_examples, collapse = "\n"), sep = "\n")
      else "",
      sep = "\n"
    )
  }, error = function(e) NULL)
}

## Pull MP:NNNNNNN and HP:NNNNNNN tokens from a message.
.extract_phenotype_candidates <- function(text) {
  if (!nzchar(text)) return(character(0))
  m <- regmatches(text, gregexpr("\\b(MP|HP):[0-9]+\\b", text))[[1]]
  unique(m)
}

## Look up a disorder by OMIM / Orphanet ID across phenodigm_matches and
## phenodigm_other. Returns a multi-line factual block listing every gene
## model matched to that disorder, with score and phenotype counts.
## Capped at the top 10 matches by score to keep prompts bounded.
## List the actual mouse models matched to a disorder, with their model
## description, MGI id where available, gene, and score. One distinct model is
## one distinct `description` value (line/allele, zygosity and life-stage
## label). Supports a minimum-score or exact-score filter and lists results
## with a configurable cap.
## This is what answers "how many models score X for disorder Y", "name their
## MGI ids", and "is MGI:NNN among them".
lookup_disorder_models <- function(disorder_id, min_score = NULL,
                                   exact_score = NULL) {
  if (!nzchar(disorder_id)) return("No disorder ID supplied.")
  tryCatch({
    if (!exists("load_phenodigm",       mode = "function")) source("read_data.R")
    if (!exists("load_phenodigm_other", mode = "function")) source("read_data.R")
    pdm <- load_phenodigm()
    pdo <- load_phenodigm_other()

    # Scores table has no mgi_id column; Other does. Both tables identify the
    # actual mouse model in `description`, so retain it in the common frame.
    main <- pdm[pdm$disorder_id == disorder_id, , drop = FALSE]
    oth  <- pdo[pdo$query == disorder_id, , drop = FALSE]

    frames <- list()
    if (nrow(main) > 0) frames[[length(frames)+1]] <- data.frame(
      model = main$description, mgi = "-", gene = main$gene_symbol,
      score = suppressWarnings(as.numeric(main$score)),
      table = "PhenoDigm Scores", stringsAsFactors = FALSE)
    if (nrow(oth) > 0) frames[[length(frames)+1]] <- data.frame(
      model = oth$description, mgi = oth$mgi_id, gene = oth$gene_symbol,
      score = suppressWarnings(as.numeric(oth$score)),
      table = "PhenoDigm Other", stringsAsFactors = FALSE)
    if (length(frames) == 0) {
      return(sprintf("No mouse models found for disorder %s.", disorder_id))
    }
    df <- do.call(rbind, frames)

    filt_note <- ""
    if (!is.null(exact_score)) {
      df <- df[!is.na(df$score) & df$score == as.numeric(exact_score), ]
      filt_note <- sprintf(" with score exactly %g", as.numeric(exact_score))
    } else if (!is.null(min_score)) {
      df <- df[!is.na(df$score) & df$score > as.numeric(min_score), ]
      filt_note <- sprintf(" with score > %g", as.numeric(min_score))
    }
    if (nrow(df) == 0) {
      return(sprintf("No mouse models for %s%s.", disorder_id, filt_note))
    }
    df <- df[order(-df$score), , drop = FALSE]

    # A gene or MGI id can have several knockout lines/zygosity/life-stage
    # combinations. Those are different mouse models in the portal.
    key <- as.character(df$model)
    n_distinct <- length(unique(key))
    table_names <- c("PhenoDigm Scores", "PhenoDigm Other")
    table_counts <- vapply(
      table_names,
      function(table_name) {
        length(unique(df$model[df$table == table_name]))
      },
      integer(1)
    )

    shown <- df[!duplicated(key), , drop = FALSE]
    # Make the FULL de-duplicated result downloadable as CSV, regardless of
    # how many rows we go on to show the model in the text summary.
    .stash_result_table(
      sprintf("models_%s", gsub("[^A-Za-z0-9]+", "_", disorder_id)),
      data.frame(mgi_id = shown$mgi, gene_symbol = shown$gene,
                 score = shown$score, source_table = shown$table,
                 stringsAsFactors = FALSE))
    # List cap: high by default so full lists come through. The distinct
    # COUNT above is always exact regardless of this cap. Raise/lower via
    # the DISORDER_MODELS_MAX environment variable (a single disorder can
    # have up to ~1,550 models, so set it to e.g. 2000 to list every one).
    list_cap <- suppressWarnings(as.integer(
      Sys.getenv("DISORDER_MODELS_MAX", unset = "500")))
    if (is.na(list_cap) || list_cap < 1) list_cap <- 500
    trunc_note <- ""
    if (nrow(shown) > list_cap) {
      trunc_note <- sprintf(
        "\n(... %d more not shown. All %d are counted above; raise DISORDER_MODELS_MAX to list them all.)",
        nrow(shown) - list_cap, nrow(shown))
      shown <- shown[seq_len(list_cap), , drop = FALSE]
    }
    count_lines <- sprintf(
      "  - %s: %d distinct model description%s",
      table_names,
      table_counts,
      ifelse(table_counts == 1L, "", "s")
    )
    lines <- sprintf(
      "  - model=%s | MGI=%s | gene=%s | score=%.2f | %s",
      shown$model, shown$mgi, shown$gene, shown$score, shown$table
    )
    paste(
      sprintf("Mouse models for %s%s: %d distinct model description%s overall.",
              disorder_id, filt_note, n_distinct,
              ifelse(n_distinct == 1L, "", "s")),
      paste(count_lines, collapse = "\n"),
      paste(
        "Definition: one mouse model is one unique description",
        "(line/allele, zygosity and life-stage label)."
      ),
      paste(lines, collapse = "\n"),
      trunc_note,
      sep = "\n"
    )
  }, error = function(e) sprintf("ERROR: %s", conditionMessage(e)))
}

## Count the DISTINCT mouse models across SEVERAL disorders combined, at an
## optional score filter. This exists because the same model can be matched to
## more than one disorder (e.g. the same 55 models appear under both ADHD and
## ADHD-8), so the correct total is a set UNION, not the sum of per-disorder
## counts. Reports per-disorder counts AND the true distinct union.
lookup_models_union <- function(disorder_ids, min_score = NULL,
                                exact_score = NULL) {
  ids <- unique(as.character(unlist(disorder_ids)))
  ids <- ids[nzchar(ids)]
  if (length(ids) == 0) return("No disorder IDs supplied.")
  tryCatch({
    if (!exists("load_phenodigm",       mode = "function")) source("read_data.R")
    if (!exists("load_phenodigm_other", mode = "function")) source("read_data.R")
    pdm <- load_phenodigm()
    pdo <- load_phenodigm_other()

    keep_scores <- function(s) {
      s <- suppressWarnings(as.numeric(s))
      if (!is.null(exact_score)) !is.na(s) & s == as.numeric(exact_score)
      else if (!is.null(min_score)) !is.na(s) & s > as.numeric(min_score)
      else !is.na(s)
    }
    models_for <- function(id) {
      k <- character(0)
      oth  <- pdo[pdo$query == id, , drop = FALSE]
      if (nrow(oth) > 0) {
        models <- oth$description[keep_scores(oth$score)]
        if (length(models) > 0) k <- c(k, models)
      }
      main <- pdm[pdm$disorder_id == id, , drop = FALSE]
      if (nrow(main) > 0) {
        models <- main$description[keep_scores(main$score)]
        if (length(models) > 0) k <- c(k, models)
      }
      unique(k)
    }

    per <- lapply(ids, models_for)
    names(per) <- ids
    per_lines <- vapply(ids, function(id)
      sprintf("  - %s: %d distinct models", id, length(per[[id]])),
      character(1))
    union_keys  <- unique(unlist(per))
    filt <- if (!is.null(exact_score)) sprintf(" (score = %g)", as.numeric(exact_score))
            else if (!is.null(min_score)) sprintf(" (score > %g)", as.numeric(min_score))
            else ""

    paste(
      sprintf("Distinct mouse models across %d disorders%s:",
              length(ids), filt),
      paste(per_lines, collapse = "\n"),
      sprintf("  TRUE DISTINCT TOTAL (union, counting each model once): %d",
              length(union_keys)),
      "  Definition: one mouse model is one unique description",
      "  (line/allele, zygosity and life-stage label).",
      "  NOTE: do NOT add the per-disorder counts -- the same model can match",
      "  several disorders. The union above is the correct total.",
      sep = "\n"
    )
  }, error = function(e) sprintf("ERROR: %s", conditionMessage(e)))
}

lookup_disorder_fact <- function(disorder_id) {
  if (!nzchar(disorder_id)) return(NULL)
  out <- tryCatch({
    if (!exists("load_phenodigm",       mode = "function")) source("read_data.R")
    if (!exists("load_phenodigm_other", mode = "function")) source("read_data.R")
    pdm <- load_phenodigm()
    pdo <- load_phenodigm_other()

    m_main  <- pdm[pdm$disorder_id == disorder_id, , drop = FALSE]
    # phenodigm_other uses `query` as the disorder-id column
    m_other <- pdo[pdo$query == disorder_id, , drop = FALSE]

    if (nrow(m_main) == 0 && nrow(m_other) == 0) {
      return(sprintf(
        "Lookup for %s:\n  Not present in PhenoDigm Scores or PhenoDigm Other tables.",
        disorder_id))
    }

    .summarise_match <- function(row, source) {
      # Phenotype lists in the data are comma-separated strings
      hp <- if (is.na(row$query_phenotype) || row$query_phenotype == "")
              character(0) else strsplit(row$query_phenotype, ",", fixed = TRUE)[[1]]
      mp <- if (is.na(row$match_phenotype) || row$match_phenotype == "")
              character(0) else strsplit(row$match_phenotype, ",", fixed = TRUE)[[1]]
      sprintf(
        "  - %s | score=%.2f | model=%s | HP terms (%d): %s | MP terms (%d): %s | source=%s",
        row$gene_symbol,
        as.numeric(row$score),
        row$description,
        length(hp),
        paste(hp, collapse = ","),
        length(mp),
        paste(mp, collapse = ","),
        source
      )
    }

    main_lines  <- if (nrow(m_main)  > 0) vapply(seq_len(nrow(m_main)),
        function(i) .summarise_match(m_main[i, ],  "PhenoDigm Scores"),
        character(1)) else character(0)
    other_lines <- if (nrow(m_other) > 0) vapply(seq_len(nrow(m_other)),
        function(i) .summarise_match(m_other[i, ], "PhenoDigm Other"),
        character(1)) else character(0)

    all_lines <- c(main_lines, other_lines)
    # Cap at 10 to keep prompts bounded
    truncated_note <- ""
    if (length(all_lines) > 10) {
      truncated_note <- sprintf(
        "\n  (... %d more rows not shown; ask narrower question if needed.)",
        length(all_lines) - 10)
      all_lines <- all_lines[seq_len(10)]
    }

    disorder_name <- if (nrow(m_main) > 0) m_main$disorder_name[1]
                     else if (nrow(m_other) > 0) m_other$disorder_name[1]
                     else "(unknown)"
    main_model_count <- length(unique(m_main$description))
    other_model_count <- length(unique(m_other$description))
    overall_model_count <- length(unique(c(
      as.character(m_main$description),
      as.character(m_other$description)
    )))

    paste(
      sprintf("Lookup for %s (%s):", disorder_id, disorder_name),
      sprintf(
        paste0(
          "  Distinct mouse models: %d overall ",
          "(PhenoDigm Scores: %d, PhenoDigm Other: %d)"
        ),
        overall_model_count, main_model_count, other_model_count
      ),
      sprintf(
        "  Matching rows: %d total (PhenoDigm Scores: %d, PhenoDigm Other: %d)",
        nrow(m_main) + nrow(m_other), nrow(m_main), nrow(m_other)
      ),
      paste(
        "  Definition: one mouse model is one unique description",
        "(line/allele, zygosity and life-stage label)."
      ),
      "  Matches:",
      paste(all_lines, collapse = "\n"),
      truncated_note,
      sep = "\n"
    )
  }, error = function(e) NULL)
  out
}

## Pull DISORDER-ID-LIKE tokens (OMIM:NNNNNN, ORPHA:NNNNNN) out of a message.
.extract_disorder_candidates <- function(text) {
  if (!nzchar(text)) return(character(0))
  m <- regmatches(text, gregexpr("\\b(OMIM|ORPHA):[0-9]+\\b", text))[[1]]
  unique(m)
}

## Pull GENE-LIKE tokens out of the message (uppercase + digits, 2-10 chars).
## Cheap, deterministic, and good enough to catch references like "SPATA16".
## We also strip out common project / database acronyms that the regex would
## otherwise mis-classify as gene symbols (IMPC, OMIM, ...).
.GENE_TOKEN_BLOCKLIST <- c(
  "IMPC", "OMIM", "HPO", "MGI", "HGNC", "MP", "AI", "LLM", "DR",
  "PHENODIGM", "QMUL", "EBI", "NA"
)

.extract_gene_candidates <- function(text) {
  if (!nzchar(text)) return(character(0))
  m <- regmatches(text, gregexpr("\\b[A-Z][A-Z0-9]{1,9}\\b", text))[[1]]
  m <- unique(m)
  m[!m %in% .GENE_TOKEN_BLOCKLIST]
}

ollama_health <- function(host = ollama_settings()$host) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    return(list(ok = FALSE, message = "Package 'httr' is not installed."))
  }

  ## OpenAI provider: check the key is set and the API is reachable.
  if (identical(active_provider(), "openai")) {
    s <- ollama_settings()
    if (!nzchar(s$openai_key)) {
      return(list(ok = FALSE, message = "OPENAI_API_KEY is not set."))
    }
    resp <- tryCatch(
      httr::GET(paste0(s$openai_base, "/v1/models"),
                httr::add_headers(Authorization = paste("Bearer", s$openai_key)),
                httr::timeout(5)),
      error = function(e) e)
    if (inherits(resp, "error")) {
      return(list(ok = FALSE, message = "Cannot reach the OpenAI API."))
    }
    if (httr::status_code(resp) == 401) {
      return(list(ok = FALSE, message = "OpenAI rejected the API key (401)."))
    }
    if (httr::status_code(resp) >= 300) {
      return(list(ok = FALSE,
                  message = sprintf("OpenAI API HTTP %d", httr::status_code(resp))))
    }
    return(list(ok = TRUE, message = "OpenAI API reachable."))
  }

  resp <- tryCatch(
    httr::GET(paste0(host, "/api/tags"), httr::timeout(5)),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    return(list(ok = FALSE,
                message = paste("Cannot reach Ollama at", host)))
  }
  if (httr::status_code(resp) >= 300) {
    return(list(ok = FALSE,
                message = sprintf("Ollama responded with HTTP %d",
                                  httr::status_code(resp))))
  }
  list(ok = TRUE, message = "Ollama is reachable.")
}

call_ollama_chat <- function(message,
                             history = NULL,
                             model   = ollama_settings()$model,
                             host    = ollama_settings()$host,
                             timeout_s = ollama_settings()$timeout_s,
                             include_portal_context = TRUE,
                             extra_system = NULL) {

  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Missing required packages: install 'httr' and 'jsonlite'.")
  }
  if (!is.character(message) || length(message) != 1 ||
      !nzchar(trimws(message))) {
    stop("Message must be a non-empty string.")
  }

  ## Clear any chart/side-effects left over from the previous turn.
  ollama_reset_side_effects()

  ## If tool-calling mode is on, dispatch to the tool-calling implementation
  ## instead of the pre-fetch RAG path. Same signature, same return value.
  if (isTRUE(ollama_settings()$use_tools)) {
    return(call_ollama_chat_with_tools(
      message      = message,
      history      = history,
      model        = model,
      host         = host,
      timeout_s    = timeout_s,
      extra_system = extra_system
    ))
  }

  system_blocks <- OLLAMA_BASE_SYSTEM_PROMPT
  if (isTRUE(include_portal_context)) {
    system_blocks <- paste(system_blocks, "", build_portal_context(),
                           sep = "\n")
  }

  ## Inject any deterministic gene lookups for genes mentioned in the message.
  ## We use lapply (not vapply) because lookup_gene_fact returns NULL when the
  ## token isn't in the gene_summary table -- vapply would error on length 0.
  candidates <- .extract_gene_candidates(message)
  gene_facts <- unlist(lapply(candidates, lookup_gene_fact))
  gene_facts <- gene_facts[!is.na(gene_facts) & nzchar(gene_facts)]
  if (length(gene_facts) > 0) {
    system_blocks <- paste(system_blocks, "",
                           "Verified gene facts (use these verbatim):",
                           paste(gene_facts, collapse = "\n\n"),
                           sep = "\n")
  }

  ## Inject any deterministic disorder lookups for OMIM/Orphanet IDs in msg.
  disorder_candidates <- .extract_disorder_candidates(message)
  disorder_facts <- unlist(lapply(disorder_candidates, lookup_disorder_fact))
  disorder_facts <- disorder_facts[!is.na(disorder_facts) & nzchar(disorder_facts)]
  if (length(disorder_facts) > 0) {
    system_blocks <- paste(system_blocks, "",
                           "Verified disorder facts (use these verbatim):",
                           paste(disorder_facts, collapse = "\n\n"),
                           sep = "\n")
  }

  ## Inject phenotype-term lookup (MP:/HP:) with union & intersection counts.
  phenotype_candidates <- .extract_phenotype_candidates(message)
  if (length(phenotype_candidates) > 0) {
    phenotype_block <- lookup_phenotype_facts(phenotype_candidates)
    if (!is.null(phenotype_block) && nzchar(phenotype_block)) {
      system_blocks <- paste(system_blocks, "",
                             "Verified phenotype facts (use these verbatim):",
                             phenotype_block,
                             sep = "\n")
    }
  }

  if (!is.null(extra_system) && nzchar(extra_system)) {
    system_blocks <- paste(system_blocks, "", extra_system, sep = "\n")
  }

  messages <- list(list(role = "system", content = system_blocks))

  if (!is.null(history) && length(history) > 0) {
    max_h <- ollama_settings()$max_history
    if (length(history) > max_h) {
      history <- tail(history, max_h)
    }
    clean_history <- lapply(history, function(item) {
      list(role = as.character(item$role),
           content = as.character(item$content))
    })
    messages <- c(messages, clean_history)
  }
  messages <- c(messages,
                list(list(role = "user", content = trimws(message))))

  ## Send via whichever provider is active (Ollama local OR OpenAI cloud).
  res <- .llm_chat_post(messages, tools = NULL, timeout_s = timeout_s)
  assistant_text <- res$content
  if (is.null(assistant_text) || !nzchar(trimws(assistant_text))) {
    stop("Empty response from the model.")
  }
  trimws(assistant_text)
}


## =========================================================================
## TOOL-CALLING MODE
## -------------------------------------------------------------------------
## Instead of pre-fetching everything the model might need, expose a small
## menu of typed R functions as tools. The model decides which one(s) to
## call for each user question, sends back tool_call requests, we dispatch
## them, feed the results back, and loop until the model produces a final
## text answer.
##
## All tools call the SAME underlying R functions as the pre-fetch path
## (lookup_gene_fact, lookup_disorder_fact, lookup_phenotype_facts, ...)
## so the data semantics are identical -- only the *control flow* differs.
## =========================================================================

OLLAMA_TOOLCALL_SYSTEM_PROMPT <- paste(
  "You are the AI Assistant embedded inside the IMPC Disease Models Portal.",
  "The portal surfaces phenotypic similarity between human disease genes and",
  "their mouse knockout orthologs, computed by the PhenoDigm algorithm.",
  "",
  "You have access to TOOLS that query the portal's underlying data files.",
  "Use them for any factual question about genes, disorders, phenotypes,",
  "score thresholds, or table contents. Do NOT guess factual values --",
  "call a tool to retrieve them.",
  "",
  "Fixed facts you can state directly WITHOUT calling a tool:",
  "- The portal uses IMPC Data Release 20.1.",
  "- The three data tables are: Gene Summary, PhenoDigm Scores, and",
  "  PhenoDigm Other.",
  "",
  "NEVER write a tool call as plain text in your answer (e.g. do not type",
  "out JSON like {\"name\": \"get_portal_stats\"}). Either call the tool",
  "properly, or give a normal sentence answer.",
  "",
  "Behaviour rules:",
  "- Answer concisely in plain English for non-technical users.",
  "- For portal pages or navigation use the sidebar tab names exactly:",
  "  Home, PhenoDigm Scores, Gene Summary, PhenoDigm Other, Publication,",
  "  AI Assistant.",
  "- Always call the most specific available tool. Examples:",
  "    user mentions a gene symbol         -> get_gene_info",
  "    user mentions OMIM:N or ORPHA:N     -> get_disorder_info",
  "    'how many models / name the MGIs / is MGI:N one of them' for a",
  "      disorder (optionally at a score)  -> list_disorder_models",
  "    a count spanning SEVERAL disorders (e.g. multiple ADHD variants)",
  "      -> count_models_across_disorders (NEVER add the per-disorder",
  "      counts yourself; the same model can match several disorders).",
  "    user mentions MP:N or HP:N terms    -> search_phenotypes",
  "    user names a disorder without an ID -> find_disorders_by_name",
  "    'how many ... score > X'            -> score_threshold_counts",
  "    'which one(s)', 'show me', 'list the top N' -> get_top_matches",
  "    'what's in table X'                 -> get_table_columns",
  "    recent papers / news / newer IMPC releases / anything NOT in the",
  "      portal data                       -> web_search",
  "",
  "=== SOURCE ATTRIBUTION RULES (these are the most important rules) ===",
  "- THREE SOURCES, NEVER MIXED. Every factual statement you make comes from",
  "  exactly one of:",
  "    (a) PORTAL DATA  - returned by the portal tools. Authoritative.",
  "    (b) WEB          - returned by web_search. External, must be cited.",
  "    (c) YOUR OWN KNOWLEDGE - from training. LEAST reliable.",
  "- LABEL EVERY ANSWER. When an answer draws on more than one source, use",
  "  clear headings so the user can see which is which, e.g.:",
  "      **From the portal data:** ...",
  "      **From the web (with sources):** ...",
  "- ALWAYS CITE WEB CLAIMS. Any statement taken from web_search must be",
  "  followed by the source URL. If a web result returned no URLs, say",
  "  'no source URL was returned' rather than presenting it as verified.",
  "- NEVER present your own training knowledge as if it were portal data or",
  "  a cited web result. If you are drawing on general knowledge, say so",
  "  explicitly: 'From general background knowledge (not verified here): ...'",
  "- PORTAL QUESTIONS ARE NEVER ANSWERED FROM THE WEB. For any question",
  "  about the portal's own genes, disorders, phenotypes, models or scores,",
  "  the portal tools are the ONLY acceptable source. Do not search the web",
  "  for these, and never let a web result override a portal figure. If the",
  "  web and the portal disagree, report BOTH and say they disagree.",
  "- ASK BEFORE SEARCHING. If a question could be answered from the portal,",
  "  answer from the portal. If it genuinely needs outside information, and",
  "  the user has not already asked you to search, FIRST give what the",
  "  portal knows, then ASK: 'Would you like me to search the web for",
  "  this?' Only search without asking if the user explicitly requested a",
  "  search (e.g. 'search for', 'look up online', 'find papers').",
  "- IF YOU DO NOT KNOW, SAY SO. Never invent a figure, a gene, a disorder,",
  "  an identifier, a score, a paper title or a URL. An honest 'I don't",
  "  have that information' is always better than a plausible guess.",
  "=== END SOURCE ATTRIBUTION RULES ===",
  "- ENTITY-SPECIFIC ALWAYS WINS over aggregate. If a question contains a",
  "  specific gene symbol (e.g. GPR173) or disorder ID (e.g. OMIM:606392),",
  "  use get_gene_info / get_disorder_info FIRST, even when the question",
  "  also mentions score thresholds. get_gene_info already returns the",
  "  gene's per-threshold counts (>40, >60, >80, >90) for both PhenoDigm",
  "  tables -- you do NOT need to call score_threshold_counts as well.",
  "  Example: 'for how many disorders could the mouse model of GPR173 be",
  "  a good model at score > 60' -> call get_gene_info('GPR173') and read",
  "  the threshold counts from the PhenoDigm Other matches section.",
  "- IMPORTANT: score_threshold_counts returns GLOBAL counts only -- across",
  "  the entire table, not filtered by gene or disorder. Never use it to",
  "  answer a question about a specific gene or disorder.",
  "- score_threshold_counts returns COUNTS only. If the user asks 'which",
  "  one(s)' or 'show me' the actual rows, you MUST call get_top_matches",
  "  as well -- never invent the row contents from the count.",
  "- You MAY call multiple tools in sequence (e.g. find a disorder by name,",
  "  then call get_disorder_info with its ID).",
  "- find_disorders_by_name is a SUBSTRING match. If the user gives an",
  "  acronym (e.g. 'ADHD', 'ALS') and the first search returns nothing,",
  "  retry with the expanded name (e.g. 'Attention Deficit', 'Amyotrophic",
  "  Lateral Sclerosis').",
  "- If a tool returns 'not found' or an empty result after a reasonable",
  "  retry, report that fact honestly instead of inventing data.",
  "- COUNTS: 'how many mouse models / genes / disorders' means DISTINCT",
  "  entities, NOT the number of rows. A tool result may give both a row",
  "  count and a 'distinct ...' count -- for a 'how many models' question",
  "  quote the DISTINCT figure, and you may add the row count in brackets",
  "  for clarity, e.g. '3 mouse models (across 12 matching rows)'.",
  "  A distinct mouse model is a distinct model description (line/allele,",
  "  zygosity and life-stage label), not a distinct gene or MGI gene ID.",
  "- CLARIFY WHEN AMBIGUOUS: if a question does not say which table",
  "  (PhenoDigm Scores vs PhenoDigm Other) and the answer differs between",
  "  them, ask ONE short clarifying question before answering, OR give both",
  "  and label each clearly. Do the same if a gene/disorder name is unclear.",
  "- TABLES: when the answer is a list of several models, genes, or",
  "  disorders (roughly 3 or more), format it as a Markdown table with a",
  "  header row, so it is easy to read. Keep tables to the most relevant",
  "  columns (e.g. gene, mouse model, disorder, score).",
  "- CHARTS: if the user asks to 'chart', 'plot', 'graph', 'visualise', or",
  "  'show a bar chart' of some counts (e.g. how many models/disorders",
  "  above score thresholds), FIRST get the numbers with the other tools,",
  "  THEN call make_bar_chart with matching labels and values. Still give a",
  "  one-line text summary -- the chart appears automatically below it.",
  "- If a list would be very long (more than ~20 rows), show the top ~15",
  "  by score and state how many more there are, rather than dumping",
  "  everything.",
  "- Keep prose answers concise; tables may be longer as needed.",
  sep = "\n"
)

## --- TOOL DEFINITIONS -----------------------------------------------------
## Each entry is a JSON-schema-shaped list that Ollama's /api/chat accepts
## under the `tools` payload field.

.tool_def <- function(name, description, properties = list(),
                      required = character(0)) {
  # jsonlite serialises an empty/unnamed list() as JSON array `[]`, but the
  # JSON Schema spec requires `properties` to be an OBJECT (`{}`) even when
  # empty. Force a named empty list so toJSON emits `{}`.
  if (length(properties) == 0 || is.null(names(properties))) {
    properties <- structure(list(), names = character(0))
  }
  list(
    type = "function",
    "function" = list(
      name        = name,
      description = description,
      parameters  = list(
        type       = "object",
        properties = properties,
        required   = as.list(required)
      )
    )
  )
}

OLLAMA_TOOLS <- list(
  .tool_def(
    name = "get_gene_info",
    description = paste(
      "Look up a human gene by symbol (e.g. SPATA16, COL4A3) in the",
      "gene_summary table and return its row plus a summary of all",
      "PhenoDigm Scores and PhenoDigm Other matches for that gene."),
    properties = list(
      symbol = list(type = "string",
                    description = "Human gene symbol, uppercase.")
    ),
    required = c("symbol")
  ),
  .tool_def(
    name = "get_disorder_info",
    description = paste(
      "Look up a disorder by OMIM or Orphanet identifier (e.g.",
      "OMIM:104200, ORPHA:404466). Returns every matched mouse model",
      "with score and phenotype lists from both PhenoDigm tables."),
    properties = list(
      disorder_id = list(type = "string",
                         description = "Disorder ID like 'OMIM:104200'.")
    ),
    required = c("disorder_id")
  ),
  .tool_def(
    name = "list_disorder_models",
    description = paste(
      "List the actual mouse models matched to a disorder, with model",
      "description, MGI id where available, gene, score, and separate counts",
      "for both PhenoDigm tables. A distinct model means a distinct model",
      "description, not a distinct MGI id or gene. Use this for 'how many",
      "models', 'name the MGI ids', or 'is MGI:NNN one of them'. Supports a score",
      "filter: min_score (score above a value) OR exact_score (score equal",
      "to a value, e.g. 100). Reports the true distinct-model count."),
    properties = list(
      disorder_id = list(type = "string",
                         description = "Disorder ID like 'OMIM:143465'."),
      min_score   = list(type = "number",
                         description = "Optional: only models scoring ABOVE this."),
      exact_score = list(type = "number",
                         description = "Optional: only models scoring EXACTLY this (e.g. 100).")
    ),
    required = c("disorder_id")
  ),
  .tool_def(
    name = "count_models_across_disorders",
    description = paste(
      "Count the TRUE distinct number of mouse models across SEVERAL",
      "disorders combined (a set union of model descriptions, not genes or",
      "MGI ids). Use this whenever a question spans",
      "more than one disorder ID -- for example several ADHD variants -- so",
      "that a model matched to two disorders is counted ONCE, not twice.",
      "Never add per-disorder counts yourself; call this instead."),
    properties = list(
      disorder_ids = list(type = "array", items = list(type = "string"),
                          description = "Disorder IDs, e.g. ['OMIM:143465','OMIM:619957']."),
      min_score    = list(type = "number",
                          description = "Optional: only models scoring ABOVE this."),
      exact_score  = list(type = "number",
                          description = "Optional: only models scoring EXACTLY this (e.g. 100).")
    ),
    required = c("disorder_ids")
  ),
  .tool_def(
    name = "search_phenotypes",
    description = paste(
      "Given one or more HP (human, HP:NNNNNNN) or MP (mouse, MP:NNNNNNN)",
      "phenotype term IDs, return per-term row counts plus the union",
      "('at least one') and intersection ('all of them') counts across",
      "both PhenoDigm tables."),
    properties = list(
      terms = list(
        type = "array",
        items = list(type = "string"),
        description = "Vector of HP:/MP: term identifiers."
      )
    ),
    required = c("terms")
  ),
  .tool_def(
    name = "find_disorders_by_name",
    description = paste(
      "Fuzzy-search the disorder_name column for a free-text query (e.g.",
      "'ADHD', 'Alport', 'Alzheimer'). Returns up to 10 distinct matched",
      "disorders with their OMIM/Orphanet IDs. Use this BEFORE",
      "get_disorder_info if the user only gave a disease name."),
    properties = list(
      query = list(type = "string",
                   description = "Free-text disorder name or substring.")
    ),
    required = c("query")
  ),
  .tool_def(
    name = "score_threshold_counts",
    description = paste(
      "Aggregate counts above a PhenoDigm score threshold in one of",
      "the two PhenoDigm tables. Returns number of rows, distinct",
      "disorders, and distinct genes above the threshold."),
    properties = list(
      threshold = list(type = "number",
                       description = "Score threshold (0-100)."),
      table = list(type = "string",
                   description = paste(
                     "Either 'phenodigm_scores' or 'phenodigm_other'.",
                     "Default 'phenodigm_scores' if omitted."))
    ),
    required = c("threshold")
  ),
  .tool_def(
    name = "get_top_matches",
    description = paste(
      "Return the actual TOP rows of one of the PhenoDigm tables, optionally",
      "filtered by a minimum score. Use this when the user asks 'which one',",
      "'show me', or 'list the top N' -- score_threshold_counts only gives",
      "counts; this gives the specific rows."),
    properties = list(
      table = list(type = "string",
                   description = "'phenodigm_scores' or 'phenodigm_other'."),
      top_n = list(type = "integer",
                   description = "How many rows to return. Default 5, max 25."),
      min_score = list(type = "number",
                       description = "Optional minimum score filter (0-100).")
    ),
    required = c("table")
  ),
  .tool_def(
    name = "get_table_columns",
    description = paste(
      "Return the column names and human-readable descriptions for one",
      "of the portal's data tables. Use this when the user asks 'what",
      "is in table X' or 'what columns does X have'."),
    properties = list(
      table = list(type = "string",
                   description = paste(
                     "One of: 'gene_summary', 'phenodigm_scores',",
                     "'phenodigm_other'."))
    ),
    required = c("table")
  ),
  .tool_def(
    name = "get_portal_stats",
    description = paste(
      "Return the high-level aggregate statistics of the portal:",
      "total genes, genes with disease association, genes in IMPC",
      "pipeline, distinct disorders covered, max PhenoDigm score,",
      "and example matched genes."),
    properties = list(),
    required = character(0)
  ),
  .tool_def(
    name = "web_search",
    description = paste(
      "Search the LIVE INTERNET for information that is not in the portal's",
      "data files -- for example recent publications about a gene or",
      "disorder, newer IMPC data releases, or background on a disease.",
      "Use this ONLY for information genuinely outside the portal: for any",
      "question about the portal's own genes, disorders, phenotypes or",
      "scores, use the portal data tools instead. Always report the sources",
      "returned."),
    properties = list(
      query   = list(type = "string",
                     description = "What to search the web for."),
      context = list(type = "string",
                     description = "Optional context from the portal session.")
    ),
    required = c("query")
  ),
  .tool_def(
    name = "make_bar_chart",
    description = paste(
      "Draw a bar chart to VISUALISE a small set of counts for the user",
      "(e.g. 'how many disorders/models above score 40, 60, 80'). First",
      "get the actual numbers using the other tools, THEN call this with",
      "matching labels and values. The chart is displayed to the user",
      "automatically -- you still give a short text summary as well."),
    properties = list(
      title  = list(type = "string",
                    description = "Short chart title."),
      labels = list(type = "array", items = list(type = "string"),
                    description = "Category labels, e.g. ['>40','>60','>80']."),
      values = list(type = "array", items = list(type = "number"),
                    description = "One number per label, same order.")
    ),
    required = c("title", "labels", "values")
  )
)

## Return the tool set that should be offered to the model.
##
## BENCHMARK MODE: set DISABLE_WEB_SEARCH=1 to remove `web_search` from the
## menu. This matters for controlled evaluation because web_search calls the
## OpenAI API regardless of which model is running -- so leaving it enabled
## would (a) make a "local" condition perform cloud calls, breaking the
## local-only claim, and (b) let the model answer portal questions from the
## web, contaminating the grounded-retrieval comparison. With it disabled the
## tool set matches the frozen benchmark build exactly.
.web_search_disabled <- function() {
  isTRUE(Sys.getenv("DISABLE_WEB_SEARCH", unset = "0") %in%
         c("1", "true", "TRUE", "yes"))
}

active_tools <- function() {
  if (.web_search_disabled()) {
    Filter(function(t) !identical(t[["function"]]$name, "web_search"),
           OLLAMA_TOOLS)
  } else {
    OLLAMA_TOOLS
  }
}

## --- TOOL DISPATCHER ------------------------------------------------------
## Maps a tool name + arguments object to the corresponding R function.
## Returns the tool result as a string (Ollama expects tool messages with
## a plain-text content field).

dispatch_tool <- function(name, args) {
  result <- tryCatch({
    if (name == "get_gene_info") {
      out <- lookup_gene_fact(as.character(args$symbol))
      if (is.null(out)) sprintf(
        "Gene '%s' not found in the gene_summary table.", args$symbol
      ) else out

    } else if (name == "get_disorder_info") {
      out <- lookup_disorder_fact(as.character(args$disorder_id))
      if (is.null(out)) sprintf(
        "Disorder ID '%s' not found.", args$disorder_id
      ) else out

    } else if (name == "list_disorder_models") {
      lookup_disorder_models(
        as.character(args$disorder_id),
        min_score   = if (is.null(args$min_score)) NULL else as.numeric(args$min_score),
        exact_score = if (is.null(args$exact_score)) NULL else as.numeric(args$exact_score))

    } else if (name == "count_models_across_disorders") {
      lookup_models_union(
        args$disorder_ids,
        min_score   = if (is.null(args$min_score)) NULL else as.numeric(args$min_score),
        exact_score = if (is.null(args$exact_score)) NULL else as.numeric(args$exact_score))

    } else if (name == "search_phenotypes") {
      terms <- as.character(unlist(args$terms))
      out <- lookup_phenotype_facts(terms)
      if (is.null(out)) "No phenotype terms supplied." else out

    } else if (name == "find_disorders_by_name") {
      .tool_find_disorders_by_name(as.character(args$query))

    } else if (name == "score_threshold_counts") {
      tbl <- if (is.null(args$table)) "phenodigm_scores" else as.character(args$table)
      .tool_score_threshold_counts(as.numeric(args$threshold), tbl)

    } else if (name == "get_top_matches") {
      top_n <- if (is.null(args$top_n)) 5L else as.integer(args$top_n)
      min_s <- if (is.null(args$min_score)) NULL else as.numeric(args$min_score)
      .tool_get_top_matches(as.character(args$table), top_n, min_s)

    } else if (name == "get_table_columns") {
      .tool_get_table_columns(as.character(args$table))

    } else if (name == "web_search") {
      # Hard block as well as removing it from the menu, so a model that
      # calls it from memory cannot reach the internet during a benchmark.
      if (.web_search_disabled()) {
        "Web search is disabled in this configuration. Answer using the portal data tools only."
      } else {
        .tool_web_search(as.character(args$query),
                         context = if (is.null(args$context)) NULL
                                   else as.character(args$context))
      }

    } else if (name == "get_portal_stats") {
      .tool_get_portal_stats()

    } else if (name == "make_bar_chart") {
      labs <- as.character(unlist(args$labels))
      vals <- suppressWarnings(as.numeric(unlist(args$values)))
      ttl  <- if (is.null(args$title)) "Chart" else as.character(args$title)
      if (length(labs) == 0 || length(labs) != length(vals) || any(is.na(vals))) {
        "ERROR: make_bar_chart needs equal-length labels and numeric values."
      } else {
        .llm_side_effects$chart <- list(title = ttl, labels = labs, values = vals)
        sprintf("Bar chart displayed to the user (%s). Data: %s.",
                ttl, paste(sprintf("%s = %g", labs, vals), collapse = ", "))
      }

    } else {
      sprintf("ERROR: Unknown tool name '%s'.", name)
    }
  }, error = function(e) {
    sprintf("ERROR running tool '%s': %s", name, conditionMessage(e))
  })
  if (is.null(result) || !nzchar(result)) {
    result <- sprintf("Tool '%s' returned an empty result.", name)
  }
  result
}

## --- TOOL HELPER FUNCTIONS ------------------------------------------------

## Common medical acronyms -> phrase to substring-search for in disorder
## names. Used by .tool_find_disorders_by_name to auto-retry when the user
## query is an acronym that doesn't itself appear in any disorder name.
.MEDICAL_ACRONYMS <- list(
  "ADHD"  = "Attention Deficit",
  "ALS"   = "Amyotrophic Lateral",
  "AD"    = "Alzheimer",
  "PD"    = "Parkinson",
  "MS"    = "Multiple Sclerosis",
  "IBD"   = "Inflammatory Bowel",
  "OCD"   = "Obsessive-Compulsive",
  "MND"   = "Motor Neuron",
  "CF"    = "Cystic Fibrosis",
  "ASD"   = "Autism Spectrum",
  "BD"    = "Bipolar",
  "HD"    = "Huntington",
  "DMD"   = "Duchenne Muscular Dystrophy",
  "BMD"   = "Becker Muscular Dystrophy",
  "SCD"   = "Sickle Cell",
  "CKD"   = "Chronic Kidney",
  "COPD"  = "Chronic Obstructive Pulmonary",
  "T1D"   = "Type 1 Diabetes",
  "T2D"   = "Type 2 Diabetes",
  "MODY"  = "Maturity-Onset Diabetes Of The Young",
  "PKU"   = "Phenylketonuria",
  "PCOS"  = "Polycystic Ovary",
  "RA"    = "Rheumatoid Arthritis",
  "OA"    = "Osteoarthritis"
)

.tool_find_disorders_by_name <- function(query, .allow_acronym_retry = TRUE) {
  if (!nzchar(query)) return("Empty query.")

  # Short acronyms (like "ALS" or "MS") match too many longer words via a
  # naive substring search. If the query is in our known acronym dict AND
  # short, expand it FIRST -- don't even try the literal substring search.
  if (isTRUE(.allow_acronym_retry) && nchar(query) <= 5) {
    expansion <- .MEDICAL_ACRONYMS[[toupper(query)]]
    if (!is.null(expansion)) {
      inner <- .tool_find_disorders_by_name(expansion,
                                            .allow_acronym_retry = FALSE)
      return(paste0(
        sprintf("(Acronym '%s' expanded to '%s' before searching.)\n",
                query, expansion),
        inner))
    }
  }
  if (!exists("load_phenodigm",       mode = "function")) source("read_data.R")
  if (!exists("load_phenodigm_other", mode = "function")) source("read_data.R")
  if (!exists("load_gene_summary",    mode = "function")) source("read_data.R")
  pdm <- load_phenodigm()
  pdo <- load_phenodigm_other()
  gs  <- load_gene_summary()

  matches <- list()
  # gene_summary uses pipe-separated names; phenodigm tables one per row
  hits1 <- unique(pdm[grepl(query, pdm$disorder_name, ignore.case = TRUE),
                      c("disorder_id", "disorder_name"), drop = FALSE])
  hits2 <- unique(pdo[grepl(query, pdo$disorder_name, ignore.case = TRUE),
                      c("query", "disorder_name"), drop = FALSE])
  names(hits2)[1] <- "disorder_id"
  # gene_summary: split on pipe, search each
  gs_rows <- gs[grepl(query, gs$disorder_name, ignore.case = TRUE),
                c("disorder_id", "disorder_name"), drop = FALSE]
  gs_pairs <- do.call(rbind, lapply(seq_len(nrow(gs_rows)), function(i) {
    ids  <- strsplit(gs_rows$disorder_id[i],   "|", fixed = TRUE)[[1]]
    nms  <- strsplit(gs_rows$disorder_name[i], "|", fixed = TRUE)[[1]]
    k <- min(length(ids), length(nms))
    if (k == 0) return(NULL)
    keep <- grepl(query, nms[seq_len(k)], ignore.case = TRUE)
    if (!any(keep)) return(NULL)
    data.frame(disorder_id = ids[seq_len(k)][keep],
               disorder_name = nms[seq_len(k)][keep],
               stringsAsFactors = FALSE)
  }))
  if (is.null(gs_pairs)) gs_pairs <- data.frame(
    disorder_id = character(0), disorder_name = character(0))

  all_hits <- unique(rbind(hits1, hits2, gs_pairs))
  if (nrow(all_hits) == 0) {
    # Auto-retry: if the query looks like an acronym we know, recurse
    # with the expanded form (only once, to prevent infinite recursion).
    if (isTRUE(.allow_acronym_retry)) {
      upper <- toupper(query)
      expansion <- .MEDICAL_ACRONYMS[[upper]]
      if (!is.null(expansion)) {
        inner <- .tool_find_disorders_by_name(expansion,
                                              .allow_acronym_retry = FALSE)
        return(paste0(
          sprintf("(No direct match for '%s'; auto-expanded to '%s'.)\n",
                  query, expansion),
          inner))
      }
    }
    return(sprintf("No disorders found whose name contains '%s'.", query))
  }
  shown <- head(all_hits, 10)
  more_note <- if (nrow(all_hits) > 10)
    sprintf("\n(... %d more matches not shown.)", nrow(all_hits) - 10) else ""
  paste(
    sprintf("Disorders matching '%s' (%d shown of %d):",
            query, nrow(shown), nrow(all_hits)),
    paste(sprintf("- %s  (%s)", shown$disorder_name, shown$disorder_id),
          collapse = "\n"),
    more_note,
    sep = "\n"
  )
}

.tool_score_threshold_counts <- function(threshold, table) {
  table <- tolower(table)
  if (!table %in% c("phenodigm_scores", "phenodigm_other")) {
    return(sprintf("Unknown table '%s'. Use 'phenodigm_scores' or 'phenodigm_other'.",
                   table))
  }
  if (!exists("load_phenodigm",       mode = "function")) source("read_data.R")
  if (!exists("load_phenodigm_other", mode = "function")) source("read_data.R")
  df <- if (table == "phenodigm_scores") load_phenodigm() else load_phenodigm_other()
  v  <- suppressWarnings(as.numeric(df$score))
  hit <- !is.na(v) & v > threshold
  disorder_col <- if ("disorder_id" %in% names(df)) "disorder_id" else "query"
  sprintf(
    "%s with score > %.2f: rows=%d, distinct disorders=%d, distinct genes=%d.",
    table, threshold, sum(hit),
    length(unique(df[[disorder_col]][hit])),
    length(unique(df$gene_symbol[hit]))
  )
}

.tool_get_top_matches <- function(table, top_n = 5L, min_score = NULL) {
  if (!table %in% c("phenodigm_scores", "phenodigm_other")) {
    return(sprintf("Unknown table '%s'. Use 'phenodigm_scores' or 'phenodigm_other'.",
                   table))
  }
  top_n <- max(1L, min(25L, as.integer(top_n)))
  if (!exists("load_phenodigm",       mode = "function")) source("read_data.R")
  if (!exists("load_phenodigm_other", mode = "function")) source("read_data.R")
  df <- if (table == "phenodigm_scores") load_phenodigm() else load_phenodigm_other()
  v <- suppressWarnings(as.numeric(df$score))
  keep <- !is.na(v)
  if (!is.null(min_score)) keep <- keep & v > min_score
  df <- df[keep, , drop = FALSE]
  v  <- v[keep]
  if (nrow(df) == 0) {
    return(sprintf("No rows in %s with the requested filter.", table))
  }
  ord <- order(-v)
  df <- df[head(ord, top_n), , drop = FALSE]
  v  <- v[head(ord, top_n)]
  disorder_col <- if ("disorder_id" %in% names(df)) "disorder_id" else "query"

  filter_note <- if (is.null(min_score)) ""
                 else sprintf(" (filtered to score > %.2f)", min_score)
  .stash_result_table(
    sprintf("top_matches_%s", table),
    data.frame(disorder_id = df[[disorder_col]],
               disorder_name = df$disorder_name,
               gene_symbol = df$gene_symbol,
               mouse_model = df$description,
               score = v, stringsAsFactors = FALSE))
  paste(
    sprintf("Top %d rows from %s%s:", nrow(df), table, filter_note),
    paste(sprintf("  - %s | %s | gene=%s | model=%s | score=%.2f",
            df[[disorder_col]], df$disorder_name, df$gene_symbol,
            df$description, v),
          collapse = "\n"),
    sep = "\n"
  )
}

.tool_get_table_columns <- function(table) {
  table <- tolower(table)
  cols <- list(
    gene_summary = c(
      "gene_symbol     : human gene symbol",
      "hgnc_id         : HGNC identifier",
      "mgi_id          : mouse ortholog MGI identifier",
      "disorder_id     : OMIM/Orphanet ID(s), pipe-separated if multiple",
      "disorder_name   : disorder name(s), pipe-separated if multiple",
      "IMPC_pipeline   : 'yes' or 'no' flag",
      "IMPC_phenotypes : 'yes' or 'no' flag (abnormal mouse phenotypes)",
      "HPO_phenotypes  : 'yes' or 'no' flag (HPO terms exist)",
      "PhenoDigm_match : 'yes' or 'no' flag",
      "max_score       : highest PhenoDigm score for this gene (or NA)"
    ),
    phenodigm_scores = c(
      "disorder_id     : OMIM/Orphanet ID for this row",
      "disorder_name   : disorder name for this row",
      "gene_symbol     : human gene symbol",
      "description     : mouse-model description (line + zygosity)",
      "score           : PhenoDigm similarity score for this row",
      "query_phenotype : comma-separated HP (human) terms",
      "match_phenotype : comma-separated MP (mouse) terms"
    ),
    phenodigm_other = c(
      "query           : disorder ID being queried (OMIM/Orphanet)",
      "match           : matched mouse model identifier",
      "mgi_id          : mouse gene MGI id",
      "hgnc_id         : human gene HGNC id",
      "gene_symbol     : human ortholog gene symbol",
      "disorder_name   : disorder name",
      "score           : PhenoDigm similarity score",
      "life_stage      : early / late",
      "description     : mouse-model description",
      "query_phenotype : comma-separated HP terms",
      "match_phenotype : comma-separated MP terms"
    )
  )
  # A plain-English purpose sentence for each table, so the assistant can
  # explain what the table is FOR before listing the columns.
  purpose <- list(
    gene_summary = paste(
      "The Gene Summary table is a one-row-per-gene overview. It tells you,",
      "for each human gene: its identifiers, any disease it is linked to,",
      "whether the mouse version entered the IMPC pipeline, and its best",
      "PhenoDigm match score. Use it for quick 'what do we know about this",
      "gene' questions."),
    phenodigm_scores = paste(
      "The PhenoDigm Scores table holds the matches for genes that ARE",
      "already known to cause a human disease. Each row compares one human",
      "disorder with one mouse model and gives a similarity score, plus the",
      "human (HP) and mouse (MP) phenotype terms behind that score. There",
      "are 3,085 rows but only 1,311 distinct genes, because one gene can",
      "have several disorders or several mouse models."),
    phenodigm_other = paste(
      "The PhenoDigm Other table is the exploratory one. It looks at mouse",
      "knockouts of genes that do NOT yet have a known human disease, and",
      "asks whether the mouse happens to resemble a known disorder. The",
      "matches (kept above a score of 40) are candidate hypotheses worth",
      "investigating, not confirmed gene-disease links.")
  )
  if (!table %in% names(cols)) {
    return(sprintf("Unknown table '%s'. Choose: %s",
                   table, paste(names(cols), collapse = ", ")))
  }
  paste(
    purpose[[table]],
    "",
    sprintf("Columns of the %s table:", table),
    paste("  -", cols[[table]], collapse = "\n"),
    sep = "\n"
  )
}

## --- WEB SEARCH via OpenAI's Responses API --------------------------------
## Design note: rather than migrating the whole conversation loop from the
## Chat Completions API to the Responses API, we keep the main loop unchanged
## and implement web search as a TOOL that makes its own one-shot call to the
## Responses API with OpenAI's hosted `web_search` tool enabled. OpenAI runs
## the actual search; we receive the synthesised text plus any citations.
## This keeps all existing tools working and confines the new API surface to
## a single function.
##
## Requires an OpenAI key (the SAME key as the chat provider). Works even when
## the main conversation is running on a local Ollama model -- the search step
## simply calls out to OpenAI.
.tool_web_search <- function(query, context = NULL) {
  if (!nzchar(query)) return("No search query supplied.")
  s <- ollama_settings()
  if (!nzchar(s$openai_key)) {
    return(paste("Web search unavailable: OPENAI_API_KEY is not set.",
                 "Set it in your .Renviron to enable online search."))
  }
  model <- Sys.getenv("OPENAI_SEARCH_MODEL", unset = "gpt-4o-mini")
  prompt <- paste0(
    "Search the web and answer concisely, citing sources with their URLs.\n",
    if (!is.null(context) && nzchar(context))
      paste0("Context from the user's portal session: ", context, "\n") else "",
    "Query: ", query)

  body <- list(
    model = model,
    input = prompt,
    tools = list(list(type = "web_search"))
  )
  resp <- tryCatch(
    httr::POST(
      url = paste0(s$openai_base, "/v1/responses"),
      body = body, encode = "json",
      httr::add_headers(Authorization = paste("Bearer", s$openai_key)),
      httr::content_type_json(),
      httr::timeout(max(60, s$timeout_s))),
    error = function(e) e)
  if (inherits(resp, "error")) {
    return(sprintf("Web search failed: %s", conditionMessage(resp)))
  }
  if (httr::status_code(resp) >= 300) {
    b <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                  error = function(e) "")
    return(sprintf("Web search returned HTTP %d. %s",
                   httr::status_code(resp), substr(b, 1, 300)))
  }
  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE)
  .record_usage(parsed, provider = "openai", model = model,
                web_search = TRUE)
  .llm_side_effects$searched <- TRUE

  # The Responses API returns an `output` array; pull out the text pieces.
  txt <- character(0)
  if (!is.null(parsed$output_text)) {
    txt <- as.character(parsed$output_text)
  } else if (!is.null(parsed$output)) {
    for (item in parsed$output) {
      if (!is.null(item$content)) {
        for (c in item$content) {
          if (!is.null(c$text)) txt <- c(txt, as.character(c$text))
        }
      }
    }
  }
  txt <- txt[nzchar(txt)]
  if (length(txt) == 0) return("Web search returned no readable result.")

  # Pull out citation URLs so the model can be required to show sources.
  urls <- character(0)
  .collect_urls <- function(x) {
    if (is.list(x)) {
      if (!is.null(x$url)) urls <<- c(urls, as.character(x$url))
      lapply(x, .collect_urls)
    }
    invisible(NULL)
  }
  .collect_urls(parsed$output)
  # Also catch bare URLs written into the text itself.
  urls <- c(urls, unlist(regmatches(txt, gregexpr("https?://[^ )\\]\"']+", txt))))
  urls <- unique(urls[nzchar(urls)])

  paste0(
    "=== EXTERNAL WEB SOURCE (live internet, retrieved ", Sys.Date(), ") ===\n",
    "This content came from the INTERNET, not from the portal's data files.\n",
    "You MUST label it as web-sourced and cite the URLs below.\n\n",
    paste(txt, collapse = "\n"),
    if (length(urls) > 0)
      paste0("\n\nSOURCE URLS (cite these):\n",
             paste0("  - ", urls, collapse = "\n"))
    else "\n\n(No source URLs were returned — say so explicitly.)",
    "\n=== END EXTERNAL WEB SOURCE ==="
  )
}

.tool_get_portal_stats <- function() {
  build_portal_context()
}

## Detect a "leaked" tool call -- i.e. the model wrote a JSON tool call as
## plain text in its answer instead of using the proper tool-call channel.
## Returns list(name=, arguments=<R list>) if it finds a valid one, else NULL.
.maybe_leaked_tool_call <- function(content) {
  if (is.null(content) || !nzchar(content)) return(NULL)
  if (!grepl("\\{", content) || !grepl('"name"', content)) return(NULL)
  start  <- regexpr("\\{", content)[1]
  closes <- gregexpr("\\}", content)[[1]]
  endpos <- closes[length(closes)]
  if (start < 1 || endpos < start) return(NULL)
  js <- substr(content, start, endpos)
  parsed <- tryCatch(jsonlite::fromJSON(js, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$name)) return(NULL)
  name <- as.character(parsed$name)
  args <- parsed$arguments
  if (is.null(args)) args <- parsed$parameters
  if (is.null(args)) args <- list()
  known <- vapply(OLLAMA_TOOLS, function(t) t[["function"]]$name, character(1))
  if (!name %in% known) return(NULL)
  list(name = name, arguments = args)
}

## --- THE TOOL-CALLING CHAT LOOP -------------------------------------------

call_ollama_chat_with_tools <- function(message,
                                        history = NULL,
                                        model   = ollama_settings()$model,
                                        host    = ollama_settings()$host,
                                        timeout_s = ollama_settings()$timeout_s,
                                        extra_system = NULL,
                                        max_iter = ollama_settings()$tool_max_iter) {

  system_blocks <- OLLAMA_TOOLCALL_SYSTEM_PROMPT
  if (!is.null(extra_system) && nzchar(extra_system)) {
    system_blocks <- paste(system_blocks, "", extra_system, sep = "\n")
  }

  # Assemble the running message stack
  messages <- list(list(role = "system", content = system_blocks))
  if (!is.null(history) && length(history) > 0) {
    max_h <- ollama_settings()$max_history
    if (length(history) > max_h) history <- tail(history, max_h)
    clean_history <- lapply(history, function(item) {
      list(role = as.character(item$role),
           content = as.character(item$content))
    })
    messages <- c(messages, clean_history)
  }
  messages <- c(messages,
                list(list(role = "user", content = trimws(message))))

  for (iter in seq_len(max_iter)) {
    ## Send to whichever provider is active; get a normalised assistant msg.
    res <- .llm_chat_post(messages, tools = active_tools(), timeout_s = timeout_s)

    if (length(res$tool_calls) == 0) {
      # No STRUCTURED tool call. But the model may have "leaked" a tool call
      # as plain text in its answer (happens even with capable 8B models).
      # If so, run the tool anyway and feed the result back, instead of
      # returning the raw JSON to the user.
      leaked <- .maybe_leaked_tool_call(res$content)
      if (!is.null(leaked)) {
        result <- dispatch_tool(leaked$name, leaked$arguments)
        messages <- c(messages,
          list(list(role = "assistant", content = res$content)),
          list(list(role = "user", content = paste0(
            "The tool ", leaked$name, " returned:\n", result,
            "\n\nNow answer my original question in plain English using ",
            "this result. Do not output any JSON."))))
        next
      }
      # Genuine final answer.
      final <- res$content
      if (is.null(final) || !nzchar(trimws(final))) {
        stop("Empty final response from the model (no content, no tool calls).")
      }
      return(trimws(final))
    }

    # Echo the assistant turn (with its tool_calls) back, then run each tool
    # and append its result. OpenAI is strict about message shape, so we
    # RECONSTRUCT a clean assistant message for it rather than echoing the
    # raw parsed object (which can carry a null/object 'content' field).
    if (identical(active_provider(), "openai")) {
      oai_tcs <- lapply(res$tool_calls, function(tc) {
        argstr <- if (length(tc$arguments) == 0) "{}"
                  else paste0(jsonlite::toJSON(tc$arguments, auto_unbox = TRUE))
        list(id = tc$id, type = "function",
             "function" = list(name = tc$name, arguments = argstr))
      })
      messages <- c(messages, list(list(
        role = "assistant", content = "", tool_calls = oai_tcs)))
    } else {
      messages <- c(messages, list(res$raw))
    }
    for (tc in res$tool_calls) {
      result <- dispatch_tool(tc$name, tc$arguments)
      messages <- .append_tool_result(messages, tc, result)
    }
    # Loop continues; model now has the tool results.
  }

  stop(sprintf(
    "Reached the tool-call iteration cap (%d). Model kept calling tools without producing a final answer.",
    max_iter))
}
