.chat_render_markdown <- function(text) {
  text <- gsub("&gt;", ">", as.character(text), fixed = TRUE)
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  if (requireNamespace("markdown", quietly = TRUE)) {
    HTML(markdown::mark_html(text = text, template = FALSE))
  } else {
    # Fallback if the markdown package is missing. tags$span already escapes
    # its text argument once -- do NOT call htmlEscape here as well, or '>'
    # gets double-escaped and shows as '&gt;' in the bubble.
    tags$span(style = "white-space: pre-wrap;", as.character(text))
  }
}

## Render a bar-chart spec (title/labels/values) to an inline PNG image that
## sits in the assistant's chat bubble. Uses base graphics -> PNG -> base64.
.chat_render_chart <- function(spec) {
  if (is.null(spec) || is.null(spec$values) || length(spec$values) == 0) {
    return(NULL)
  }
  img_uri <- tryCatch({
    tmp <- tempfile(fileext = ".png")
    grDevices::png(tmp, width = 640, height = 380, res = 96)
    op <- graphics::par(mar = c(5, 4, 3, 1))
    bp <- graphics::barplot(
      spec$values, names.arg = spec$labels, col = "#2F8F9D", border = NA,
      main = spec$title, ylab = "Count", las = 1,
      ylim = c(0, max(spec$values) * 1.18))
    graphics::text(bp, spec$values, labels = spec$values,
                   pos = 3, cex = 0.9, col = "#1F2A47")
    graphics::par(op)
    grDevices::dev.off()
    uri <- paste0("data:image/png;base64,", base64enc::base64encode(tmp))
    unlink(tmp)
    uri
  }, error = function(e) NULL)
  if (is.null(img_uri)) return(NULL)
  tags$img(src = img_uri,
           style = paste("max-width:100%; margin-top:8px;",
                         "border:1px solid #eee; border-radius:4px;"))
}

.chat_format_elapsed <- function(seconds) {
  if (is.null(seconds) || length(seconds) != 1 || is.na(seconds)) {
    return(NULL)
  }
  seconds <- max(0, as.numeric(seconds))
  if (seconds < 60) {
    sprintf("%.1fs", seconds)
  } else {
    sprintf("%dm %.1fs", floor(seconds / 60), seconds %% 60)
  }
}

mod_chat_ui <- function(id) {
  ns <- NS(id)

  enter_to_send_js <- sprintf(
    "$(document).on('keydown', '#%s', function(e){
       if (e.key === 'Enter' && !e.shiftKey){
         e.preventDefault();
         $('#%s').click();
       }
     });",
    ns("chat_input"), ns("send_chat")
  )

  autoscroll_js <- sprintf(
    "Shiny.addCustomMessageHandler('%s', function(_) {
       var el = document.getElementById('%s');
       if (el) { el.scrollTop = el.scrollHeight; }
     });",
    ns("scroll_chat"), ns("chat_scroll")
  )

  thinking_timer_js <- sprintf(
    "window.__chatThinkingTimers = window.__chatThinkingTimers || {};
     Shiny.addCustomMessageHandler('%s', function(msg) {
       var key = '%s';
       var intervalId = window.__chatThinkingTimers[key];
       if (intervalId) { clearInterval(intervalId); }
       window.__chatThinkingTimers[key] = null;
       if (!msg || msg.action !== 'start') return;
       var startedAt = Date.now();
       var tick = function() {
         var el = document.getElementById('%s');
         if (!el) return;
         var secs = Math.max(0, Math.floor((Date.now() - startedAt) / 1000));
         el.textContent = ' (' + secs + 's)';
       };
       tick();
       window.__chatThinkingTimers[key] = setInterval(tick, 250);
     });",
    ns("thinking_timer"), ns("thinking_timer"), ns("thinking_timer_text")
  )

  tagList(
    tags$head(tags$script(HTML(enter_to_send_js)),
              tags$script(HTML(autoscroll_js)),
              tags$script(HTML(thinking_timer_js))),
    fluidRow(
      box(
        title = "Local AI Assistant",
        width = 12,
        solidHeader = TRUE,
        status = "primary",

        fluidRow(
          column(8,
            tags$p(
              "Ask about the IMPC Disease Models portal: pages, navigation,",
              "PhenoDigm scores, gene summaries."
            )
          ),
          column(4, align = "right",
            uiOutput(ns("status_pill"), inline = TRUE),
            actionButton(ns("clear_chat"), "Clear",
                         icon = icon("eraser"),
                         class = "btn-sm btn-default",
                         style = "margin-left:8px;")
          )
        ),

        tags$div(
          id = ns("chat_scroll"),
          style = paste(
            "height: 440px;",
            "overflow-y: auto;",
            "padding: 12px;",
            "border: 1px solid #d2d6de;",
            "border-radius: 4px;",
            "background-color: #fafafa;"
          ),
          uiOutput(ns("chat_history_ui"))
        ),

        tags$br(),
        tags$div(
          style = "display:flex; gap:8px; align-items:flex-end;",
          tags$div(style = "flex:1;",
            textInput(
              inputId = ns("chat_input"),
              label = NULL,
              placeholder = "Ask anything about the portal..."
            )
          ),
          actionButton(ns("send_chat"), "Send",
                       icon = icon("paper-plane"),
                       class = "btn-primary")
        )
      )
    )
  )
}

## Persistent chat store, keyed by session token. The app re-creates the
## chat module every time the user switches tabs, which would otherwise reset
## the conversation. Keeping the history in this file-level environment (keyed
## per session so users don't share chats) means it survives those re-creations.
.chat_store <- new.env(parent = emptyenv())

mod_chat_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    welcome <- list(
      role = "assistant",
      content = paste(
        "Hi! I can explain the Disease Models portal, help you navigate",
        "between tabs, and answer questions about the PhenoDigm data.",
        "Type your question below."
      )
    )

    # Restore prior history for this session if the module was re-created
    # (e.g. after a tab switch); otherwise start with the welcome message.
    store_key <- session$token
    initial_history <- if (!is.null(.chat_store[[store_key]])) {
      .chat_store[[store_key]]
    } else {
      list(welcome)
    }

    chat_history <- reactiveVal(initial_history)
    is_thinking  <- reactiveVal(FALSE)
    health       <- reactiveVal(ollama_health())

    # Persist the conversation on every change so it survives tab switches.
    observe({
      .chat_store[[store_key]] <- chat_history()
    })

    output$status_pill <- renderUI({
      h <- health()
      if (isTRUE(h$ok)) {
        tags$span(
          style = paste(
            "background:#dff0d8; color:#3c763d;",
            "padding:3px 8px; border-radius:12px;",
            "font-size:85%; border:1px solid #c5e1b8;"
          ),
          icon("circle-check"),
          sprintf(" %s online (%s)",
                  if (identical(active_provider(), "openai")) "OpenAI" else "Ollama",
                  active_model())
        )
      } else {
        tags$span(
          title = h$message,
          style = paste(
            "background:#f2dede; color:#a94442;",
            "padding:3px 8px; border-radius:12px;",
            "font-size:85%; border:1px solid #ebccd1;"
          ),
          icon("circle-exclamation"),
          # Name the ACTIVE provider and show the real reason on hover,
          # so an OpenAI key problem isn't mislabelled as "Ollama offline".
          sprintf(" %s unavailable — %s",
                  if (identical(active_provider(), "openai")) "OpenAI" else "Ollama",
                  h$message)
        )
      }
    })

    output$chat_history_ui <- renderUI({
      history <- chat_history()
      thinking <- is_thinking()

      bubbles <- lapply(history, function(msg) {
        is_user <- identical(msg$role, "user")
        bg      <- if (is_user) "#d9edf7" else "#ffffff"
        border  <- if (is_user) "#bce8f1" else "#d2d6de"
        label   <- if (is_user) "You" else "Assistant"
        align   <- if (is_user) "flex-end" else "flex-start"

        tags$div(
          style = sprintf("display:flex; justify-content:%s;", align),
          tags$div(
            style = paste(
              "max-width: 82%;",
              "margin-bottom: 10px;",
              "padding: 8px 12px;",
              "border-radius: 10px;",
              sprintf("background-color:%s;", bg),
              sprintf("border:1px solid %s;", border)
            ),
            tags$div(
              style = "font-size:80%; color:#888; margin-bottom:3px;",
              tags$strong(label)
            ),
            if (is_user) {
              # tags$span already escapes text args; do NOT call
              # htmltools::htmlEscape here -- that double-escapes, turning
              # '>' into '&gt;' visibly in the chat bubble.
              tags$span(style = "white-space: pre-wrap;",
                        as.character(msg$content))
            } else {
              tagList(
                .chat_render_markdown(msg$content),
                if (!is.null(msg$chart)) .chat_render_chart(msg$chart),
                if (!is.null(msg$table)) {
                  tags$div(
                    style = "margin-top:8px;",
                    downloadButton(
                      ns("download_table"),
                      sprintf("Download full table (%d rows, CSV)",
                              nrow(msg$table$data)),
                      class = "btn-sm btn-default")
                  )
                },
                if (!is.null(msg$elapsed_s)) {
                  u <- msg$usage
                  parts <- sprintf("Answered in %s",
                                   .chat_format_elapsed(msg$elapsed_s))
                  if (!is.null(u) && !is.null(u$tokens_total) &&
                      u$tokens_total > 0) {
                    parts <- paste0(
                      parts,
                      sprintf("  ·  %g tokens (in %g / out %g)",
                              u$tokens_total, u$tokens_in, u$tokens_out),
                      sprintf("  ·  %g API call%s", u$api_calls,
                              if (u$api_calls == 1) "" else "s"))
                  }
                  if (!is.null(u) && isTRUE(u$searched)) {
                    parts <- paste0(parts, "  ·  \U0001F310 used web search")
                  }
                  if (!is.null(u) && !is.null(u$openai_api_calls) &&
                      u$openai_api_calls > 0) {
                    if (isTRUE(u$openai_cost_known) &&
                        is.finite(u$estimated_openai_cost_usd)) {
                      cost_text <- sprintf(
                        "  ·  estimated OpenAI cost US$%.6f",
                        u$estimated_openai_cost_usd)
                      if (u$web_search_calls > 0) {
                        cost_text <- paste0(
                          cost_text,
                          sprintf(" (tokens US$%.6f + %d web search%s US$%.6f)",
                                  u$openai_model_cost_usd,
                                  u$web_search_calls,
                                  if (u$web_search_calls == 1) "" else "es",
                                  u$openai_web_search_cost_usd)
                        )
                      }
                      parts <- paste0(parts, cost_text)
                    } else {
                      parts <- paste0(
                        parts,
                        "  ·  OpenAI cost unavailable for the selected model")
                    }
                  }
                  tags$div(
                    style = paste(
                      "font-size:80%; color:#777; margin-top:6px;",
                      "border-top:1px solid #eee; padding-top:4px;"
                    ),
                    parts
                  )
                }
              )
            }
          )
        )
      })

      if (isTRUE(thinking)) {
        bubbles <- c(bubbles, list(
          tags$div(
            style = "display:flex; justify-content:flex-start;",
            tags$div(
              style = paste(
                "padding:8px 12px; border-radius:10px;",
                "background:#fff; border:1px solid #d2d6de;",
                "color:#888; font-style:italic;"
              ),
              icon("spinner", class = "fa-spin"),
              " Assistant is thinking...",
              tags$span(id = ns("thinking_timer_text"), " (0s)")
            )
          )
        ))
      }

      do.call(tagList, bubbles)
    })

    trigger_send <- function(text) {
      text <- trimws(text)
      if (!nzchar(text)) return(invisible())

      current <- chat_history()
      history_with_user <- c(current, list(list(role = "user", content = text)))
      chat_history(history_with_user)
      updateTextInput(session, "chat_input", value = "")
      session$sendCustomMessage(ns("scroll_chat"), 1)

      is_thinking(TRUE)
      session$sendCustomMessage(ns("thinking_timer"), list(action = "start"))
      session$onFlushed(function() {
        history_for_ollama <- history_with_user
        ## Drop the just-added user message; call_ollama_chat re-adds it.
        history_for_ollama <- history_for_ollama[
          seq_len(length(history_for_ollama) - 1)]
        ## Drop the welcome message from history sent to model (it's static).
        if (length(history_for_ollama) > 0 &&
            identical(history_for_ollama[[1]]$content, welcome$content)) {
          history_for_ollama <- history_for_ollama[-1]
        }

        active_model <- ollama_settings()$model
        started_at <- Sys.time()
        reply <- tryCatch(
          call_ollama_chat(message = text, history = history_for_ollama,
                           model = active_model),
          error = function(e) {
            health(ollama_health())
            paste0("**Local AI assistant unavailable.** ", conditionMessage(e),
                   "\n\nMake sure Ollama is running (`ollama serve`) and that ",
                   "the model `", active_model,
                   "` is pulled (`ollama pull ", active_model, "`).")
          }
        )
        elapsed_s <- as.numeric(difftime(Sys.time(), started_at,
                                         units = "secs"))
        # A tool may have produced a bar chart during this turn.
        chart_spec <- tryCatch(ollama_last_chart(), error = function(e) NULL)
        # A data tool may have produced a downloadable result table.
        table_spec <- tryCatch(ollama_last_table(), error = function(e) NULL)
        usage_spec <- tryCatch(ollama_last_usage(), error = function(e) NULL)

        chat_history(c(history_with_user,
                       list(list(role = "assistant",
                                 content = reply,
                                 elapsed_s = elapsed_s,
                                 chart = chart_spec,
                                 table = table_spec,
                                 usage = usage_spec,
                                 model = active_model))))
        is_thinking(FALSE)
        session$sendCustomMessage(ns("thinking_timer"), list(action = "stop"))
        session$sendCustomMessage(ns("scroll_chat"), 1)
      }, once = TRUE)
    }

    observeEvent(input$send_chat, {
      req(nzchar(trimws(input$chat_input)))
      trigger_send(input$chat_input)
    })

    observeEvent(input$clear_chat, {
      chat_history(list(welcome))
      .chat_store[[store_key]] <- list(welcome)
      health(ollama_health())
      session$sendCustomMessage(ns("scroll_chat"), 1)
    })

    # CSV download of the most recent result table. Opens in Excel directly.
    output$download_table <- downloadHandler(
      filename = function() {
        hist <- chat_history()
        tbls <- Filter(Negate(is.null), lapply(hist, function(m) m$table))
        nm <- if (length(tbls) > 0) tbls[[length(tbls)]]$name else "results"
        sprintf("%s_%s.csv", nm, format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        hist <- chat_history()
        tbls <- Filter(Negate(is.null), lapply(hist, function(m) m$table))
        df <- if (length(tbls) > 0) tbls[[length(tbls)]]$data else
                data.frame(note = "No table available")
        utils::write.csv(df, file, row.names = FALSE)
      }
    )
  })
}
