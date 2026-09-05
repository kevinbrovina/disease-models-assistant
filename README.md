# Disease Models Portal — LLM assistant

A conversational assistant built into the IMPC Disease Models Portal, an R Shiny
application that compares human disease genes with their mouse knockout
orthologues using PhenoDigm phenotype-similarity scores.

You ask a question in plain English and get an answer computed from the portal's
own data. The language model reads the question and writes the sentence — every
number in the answer comes from R.

Built as an MSc Bioinformatics dissertation project at Queen Mary University of
London.

## What's here

| File | What it does |
|---|---|
| `R/ollama_client.R` | Everything to do with the models: providers, prompts, the twelve tools, retrieval, token and cost tracking |
| `R/mod_chat.R` | The Shiny chat module — UI, conversation history, rendering, the footer that shows time/tokens/cost |
| `analysis/make_figures.R` | Builds the four results figures from the benchmark CSV |
| `analysis/make_flowchart.R` | Builds the architecture diagram |
| `analysis/benchmark_results.csv` | All 132 scored responses — score, time, tokens, API calls |

The rest of the portal (`app.R`, the other `mod_*.R` files, `read_data.R`) is the
research group's existing application and is not included here.

## How it works

The assistant never lets the model touch the data. Two retrieval methods were
built and compared:

**Pre-fetch** — regular expressions scan your question for gene symbols, disorder
IDs and phenotype IDs. R looks them up first and pastes the facts into the
prompt. Simple and predictable, but it only handles question shapes I wrote rules
for.

**Tool calling** — the model gets twelve typed tool declarations and picks which
one to call, supplying the arguments. R validates them and runs a pre-written
function. The model can chain up to four rounds.

In both cases the R code is fixed. The model chooses *which* function runs and
*what values* go in — it never writes or modifies code, and there's no `eval()`
anywhere in the project.

## Running it

**This repository is the assistant module, not a complete application.** It
contains the two files I wrote. To run it you also need two things I can't
distribute:

1. **The Disease Models Portal itself** — the existing R Shiny app (`app.R`,
   `read_data.R` and the other `mod_*.R` modules). That belongs to the research
   group, not to me.
2. **IMPC Data Release 20.1** — the three `.fst` files, about 197 MB. Available
   from the IMPC.

If you already have the portal (my supervisor and the group do), setup is:

1. Copy `R/mod_chat.R` and `R/ollama_client.R` into the portal folder.
2. Put the three `.fst` files in `data/`.
3. Register the chat module in `app.R` so it appears as a tab.
4. Put your OpenAI key in `~/.Renviron` — never in the project folder:

```
OPENAI_API_KEY=sk-...
```

Then pick a condition with environment variables before starting Shiny:

```r
Sys.setenv(LLM_PROVIDER    = "ollama")     # or "openai"
Sys.setenv(OLLAMA_MODEL    = "llama3.1:8b")
Sys.setenv(OLLAMA_USE_TOOLS = "1")         # 0 = pre-fetch, 1 = tool calling
Sys.setenv(LLM_TEMPERATURE = "0")
shiny::runApp()
```

For local models you need [Ollama](https://ollama.com/) running with the model
pulled (`ollama pull llama3.1:8b`).

**The analysis scripts run on their own.** `analysis/` needs nothing but R and
ggplot2 — the benchmark data is included, so anyone can regenerate every figure
in the dissertation without the portal or the IMPC files:

```bash
cd analysis && Rscript make_figures.R
```

## What the evaluation found

22 questions × 3 models × 2 retrieval methods = 132 scored answers, marked
against reference values computed separately in R.

| Model | Pre-fetch | Tool calling | Difference |
|---|---|---|---|
| Qwen 2.5 3B (local) | 47.7% | 36.4% | −11.4 pp |
| Llama 3.1 8B (local) | 75.0% | 75.0% | no change |
| gpt-4o (hosted) | 88.6% | 95.5% | +6.8 pp |

The main result is that **you can't say which retrieval method is better without
saying which model is using it.** Tool calling made the small model noticeably
worse, did nothing at 8B, and only helped the hosted model. Letting a model pick
its own retrieval only pays off above a certain capability.

A second finding: grounding controls what the model *receives*, not what it
*adds*. gpt-4o returned all 18 phenotype identifiers for one question correctly,
then invented human-readable names for them that the portal doesn't store — and
presented both under the same "from the portal data" heading.

Regenerate the figures with:

```bash
cd analysis && Rscript make_figures.R
```

The generated figures are in `figures/`.

## Licence

MIT — see `LICENSE`.
