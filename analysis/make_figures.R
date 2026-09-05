# makes the 4 figures for chapter 3
# reads benchmark_results.csv - that's just my benchmark workbook saved out as a csv
# and drops the pngs into figures/
# run from the dissertations folder: Rscript make_figures.R

library(ggplot2)
library(dplyr)

results <- read.csv("benchmark_results.csv", stringsAsFactors = FALSE,
                    fileEncoding = "UTF-8")

# force the model order - otherwise R sorts them alphabetically and
# GPT-4o ends up first which looks wrong when the point is model size
model_order  <- c("Qwen2.5 3B", "Llama 3.1 8B", "GPT-4o")
method_order <- c("Pre-fetch", "Tool calling")
results$model  <- factor(results$model,  levels = model_order)
results$method <- factor(results$method, levels = method_order)

dir.create("figures", showWarnings = FALSE)

# ---- colours ----
# warm palette, all set here so I only change them in one place
sand   <- "#E8C39E"   # pre-fetch bars
rust   <- "#B4541D"   # tool calling bars
ink    <- "#3A2A1E"
grid   <- "#E6DACE"
brick  <- "#A33B24"   # score 0
amber  <- "#E4A33C"   # score 1
moss   <- "#5F7A4A"   # score 2

fills <- c("Pre-fetch" = sand, "Tool calling" = rust)

# same theme on all four so they look like a set
my_theme <- theme_minimal(base_size = 11) +
  theme(
    text             = element_text(colour = ink),
    axis.text        = element_text(colour = ink),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = grid, linewidth = 0.4),
    axis.line.x      = element_line(colour = ink, linewidth = 0.4),
    legend.title     = element_blank(),
    legend.position  = "top",
    legend.justification = "left",
    plot.margin      = margin(10, 14, 10, 10)
  )

# ---- Fig 3.3 - accuracy ----
acc <- results %>%
  group_by(model, method) %>%
  summarise(points = sum(score, na.rm = TRUE), .groups = "drop") %>%
  mutate(accuracy = points / 44 * 100)   # 22 questions x 2 marks = 44

# the +/- pp labels above each pair. this is the main finding so I wanted it
# on the chart rather than only in the text
delta <- acc %>%
  tidyr::pivot_wider(id_cols = model, names_from = method,
                     values_from = accuracy) %>%
  mutate(diff  = `Tool calling` - `Pre-fetch`,
         top   = pmax(`Pre-fetch`, `Tool calling`) + 14,
         label = ifelse(abs(diff) < 0.05, "no change",
                        sprintf("%+.1f pp", diff)),
         col   = ifelse(diff < -0.05, brick,
                        ifelse(diff > 0.05, moss, "#8A7B6D")))

p <- ggplot(acc, aes(model, accuracy, fill = method)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68,
           colour = ink, linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", accuracy)),
            position = position_dodge(width = 0.78),
            vjust = -0.5, size = 3.2, colour = ink) +
  geom_text(data = delta, aes(x = model, y = top, label = label),
            inherit.aes = FALSE, colour = delta$col,
            fontface = "bold", size = 3.4) +
  scale_fill_manual(values = fills) +
  scale_y_continuous(limits = c(0, 118), breaks = seq(0, 100, 20),
                     expand = c(0, 0)) +
  labs(x = NULL, y = "Accuracy (% of 44 points)") +
  my_theme
ggsave("figures/fig_3_3_accuracy.png", p, width = 6.2, height = 3.6, dpi = 300)

# ---- Fig 3.4 - every question in every condition ----
hm <- results %>%
  mutate(condition = paste(model, method, sep = "\n"))

# rev() so the rows read top-to-bottom in the same order as the other figures
cond_levels <- rev(as.vector(t(outer(model_order, method_order,
                                     paste, sep = "\n"))))
hm$condition <- factor(hm$condition, levels = cond_levels)

p <- ggplot(hm, aes(factor(question), condition, fill = factor(score))) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(aes(label = score), colour = "white", size = 2.5,
            fontface = "bold") +
  scale_fill_manual(values = c("0" = brick, "1" = amber, "2" = moss),
                    labels = c("0 — incorrect", "1 — partly correct",
                               "2 — fully correct")) +
  labs(x = "Question number", y = NULL) +
  my_theme +
  theme(panel.grid = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "bottom",
        legend.justification = "center",
        legend.margin = margin(t = -4, b = 0),
        axis.title.x = element_text(margin = margin(t = 4)),
        axis.text.y = element_text(size = 8))
ggsave("figures/fig_3_4_per_question.png", p, width = 7.8, height = 2.9, dpi = 300)

# ---- Fig 3.5 - response times ----
tm <- results %>%
  filter(!is.na(time_s)) %>%
  mutate(condition = paste(model, method, sep = "\n"))
tm$condition <- factor(tm$condition,
                       levels = as.vector(t(outer(model_order, method_order,
                                                  paste, sep = "\n"))))

p <- ggplot(tm, aes(condition, time_s, fill = method)) +
  geom_boxplot(colour = ink, linewidth = 0.35, width = 0.55,
               outlier.size = 1, outlier.colour = ink, outlier.alpha = 0.5) +
  scale_fill_manual(values = fills) +
  labs(x = NULL, y = "Response time (s)") +
  my_theme +
  theme(axis.text.x = element_text(size = 8))
ggsave("figures/fig_3_5_response_time.png", p, width = 6.2, height = 3.5, dpi = 300)

# ---- Fig 3.6 - token usage ----
tok <- results %>%
  filter(!is.na(tokens)) %>%   # skips any cell I never recorded
  group_by(model, method) %>%
  summarise(mean_tokens = mean(tokens), .groups = "drop")

mult <- tok %>%
  tidyr::pivot_wider(id_cols = model, names_from = method,
                     values_from = mean_tokens) %>%
  mutate(label = sprintf("×%.2f", `Tool calling` / `Pre-fetch`),
         top   = pmax(`Pre-fetch`, `Tool calling`) + 900)

p <- ggplot(tok, aes(model, mean_tokens, fill = method)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.68,
           colour = ink, linewidth = 0.3) +
  geom_text(aes(label = format(round(mean_tokens), big.mark = ",")),
            position = position_dodge(width = 0.78),
            vjust = -0.5, size = 3.1, colour = ink) +
  geom_text(data = mult, aes(x = model, y = top, label = label),
            inherit.aes = FALSE, colour = rust, fontface = "bold", size = 3.4) +
  scale_fill_manual(values = fills) +
  scale_y_continuous(limits = c(0, 10600), expand = c(0, 0),
                     labels = scales::comma) +
  labs(x = NULL, y = "Mean tokens per question") +
  my_theme
ggsave("figures/fig_3_6_tokens.png", p, width = 6.2, height = 3.5, dpi = 300)

cat("done - four figures written to figures/\n")
