# builds the appendix question table as png images
# booktabs style - horizontal rules only, no shading, no vertical lines
# run from the dissertations folder: Rscript make_appendix_table.R

dir.create("figures", showWarnings = FALSE)

q <- read.csv("appendix_questions.csv", stringsAsFactors = FALSE,
              fileEncoding = "UTF-8")

ink  <- "#1A1A1A"
grey <- "#8A8A8A"

# text wrapping - base R won't do it for you
wrap_lines <- function(s, n) strwrap(s, width = n)

draw_page <- function(rows, file, title, part) {
  qw <- 40; aw <- 46
  heights <- mapply(function(a, b) max(length(wrap_lines(a, qw)),
                                       length(wrap_lines(b, aw))) + 0.7,
                    rows$question, rows$answer)
  total <- sum(heights) + 5

  png(file, width = 1900, height = round(34 * total), res = 160)
  par(mar = c(0.2, 0.2, 0.2, 0.2), family = "serif")
  plot(NA, xlim = c(0, 100), ylim = c(total, 0), axes = FALSE, xlab = "", ylab = "")

  x_num <- 1.5; x_q <- 6; x_a <- 51

  # caption above the rule, as a table caption should be
  text(x_num, 1.0, title, pos = 4, font = 2, cex = 0.92, col = ink)
  text(x_num, 1.9, part,  pos = 4, font = 3, cex = 0.8,  col = grey)

  y <- 3.1
  segments(1, y - 0.55, 99, y - 0.55, col = ink, lwd = 1.9)      # top rule
  text(x_num, y, "#",                pos = 4, font = 2, cex = 0.8, col = ink)
  text(x_q,   y, "Question",         pos = 4, font = 2, cex = 0.8, col = ink)
  text(x_a,   y, "Reference answer", pos = 4, font = 2, cex = 0.8, col = ink)
  segments(1, y + 0.5, 99, y + 0.5, col = ink, lwd = 0.9)        # mid rule

  y <- y + 1.3
  for (i in seq_len(nrow(rows))) {
    ql <- wrap_lines(rows$question[i], qw)
    al <- wrap_lines(rows$answer[i],   aw)
    text(x_num, y, rows$q[i], pos = 4, cex = 0.78, col = ink)
    for (k in seq_along(ql)) text(x_q, y + (k - 1) * 0.82, ql[k], pos = 4, cex = 0.78, col = ink)
    for (k in seq_along(al)) text(x_a, y + (k - 1) * 0.82, al[k], pos = 4, cex = 0.78, col = ink)
    y <- y + heights[i]
    if (i < nrow(rows)) segments(1, y - 0.62, 99, y - 0.62, col = grey, lwd = 0.35)
  }
  segments(1, y - 0.55, 99, y - 0.55, col = ink, lwd = 1.9)      # bottom rule

  dev.off()
  cat("  ", file, "\n")
}

draw_page(q[1:11, ],  "figures/appendix_a_questions_1.png",
          "Table A.1. Benchmark questions and reference answers.",
          "Questions 1-11 of 22.")
draw_page(q[12:22, ], "figures/appendix_a_questions_2.png",
          "Table A.1 continued.",
          "Questions 12-22 of 22.")

cat("done\n")
