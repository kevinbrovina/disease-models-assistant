# figure 2.1 - the two retrieval methods side by side
# boxes are coloured by WHO does the step - my R code or the LLM. did it this
# way because my supervisor asked whether the model writes the code, and the
# colouring answers that without needing a paragraph
# just base graphics, no extra packages to install
# run from the dissertations folder: Rscript make_flowchart.R

dir.create("figures", showWarnings = FALSE)

r_fill    <- "#E8C39E"   # steps performed by R
llm_fill  <- "#B4541D"   # steps performed by the language model
r_text    <- "#3A2A1E"
llm_text  <- "#FFFFFF"
ink       <- "#3A2A1E"
rule      <- "#9C8875"

# draws one box with text in it. pass the text as a vector, one item per line
# (base R won't wrap text for you so I split the lines myself)
draw_box <- function(x, y, w, h, lines, fill, col_text, cex = 0.72, bold = FALSE) {
  rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2,
       col = fill, border = ink, lwd = 1.1)
  n <- length(lines)
  ys <- y + (n - 1) / 2 * (h / (n + 1)) - (seq_len(n) - 1) * (h / (n + 1))
  text(x, ys, lines, col = col_text, cex = cex,
       font = if (bold) 2 else 1)
}

# arrow going down between two boxes
arrow_down <- function(x, y_from, y_to, lty = 1) {
  arrows(x, y_from, x, y_to, length = 0.07, lwd = 1.2, col = ink, lty = lty)
}

png("figures/fig_2_1_architectures.png", width = 2250, height = 1900, res = 300)
par(mar = c(0, 0, 0, 0))
plot(NA, xlim = c(0, 100), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")

left  <- 27      # x centre of the pre-fetch column
right <- 73      # x centre of the tool-calling column
bw    <- 40      # box width
bh    <- 9       # box height

# ---- both methods start from the same question ----
draw_box(50, 95, 44, 7.5, "User submits a question in plain English",
         "#FFFFFF", ink, cex = 0.78, bold = TRUE)

# split
segments(50, 91.2, 50, 88, col = ink, lwd = 1.2)
segments(left, 88, right, 88, col = ink, lwd = 1.2)
arrow_down(left, 88, 82.8)
arrow_down(right, 88, 82.8)

# ---- column headings ----
text(left,  99.5, "", cex = 0.8)
draw_box(left,  78.5, bw, 6, "PRE-FETCH RETRIEVAL (baseline)",
         "#F5EDE4", ink, cex = 0.75, bold = TRUE)
draw_box(right, 78.5, bw, 6, "STRUCTURED TOOL CALLING",
         "#F5EDE4", ink, cex = 0.75, bold = TRUE)

# ---- left side: pre-fetch ----
arrow_down(left, 75.5, 70.5)
draw_box(left, 65.5, bw, bh,
         c("Regular-expression rules scan the",
           "question for gene, disorder and",
           "phenotype identifiers"), r_fill, r_text)

arrow_down(left, 61, 56)
draw_box(left, 51, bw, bh,
         c("Fixed R functions retrieve the",
           "matching rows and compute",
           "counts"), r_fill, r_text)

arrow_down(left, 46.5, 41.5)
draw_box(left, 36.5, bw, bh,
         c("Retrieved facts are inserted",
           "into the prompt as a",
           "portal-context block"), r_fill, r_text)

arrow_down(left, 32, 24.5)

# ---- right side: tool calling ----
arrow_down(right, 75.5, 70.5)
draw_box(right, 65.5, bw, bh,
         c("Question is sent with twelve",
           "typed tool declarations",
           "(names, arguments, types)"), r_fill, r_text)

arrow_down(right, 61, 56)
draw_box(right, 51, bw, bh,
         c("Model selects one tool and",
           "supplies argument values",
           "- it writes no code"), llm_fill, llm_text)

arrow_down(right, 46.5, 41.5)
draw_box(right, 36.5, bw, bh,
         c("R validates the arguments and",
           "runs the pre-written function",
           "against the FST tables"), r_fill, r_text)

arrow_down(right, 32, 27)
draw_box(right, 22.5, bw, bh - 1,
         c("Result is returned to the model",
           "as text"), r_fill, r_text)

# dashed loop back up to the tool-selection box - the model can go round
# this up to 4 times before it has to answer
loop_x <- right + bw / 2 + 3.5
segments(right + bw / 2 + 1.5, 22.5, loop_x, 22.5, col = ink, lwd = 1.1, lty = 2)
segments(loop_x, 22.5, loop_x, 51, col = ink, lwd = 1.1, lty = 2)
arrows(loop_x, 51, right + bw / 2 + 1.5, 51,
       length = 0.06, lwd = 1.1, col = ink, lty = 2)
text(loop_x, 17.5, "up to 4 rounds", cex = 0.6, col = ink, pos = 2)

arrow_down(right, 18, 14.5)

# ---- both sides join back up at the bottom ----
segments(left, 24.5, left, 12, col = ink, lwd = 1.2)
segments(right, 14.5, right, 12, col = ink, lwd = 1.2)
segments(left, 12, right, 12, col = ink, lwd = 1.2)
arrow_down(50, 12, 9.5)

draw_box(50, 5.5, 52, 7.5,
         c("Language model composes the answer from the verified facts",
           "and the interface labels the source, time and token cost"),
         llm_fill, llm_text, cex = 0.7)

# ---- legend ----
# stuck it bottom left, it's the only empty corner
text(2.5, 11.5, "Performed by:", pos = 4, cex = 0.64, col = ink, font = 2)
rect(3, 6.6, 6, 8.6, col = r_fill, border = ink, lwd = 1)
text(6.6, 7.6, "fixed R code", pos = 4, cex = 0.62, col = ink)
rect(3, 2.4, 6, 4.4, col = llm_fill, border = ink, lwd = 1)
text(6.6, 3.4, "language model", pos = 4, cex = 0.62, col = ink)

dev.off()
cat("done - figures/fig_2_1_architectures.png\n")
