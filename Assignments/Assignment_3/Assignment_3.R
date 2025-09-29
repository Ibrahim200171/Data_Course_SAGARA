# BIOL3100 - Assignment 3
# Name: Ibrahim Michael Sagara
# Date: 2025-09-28
# Notes: Self-contained. Uses only base datasets. Relative paths only.

cat("===== BIOL3100 Assignment 3: Running =====\n\n")

# -----------------------------
# Section 1 — Vectors & Conversions
# -----------------------------
cat("Section 1 — Vectors & Conversions\n")

mixed_vec <- c("1","2","3","4.5","six","7")
num_vec   <- suppressWarnings(as.numeric(mixed_vec))  # "six" becomes NA
conversion_success <- !is.na(num_vec)

cat("Original:", paste(mixed_vec, collapse=", "), "\n")
cat("As numeric:", paste(num_vec, collapse=", "), "\n")
cat("Converted OK?:", paste(conversion_success, collapse=", "), "\n\n")

# -----------------------------
# Section 2 — Summaries
# -----------------------------
cat("Section 2 — Summaries\n")

x <- c(3.2, 5.1, NA, 7.4, 2.0, 9.3, 4.4)
n_non_na <- sum(!is.na(x))
x_mean   <- mean(x, na.rm=TRUE)
x_median <- median(x, na.rm=TRUE)
x_sd     <- sd(x, na.rm=TRUE)
x_min    <- min(x, na.rm=TRUE)
x_max    <- max(x, na.rm=TRUE)

cat(sprintf("n=%d\nmean=%.3f\nmedian=%.3f\nsd=%.3f\nmin=%.1f\nmax=%.1f\n\n",
            n_non_na, x_mean, x_median, x_sd, x_min, x_max))

# -----------------------------
# Section 3 — Logical Evaluations
# -----------------------------
cat("Section 3 — Logical Evaluations\n")

y <- c(12,18,21,5,30,8)
is_big <- y > 15
any_big <- any(is_big)
all_even <- all(y %% 2 == 0)

cat("y:", paste(y, collapse=", "), "\n")
cat("y > 15:", paste(is_big, collapse=", "), "\n")
cat("Any y > 15?:", any_big, "\n")
cat("All y even?:", all_even, "\n\n")

# -----------------------------
# Section 4 — Subsetting (mtcars)
# -----------------------------
cat("Section 4 — Subsetting (mtcars)\n")

data("mtcars")
subset_rows <- mtcars$mpg > 25 & mtcars$cyl == 4
mtcars_sub  <- mtcars[subset_rows, c("mpg","hp","wt")]
mtcars_sub_models <- rownames(mtcars)[subset_rows]

cat("Models with mpg > 25 and cyl == 4:\n")
if (nrow(mtcars_sub) == 0) {
  cat("  (none)\n\n")
} else {
  for (i in seq_len(nrow(mtcars_sub))) {
    cat(sprintf("  %s: mpg=%.1f, hp=%d, wt=%.3f\n",
                mtcars_sub_models[i], mtcars_sub$mpg[i], mtcars_sub$hp[i], mtcars_sub$wt[i]))
  }
  cat("\n")
}

ord <- order(mtcars$mpg, decreasing=TRUE)
top5 <- mtcars[ord[1:5], c("mpg","cyl","hp","wt")]
cat("Top 5 cars by mpg (mpg, cyl, hp, wt):\n")
print(top5)
cat("\n")

# -----------------------------
# Section 5 — Numbered Answers (INCLUDES CODE + RESULTS)
# -----------------------------
cat("Section 5 — Numbered Answers (code + results)\n")

answers_lines <- c(
  "1) Vectors & conversions — CODE:",
  "   mixed_vec <- c(\"1\",\"2\",\"3\",\"4.5\",\"six\",\"7\")",
  "   num_vec   <- suppressWarnings(as.numeric(mixed_vec))",
  "   conversion_success <- !is.na(num_vec)",
  paste0("   RESULTS: num_vec=[", paste(num_vec, collapse=", "), "]; converted_ok=[",
         paste(conversion_success, collapse=", "), "]"),
  "",
  "2) Summaries — CODE:",
  "   x <- c(3.2, 5.1, NA, 7.4, 2.0, 9.3, 4.4)",
  "   n_non_na <- sum(!is.na(x)); mean(x,na.rm=TRUE); median(x,na.rm=TRUE); sd(x,na.rm=TRUE); min(x,na.rm=TRUE); max(x,na.rm=TRUE)",
  sprintf("   RESULTS: n=%d; mean=%.3f; median=%.3f; sd=%.3f; min=%.1f; max=%.1f",
          n_non_na, x_mean, x_median, x_sd, x_min, x_max),
  "",
  "3) Logical eval — CODE:",
  "   y <- c(12,18,21,5,30,8); is_big <- y>15; any_big <- any(is_big); all_even <- all(y%%2==0)",
  paste0("   RESULTS: y>15=[", paste(is_big, collapse=", "), "]; any_big=",
         any_big, "; all_even=", all_even),
  "",
  "4) Subsetting — CODE:",
  "   data(\"mtcars\"); subset_rows <- mtcars$mpg>25 & mtcars$cyl==4; mtcars[subset_rows, c(\"mpg\",\"hp\",\"wt\")]",
  if (nrow(mtcars_sub) == 0) "   RESULTS: (no rows meet mpg>25 & cyl==4)"
  else paste0("   RESULTS: models = ", paste(mtcars_sub_models, collapse=\"; \")),
  "",
  "5) Top mpg models — CODE:",
  "   ord <- order(mtcars$mpg, decreasing=TRUE); top5 <- mtcars[ord[1:5], c(\"mpg\",\"cyl\",\"hp\",\"wt\")]",
  paste0("   RESULTS: ", paste(rownames(top5), collapse=\"; \"))
)

out_dir <- file.path("Assignments","Assignment_3")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)
out_file <- file.path(out_dir, "answers_SAGARA.txt")
writeLines(answers_lines, out_file, useBytes = TRUE)

cat("\nWrote answers to: ", out_file, "\n", sep="")
cat("===== Done. =====\n")
