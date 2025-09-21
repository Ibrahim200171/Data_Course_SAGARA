https://github.com/Ibrahim200171/Data_Course_SAGARA.git############################################################
# Assignment 2 – Tasks 4–10
# Run inside your Data_Course_LASTNAME project (or setwd() above)
############################################################

# (Optional) Check we’re at the repo root
getwd()

# ----- Task 4: list .csv files in Data/ (non-recursive) -----
csv_files <- list.files(
  path = "Data",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)
csv_files

# ----- Task 5: how many .csv files? -----
length(csv_files)

# ----- Task 6: read wingspan_vs_mass.csv into df -----
df <- read.csv(file.path("Data", "wingspan_vs_mass.csv"), stringsAsFactors = FALSE)

# ----- Task 7: first 5 rows -----
head(df, 5)
https://github.com/Ibrahim200171/Data_Course_SAGARA.git


# ----- Task 8: files in Data/ (recursively) that start with lowercase 'b' -----
b_files <- list.files(
  path = "Data",
  pattern = "^b",
  full.names = TRUE,
  recursive = TRUE
)
b_files

# ----- Task 9: print first line of each of those 'b' files (for-loop) -----
if (length(b_files) > 0) {
  for (f in b_files) {
    first_line <- tryCatch(readLines(f, n = 1, warn = FALSE),
                           error = function(e) "<binary or unreadable>")
    cat("FILE:", f, "\nFIRST LINE:", first_line, "\n\n")
  }
} else {
  cat("No files starting with 'b' were found under Data/.\n")
}

# ----- Task 10: first line of every .csv under Data/ (recursive) -----
csv_all <- list.files(
  path = "Data",
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = TRUE
)
if (length(csv_all) > 0) {
  for (f in csv_all) {
    first_line <- tryCatch(readLines(f, n = 1, warn = FALSE),
                           error = function(e) "<unreadable>")
    cat("CSV FILE:", f, "\nFIRST LINE:", first_line, "\n\n")
  }
} else {
  cat("No .csv files were found under Data/.\n")
}