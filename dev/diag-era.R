# Dekomponerer hvor forskjellen mellom kohortene 1935 og 1980 oppstaar.
# Kjoer: Rscript -e 'devtools::load_all(); source("dev/diag-era.R")'
devtools::load_all(quiet = TRUE)
set.seed(1)

n <- 300L

probe <- function(birth_year) {
  band <- integer(0); cohort <- integer(0)
  for (i in seq_len(n)) {
    e <- counterfact_era_lives(eras = birth_year)[[1]]
    band   <- c(band,   if (is.null(e$education_band_code)) NA_integer_
                        else as.integer(e$education_band_code))
    cohort <- c(cohort, if (is.null(e$education_code)) NA_integer_
                        else as.integer(e$education_code))
  }
  list(band = band, cohort = cohort)
}

cat("ref_year:", CounterfactMe:::.cfm_env$ref_year, "\n")
cat("multiplikator 1935:", CounterfactMe:::.edu_cohort_tertiary_mult(1935L), "\n")
cat("multiplikator 1980:", CounterfactMe:::.edu_cohort_tertiary_mult(1980L), "\n\n")

for (by in c(1935L, 1980L)) {
  p <- probe(by)
  cat(sprintf("--- kohort %d (alder %d) ---\n",
              by, CounterfactMe:::.cfm_env$ref_year - by))
  cat(sprintf("  baand  : snitt %.3f | andel >=6: %.3f | andel >=7: %.3f\n",
              mean(p$band, na.rm = TRUE),
              mean(p$band >= 6, na.rm = TRUE),
              mean(p$band >= 7, na.rm = TRUE)))
  cat(sprintf("  justert: snitt %.3f | andel >=6: %.3f | andel >=7: %.3f\n",
              mean(p$cohort, na.rm = TRUE),
              mean(p$cohort >= 6, na.rm = TRUE),
              mean(p$cohort >= 7, na.rm = TRUE)))
  cat(sprintf("  nedjustert i %.1f pst av trekkene\n",
              100 * mean(p$cohort < p$band, na.rm = TRUE)))
  cat(sprintf("  fordeling baand : %s\n",
              paste(names(table(p$band)), table(p$band), sep = ":", collapse = " ")))
  cat(sprintf("  fordeling justert: %s\n\n",
              paste(names(table(p$cohort)), table(p$cohort), sep = ":", collapse = " ")))
  assign(paste0("res", by), p)
}

cat(sprintf("DIFFERANSE baand  (1980 - 1935): %.3f\n",
            mean(res1980$band, na.rm = TRUE) - mean(res1935$band, na.rm = TRUE)))
cat(sprintf("DIFFERANSE justert (1980 - 1935): %.3f   [testen krever > 0.8]\n",
            mean(res1980$cohort, na.rm = TRUE) - mean(res1935$cohort, na.rm = TRUE)))
