# Les fortellinger foer publisering.
#
# Testene i tests/testthat/test-coherence.R fanger feil vi allerede kjenner.
# Nye feil fanges bare av at noen faktisk LESER. Dette skriptet trekker 48
# liv jevnt fordelt over aldersgrupper og skriver dem til en tekstfil med
# noekkeltallene ved siden av, slik at de kan leses kritisk i ett strekk.
#
#   source("dev/audit-fortellinger.R")
#
# Se etter: alder mot utdanning, yrke og inntekt; bosituasjon mot eierskap;
# tid (presens/fortid); gjentakelser; etiketter som er limt inn raatt.

devtools::load_all(quiet = TRUE)
set.seed(as.integer(Sys.Date()))
bands <- list(c(0, 12), c(13, 19), c(20, 29), c(30, 49), c(50, 66), c(67, 95))
out <- character(0)
for (i in seq_len(48)) {
  b <- bands[[(i - 1) %% 6 + 1]]
  x <- counterfact_me(min_age = b[1], max_age = b[2])
  head <- sprintf("#%02d  %s aar | %s | %s | %s kr | %s | %s", i, x$age,
                  x$education %||% "-", x$occupation %||% "-",
                  format(x$income_nok %||% NA, big.mark = " "),
                  x$household %||% "-", x$housing_tenure %||% "-")
  out <- c(out, head, as.character(narrate_life(x)), "", strrep("-", 70), "")
}
dir.create("dev/audit", showWarnings = FALSE)
f <- file.path("dev/audit", sprintf("fortellinger-%s.txt", Sys.Date()))
writeLines(out, f)
message("Skrevet: ", f)
