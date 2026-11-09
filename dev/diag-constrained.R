# Hvor ofte faller counterfact_me_constrained() tilbake paa direkte
# overstyring, og hvor inkoherent blir livet naar den gjoer det?
#
# Overstyringen skriver givens-verdien rett inn i det FERDIGE objektet uten
# aa trekke noe paa nytt. For county betyr det at x$county settes, mens
# x$municipality -- og alt som er betinget paa kommunen -- blir staaende.

devtools::load_all(quiet = TRUE)

n <- 40L

cat("\n=== 1. Andel trekk som ender i overstyring, per vary_dim ===\n")
cat("    (max_attempts = 300, altsaa pakkens default)\n\n")

for (vd in c("county", "occupation", "education", "background",
             "religion", "party")) {
  vals <- CounterfactMe:::.sample_vary_values(vd, n)
  vals <- rep_len(vals, n)
  overrode <- vapply(seq_len(n), function(i) {
    g <- list(age = 40L, gender = "M")
    g[[vd]] <- vals[i]
    w <- FALSE
    withCallingHandlers(
      counterfact_me_constrained(g, max_attempts = 300L),
      warning = function(cnd) {
        if (grepl("direct override", conditionMessage(cnd))) w <<- TRUE
        invokeRestart("muffleWarning")
      })
    w
  }, logical(1))
  cat(sprintf("  %-12s %5.1f %% overstyrt  (%d av %d)\n",
              vd, 100 * mean(overrode), sum(overrode), n))
}

cat("\n=== 2. Blir fylke og kommune motstridende? ===\n")
cat("    Hver linje er et liv der overstyringen slo til.\n\n")

.load_data()
mn <- CounterfactMe:::.cfm_env$municipalities
true_county <- function(mun) {
  ix <- which(mn$name == mun)
  if (length(ix)) as.character(mn$county[ix[1]]) else NA_character_
}

mismatch <- 0L; shown <- 0L
for (i in seq_len(n)) {
  cty <- sample(unique(mn$county), 1)
  w <- FALSE
  x <- withCallingHandlers(
    counterfact_me_constrained(list(age = 40L, gender = "M", county = cty),
                               max_attempts = 300L),
    warning = function(cnd) {
      if (grepl("direct override", conditionMessage(cnd))) w <<- TRUE
      invokeRestart("muffleWarning")
    })
  if (!w) next
  real <- true_county(x$municipality)
  if (!is.na(real) && !identical(real, as.character(x$county))) {
    mismatch <- mismatch + 1L
    if (shown < 6L) {
      shown <- shown + 1L
      cat(sprintf("  county = %-22s men %s ligger i %s\n",
                  as.character(x$county), x$municipality, real))
    }
  }
}
cat(sprintf("\n  %d av %d overstyrte liv har fylke som motsier kommunen.\n",
            mismatch, n))

cat("\n=== 3. Testen som ikke kan feile ===\n")
cat("    test-api-contracts.R:76 krever bare at fylkene er ulike.\n")
cat("    Overstyringen SKRIVER distinkte fylker, saa paastanden er sann\n")
cat("    ved konstruksjon -- ogsaa naar ingen av livene faktisk ble\n")
cat("    trukket i det fylket de er merket med.\n\n")
