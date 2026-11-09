# Yrkesoverstyringen: henger yrkesetiketten sammen med inntekten?
#
# RETTELSE fra forrige versjon av denne fila: den delte income_nok
# (aarsinntekt) paa median_nok fra SSB 11418, som er MAANEDSLOENN. Derfor
# laa "forholdstallet" rundt 11 og "andel som bommer med faktor 2" var
# 100 pst ogsaa for koherente liv. Det var min enhetsfeil, ikke et funn.
#
# Her brukes i stedet Spearman-korrelasjon mellom yrkets loennsnivaa og
# livets inntekt. Den er enhetsfri, saa maanedslønn mot aarsinntekt spiller
# ingen rolle.
#
#   A  vanlige counterfact_me()-liv, maalt mot sitt EGET trukne yrke.
#      Her er inntekten faktisk trukket betinget paa yrket, saa rho skal
#      vaere klart positiv.
#   B  overstyrte liv, maalt mot yrket de er MERKET med. Er etiketten
#      loesrevet, gaar rho mot null.
#
# Nullhypotesen som skal kunne vinne: rho_B er ikke vesentlig lavere enn
# rho_A, altsaa overstyringen gir fortsatt et liv der tittel og inntekt
# henger sammen.

devtools::load_all(quiet = TRUE)
.load_data()
os <- CounterfactMe:::.cfm_env$occupations_salary
wage <- function(lbl) {
  ix <- which(os$label == lbl)
  if (length(ix)) as.numeric(os$median_nok[ix[1]]) else NA_real_
}

n <- 120L
set.seed(12)

pair_of <- function(x) {
  w <- wage(as.character(x$occupation))
  inc <- suppressWarnings(as.numeric(x$income_nok))
  if (is.na(w) || is.na(inc) || w <= 0 || inc <= 0) return(NULL)
  c(w, inc)
}

collect <- function(gen) {
  out <- matrix(NA_real_, n, 2)
  i <- 0L
  while (i < n) {
    p <- pair_of(gen())
    if (!is.null(p)) { i <- i + 1L; out[i, ] <- p }
  }
  out
}

cat("\nAntall yrkeskoder:", length(unique(os$label)), "\n")

A <- collect(function()
  counterfact_me(min_age = 30L, max_age = 60L, gender = "M"))

cands <- unique(os$label[os$schedule == "all" & nzchar(os$code) & os$code != "0000"])
B <- collect(function()
  suppressWarnings(counterfact_me_constrained(
    list(age = sample(30:60, 1), gender = "M", occupation = sample(cands, 1)),
    max_attempts = 300L)))

rho <- function(m) suppressWarnings(stats::cor(m[, 1], m[, 2], method = "spearman"))
ci <- function(m) {
  r <- replicate(600, {
    ix <- sample(nrow(m), nrow(m), replace = TRUE)
    rho(m[ix, , drop = FALSE])
  })
  stats::quantile(r, c(.025, .975), na.rm = TRUE)
}

rA <- rho(A); rB <- rho(B); cA <- ci(A); cB <- ci(B)

cat("\n=== Spearman: yrkets loennsnivaa mot livets inntekt ===\n\n")
cat(sprintf("  A  eget trukne yrke    rho = %+.3f   95 %% KI [%+.3f, %+.3f]\n",
            rA, cA[1], cA[2]))
cat(sprintf("  B  paaskrevet yrke     rho = %+.3f   95 %% KI [%+.3f, %+.3f]\n",
            rB, cB[1], cB[2]))

cat("\n")
if (cB[2] < cA[1]) {
  cat("  Intervallene overlapper ikke: sammenhengen er svakere i B.\n")
} else {
  cat("  Intervallene overlapper. Dataene skiller dem ikke ved n =", n, "\n")
}
if (cB[1] <= 0 && cB[2] >= 0) {
  cat("  Og B er ikke skillbar fra null: yrkestittelen i de overstyrte\n")
  cat("  livene baerer ingen informasjon om inntekten.\n")
}
cat("\n")
