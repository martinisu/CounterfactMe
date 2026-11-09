# ============================================================
# CounterfactMe — smoke test før publisering
# Kjør:  source("smoke-test.R")
# Alt som skrives ut med [FEIL] må undersøkes før push.
# ============================================================

library(CounterfactMe)

ok   <- function(msg) cat(sprintf("  [ok]   %s\n", msg))
feil <- function(msg) cat(sprintf("  [FEIL] %s\n", msg))
sjekk <- function(betingelse, msg) if (isTRUE(betingelse)) ok(msg) else feil(msg)

cat("\n=== 0. Versjon ===\n")
cat("  ", as.character(packageVersion("CounterfactMe")), "\n")

# ------------------------------------------------------------
cat("\n=== 1. Norske tegn rendres riktig (208 escapes) ===\n")
# Dette er det viktigste testen i kveld. Skal vise æ ø å,
# IKKE æ eller <U+00E6> eller ??.
set.seed(1)
x <- counterfact_me(lang = "no")
utskrift <- paste(capture.output(print(x)), collapse = "\n")

sjekk(!grepl("\\\\u00|<U\\+", utskrift),
      "ingen ra escape-sekvenser i utskriften")
sjekk(grepl("[æøåÆØÅ]", utskrift),
      "norske tegn faktisk til stede")
cat("\n--- se selv, skal vere lesbart: ---\n")
print(x)

# ------------------------------------------------------------
cat("\n=== 2. Helse-dimensjonen (CSV-kolonner dopt om) ===\n")
# p_saa_som / p_daarlig / p_meget_daarlig — hvis renamet gikk galt
# blir self_rated NA for alle.
set.seed(2)
helse <- vapply(1:200, function(i) {
  h <- counterfact_me(lang = "no", min_age = 20)$self_rated_health
  if (is.null(h)) NA_character_ else as.character(h)
}, character(1))

sjekk(mean(is.na(helse)) < 0.10,
      sprintf("self_rated_health er utfylt (%.0f%% NA)", 100 * mean(is.na(helse))))
sjekk(length(unique(stats::na.omit(helse))) >= 3,
      sprintf("varierer over flere verdier (%d unike)",
              length(unique(stats::na.omit(helse)))))
cat("  fordeling:\n")
print(table(helse, useNA = "ifany"))

# ------------------------------------------------------------
cat("\n=== 3. Bourdieu (felt dopt om til bourdieu_okonomisk) ===\n")
set.seed(3)
b <- counterfact_me(lang = "no", min_age = 25)
sjekk(!is.null(b$bourdieu_okonomisk) && !is.na(b$bourdieu_okonomisk),
      "bourdieu_okonomisk finnes og er utfylt")
sjekk(is.null(b$bourdieu_økonomisk),
      "gammelt feltnavn er borte")
sjekk(grepl("apital|lasse", paste(capture.output(print(b)), collapse = "")),
      "bourdieu vises i print uten feil")
cat(sprintf("  okonomisk=%s kulturell=%s sosial=%s\n",
            b$bourdieu_okonomisk, b$bourdieu_kulturell, b$bourdieu_sosial))

# ------------------------------------------------------------
cat("\n=== 4. conditional = FALSE (buggen jeg fikset) ===\n")
r <- tryCatch({ counterfact_me(conditional = FALSE); "ok" },
              error = function(e) conditionMessage(e))
sjekk(identical(r, "ok"), paste("conditional=FALSE kjorer:", r))

r2 <- tryCatch({
  for (i in 1:25) counterfact_me(conditional = FALSE, min_age = 18)
  "ok"
}, error = function(e) conditionMessage(e))
sjekk(identical(r2, "ok"), paste("25 uavhengige trekninger:", r2))

# ------------------------------------------------------------
cat("\n=== 5. Narrativ ===\n")
set.seed(5)
x5 <- counterfact_me(lang = "no", min_age = 30)
for (stil in c("biography", "compact", "obituary")) {
  txt <- tryCatch(narrate_life(x5, style = stil),
                  error = function(e) paste("ERROR:", conditionMessage(e)))
  sjekk(!grepl("^ERROR", txt) && nchar(txt) > 40,
        sprintf("narrate_life(style='%s') gir tekst (%d tegn)", stil, nchar(txt)))
  sjekk(!grepl("\\\\u00|<U\\+|NULL|NA\\b", txt),
        sprintf("  ... uten escapes/NULL/NA i '%s'", stil))
}
cat("\n--- biografi, les den: ---\n")
cat(narrate_life(x5, style = "biography"), "\n")

# ------------------------------------------------------------
cat("\n=== 6. Constrained + parallelle liv ===\n")
set.seed(6)
c1 <- tryCatch(counterfact_me_constrained(list(age = 42, gender = "M", county = "Oslo")),
               error = function(e) NULL)
sjekk(!is.null(c1), "counterfact_me_constrained kjorer")
if (!is.null(c1)) {
  sjekk(identical(c1$age, 42L) || identical(as.integer(c1$age), 42L),
        sprintf("alder-constraint holdt (fikk %s)", c1$age))
  sjekk(identical(c1$gender, "M"), sprintf("kjonn-constraint holdt (fikk %s)", c1$gender))
}

p1 <- tryCatch(counterfact_parallel_lives(
        givens = list(age = 40, gender = "F"), vary_dim = "county", n = 4),
      error = function(e) NULL)
sjekk(!is.null(p1) && length(p1) == 4, "counterfact_parallel_lives gir 4 liv")
if (!is.null(p1)) {
  fylker <- vapply(p1, function(l) as.character(l$county %||% NA), character(1))
  sjekk(length(unique(fylker)) > 1,
        sprintf("fylkene varierer: %s", paste(unique(fylker), collapse = ", ")))
}

# ------------------------------------------------------------
cat("\n=== 7. Umulighets-filteret ===\n")
set.seed(7)
probs <- unlist(lapply(1:300, function(i) find_impossibilities(counterfact_me(lang = "no"))))
sjekk(length(probs) == 0,
      sprintf("300 liv uten umuligheter (fant %d)", length(probs)))
if (length(probs)) print(table(probs))

# ------------------------------------------------------------
cat("\n=== 8. Begge sprak ===\n")
set.seed(8); en <- counterfact_me(lang = "en")
set.seed(8); no <- counterfact_me(lang = "no")
sjekk(identical(en$age, no$age) && identical(en$gender, no$gender),
      "samme seed gir samme person pa begge sprak")
sjekk(!grepl("\\\\u00", paste(capture.output(print(en)), collapse = "")),
      "engelsk utskrift uten escapes")

# ------------------------------------------------------------
cat("\n=== 9. Robusthet: 500 liv, alle aldre ===\n")
set.seed(9)
feilet <- 0
for (i in 1:500) {
  r <- tryCatch({ invisible(capture.output(print(counterfact_me(lang = "no")))); TRUE },
                error = function(e) { if (feilet < 3) message("  ", conditionMessage(e)); FALSE })
  if (!isTRUE(r)) feilet <- feilet + 1
}
sjekk(feilet == 0, sprintf("500 liv trukket og printet (%d feil)", feilet))

# ------------------------------------------------------------
cat("\n=== 10. verify_consistency ===\n")
vc <- tryCatch(verify_consistency(N = 300, verbose = FALSE),
               error = function(e) NULL)
sjekk(!is.null(vc), "verify_consistency kjorer")
if (!is.null(vc) && nrow(vc$summary) > 0) {
  cat("  topp inkonsistenser (forventet: bare 'usannsynlig', ikke 'umulig'):\n")
  print(utils::head(vc$summary, 8))
} else if (!is.null(vc)) {
  ok("ingen inkonsistenser i det hele tatt")
}

cat("\n============================================\n")
cat("Ferdig. Scroll opp og se etter [FEIL].\n")
cat("Les ogsa biografien i seksjon 5 — den skal\n")
cat("vaere lesbar norsk, ikke escape-suppe.\n")
cat("============================================\n")
