# Automatisk sveip: N liv, teller regelbrudd i foerstetrekket (det
# sluttsjekken i counterfact_me() fanger), tekstflagg i fortellingen, og
# skriver et utvalg fortellinger til lesing. Kjoeres i biter:
#   sed 's/SEED <- 1/SEED <- 7/' sweep.R > s7.R && node run.mjs <pakke> s7.R
SEED <- 1; N <- 500
set.seed(SEED)
bands <- list(c(0,12), c(13,19), c(20,29), c(30,49), c(50,66), c(67,95))
gate <- character(0); flags <- character(0); sample_txt <- character(0)
textflag <- list(
  "Med ingen fullf" = "Med ingen fullf",
  "Den ene forelderen bodde i" = "Den ene forelderen bodde i",
  "på deltid uten andel" = "på deltid —|på deltid, og",
  "dobbel parentes" = "\\([^()]*\\([^()]*\\)[^()]*\\)",
  "parti to ganger" = "stemmer[^.]*\\..*stemmer",
  "NA i tekst" = "\\bNA\\b",
  "tom setning" = "\\.\\s*\\.",
  "liten forbokstav etter punktum" = "\\. [a-zæøå]",
  "dobbelt mellomrom" = "[^ ]  [^ ]",
  "ordet gjentatt" = "\\b(\\w+) \\1\\b",
  "uoppgitt i tekst" = "[Uu]oppgitt",
  "engelsk ord" = "\\b(and|the|with|unknown)\\b")
for (i in seq_len(N)) {
  b <- bands[[(i - 1) %% 6 + 1]]
  x0 <- .counterfact_conditional(available_dimensions(), "no", NULL, b[1], b[2])
  gate <- c(gate, .find_implausibilities(x0))
  x <- counterfact_me(min_age = b[1], max_age = b[2])
  t <- as.character(narrate_life(x))
  for (nm in names(textflag)) if (grepl(textflag[[nm]], t, perl = TRUE))
    flags <- c(flags, paste0("[", nm, "] ...", regmatches(t, regexpr(paste0(".{0,70}(", textflag[[nm]], ").{0,40}"), t, perl = TRUE)), "..."))
  if (i %% 12 == 0) sample_txt <- c(sample_txt, sprintf("#%d %s aar | %s | %s | %s kr", i, x$age,
      x$education %||% "-", x$occupation %||% "-", format(x$income_nok %||% NA, big.mark = " ")),
      gsub("\n\n", " / ", t), "")
}
out <- c(sprintf("SEED %d, N %d", SEED, N),
         sprintf("Sluttsjekken ville trukket paa nytt: %d av %d foerstetrekk", length(gate), N),
         capture.output(print(sort(table(gate), decreasing = TRUE))),
         sprintf("Tekstflagg: %d", length(flags)), head(flags, 40), "", "UTVALG:", sample_txt)
writeLines(out, "/out.txt")
