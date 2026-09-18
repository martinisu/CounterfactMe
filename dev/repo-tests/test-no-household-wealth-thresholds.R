# Repo-sjekk: ingen husholdningsterskler skal ha sneket seg tilbake inn i koden.
# 12.14e6 og 28.25e6 er Top5- og Top1-grensene fra SSB 10318, en
# husholdningstabell. De hoerer ikke hjemme i en personbasert sampler.
src <- readLines("R/conditional.R", warn = FALSE)
code <- src[!grepl("^\\s*#", src)]
forbudt <- c("133e6", "133.4e6", "28.25e6", "12.14e6")
for (m in forbudt) {
  treff <- grep(m, code, fixed = TRUE)
  if (length(treff)) {
    stop(sprintf("Husholdningsterskel '%s' funnet i R/conditional.R paa %d sted(er): %s",
                 m, length(treff), paste(code[treff], collapse = " | ")))
  }
}
cat("OK: ingen husholdningsterskler i koden\n")
