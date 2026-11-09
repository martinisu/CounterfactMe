# Forventet levetid. Kjør: Rscript dev/demo-levetid.R
if (!requireNamespace("CounterfactMe", quietly = TRUE)) devtools::load_all(".")
library(CounterfactMe); set.seed(12)

cat("\n##### 1. Oppslag mot SSB 14569 #####\n")
cat(sprintf("  %-28s %8s %8s %8s\n", "", "v/30", "v/50", "v/70"))
for (g in c("M","F")) for (s in c(1,6)) {
  lab <- sprintf("%s, sentralitet %d", ifelse(g=="M","mann","kvinne"), s)
  v <- vapply(c(30,50,70), function(a) CounterfactMe:::.life_expectancy(a,g,s), numeric(1))
  cat(sprintf("  %-28s %8.1f %8.1f %8.1f\n", lab, v[1], v[2], v[3]))
}
cat("\n  Kjonnsgap v/30: ")
cat(sprintf("%.1f aar\n", CounterfactMe:::.life_expectancy(30,"F",NA) -
                          CounterfactMe:::.life_expectancy(30,"M",NA)))

cat("\n##### 2. Mor og far doer ikke lenger likt #####\n")
n <- 500
d <- t(vapply(seq_len(n), function(i) {
  x <- counterfact_me(min_age = 55, max_age = 75)
  c(mor = as.numeric(!is.null(x$mother$death_year) && !is.na(x$mother$death_year)),
    far = as.numeric(!is.null(x$father$death_year) && !is.na(x$father$death_year)))
}, numeric(2)))
cat(sprintf("  ego 55-75, %d trekk:  mor doed %.1f pst   far doed %.1f pst\n",
            n, 100*mean(d[,"mor"]), 100*mean(d[,"far"])))
cat("  Ventet: far doed oftere. Var identisk for fiksen.\n")

cat("\n##### 3. Levetid paa et trukket liv #####\n")
for (i in 1:4) {
  x <- counterfact_me(min_age = 25, max_age = 70)
  cat(sprintf("  %s, %d aar, %s (sentr. %s): %.1f aar igjen -> %.1f\n",
              x$gender, x$age, x$municipality, x$sentralitet,
              x$life_expectancy_remaining, x$life_expectancy_at))
}

cat("\n##### 4. FYRER GRADIENTEN? #####\n")
cat("Fire enkeltliv kan ikke vise dette. Her er fordelingen over mange trekk.\n\n")
set.seed(44)
n <- 800
d <- t(vapply(seq_len(n), function(i) {
  x <- counterfact_me(min_age = 30, max_age = 60)
  c(off = if (is.null(x$life_expectancy_income_offset)) NA_real_
          else x$life_expectancy_income_offset,
    inc = if (is.null(x$income_nok)) NA_real_ else x$income_nok,
    le  = if (is.null(x$life_expectancy_at)) NA_real_ else x$life_expectancy_at,
    base = if (is.null(x$life_expectancy_baseline)) NA_real_
           else x$life_expectancy_baseline,
    m = as.numeric(identical(x$gender, "M")))
}, numeric(5)))
d <- d[!is.na(d[,"off"]), , drop = FALSE]

cat(sprintf("  trekk med et avvik satt: %d av %d\n", nrow(d), n))
cat(sprintf("  avvik: min %+.1f  median %+.1f  maks %+.1f\n",
            min(d[,"off"]), median(d[,"off"]), max(d[,"off"])))
cat(sprintf("  andel med avvik = 0 (fyrer ikke): %.1f pst\n",
            100*mean(d[,"off"] == 0)))
cat("  Ventet: spenn fra ca -7 til +7 for menn. Er alt 0, fyrer den ikke.\n")

cat("\n  Forventet levealder etter inntektskvartil:\n")
q <- cut(d[,"inc"], quantile(d[,"inc"], c(0,.25,.5,.75,1), na.rm = TRUE),
         labels = c("Q1","Q2","Q3","Q4"), include.lowest = TRUE)
for (lv in levels(q)) {
  s <- d[q == lv, , drop = FALSE]
  cat(sprintf("    %s  inntekt median %9s   levealder %.1f\n", lv,
              format(round(median(s[,"inc"])), big.mark=" "), mean(s[,"le"])))
}
cat(sprintf("\n  Q4 minus Q1: %.1f aar\n",
            mean(d[q=="Q4","le"]) - mean(d[q=="Q1","le"])))

cat("\n  Null-sum-sjekken: flytter gradienten snittet?\n")
cat(sprintf("    snitt uten gradient: %.2f\n", mean(d[,"base"])))
cat(sprintf("    snitt med gradient:  %.2f\n", mean(d[,"le"])))
cat("    Skal vaere nesten like. Stort avvik = gradienten er ikke null-sum\n")
cat("    i den trukne populasjonen, selv om den er det over persentiler.\n")
