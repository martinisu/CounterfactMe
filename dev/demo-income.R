# Ny inntektsfordeling mot SSB 06655. Kjør: Rscript dev/demo-income.R
if (!requireNamespace("CounterfactMe", quietly = TRUE)) devtools::load_all(".")
library(CounterfactMe)
set.seed(7)

cat("\n##### 1. Trukket fordeling mot SSBs faktiske intervaller #####\n")
n <- 40000
d <- sample_income(n)
tbl <- CounterfactMe:::.cfm_env$income_by_age
agg <- aggregate(count ~ bracket_label, data = tbl, FUN = sum)
agg$ssb_andel <- agg$count / sum(agg$count)
obs <- as.data.frame(table(d$bracket), stringsAsFactors = FALSE)
names(obs) <- c("bracket_label", "n")
m <- merge(agg, obs, all.x = TRUE)
m$trukket <- ifelse(is.na(m$n), 0, m$n) / n
m <- m[order(-m$ssb_andel), c("bracket_label", "ssb_andel", "trukket")]
m$ssb_andel <- round(m$ssb_andel, 4); m$trukket <- round(m$trukket, 4)
print(m, row.names = FALSE)

cat("\n##### 2. Treffer snittet? #####\n")
ssb_mean <- sum(as.numeric(tbl$sum_mill)) * 1e6 / sum(as.numeric(tbl$count))
cat(sprintf("  SSB snitt (m/ inntekt): %12s kr\n", format(round(ssb_mean), big.mark = " ")))
cat(sprintf("  Trukket snitt:          %12s kr\n", format(round(mean(d$nok)), big.mark = " ")))
cat(sprintf("  Avvik: %.1f pst\n", (mean(d$nok) / ssb_mean - 1) * 100))
cat("  Ventet avvik: ca -1.6 pst, fordi andelen uten inntekt teller med her\n")
cat("  men ikke i SSBs snitt, som bare teller dem MED belop. Er avviket\n")
cat("  vesentlig storre, mangler trolig halen -- se kommentaren i\n")
cat("  sample_income() om aggregate() og NA i grupperingsvariabel.\n")

cat("\n##### 3. Halen: alpha > 2 gir endelig varians, saa snittet KAN testes #####\n")
tail_draws <- replicate(20000, CounterfactMe:::.income_tail_draw(NA_character_))
tp <- CounterfactMe:::.cfm_env$income_tail_person
# as.numeric: mean_nok * n_persons sprenger 32-bit integer
forventet <- sum(as.numeric(tp$mean_nok) * as.numeric(tp$n_persons)) /
             sum(as.numeric(tp$n_persons))
cat(sprintf("  SSB snitt i halen: %12s kr\n", format(round(forventet), big.mark = " ")))
cat(sprintf("  Trukket:           %12s kr\n", format(round(mean(tail_draws)), big.mark = " ")))
cat(sprintf("  Median trukket:    %12s kr\n", format(round(median(tail_draws)), big.mark = " ")))

cat("\n##### 4. Andel uten inntekt, per aldersband #####\n")
print(CounterfactMe:::.cfm_env$income_none_share[, c("age_band","share_none")],
      row.names = FALSE)
