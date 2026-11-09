# Historisk modus — kjapp manuell titt.
# Kjør:  Rscript dev/demo-era.R    (eller devtools::load_all(); source(...))

if (!requireNamespace("CounterfactMe", quietly = TRUE)) devtools::load_all(".")
library(CounterfactMe)
set.seed(4)

cat("\n########## 1. Samme kvinne, fire ulike Norge ##########\n")
print(counterfact_era_lives(givens = list(gender = "F"), lang = "no"))

cat("\n########## 2. Utdanningsekspansjonen, 100 trekk per kohort ##########\n")
eras <- c(1935L, 1950L, 1965L, 1980L)
tab <- t(vapply(eras, function(b) {
  cds <- replicate(100, {
    e <- counterfact_era_lives(eras = b, lang = "no")[[1]]
    c(kode = as.numeric(e$education_code))
  })
  c(fodt = b,
    snitt_nus = round(mean(cds, na.rm = TRUE), 2),
    andel_hoyere = round(mean(cds %in% 6:8, na.rm = TRUE), 2))
}, numeric(3)))
print(as.data.frame(tab), row.names = FALSE)
cat("\nNUS-kode: hoyere tall = lengre utdanning. 6-8 = universitet/hogskole.\n")
cat("(kode 9 er Uoppgitt og telles IKKE som hoyere utdanning)\n")

cat("\n########## 2b. Kohortjusteringen: baand mot justert ##########\n")
cat("1935 og 1950 er BEGGE 67+ i dag og trekker fra identisk registerbaand.\n")
cat("Uten justeringen er forskjellen mellom dem ren stoy.\n\n")
tab2 <- t(vapply(c(1935L, 1950L), function(b) {
  d <- replicate(80, {
    e <- counterfact_era_lives(eras = b, lang = "no")[[1]]
    c(baand = as.numeric(e$education_band_code),
      just  = as.numeric(e$education_code))
  })
  c(fodt = b,
    tertiaer_baand = round(mean(d["baand", ] %in% 6:8, na.rm = TRUE), 3),
    tertiaer_justert = round(mean(d["just", ] %in% 6:8, na.rm = TRUE), 3))
}, numeric(3)))
print(as.data.frame(tab2), row.names = FALSE)
cat("\nVentet: baand-kolonnen lik for begge (samme baand, saa avvik er stoy),\n")
cat("justert-kolonnen ca 0.089 for 1935 og 0.160 for 1950.\n")
cat("Regnestykket: baandet 67+ har 20.2 pst UH kort og 7.3 pst UH lang.\n")
cat(".demote_edu_for_cohort() ganger m med 0.6 for lange grader, saa\n")
cat("0.202*m + 0.073*m*0.6 med m = 0.36 og 0.65. Forholdet blir 1.81.\n")

cat("\n########## 3. Realboligpris ved 40 aars alder ##########\n")
for (b in eras) {
  cat(sprintf("  fodt %d -> 40 aar i %d -> indeks %6.1f (1992 = 100)\n",
              b, b + 40, CounterfactMe:::.era_housing_index(b + 40)))
}

cat("\n########## 4. Hva er IKKE aera-data ##########\n")
res <- counterfact_era_lives(eras = 1975L)
cat("  Felt trukket fra dagens fordelinger:\n  ")
cat(paste(utils::head(attr(res, "assumed"), 12), collapse = ", "), "...\n")
