# ============================================================
# Stratifisert gjennomlesing.
#
# Kveldens tre viktigste funn -- Fosterfar til en kvinne, engelsk som
# default, og barn med voksen-instrumenter -- kom alle fra at noen SA pa
# output. Testene fanget bare krasjen. Dette skriptet er derfor ikke en
# test, men et lesestykke: det trekker liv systematisk gjennom strata
# som er lette a overse, og skriver dem ut for oyet.
#
# Kjor:  source("review-lives.R")
# Se etter: noe som ikke kunne vaere sant om et ekte menneske.
# ============================================================

library(CounterfactMe)

# %||% er intern i pakken, sa den defineres her.
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b

hdr <- function(t) cat("\n\n", strrep("=", 68), "\n", t, "\n",
                       strrep("=", 68), "\n", sep = "")
sub <- function(t) cat("\n", strrep("-", 68), "\n", t, "\n",
                       strrep("-", 68), "\n", sep = "")

set.seed(as.integer(Sys.time()) %% 100000)   # nytt utvalg hver gang

# ------------------------------------------------------------
hdr("1. ALDERSSTRATA -- ett liv per tiar")
# Aldersgrenser og kohorteffekter slar ut her. Se etter felt som
# ikke burde finnes i den alderen, og felt som mangler der de burde vaere.
for (a in c(3, 9, 14, 17, 22, 31, 45, 58, 67, 74, 88)) {
  sub(sprintf("%d ar", a))
  print(counterfact_me(min_age = a, max_age = a))
}

# ------------------------------------------------------------
hdr("2. BARN -- fem stykker")
# Etter gatene skal et barn bare ha SSB-baserte eller apenbart skrevne
# felt. Ser du noe som ligner en maling, er det en feil.
for (i in 1:5) {
  sub(sprintf("barn %d", i))
  print(counterfact_me(min_age = 0, max_age = 15))
}

# ------------------------------------------------------------
hdr("3. KJONN x YRKE -- ti av hver")
# Fosterfar-buggen bodde her. Les yrkestitlene, ikke resten.
for (g in c("F", "M")) {
  sub(sprintf("kjonn = %s", g))
  for (i in 1:10) {
    x <- counterfact_me(gender = g, min_age = 25, max_age = 66)
    cat(sprintf("  %-22s %-42s %s\n",
                x$name, x$occupation %||% "-", x$education %||% "-"))
  }
}

# ------------------------------------------------------------
hdr("4. UTDANNING x YRKE -- de hoyt utdannede")
# Lastebilsjafor med master. Les parene.
for (i in 1:20) {
  x <- counterfact_me(min_age = 30, max_age = 60)
  e <- x$education %||% ""
  if (grepl("lang|Forskerutdanning|Master|PhD", e)) {
    cat(sprintf("  %-46s %s\n", x$occupation %||% "-", e))
  }
}

# ------------------------------------------------------------
hdr("5. INNVANDRERBAKGRUNN -- navn, land, botid, foreldre")
# Anakronismer: botid som overgar alder, foreldre med norske yrker fra
# for de kom, navn som ikke passer opprinnelsen.
n <- 0
for (i in 1:60) {
  x <- counterfact_me(min_age = 18)
  if (identical(x$background, "majority")) next
  n <- n + 1; if (n > 6) break
  sub(sprintf("%s -- %s", x$name, x$country_background %||% "?"))
  cat(sprintf("  alder %d, botid %s, bakgrunn %s\n",
              x$age, x$years_in_norway %||% "-", x$background))
  cat(sprintf("  mor: %s (f. %s) -- %s\n",
              x$mother$name %||% "-", x$mother$birth_year %||% "-",
              x$mother$occupation %||% "-"))
  cat(sprintf("  far: %s (f. %s) -- %s\n",
              x$father$name %||% "-", x$father$birth_year %||% "-",
              x$father$occupation %||% "-"))
  cat(sprintf("  religion: %s\n", x$religion %||% "-"))
}

# ------------------------------------------------------------
hdr("6. ELDSTE KOHORTER -- studieretning")
# IKT-fag for en 78-aring. Les fagfeltene.
for (i in 1:15) {
  x <- counterfact_me(min_age = 68, max_age = 95)
  cat(sprintf("  f. %d  %-34s %s\n",
              2026L - x$age,
              x$field_of_study %||% "-",
              x$field_of_study_detail %||% ""))
}

# ------------------------------------------------------------
hdr("7. FAMILIETRE -- tre generasjoner")
# Fodselsar som ikke gar opp, navn som ikke passer kohorten.
for (i in 1:3) {
  x <- counterfact_me(min_age = 30, max_age = 55)
  sub(sprintf("%s, %d ar (f. %d)", x$name, x$age, 2026L - x$age))
  cat(sprintf("  mor    %-14s f. %s\n", x$mother$name %||% "-", x$mother$birth_year %||% "-"))
  cat(sprintf("  far    %-14s f. %s\n", x$father$name %||% "-", x$father$birth_year %||% "-"))
  for (gp in c("mormor", "morfar", "farmor", "farfar")) {
    g <- x[[gp]]
    if (!is.null(g)) cat(sprintf("  %-6s %-14s f. %s\n", gp, g$name %||% "-", g$birth_year %||% "-"))
  }
  if (length(x$siblings)) {
    for (s in x$siblings)
      cat(sprintf("  sosken %-14s f. %s\n", s$name %||% "-", s$birth_year %||% "-"))
  }
}

# ------------------------------------------------------------
hdr("8. OKONOMI -- inntekt, bolig, formue")
# Bolig som ikke star i forhold til inntekt; formue uten kilde.
for (i in 1:12) {
  x <- counterfact_me(min_age = 28, max_age = 70)
  cat(sprintf("  %-11s inntekt %9s  %-7s %-13s bolig %7s  formue %9s\n",
              substr(x$occupation %||% "-", 1, 11),
              format(x$income_nok %||% NA, big.mark = " "),
              x$housing_tenure %||% "-",
              substr(x$housing_type %||% "-", 1, 13),
              if (is.null(x$housing_value_nok) || is.na(x$housing_value_nok)) "-"
              else sprintf("%.1fM", x$housing_value_nok / 1e6),
              format(x$net_wealth_nok %||% NA, big.mark = " ")))
}

# ------------------------------------------------------------
hdr("9. NARRATIV -- les dem som tekst")
# Her ser man hull i malene: gjentakelser, hengende komma, halve
# setninger, felt som mangler etter gatingen.
for (a in c(8, 24, 41, 63, 82)) {
  x <- counterfact_me(min_age = a, max_age = a)
  sub(sprintf("%d ar", a))
  cat(narrate_life(x, style = "biography"), "\n")
}

# ------------------------------------------------------------
hdr("10. PARALLELLE LIV -- samme person, ulikt fylke")
lives <- counterfact_parallel_lives(
  givens = list(age = 40, gender = "F"), vary_dim = "county", n = 5)
for (l in lives) {
  cat(sprintf("\n  %-12s %s\n", l$county %||% "-", strrep("-", 50)))
  cat("  ", narrate_life(l, style = "compact"), "\n", sep = "")
}

cat("\n\n", strrep("=", 68), "\n", sep = "")
cat("Ferdig. Det du ser etter er ikke feilmeldinger, men setninger\n")
cat("som ikke kunne vaert sanne om et ekte menneske.\n")
cat(strrep("=", 68), "\n")
