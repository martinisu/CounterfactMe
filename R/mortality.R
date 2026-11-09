# Forventet gjenstaende levetid, SSB 14569 (2025).
#
# Tabellen gir REMAINING life expectancy fra en gitt alder -- riktig storrelse
# for en person som allerede lever -- etter enkeltaar 0-99, kjonn og de seks
# sentralitetsnivaaene pakka alt baerer i kommune_sentralitet.csv.
#
# Maalte spenn ved fodsel, 2025:
#   kjonn                 3.2 aar (kvinner lengst)
#   sentralitet, menn     2.2 aar
#   sentralitet, kvinner  1.2 aar
#
# IKKE med: utdanning og inntekt. SSB publiserer ingen statbank-tabell over
# levealder etter utdanningsniva -- sjekket 2026-10-15. Naermeste kilde er en
# SSB-artikkel med 11-12 aars gap mellom hoyeste og laveste YRKESgruppe for
# menn, men den hviler paa 1996-2000 og 2009-2013 og handler om yrke, ikke
# utdanning. Aa stable en slik gradient oppa kjonn og sentralitet ville vaert
# dobbelttelling: de tre overlapper, og marginale sammenhenger er ikke
# additive. Se data-raw/fetch_life_expectancy.py.

#' Forventet gjenstående levetid
#'
#' Slår opp SSB 14569. Faller tilbake på landstallet når sentralitet mangler.
#'
#' @param age Alder i år (0-99; høyere alder bruker 99).
#' @param gender \code{"M"} eller \code{"F"}.
#' @param sentralitet Sentralitetskode 1-6, eller NA for hele landet.
#' @return Forventet gjenstående leveår, eller NA.
#' @keywords internal
.life_expectancy <- function(age, gender = NULL, sentralitet = NA) {
  .load_data()
  tbl <- .cfm_env$life_expectancy
  if (is.null(tbl) || !nrow(tbl)) return(NA_real_)
  if (is.null(age) || is.na(age)) return(NA_real_)
  a <- max(0L, min(99L, as.integer(age)))
  g <- if (is.null(gender) || is.na(gender)) NA_character_ else toupper(substr(gender, 1, 1))
  if (!g %in% c("M", "F")) return(NA_real_)

  s <- suppressWarnings(as.integer(sentralitet))
  code <- if (!is.na(s) && s >= 1L && s <= 6L) sprintf("Se%02d", s) else "0"
  hit <- tbl$remaining_years[tbl$region_code == code &
                             tbl$gender == g & tbl$age == a]
  if (!length(hit)) {
    hit <- tbl$remaining_years[tbl$region_code == "0" &
                               tbl$gender == g & tbl$age == a]
  }
  if (!length(hit)) return(NA_real_)
  as.numeric(hit[1])
}

#' Kjønnsjustering av dødelighetsstigen
#'
#' \code{.death_year()} brukte én aldersstige for begge foreldre, så mor og
#' far døde etter identisk skjema. Kjønnsgapet er 3,2 år (SSB 14569), og det
#' er den største enkeltfeilen der.
#'
#' Stigen selv er et anslag og beholdes. Justeringen flytter alderen som
#' leses inn i den: en person med lengre forventet levetid behandles som
#' tilsvarende yngre. Differansen hentes mot landstallet for begge kjønn.
#'
#' Sentralitet brukes IKKE her. Foreldrenes bosted er ikke modellert, bare
#' egos, og å låne egos sentralitet til foreldrene ville vært en antakelse
#' uten dekning. Kjønn er dessuten den klart største av de to.
#'
#' @param age Alderen forelderen ville hatt i dag.
#' @param gender \code{"M"} eller \code{"F"}.
#' @return Justert alder til bruk i dødelighetsstigen.
#' @keywords internal
.mortality_adjusted_age <- function(age, gender = NULL) {
  if (is.null(age) || is.na(age)) return(age)
  own <- .life_expectancy(age, gender, NA)
  if (is.na(own)) return(age)
  base <- .life_expectancy(age, "M", NA)
  base_f <- .life_expectancy(age, "F", NA)
  if (is.na(base) || is.na(base_f)) return(age)
  mid <- (base + base_f) / 2
  as.numeric(age) - (own - mid)
}

#' Inntektspersentil fra et kronebeløp
#'
#' Empirisk fordelingsfunksjon over SSB 06655 (personnivå, 17+, 2024).
#' Interpolerer lineært innenfor intervallet.
#'
#' @param income_nok Bruttoinntekt i kroner.
#' @return Persentil 1-99, eller NA.
#' @keywords internal
.income_percentile <- function(income_nok) {
  .load_data()
  if (is.null(income_nok) || is.na(income_nok)) return(NA_integer_)
  tbl <- .cfm_env$income_by_age
  if (is.null(tbl)) return(NA_integer_)
  codes <- unique(tbl$bracket_code)
  lo <- vapply(codes, function(k) tbl$lower_nok[tbl$bracket_code == k][1], numeric(1))
  n  <- vapply(codes, function(k) sum(as.numeric(tbl$count[tbl$bracket_code == k])),
               numeric(1))
  ord <- order(lo); lo <- lo[ord]; n <- n[ord]
  cum <- cumsum(n) / sum(n)
  below <- c(0, cum[-length(cum)])
  k <- sum(lo <= income_nok)
  if (k < 1) return(1L)
  if (k >= length(lo)) return(99L)
  frac <- (income_nok - lo[k]) / max(1, lo[k + 1] - lo[k])
  p <- (below[k] + frac * (cum[k] - below[k])) * 100
  max(1L, min(99L, as.integer(round(p))))
}

#' Inntektsgradient i forventet levetid
#'
#' Avviket i leveår for en gitt inntektspersentil, fra Kinge et al. (JAMA
#' 2019). Null-sum over fordelingen, så populasjonssnittet er upåvirket.
#'
#' Avtar med alder: Kinges tall gjelder 40+, og gradienten kan ikke gjelde
#' uavkortet for en som allerede har overlevd til 85. Den skaleres derfor med
#' gjenstående levetid relativt til en 40-årings, med gulv på et halvt år.
#'
#' @param income_nok Bruttoinntekt.
#' @param gender \code{"M"} eller \code{"F"}.
#' @param age Alder, for avtrappingen.
#' @return Avvik i år, eller 0.
#' @keywords internal
.mortality_income_offset <- function(income_nok, gender = NULL, age = NULL) {
  .load_data()
  g <- .cfm_env$mortality_income_gradient
  if (is.null(g) || !nrow(g)) return(0)
  sex <- if (is.null(gender) || is.na(gender)) NA_character_
         else toupper(substr(gender, 1, 1))
  if (!sex %in% c("M", "F")) return(0)
  p <- .income_percentile(income_nok)
  if (is.na(p)) return(0)
  hit <- g$years_offset[g$gender == sex & g$income_percentile == p]
  if (!length(hit)) return(0)
  off <- as.numeric(hit[1])

  if (is.null(age) || is.na(age) || age <= 40) return(off)
  r_now <- .life_expectancy(age, sex, NA)
  r_40  <- .life_expectancy(40, sex, NA)
  if (is.na(r_now) || is.na(r_40) || r_40 <= 0) return(off)
  off * (r_now / r_40)
}
