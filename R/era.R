# Historical mode: the same background, born into a different Norway.
#
# See dev/design/historical-mode.md for why party-labelled policy sliders were
# rejected in favour of this.
#
# Education is essentially fixed by age 30, so a 91-year-old's registered
# education is her attainment at 40 as well. That much is a register fact.
#
# What is NOT a register fact is the cohort. SSB's education table is banded,
# and the top band lumps births 1926-1959 together, so the register alone
# cannot tell a 1935-born from a 1950-born. The band therefore sets the level
# and .demote_edu_for_cohort() tilts it by birth year -- the same adjustment
# the sampler already applies to parents, for the same reason. So education
# here is "register band x documented cohort multiplier", not a pure read-off.

# Dimensions for which era-specific data genuinely exists AND survives the
# transfer from register age to observation age. Everything else is drawn from
# today's distributions and must be reported as such.
#
# Education qualifies because attainment is fixed by ~30: a 91-year-old's
# registered education IS her education at 40.
#
# Occupation does NOT qualify, and this is easy to get wrong. Reading the
# occupation of a 91-year-old gives "pensjonist", not the job she held at 40.
# The era's occupational structure does exist as data (.styrk_cohort_mult),
# and is carried on the result as $styrk_mult, but nothing draws from it yet.
# Until it does, occupation is reported as present-day and flagged.
.era_grounded_dims <- c("education", "housing_index")

#' Resolve an education label back to its NUS code
#'
#' The public life object carries only the label, so the numeric level has to
#' be looked up to compare cohorts quantitatively.
#'
#' @param label Education label, in either language.
#' @return Integer code, or NA.
#' @keywords internal
.era_edu_code <- function(label) {
  .load_data()
  if (is.null(label) || is.na(label) || !nzchar(label)) return(NA_integer_)
  edu <- .cfm_env$education
  hit <- edu$code[edu$level_no == label]
  if (!length(hit)) hit <- edu$code[edu$level == label]
  if (!length(hit)) return(NA_integer_)
  as.integer(hit[1])
}

#' Label for an education code
#'
#' @param code Integer NUS-mapped package code.
#' @param lang \code{"no"} or \code{"en"}.
#' @return Character label, or NA.
#' @keywords internal
.era_edu_label <- function(code, lang = "no") {
  .load_data()
  if (is.null(code) || is.na(code)) return(NA_character_)
  edu <- .cfm_env$education
  col <- if (identical(lang, "no")) "level_no" else "level"
  hit <- edu[[col]][edu$code == as.integer(code)]
  if (!length(hit)) return(NA_character_)
  as.character(hit[1])
}

#' Real house price index for a given year
#'
#' Splices the pre-war reconstruction (1920-1992) onto the SSB series
#' (1992-2025). Returns NA outside 1920..ref_year.
#'
#' @param year Calendar year.
#' @return Numeric index, 1992 = 100, or NA.
#' @keywords internal
.era_housing_index <- function(year) {
  .load_data()
  if (is.null(year) || is.na(year)) return(NA_real_)
  # Reuse the tested splice rather than reimplementing it: the pre-1992
  # reconstruction and its documented real-price estimate live in
  # .housing_index_factor(), and two copies would drift apart.
  f <- .housing_index_factor(county = "Hele landet",
                            boligtype_code = "00",
                            from_year = 1992L,
                            to_year = as.integer(year))
  if (is.null(f) || is.na(f)) return(NA_real_)
  as.numeric(f) * 100
}

#' Parallel lives across birth cohorts
#'
#' Holds the social background fixed and varies which Norway the person was
#' born into. Each life is observed at the same life stage (\code{at_age}),
#' not at the same calendar moment -- otherwise cohort and ageing are
#' hopelessly confounded and the reader sees old age rather than history.
#'
#' Two dimensions carry era data. Real house prices are a property of the
#' calendar year, so they are read straight off the spliced index. Education is
#' a composite: the register's age band sets the level, and the documented
#' cohort multiplier tilts it by birth year, because the top band lumps births
#' 1926-1959 into a single figure and cannot separate them. The raw band draw
#' is kept alongside as \code{$education_band}. Occupation does not
#' survive it -- a 91-year-old's registered occupation is "pensjonist", not the
#' job she held at 40 -- so it, and every other field, is drawn from
#' present-day distributions and flagged in \code{attr(x, "assumed")} rather
#' than silently presented as historical.
#'
#' @param eras Birth years to compare. Defaults to four cohorts spanning the
#'   Norwegian education expansion. Every cohort must already have reached
#'   \code{at_age}; later ones are dropped with a warning.
#' @param at_age Life stage at which each cohort is described. Education is
#'   complete and careers established by 40, which makes it the natural
#'   comparison point.
#' @param givens Background held fixed across eras, passed to
#'   \code{counterfact_me_constrained()} (e.g. \code{list(gender = "F")}).
#' @param lang \code{"no"} or \code{"en"}.
#' @param max_attempts Rejection-sampling budget per era.
#' @return An object of class \code{counterfact_era}.
#' @examples
#' \donttest{
#' counterfact_era_lives(givens = list(gender = "F"))
#' }
#' @export
counterfact_era_lives <- function(eras = c(1935L, 1950L, 1965L, 1980L),
                                  at_age = 40L,
                                  givens = list(),
                                  lang = c("no", "en"),
                                  max_attempts = 300L) {
  lang <- match.arg(lang)
  .load_data()
  ref <- .cfm_env$ref_year
  eras <- sort(unique(as.integer(eras)))

  age_today <- ref - eras

  # A cohort can only be read off the register if it is still alive in it.
  too_old <- age_today > 100L
  if (any(too_old)) {
    warning(sprintf(
      "Kohort(er) %s er eldre enn registeret rekker (>100 ar i %d) og utelates.",
      paste(eras[too_old], collapse = ", "), ref), call. = FALSE)
    eras <- eras[!too_old]
    age_today <- age_today[!too_old]
  }

  # ...and it can only be described AT age `at_age` if it has reached that age.
  # A cohort born in 1995 is 31 today; the register cannot say what it holds
  # at 40, because 40 has not happened. Claiming otherwise would date-stamp a
  # 31-year-old's schooling as a 40-year-old's.
  too_young <- age_today < at_age
  if (any(too_young)) {
    warning(sprintf(
      paste0("Kohort(er) %s har ikke fylt %d ar enda (er %s i %d) og utelates. ",
             "Senk at_age for a ta dem med."),
      paste(eras[too_young], collapse = ", "), at_age,
      paste(age_today[too_young], collapse = ", "), ref), call. = FALSE)
    eras <- eras[!too_young]
    age_today <- age_today[!too_young]
  }
  if (!length(eras)) {
    stop(sprintf("Ingen kohorter igjen a sammenlikne ved %d ar.", at_age),
         call. = FALSE)
  }

  out <- vector("list", length(eras))
  for (i in seq_along(eras)) {
    g <- givens
    g$age <- age_today[i]
    life <- counterfact_me_constrained(givens = g,
                                       max_attempts = max_attempts,
                                       lang = lang)
    work_year <- eras[i] + at_age

    # The register's education is banded, and the top band ("67 ar eller
    # eldre") lumps births 1926-1959 into one number -- 34 cohorts. Read
    # straight off, a 1935-born and a 1950-born are the SAME draw, and both
    # get the band's 27.5 pct tertiary when the true figures are nearer 10
    # and 18. So the band gives the level and .demote_edu_for_cohort() tilts
    # it by cohort, exactly as it already does for parents, whose age is
    # likewise not a register fact. Both values are kept: $education is
    # cohort-adjusted, $education_band is the raw band draw.
    band_code <- .era_edu_code(life$education)
    cohort_code <- .demote_edu_for_cohort(band_code, eras[i])
    cohort_label <- if (identical(cohort_code, band_code)) life$education
                    else .era_edu_label(cohort_code, lang)

    out[[i]] <- list(
      birth_year     = eras[i],
      observed_at    = at_age,
      calendar_year  = work_year,
      age_in_register = age_today[i],
      education      = cohort_label,
      education_code = cohort_code,
      education_band = life$education,
      education_band_code = band_code,
      occupation     = life$occupation,
      styrk_mult     = .styrk_cohort_mult(work_year),
      housing_index  = .era_housing_index(work_year),
      life           = life
    )
  }

  structure(out,
            class  = c("counterfact_era", "list"),
            eras   = eras,
            at_age = at_age,
            givens = givens,
            lang   = lang,
            # Fields carried over from present-day distributions.
            assumed = setdiff(names(out[[1]]$life), .era_grounded_dims))
}

#' Print method for era comparisons
#' @param x A \code{counterfact_era} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.counterfact_era <- function(x, ...) {
  .load_data()
  lang <- attr(x, "lang")
  at_age <- attr(x, "at_age")
  givens <- attr(x, "givens")
  is_no <- identical(lang, "no")

  cat(if (is_no)
        sprintf("\n=== Samme bakgrunn, %d ulike Norge (sett ved %d ar) ===\n",
                length(x), at_age)
      else
        sprintf("\n=== One background, %d different Norways (observed at %d) ===\n",
                length(x), at_age))
  if (length(givens)) {
    cat(sprintf(if (is_no) "Holdt fast: %s\n" else "Held fixed: %s\n",
                paste(names(givens), unlist(givens), sep = "=", collapse = ", ")))
  }

  for (e in x) {
    cat(sprintf(if (is_no) "\n-- Fodt %d, altsa %d ar i %d --\n"
                else "\n-- Born %d, i.e. %d in %d --\n",
                e$birth_year, at_age, e$calendar_year))
    cat(sprintf(if (is_no) "  Utdanning:  %s\n" else "  Education:  %s\n",
                if (is.null(e$education)) "-" else e$education))
    if (!is.null(e$education_band) && !identical(e$education_band, e$education)) {
      cat(sprintf(if (is_no) "              (registerbaand ga: %s)\n"
                  else "              (register band gave: %s)\n",
                  e$education_band))
    }
    cat(sprintf(if (is_no) "  Yrke (i dag, ikke aera): %s\n"
                else "  Occupation (today, not era): %s\n",
                if (is.null(e$occupation)) "-" else e$occupation))
    if (!is.na(e$housing_index)) {
      cat(sprintf(if (is_no) "  Boligpris:  %.0f (1992 = 100, reelt)\n"
                  else "  House price: %.0f (1992 = 100, real)\n",
                  e$housing_index))
    }
  }

  # Survivor selection: the oldest cohorts are read off those still alive,
  # who are positively selected on education. The bias understates the
  # expansion, so the figure is safe to show -- but not unlabelled.
  oldest <- min(vapply(x, function(e) e$birth_year, integer(1)))
  if ((.cfm_env$ref_year - oldest) >= 70L) {
    cat(if (is_no) paste0(
          "\nAdvarsel: kohorten fodt ", oldest, " leses av dem som fortsatt\n",
          "lever. De er positivt selektert paa utdanning, saa ekspansjonen\n",
          "vises svakere enn den var.\n")
        else paste0(
          "\nCaveat: the ", oldest, " cohort is read off survivors, who are\n",
          "positively selected on education. The expansion shown is therefore\n",
          "an understatement.\n"))
  }

  cat(if (is_no) paste0(
        "\nMerk: boligpris er lest rett av indeksen. Utdanning er registerbaand\n",
        "ganget med dokumentert kohortmultiplikator - ikke rent registeruttrekk,\n",
        "fordi ovre baand slaar sammen fodselsaarene 1926-1959. Yrke og alle\n",
        "ovrige felt er trukket fra dagens fordelinger.\n",
        "Se dev/design/historical-mode.md.\n")
      else paste0(
        "\nNote: house prices are read off the index. Education is the register\n",
        "band times a documented cohort multiplier, not a pure read-off, because\n",
        "the top band merges births 1926-1959. Occupation and all other fields\n",
        "come from present-day distributions.\n",
        "See dev/design/historical-mode.md.\n"))
  invisible(x)
}
