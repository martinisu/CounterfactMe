#' Sample a random Norwegian occupation
#'
#' Draws one occupation from the 7,020 registered Norwegian occupation codes
#' (yrkeskoder). All occupations are equally likely by default.
#'
#' @param n Number of occupations to draw. Default 1.
#' @return A character vector of occupation names.
#' @export
#' @examples
#' sample_occupation()
#' sample_occupation(5)
sample_occupation <- function(n = 1) {
  .load_data()
  sample(.cfm_env$occupations$name, size = n, replace = TRUE)
}

#' Sample a random Norwegian municipality
#'
#' Draws one municipality (kommune), weighted by population size so that
#' larger municipalities are more likely — just as in real life.
#'
#' @param n Number of municipalities to draw. Default 1.
#' @param weighted If \code{TRUE} (default), sampling is proportional to
#'   population. Set to \code{FALSE} for uniform sampling.
#' @return A character vector of municipality names.
#' @export
#' @examples
#' sample_municipality()
#' sample_municipality(3, weighted = FALSE)
sample_municipality <- function(n = 1, weighted = TRUE) {
  .load_data()
  mun <- .cfm_env$municipalities
  if (weighted) {
    probs <- mun$population / sum(mun$population)
    sample(mun$name, size = n, replace = TRUE, prob = probs)
  } else {
    sample(mun$name, size = n, replace = TRUE)
  }
}

#' Sample a random Norwegian first name
#'
#' Draws a first name from Norwegian birth-registry names (SSB table 10467),
#' weighted by frequency. When a `birth_year` is supplied the sample is
#' restricted to that person's birth-decade cohort (e.g. someone born in
#' 1996 draws from names given to children born 1990-1999), so the name is
#' plausible for their age. Without `birth_year`, frequencies are summed
#' across all cohorts (1940-2020).
#'
#' @param n Number of names to draw. Default 1.
#' @param gender Filter by gender: `"M"`, `"F"`, or `NULL` (default) for both.
#' @param birth_year Optional integer birth year. Cohorts outside 1940-2020
#'   are snapped to the nearest available decade.
#' @return A character vector of first names.
#' @export
#' @examples
#' sample_first_name()
#' sample_first_name(3, gender = "F")
#' sample_first_name(5, gender = "M", birth_year = 1996)
sample_first_name <- function(n = 1, gender = NULL, birth_year = NULL) {
  .load_data()
  nms <- .cfm_env$first_names
  # Exclude clearly foreign-coded names that appear in names_by_region.csv —
  # for narrative consistency, majority-Norwegian draws should not pull names
  # like "Mohammed" or "Fatima" from the SSB cohort registry (which contains
  # them because immigrants ARE registered in Norway).
  nbr <- .cfm_env$names_by_region
  if (!is.null(nbr)) {
    foreign_names <- unique(nbr$name[nbr$region != "norden"])
    if (length(foreign_names) > 0) {
      nms <- nms[!(nms$name %in% foreign_names), ]
    }
  }
  # names_by_region holds 40 names per region, so common immigrant names
  # outside that list still came through: the live site produced
  # "Przemyslaw" and "Abdul" as children of Toril and Svein. Drop names
  # with unmistakably non-Nordic spelling or roots. Names in shared use
  # (Maria, Daniel, Sara, Ali as a Norwegian short form is rare) are kept
  # unless listed.
  foreign_re <- paste0(
    "sz|cz|rz|\u0142|aw$|^(Piotr|Pawel|Marcin|Lukasz|Marek|Dariusz|Wojciech|",
    "Jakub|Agnieszka|Beata|Malgorzata|Magdalena|Katarzyna|Aleksandra|",
    "Svetlana|Thi|Van|Omar|Ali|Amir|Mariam|Maryam)$|",
    "^(Abd|Moha|Muha|Mohd|Ahm|Ibrah|Musta|Huss|Hass|Ism[ae]|Yus|Khal|",
    "Mahm|Hamz|Fatim|Amina|Aish|Zahr|Yasm|Mehm)|",
    "^(Arturas|Darius|Mantas|Tadas|Deividas|Dovydas|Andrius|Mindaugas|",
    "Vytautas|Aurimas|Rimantas|Gintaras|Paulius|Karolis|Edgaras|Ernestas|",
    "Justinas|Vladimir|Sergei|Sergey|Dmitri|Dmitry|Aleksandr|Oleksandr|",
    "Volodymyr|Tatiana|Tetiana|Nguyen|Huong|Linh)$")
  nms <- nms[!grepl(foreign_re, nms$name, perl = TRUE), ]
  if (!is.null(gender)) {
    gender <- toupper(gender)
    nms <- nms[nms$gender == gender, ]
  }
  if (!is.null(birth_year)) {
    target <- (as.integer(birth_year) %/% 10L) * 10L
    available <- sort(unique(.cfm_env$first_names$cohort))
    if (!(target %in% available)) {
      target <- available[which.min(abs(available - target))]
    }
    nms <- nms[nms$cohort == target, ]
  } else {
    # Collapse across cohorts for age-agnostic draws
    nms <- stats::aggregate(frequency ~ name + gender, data = nms, FUN = sum)
  }
  probs <- nms$frequency / sum(nms$frequency)
  sample(nms$name, size = n, replace = TRUE, prob = probs)
}

#' Sample a random education level
#'
#' Draws an education level from the Norwegian NUS classification, weighted
#' by the share of the population at each level.
#'
#' @param n Number of draws. Default 1.
#' @param lang Language for labels: \code{"en"} (default) or \code{"no"}.
#' @return A character vector of education level descriptions.
#' @export
#' @examples
#' sample_education()
#' sample_education(3, lang = "no")
sample_education <- function(n = 1, lang = c("en", "no")) {
  .load_data()
  lang <- match.arg(lang)
  edu <- .cfm_env$education
  col <- if (lang == "no") "level_no" else "level"
  sample(edu[[col]], size = n, replace = TRUE, prob = edu$population_share)
}

#' Sample a random income bracket
#'
#' Draws a gross income from SSB 06655, the person-level distribution for
#' residents 17+ (2024). Brackets are drawn with their real population
#' weights -- they are SSB's own intervals, not deciles, so they are NOT
#' equally likely. Within a bracket the draw reproduces that bracket's mean
#' rather than assuming a flat rectangle, the open top bracket is a Pareto
#' anchored on SSB's mean above 2 MNOK, and the share with no gross income
#' at all is carried explicitly.
#'
#' This replaced income_deciles.csv, which was "untraced" in SOURCES.csv and
#' turned out to be the 2022 distribution with a uniform bottom and a
#' uniform top capped at 2 MNOK. See data-raw/fetch_income.py.
#'
#' @param n Number of draws. Default 1.
#' @param age Optional age. When given, the income distribution for that
#'   age band is used; otherwise the national distribution.
#' @return A list with two elements: \code{bracket} (character vector of
#'   labels) and \code{nok} (integer vector of NOK amounts).
#' @export
#' @examples
#' sample_income()
#' sample_income(5)
#' sample_income(3, age = 45)
sample_income <- function(n = 1, age = NULL) {
  .load_data()
  tbl <- .cfm_env$income_by_age
  if (is.null(tbl)) return(list(bracket = rep(NA_character_, n),
                                nok = rep(NA_integer_, n)))

  # Age band if given, otherwise the national distribution (sum over bands).
  band <- if (is.null(age) || is.na(age)) NA_character_ else .wealth_age_band(age)
  sub <- if (!is.na(band)) tbl[tbl$age_band == band, , drop = FALSE] else tbl
  if (!nrow(sub)) sub <- tbl
  # Aggregate by bracket_code alone. An earlier version grouped by
  # upper_nok too, and the open top bracket carries upper_nok = NA -- formula
  # aggregation DROPS rows with NA in a grouping variable, so the entire
  # tail silently vanished: 0 of 40 000 draws landed above 2 MNOK against an
  # expected 716, and the mean came out 10 pct low. The missing 1.79 pct at
  # a mean of 3.77 MNOK accounted for almost exactly that gap.
  codes <- unique(sub$bracket_code)
  first <- match(codes, sub$bracket_code)
  agg <- data.frame(
    bracket_code  = codes,
    bracket_label = sub$bracket_label[first],
    lower_nok     = sub$lower_nok[first],
    upper_nok     = sub$upper_nok[first],
    count    = vapply(codes, function(k) sum(sub$count[sub$bracket_code == k]), numeric(1)),
    sum_mill = vapply(codes, function(k) sum(as.numeric(sub$sum_mill[sub$bracket_code == k])), numeric(1)),
    stringsAsFactors = FALSE
  )
  agg$mean_nok <- agg$sum_mill * 1e6 / agg$count

  # Persons with NO gross income at all. 06655 counts only those with an
  # amount; the rest of the 17+ population has none. Conflating them with
  # "low income" was one of the errors in the old decile file.
  p_none <- 0
  ns <- .cfm_env$income_none_share
  if (!is.null(ns) && nrow(ns)) {
    row <- if (!is.na(band)) ns[ns$age_band == band, , drop = FALSE] else ns
    p_none <- if (nrow(row) == 1) row$share_none[1]
              else sum(row$none) / sum(row$population)
  }

  idx <- sample(seq_len(nrow(agg)), size = n, replace = TRUE,
                prob = agg$count / sum(agg$count))
  nok <- vapply(seq_len(n), function(i) {
    if (stats::runif(1) < p_none) return(0)
    k <- idx[i]
    lo <- agg$lower_nok[k]; hi <- agg$upper_nok[k]; mu <- agg$mean_nok[k]
    if (is.na(hi)) return(.income_tail_draw(band))
    # Draw within the interval so the interval MEAN is reproduced, rather
    # than assuming a flat rectangle. A triangular density on [lo, hi] with
    # mode chosen to hit mu does that with one parameter.
    mode <- pmin(pmax(3 * mu - lo - hi, lo), hi)
    u <- stats::runif(1)
    fc <- (mode - lo) / (hi - lo)
    v <- if (u < fc) lo + sqrt(u * (hi - lo) * (mode - lo))
         else hi - sqrt((1 - u) * (hi - lo) * (hi - mode))
    v
  }, numeric(1))

  list(bracket = agg$bracket_label[idx],
       nok = as.integer(round(pmin(nok, .Machine$integer.max - 1), -3)))
}

#' Draw from the person-level income tail
#'
#' Pareto above 2 MNOK with alpha solved so the truncated mean equals SSB's
#' figure for the band. See data-raw/fetch_income.py.
#'
#' @param band Age band, or NA for the national tail.
#' @return Numeric income in NOK.
#' @keywords internal
.income_tail_draw <- function(band = NA_character_) {
  tp <- .cfm_env$income_tail_person
  if (is.null(tp) || !nrow(tp)) return(2e6)
  row <- if (!is.na(band)) tp[tp$age_band == band, , drop = FALSE] else tp
  if (nrow(row) != 1) {
    # National tail: pool the bands, weighting alpha by headcount.
    a <- sum(tp$pareto_alpha * tp$n_persons) / sum(tp$n_persons)
    xm <- tp$pareto_xm[1]; cap <- tp$pareto_cap[1]
  } else {
    a <- row$pareto_alpha[1]; xm <- row$pareto_xm[1]; cap <- row$pareto_cap[1]
  }
  min(xm / stats::runif(1)^(1 / a), cap)
}

#' Sample a random household type
#'
#' Draws a household type weighted by the Norwegian population distribution.
#'
#' @param n Number of draws. Default 1.
#' @param lang Language for labels: \code{"en"} (default) or \code{"no"}.
#' @return A character vector of household type descriptions.
#' @export
#' @examples
#' sample_household()
sample_household <- function(n = 1, lang = c("en", "no")) {
  .load_data()
  lang <- match.arg(lang)
  hh <- .cfm_env$households
  col <- if (lang == "no") "type_no" else "type"
  sample(hh[[col]], size = n, replace = TRUE, prob = hh$population_share)
}

#' Sample a random marital status
#'
#' Draws a marital status weighted by Norwegian adult population shares.
#'
#' @param n Number of draws. Default 1.
#' @param lang Language for labels: \code{"en"} (default) or \code{"no"}.
#' @return A character vector of marital statuses.
#' @export
#' @examples
#' sample_marital_status()
sample_marital_status <- function(n = 1, lang = c("en", "no")) {
  .load_data()
  lang <- match.arg(lang)
  ms <- .cfm_env$marital
  col <- if (lang == "no") "status_no" else "status"
  sample(ms[[col]], size = n, replace = TRUE,
         prob = ms$population_share_18plus)
}

#' Sample a random age
#'
#' Draws an age from the Norwegian age distribution. A uniform draw within
#' the selected 5-year band gives a specific age.
#'
#' @param n Number of draws. Default 1.
#' @param min_age Minimum age to draw. Default 0 (all ages). Set to e.g. 18 to draw only adults.
#' @param max_age Maximum age to draw. Default 99.
#' @return An integer vector of ages.
#' @export
#' @examples
#' sample_age()
#' sample_age(10)
#' sample_age(5, min_age = 0)  # include children
sample_age <- function(n = 1, min_age = 0L, max_age = 99L) {
  .load_data()
  ad <- .cfm_env$age_dist

  # Zero out bands entirely below min_age or above max_age
  w <- ad$population_share
  w[ad$age_upper < min_age] <- 0
  w[ad$age_lower > max_age] <- 0

  if (sum(w) == 0) {
    warning("No age bands in range [", min_age, ", ", max_age,
            "]. Returning min_age.")
    return(rep(as.integer(min_age), n))
  }

  w <- w / sum(w)
  ages <- integer(n)
  for (i in seq_len(n)) {
    repeat {
      band <- sample(seq_len(nrow(ad)), 1, prob = w)
      age <- as.integer(stats::runif(1, ad$age_lower[band],
                                     ad$age_upper[band] + 1))
      if (age >= min_age && age <= max_age) break
    }
    ages[i] <- age
  }
  ages
}

#' List available counterfactual dimensions
#'
#' Returns the names of all life dimensions that \code{counterfact_me()} can
#' sample from. Useful for checking what's available or for selecting a
#' subset with the \code{dimensions} argument.
#'
#' @return A character vector of dimension names.
#' @export
#' @examples
#' available_dimensions()
available_dimensions <- function() {
  c("name", "age", "municipality", "occupation", "education",
    "field_of_study", "field_of_study_detail", "income", "household", "marital_status",
    "parents", "siblings", "grandparents", "children", "housing", "wealth",
    "background", "orientation", "religion", "party", "neet", "bourdieu",
    "sentralitet", "health", "deprivation", "isolation", "hobbies",
    "media", "sleep", "diet", "alcohol", "crime")
}
