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
#' Draws an income decile from the Norwegian gross income distribution.
#' Each decile is equally likely (10 % each). Returns both the bracket
#' label and a random NOK amount within that bracket.
#'
#' @param n Number of draws. Default 1.
#' @return A list with two elements: \code{bracket} (character vector of
#'   labels) and \code{nok} (integer vector of NOK amounts).
#' @export
#' @examples
#' sample_income()
#' sample_income(5)
sample_income <- function(n = 1) {
  .load_data()
  inc <- .cfm_env$income
  idx <- sample(seq_len(nrow(inc)), size = n, replace = TRUE)
  # Cap top decile at 2 million for realism
  upper <- pmin(inc$upper_nok[idx], 2000000)
  lower <- inc$lower_nok[idx]
  nok <- as.integer(round(stats::runif(n, lower, upper), -3))
  list(bracket = inc$label[idx], nok = nok)
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
