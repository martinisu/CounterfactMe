#' Generate a counterfactual life with constraints
#'
#' Like \code{\link{counterfact_me}} but lets you fix some dimensions to
#' specific values while letting the rest be drawn conditionally. Uses
#' rejection sampling with up to \code{max_attempts} tries; if no match is
#' found, falls back to direct override (with a warning).
#'
#' @param givens A named list of constraints, e.g.,
#'   \code{list(age = 42, gender = "M", county = "Oslo")}.
#'   Supported fields: \code{age}, \code{gender}, \code{county},
#'   \code{municipality}, \code{occupation}, \code{education},
#'   \code{marital_status}, \code{religion}, \code{party}, \code{background}.
#' @param max_attempts Max rejection-sampling attempts. Default 500.
#' @param lang Language for labels. Default "no".
#' @param dimensions Which dimensions to draw. Default: all.
#' @param verbose If TRUE, print attempts info.
#' @return A \code{counterfactme} object with the constraints respected.
#' @export
#' @examples
#' \dontrun{
#' counterfact_me_constrained(list(age = 42, gender = "M", county = "Oslo"))
#' counterfact_me_constrained(list(
#'   age = 30, gender = "F", occupation_substring = "lærer"
#' ))
#' }
counterfact_me_constrained <- function(givens = list(),
                                       max_attempts = 500L,
                                       lang = c("no", "en"),
                                       dimensions = available_dimensions(),
                                       verbose = FALSE) {
  lang <- match.arg(lang)
  if (length(givens) == 0) {
    return(counterfact_me(lang = lang, dimensions = dimensions))
  }

  # Direct parameters: age, gender
  age_val <- givens$age
  gender_val <- givens$gender

  # All other constraints checked post-hoc
  free_givens <- givens[!(names(givens) %in% c("age", "gender"))]

  for (attempt in seq_len(max_attempts)) {
    x <- counterfact_me(
      min_age = if (!is.null(age_val)) as.integer(age_val) else 0L,
      max_age = if (!is.null(age_val)) as.integer(age_val) else 99L,
      gender = gender_val,
      lang = lang,
      dimensions = dimensions
    )

    if (.matches_givens(x, free_givens)) {
      if (verbose) message("Matched on attempt ", attempt)
      return(x)
    }
  }

  warning("Could not satisfy all constraints in ", max_attempts,
          " attempts. Falling back to direct override.")
  # Direct override
  x <- counterfact_me(
    min_age = if (!is.null(age_val)) as.integer(age_val) else 0L,
    max_age = if (!is.null(age_val)) as.integer(age_val) else 99L,
    gender = gender_val,
    lang = lang,
    dimensions = dimensions
  )
  for (key in names(free_givens)) {
    if (key %in% names(x)) {
      x[[key]] <- free_givens[[key]]
    } else {
      # Try alias
      x[[key]] <- free_givens[[key]]
    }
  }
  x
}

# Helper: check if x satisfies the givens (substring match for strings, exact for nums)
.matches_givens <- function(x, givens) {
  for (key in names(givens)) {
    target <- givens[[key]]
    if (is.null(target) || all(is.na(target))) next

    # Find actual value with key/alias mapping
    actual <- .lookup_field(x, key)
    if (is.null(actual) || all(is.na(actual))) return(FALSE)

    # Compare
    if (is.character(target)) {
      # grepl() cannot combine fixed = TRUE with ignore.case = TRUE: it
      # warns and drops ignore.case, so this matched case-sensitively
      # while claiming otherwise. A constraint of "laerer" therefore
      # missed "Laerer" and the rejection sampler ran out of attempts.
      # Fold both sides to lower case instead and keep the literal match,
      # which is what was meant. It also emitted one warning per
      # comparison -- 20,508 from twenty parallel-lives calls.
      hay <- tolower(as.character(actual))
      ok <- any(vapply(target, function(t)
        any(grepl(tolower(t), hay, fixed = TRUE)), logical(1)))
      if (!ok) return(FALSE)
    } else if (is.numeric(target)) {
      ok <- any(as.numeric(actual) == as.numeric(target))
      if (!ok) return(FALSE)
    }
  }
  TRUE
}

# Helper: lookup field in x, with aliases
.lookup_field <- function(x, key) {
  alias_map <- list(
    county = c("county", "municipality"),
    municipality = c("municipality", "county"),
    occupation = c("occupation"),
    education = c("education"),
    marital_status = c("marital_status"),
    religion = c("religion"),
    party = c("party"),
    background = c("background", "country_background")
  )
  candidates <- if (key %in% names(alias_map)) alias_map[[key]] else key
  for (c in candidates) {
    if (c %in% names(x) && !is.null(x[[c]])) return(x[[c]])
  }
  NULL
}

#' Generate parallel counterfactual lives varying one dimension
#'
#' Creates \code{n} counterfactual lives that share the \code{givens} but
#' vary in one dimension. Useful for "what if you had been born in...",
#' "what if you had become...", etc.
#'
#' @param givens A named list of constraints to hold fixed across all lives.
#' @param vary_dim Which dimension to vary. One of \code{"county"},
#'   \code{"occupation"}, \code{"education"}, \code{"background"}.
#' @param n Number of parallel lives. Default 5.
#' @param vary_values If non-NULL, a vector of specific values to use for
#'   the varying dimension. If NULL, draws random distinct values.
#' @param lang Language for labels. Default "no".
#' @param max_attempts_each Max attempts per individual life.
#' @return A list of \code{counterfactme} objects (length \code{n}).
#' @export
#' @examples
#' \dontrun{
#' counterfact_parallel_lives(
#'   givens = list(age = 42, gender = "M"),
#'   vary_dim = "county",
#'   n = 5
#' )
#' counterfact_parallel_lives(
#'   givens = list(age = 30, gender = "F"),
#'   vary_dim = "background",
#'   vary_values = c("majority", "first_gen"),
#'   n = 2
#' )
#' }
counterfact_parallel_lives <- function(givens = list(),
                                       vary_dim = c("county", "occupation",
                                                    "education", "background",
                                                    "religion", "party"),
                                       n = 5,
                                       vary_values = NULL,
                                       lang = c("no", "en"),
                                       max_attempts_each = 300L) {
  vary_dim <- match.arg(vary_dim)
  lang <- match.arg(lang)

  # If vary_values not supplied, sample distinct random values
  if (is.null(vary_values)) {
    vary_values <- .sample_vary_values(vary_dim, n, lang = lang)
  } else if (length(vary_values) < n) {
    vary_values <- rep_len(vary_values, n)
  }

  lives <- vector("list", n)
  for (i in seq_len(n)) {
    target <- vary_values[i]
    full_givens <- givens
    full_givens[[vary_dim]] <- target
    lives[[i]] <- counterfact_me_constrained(
      givens = full_givens,
      max_attempts = max_attempts_each,
      lang = lang
    )
  }
  class(lives) <- c("counterfact_parallel", "list")
  attr(lives, "vary_dim") <- vary_dim
  attr(lives, "givens") <- givens
  lives
}

# Helper: sample distinct values for the vary dimension
.sample_vary_values <- function(vary_dim, n, lang = "no") {
  .load_data()
  if (identical(vary_dim, "county")) {
    cs <- unique(.cfm_env$municipalities$county)
    sample(cs, min(n, length(cs)))
  } else if (identical(vary_dim, "occupation")) {
    os <- .cfm_env$occupations_salary
    if (is.null(os)) return(rep("Sykepleier", n))
    candidates <- os$label[os$schedule == "all" & nzchar(os$code) & os$code != "0000"]
    sample(unique(candidates), min(n, length(unique(candidates))))
  } else if (identical(vary_dim, "education")) {
    edu <- .cfm_env$education
    col <- if (identical(lang, "no")) "level_no" else "level"
    sample(edu[[col]], min(n, nrow(edu)))
  } else if (identical(vary_dim, "background")) {
    sample(c("majority", "first_gen", "second_gen"), min(n, 3))
  } else if (identical(vary_dim, "religion")) {
    rb <- .cfm_env$religion_baseline
    col <- if (identical(lang, "no")) "label_no" else "label_en"
    sample(rb[[col]], min(n, nrow(rb)))
  } else if (identical(vary_dim, "party")) {
    pb <- .cfm_env$party_baseline
    sample(pb$label, min(n, nrow(pb)))
  } else {
    rep(NA, n)
  }
}

#' Print method for parallel lives
#' @param x A \code{counterfact_parallel} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.counterfact_parallel <- function(x, ...) {
  vary_dim <- attr(x, "vary_dim")
  givens <- attr(x, "givens")
  is_no <- !is.null(x[[1]]$lang) && identical(x[[1]]$lang, "no")
  header <- if (is_no) {
    sprintf("\n=== %d parallelle liv (varierer %s) ===\n", length(x), vary_dim)
  } else {
    sprintf("\n=== %d parallel lives (varying %s) ===\n", length(x), vary_dim)
  }
  cat(header)
  if (length(givens) > 0) {
    given_str <- paste(names(givens), unlist(givens), sep = "=", collapse = ", ")
    if (is_no) cat(sprintf("Gitt: %s\n", given_str))
    else cat(sprintf("Given: %s\n", given_str))
  }
  for (i in seq_along(x)) {
    cat(sprintf("\n--- Liv %d ---\n", i))
    print(x[[i]])
  }
  invisible(x)
}
