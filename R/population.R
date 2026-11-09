# Population mode: draw many lives and look at the DISTRIBUTION.
#
# Inequality is a property of a distribution, not of a person, so nothing in
# the policy work can be checked until the sampler can be asked to produce a
# population and that population can be held against the register.
#
# Unit of analysis is the PERSON throughout -- see dev/design/policy-mode-plan.md.
# That is the right unit for the wealth tax (which is levied on persons), but
# it means the figures here are NOT comparable to SSB's published Gini, which
# is household equivalised. Saying "Norwegian inequality is 0.27" and then
# showing a person-level Gini would be comparing two different quantities.

#' Gini coefficient
#'
#' @param x Numeric vector. NAs are dropped.
#' @return Gini between 0 and 1, or NA if fewer than two values remain.
#' @keywords internal
.gini <- function(x) {
  x <- as.numeric(x[!is.na(x)])
  if (length(x) < 2) return(NA_real_)
  # Negative values (debt exceeding assets) are real in wealth data and make
  # the standard formula exceed 1. Reported anyway, but flagged by the caller.
  x <- sort(x)
  n <- length(x)
  total <- sum(x)
  if (total == 0) return(NA_real_)
  (2 * sum(seq_len(n) * x) / (n * total)) - (n + 1) / n
}

#' Draw a population of lives
#'
#' Repeatedly calls \code{counterfact_me()} and collects the fields needed for
#' distributional work into one data frame.
#'
#' @param n Number of lives.
#' @param min_age,max_age Age range. Defaults to 17+, matching the SSB
#'   person-level tables the result is calibrated against.
#' @param lang \code{"no"} or \code{"en"}.
#' @param progress Print a dot every 250 draws.
#' @return A data frame of class \code{counterfact_pop}, one row per life.
#' @examples
#' \donttest{
#' pop <- counterfact_population(n = 200)
#' summary(pop)
#' }
#' @export
counterfact_population <- function(n = 1000L, min_age = 17L, max_age = 99L,
                                   lang = c("no", "en"), progress = FALSE) {
  lang <- match.arg(lang)
  .load_data()
  n <- as.integer(n)

  pick <- function(x, default = NA) {
    if (is.null(x) || length(x) != 1) default else x
  }

  rows <- vector("list", n)
  for (i in seq_len(n)) {
    if (isTRUE(progress) && i %% 250 == 0) cat(".")
    x <- counterfact_me(min_age = min_age, max_age = max_age, lang = lang)
    rows[[i]] <- data.frame(
      age            = as.integer(pick(x$age, NA_integer_)),
      gender         = as.character(pick(x$gender, NA_character_)),
      education      = as.character(pick(x$education, NA_character_)),
      education_code = .era_edu_code(pick(x$education, NA_character_)),
      occupation     = as.character(pick(x$occupation, NA_character_)),
      income_nok     = as.numeric(pick(x$income_nok, NA_real_)),
      net_wealth_nok = as.numeric(pick(x$net_wealth_nok, NA_real_)),
      wealth_class   = as.character(pick(x$wealth_class, NA_character_)),
      county         = as.character(pick(x$county, NA_character_)),
      sentralitet    = as.character(pick(x$sentralitet, NA_character_)),
      stringsAsFactors = FALSE
    )
  }
  if (isTRUE(progress)) cat("\n")
  out <- do.call(rbind, rows)
  class(out) <- c("counterfact_pop", "data.frame")
  attr(out, "drawn") <- n
  attr(out, "age_range") <- c(min_age, max_age)
  out
}

#' Summary method for a drawn population
#' @param object A \code{counterfact_pop} data frame.
#' @param ... Ignored.
#' @return Invisibly returns a list of the computed figures.
#' @export
summary.counterfact_pop <- function(object, ...) {
  inc <- object$income_nok[!is.na(object$income_nok)]
  wea <- object$net_wealth_nok[!is.na(object$net_wealth_nok)]

  cat(sprintf("\n=== %d trukne liv, alder %d-%d ===\n", nrow(object),
              attr(object, "age_range")[1], attr(object, "age_range")[2]))
  cat("\nAnalyseenhet: PERSON. Tallene er derfor IKKE sammenliknbare med\n")
  cat("SSBs publiserte Gini, som er husholdningsekvivalent.\n")

  cat(sprintf("\nInntekt   n = %d\n", length(inc)))
  cat(sprintf("  median   %14s kr\n", format(round(stats::median(inc)), big.mark = " ")))
  cat(sprintf("  snitt    %14s kr\n", format(round(mean(inc)), big.mark = " ")))
  cat(sprintf("  Gini     %14.3f\n", .gini(inc)))

  cat(sprintf("\nFormue    n = %d\n", length(wea)))
  cat(sprintf("  median   %14s kr\n", format(round(stats::median(wea)), big.mark = " ")))
  cat(sprintf("  snitt    %14s kr\n", format(round(mean(wea)), big.mark = " ")))
  g <- .gini(wea)
  cat(sprintf("  Gini     %14.3f%s\n", g,
              if (!is.na(g) && g > 1) "   (over 1: negativ formue i bunnen)" else ""))
  cat(sprintf("  andel negativ  %8.1f %%\n", 100 * mean(wea < 0)))

  invisible(list(income_gini = .gini(inc), wealth_gini = g,
                 income_median = stats::median(inc),
                 wealth_median = stats::median(wea)))
}

#' Hold a drawn population against the register
#'
#' The whole point of population mode. If the sampler does not reproduce SSB's
#' actual person-level distribution, then any distributional effect computed on
#' top of it -- a Gini shift under some tax change, say -- is an artefact of
#' the sampler rather than a finding about Norway.
#'
#' Both targets are person-level and 2024: income from SSB 06655, wealth from
#' 08589. Compared on bracket shares, which is what those tables publish.
#'
#' @param pop A \code{counterfact_pop} from \code{counterfact_population()}.
#' @param tol Flag brackets whose share is off by more than this (absolute).
#' @return Invisibly, a list of two data frames (\code{income}, \code{wealth}).
#' @examples
#' \donttest{
#' population_calibration(counterfact_population(n = 500))
#' }
#' @export
population_calibration <- function(pop, tol = 0.03) {
  .load_data()
  stopifnot(inherits(pop, "counterfact_pop"))

  cmp <- function(drawn, breaks, labels, target_counts, what) {
    drawn <- drawn[!is.na(drawn)]
    obs <- table(cut(drawn, breaks = breaks, labels = labels,
                     right = FALSE, include.lowest = TRUE))
    obs <- as.numeric(obs) / length(drawn)
    ssb <- target_counts / sum(target_counts)
    d <- data.frame(intervall = labels, ssb = round(ssb, 4),
                    trukket = round(obs, 4),
                    avvik = round(obs - ssb, 4),
                    stringsAsFactors = FALSE)
    d$flagg <- ifelse(abs(d$avvik) > tol, "  <-- ", "")
    cat(sprintf("\n=== %s: trukket mot SSB (n = %d) ===\n", what, length(drawn)))
    print(d, row.names = FALSE)
    verste <- max(abs(d$avvik))
    cat(sprintf("  storste avvik: %.4f   (terskel %.2f)\n", verste, tol))
    if (verste > tol) {
      cat("  MISMATCH. Fordelingseffekter regnet paa denne populasjonen er\n")
      cat("  artefakter av sampleren, ikke funn om Norge.\n")
    }
    d
  }

  it <- .cfm_env$income_by_age
  ib <- it[!duplicated(it$bracket_code), ]
  ib <- ib[order(ib$lower_nok), ]
  inc_counts <- vapply(ib$bracket_code,
                       function(k) sum(as.numeric(it$count[it$bracket_code == k])),
                       numeric(1))
  inc <- cmp(pop$income_nok,
             breaks = c(ib$lower_nok, Inf), labels = ib$bracket_label,
             target_counts = inc_counts, what = "Inntekt (SSB 06655)")

  wt <- .cfm_env$wealth_by_age
  wb <- wt[!duplicated(wt$bracket_code), ]
  ord <- c("00-", "01", "02", "03", "04", "05", "07", "08", "09", "10+")
  wb <- wb[match(ord, wb$bracket_code), ]
  wb <- wb[!is.na(wb$bracket_code), ]
  lower <- vapply(wb$bracket_code, function(k) .wealth_bracket_range(k)[1], numeric(1))
  wea_counts <- vapply(wb$bracket_code,
                       function(k) sum(as.numeric(wt$count[wt$bracket_code == k])),
                       numeric(1))
  wea <- cmp(pop$net_wealth_nok,
             breaks = c(lower, Inf), labels = wb$bracket_label,
             target_counts = wea_counts, what = "Formue (SSB 08589)")

  invisible(list(income = inc, wealth = wea))
}
