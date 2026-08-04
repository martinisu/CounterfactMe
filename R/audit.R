# ============================================================
# Soft-plausibility audit (added v0.9.24)
#
# verify_consistency() catches the IMPOSSIBLE. This catches the
# IMPLAUSIBLE-TOO-OFTEN: it cross-tabulates every categorical dimension
# by a grouping variable (default gender) and reports the group share of
# each value, so a sampler that isn't conditioned strongly enough shows up
# (e.g. women getting male-typed hobbies more often than is believable).
# ============================================================

#' Audit soft plausibility of generated lives across dimensions
#'
#' Generates N lives and, for each categorical dimension, reports how each
#' value splits across a grouping variable (default \code{"gender"}). The
#' point is to surface combinations that are possible but appear far more
#' often than is believable — the kind of thing that drains realism even
#' though it is not strictly impossible.
#'
#' @param N Number of lives. Default 5000.
#' @param by Grouping variable, a top-level field. Default \code{"gender"}.
#' @param lang Language. Default \code{"no"}.
#' @param dims Dimensions to audit. Default covers the main categorical ones.
#' @param min_n Minimum occurrences of a value before it is reported. Default 15.
#' @param top Show only the \code{top} most- and least-skewed values per
#'   dimension (0 = all). Default 12.
#' @param verbose Print a report. Default TRUE.
#' @return Invisibly: a named list of data frames (one per dimension).
#' @export
audit_plausibility <- function(N = 5000L, by = "gender", lang = "no",
                               dims = c("hobbies", "diet", "party", "religion",
                                        "orientation", "education", "occupation",
                                        "marital_status", "disability",
                                        "close_friends"),
                               min_n = 15L, top = 12L, verbose = TRUE) {
  message(sprintf("Generating %d lives...", N))
  draws <- vector("list", N)
  for (i in seq_len(N)) {
    draws[[i]] <- tryCatch(counterfact_me(lang = lang), error = function(e) NULL)
  }
  draws <- Filter(Negate(is.null), draws)

  get_by <- function(d) {
    v <- d[[by]]
    if (is.null(v) || length(v) != 1 || is.na(v)) return(NA_character_)
    if (identical(by, "gender")) return(if (identical(v, "M")) "M" else "F")
    as.character(v)
  }

  value_of <- function(d, dm) {
    if (identical(dm, "hobbies")) {
      hs <- d$hobbies
      if (is.null(hs) || !length(hs)) return(character(0))
      return(vapply(hs, function(h) if (is.list(h)) as.character(h$label %||% "")
                    else as.character(h), character(1)))
    }
    if (identical(dm, "disability")) {
      if (!isTRUE(d$has_disability)) return(character(0))
      return(as.character(d$disability_type %||% d$disability %||% NA))
    }
    v <- d[[dm]]
    if (is.null(v) || length(v) != 1 || is.na(v)) return(character(0))
    as.character(v)
  }

  results <- list()
  for (dm in dims) {
    vals <- character(0); grp <- character(0)
    for (d in draws) {
      b <- get_by(d); if (is.na(b)) next
      vv <- value_of(d, dm)
      vv <- vv[nzchar(vv) & !is.na(vv)]
      if (!length(vv)) next
      vals <- c(vals, vv); grp <- c(grp, rep(b, length(vv)))
    }
    if (!length(vals)) next
    tab <- table(vals, grp)
    levs <- colnames(tab)
    total <- rowSums(tab)
    df <- data.frame(value = rownames(tab), n = as.integer(total),
                     stringsAsFactors = FALSE)
    for (lv in levs) df[[paste0("pct_", lv)]] <- round(100 * tab[, lv] / total, 1)
    df <- df[df$n >= min_n, , drop = FALSE]
    if (nrow(df) == 0) next
    # sort by the first group level's share (for gender: %F if present else first)
    sort_col <- if ("pct_F" %in% names(df)) "pct_F" else paste0("pct_", levs[1])
    df <- df[order(-df[[sort_col]]), , drop = FALSE]
    rownames(df) <- NULL
    results[[dm]] <- df
  }

  if (verbose) {
    cat(sprintf("\n=== Plausibilitets-revisjon (N = %d, gruppert p\u{00e5} '%s') ===\n",
                length(draws), by))
    cat("Andel (%) av hver verdi som faller i hver gruppe. Se etter verdier\n")
    cat("som burde v\u{00e6}rt sterkt skjeve, men ikke er det.\n")
    for (dm in names(results)) {
      df <- results[[dm]]
      cat(sprintf("\n--- %s (%d verdier med n >= %d) ---\n", dm, nrow(df), min_n))
      show <- df
      if (top > 0 && nrow(df) > 2 * top) {
        show <- rbind(utils::head(df, top), utils::tail(df, top))
        show <- show[!duplicated(show$value), ]
      }
      print(show, row.names = FALSE)
    }
  }
  invisible(results)
}
