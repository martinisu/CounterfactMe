# ============================================================
# Data provenance.
#
# The package ships 50 CSV files. Some are fetched from Statistics
# Norway's API by a script in data-raw/ and can be reproduced by running
# it again. Others were entered by hand, and for most of those the
# original table was never recorded.
#
# That distinction was invisible until now: an estimate sat in the same
# directory as a register figure and looked exactly as authoritative.
# For a package described as built on open SSB data, that is the
# dangerous kind of gap -- not wrong in an obvious way, just unverifiable.
#
# SOURCES.csv records what is actually known. "untraced" is deliberately
# not a synonym for "invented": it means no script and no citation was
# found in the package, so the figure may be an accurate transcription
# or may be an estimate. Establishing which is the remaining work.
# ============================================================

#' Provenance of the packaged data files
#'
#' Returns a table describing where each CSV in \code{inst/extdata} came
#' from, so that register figures can be told apart from estimates.
#'
#' The \code{status} column takes four values:
#' \describe{
#'   \item{\code{ssb_api}}{Fetched from Statistics Norway's API by a
#'     script in \code{data-raw/}. Reproducible by re-running it.}
#'   \item{\code{ssb_cited}}{An SSB table is cited in the R source, but
#'     the figures were transcribed by hand rather than fetched.}
#'   \item{\code{untraced}}{No generating script and no citation found in
#'     the package. This is not a claim that the figures are invented --
#'     only that their origin has not been established.}
#'   \item{\code{authored}}{Deliberately written content, such as the
#'     humorous labels. Not data, and not presented as such.}
#' }
#'
#' @param status Optional filter, e.g. \code{"untraced"}. \code{NULL}
#'   (default) returns every file.
#' @return A data frame with columns \code{file}, \code{status},
#'   \code{ssb_table}, \code{script} and \code{note}.
#' @export
#' @examples
#' data_sources()
#' data_sources("untraced")
#' table(data_sources()$status)
data_sources <- function(status = NULL) {
  p <- system.file("extdata", "SOURCES.csv", package = "CounterfactMe")
  if (!nzchar(p)) stop("SOURCES.csv not found in the installed package.")
  d <- utils::read.csv(p, stringsAsFactors = FALSE, encoding = "UTF-8")
  if (!is.null(status)) {
    bad <- setdiff(status, unique(d$status))
    if (length(bad)) {
      stop("Unknown status: ", paste(bad, collapse = ", "),
           "\nAvailable: ", paste(sort(unique(d$status)), collapse = ", "),
           call. = FALSE)
    }
    d <- d[d$status %in% status, , drop = FALSE]
  }
  rownames(d) <- NULL
  d
}
