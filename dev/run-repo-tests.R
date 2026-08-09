#!/usr/bin/env Rscript
# Repository checks.
#
# These verify the repository rather than the package: version numbers
# agreeing across DESCRIPTION, CITATION.cff, inst/CITATION and README;
# README counts matching the code; no SSB table cited in the docs that
# the manifest cannot back; every export documented; and no test that
# skips without counting what it inspected.
#
# They read the source tree, so they cannot run against an installed
# package. That is why they are not in tests/: R CMD check and covr kept
# trying to run them there, and twice brought the build down over
# nothing that was wrong with the package.
#
# Run from the package root:
#
#   Rscript dev/run-repo-tests.R

if (!file.exists("DESCRIPTION")) {
  stop("run this from the package root", call. = FALSE)
}

library(testthat)
suppressMessages(pkgload::load_all(".", quiet = TRUE))

res <- test_dir("dev/repo-tests", stop_on_failure = TRUE, reporter = "summary")
