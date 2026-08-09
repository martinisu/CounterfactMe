# Documentation and metadata consistency.
#
# Written after a round of avoidable errors: a README describing a version
# five releases old, SSB table numbers cited for data the package cannot
# trace, a fabricated ORCID, a GitHub username inferred from an email
# address, and tests that passed while checking nothing.
#
# None of these fail R CMD check. They fail a reader.
#
# These tests only run from a source checkout, since the built package
# does not ship README.md or data-raw/.

# An installed package also has an R/ directory -- of .rdb binaries, not
# source -- so the guard must look for real .R files.
# These tests check the repository, not the package: version numbers
# agreeing across files, README counts matching the code, no test that
# skips without counting. They read the source tree, so they are correct
# only in a source checkout -- and they were breaking R CMD check and
# covr, which run against an installed package.
#
# They are therefore opt-in. Run them with:
#
#   Sys.setenv(CFM_SOURCE_CHECKS = "true"); devtools::test()
#
# or via the source-checks job in CI.
skip_unless_source_checks <- function() {
  skip_if_not(nzchar(Sys.getenv("CFM_SOURCE_CHECKS")),
              "set CFM_SOURCE_CHECKS to run repository checks")
}

read_file <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")

test_that("the version is the same everywhere it appears", {
  skip_unless_source_checks()
  ver <- read.dcf("../../DESCRIPTION", "Version")[[1]]

  cff <- read_file("../../CITATION.cff")
  expect_true(grepl(sprintf("version: %s", ver), cff, fixed = TRUE))

  cit <- read_file("../../inst/CITATION")
  expect_true(grepl(sprintf("version %s", ver), cit, fixed = TRUE))

  rme <- read_file("../../README.md")
  stale <- setdiff(unique(unlist(regmatches(
    rme, gregexpr("version [0-9]+\\.[0-9]+\\.[0-9]+", rme)))),
    sprintf("version %s", ver))
  expect_equal(stale, character(0))
})

test_that("the README's counts match the package", {
  skip_unless_source_checks()
  rme <- read_file("../../README.md")

  expect_true(grepl(sprintf("%d dimensions", length(available_dimensions())), rme))

  ed <- system.file("extdata", package = "CounterfactMe")
  n_csv <- length(setdiff(list.files(ed, pattern = "\\.csv$"), "SOURCES.csv"))
  expect_equal(n_csv, 50)   # README says "Fifty CSV files"
  expect_true(grepl("Fifty CSV files", rme))

  n_tests <- sum(vapply(
    list.files("../testthat", pattern = "^test-.*\\.R$", full.names = TRUE),
    function(f) length(gregexpr("test_that(", read_file(f), fixed = TRUE)[[1]]),
    integer(1)))
  expect_true(grepl(sprintf("%d tests", n_tests), rme),
              label = sprintf("README should say %d tests", n_tests))
})

test_that("the README's provenance table matches SOURCES.csv", {
  skip_unless_source_checks()
  rme <- read_file("../../README.md")
  tab <- table(data_sources()$status)
  for (st in names(tab)) {
    expect_true(grepl(sprintf("`%s`", st), rme, fixed = TRUE),
                label = sprintf("README mentions status '%s'", st))
    expect_true(grepl(sprintf("| %d |", tab[[st]]), rme, fixed = TRUE),
                label = sprintf("README count for '%s' (%d)", st, tab[[st]]))
  }
})

test_that("no SSB table is cited in the docs that the package cannot back", {
  # This is the specific failure: the README cited 06035, 07230 and 06929
  # for files recorded as untraced. A citation the package cannot support
  # is worse than none, because it looks checkable.
  skip_unless_source_checks()

  known <- unique(c(
    as.character(data_sources()$ssb_table),
    unlist(regmatches(
      paste(vapply(list.files("../../R", pattern = "\\.[Rr]$", full.names = TRUE), read_file,
                   character(1)), collapse = "\n"),
      gregexpr("\\b[01][0-9]{4}\\b",
               paste(vapply(list.files("../../R", pattern = "\\.[Rr]$", full.names = TRUE), read_file,
                            character(1)), collapse = "\n"))))
  ))
  known <- known[nzchar(known)]

  docs <- Filter(file.exists,
                 c("../../README.md", "../../vignettes/introduction.Rmd"))
  expect_gt(length(docs), 0L)
  for (f in docs) {
    txt <- read_file(f)
    cited <- unlist(regmatches(txt, gregexpr("\\b[01][0-9]{4}\\b", txt)))
    expect_equal(setdiff(unique(cited), known), character(0),
                 label = sprintf("unsupported SSB tables cited in %s", basename(f)))
  }
})

test_that("every exported function is documented and defined", {
  ns <- readLines("../../NAMESPACE", warn = FALSE)
  exported <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
  for (e in exported) {
    expect_true(exists(e, where = asNamespace("CounterfactMe")),
                label = sprintf("%s is defined", e))
    skip_unless_source_checks()
    expect_true(file.exists(file.path("../../man", paste0(e, ".Rd"))),
                label = sprintf("%s.Rd exists", e))
  }
})

test_that("no test can pass without checking something", {
  # Two tests written in this project passed while every draw hit a
  # `next`. A loop that may skip has to say how much it looked at.
  skip_unless_source_checks()
  for (f in list.files("../testthat", pattern = "^test-.*\\.R$", full.names = TRUE)) {
    txt <- read_file(f)
    blocks <- strsplit(txt, "\ntest_that(", fixed = TRUE)[[1]][-1]
    for (b in blocks) {
      nm <- sub("^\"([^\"]+)\".*", "\\1", b)
      expect_true(grepl("expect_", b, fixed = TRUE),
                  label = sprintf("%s: '%s' asserts something", basename(f), nm))
      if (grepl("\\bnext\\b", b)) {
        expect_true(grepl("checked|inspected", b),
                    label = sprintf("%s: '%s' counts what it inspected",
                                    basename(f), nm))
      }
    }
  }
})
