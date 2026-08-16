# Repository checks: version numbers agreeing across files, README counts
# matching the code, SSB tables cited in the docs, every export
# documented, and no test that skips without counting what it inspected.
#
# Written after a round of avoidable errors: a README describing a
# version five releases old, SSB table numbers cited for data the
# package cannot trace, a fabricated ORCID, and a GitHub username
# inferred from an email address. None of these fail R CMD check. They
# fail a reader.
#
# These examine the repository, not the package. They read the source
# tree, so they cannot run against an installed one -- which is why they
# live here rather than in tests/.
#
# Run them with:  Rscript dev/run-repo-tests.R

read_file <- function(p) paste(readLines(p, warn = FALSE), collapse = "\n")

test_that("the version is the same everywhere it appears", {
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

test_that("DESCRIPTION's dimension count matches the code", {
  # DESCRIPTION is the first thing a reader sees, on GitHub and on CRAN.
  # It said 33 for four months while available_dimensions() returned 32,
  # because the version check below only ever read the Version field.
  d <- paste(readLines("../../DESCRIPTION", warn = FALSE), collapse = " ")
  n <- length(available_dimensions())
  stated <- regmatches(d, regexpr("[0-9]+ dimensions", d))
  expect_equal(stated, sprintf("%d dimensions", n))
})

test_that("the README's counts match the package", {
  rme <- read_file("../../README.md")

  expect_true(grepl(sprintf("%d dimensions", length(available_dimensions())), rme))

  # Derive the count rather than hardcode it: the literal 50 went stale
  # the first time a data file was added.
  ed <- system.file("extdata", package = "CounterfactMe")
  n_csv <- length(setdiff(list.files(ed, pattern = "\\.csv$"), "SOURCES.csv"))
  expect_true(grepl(sprintf("%d CSV files ship", n_csv), rme),
              label = sprintf("README should say %d CSV files", n_csv))

  n_tests <- sum(vapply(
    list.files("../../tests/testthat", pattern = "^test-.*\\.R$", full.names = TRUE),
    function(f) length(gregexpr("test_that(", read_file(f), fixed = TRUE)[[1]]),
    integer(1)))
  expect_true(grepl(sprintf("%d tests", n_tests), rme),
              label = sprintf("README should say %d tests", n_tests))
})

test_that("the README's provenance table matches SOURCES.csv", {
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
      expect_true(file.exists(file.path("../../man", paste0(e, ".Rd"))),
                label = sprintf("%s.Rd exists", e))
  }
})

test_that("no test can pass without checking something", {
  # Two tests written in this project passed while every draw hit a
  # `next`. A loop that may skip has to say how much it looked at.
  files <- c(list.files("../../tests/testthat", pattern = "^test-.*\\.R$",
                        full.names = TRUE),
             list.files(".", pattern = "^test-.*\\.R$", full.names = TRUE))
  expect_gt(length(files), 5L)   # or the glob found nothing and proved nothing
  for (f in files) {
    txt <- read_file(f)
    blocks <- strsplit(txt, "\ntest_that(", fixed = TRUE)[[1]][-1]
    for (b in blocks) {
      nm <- sub("^\"([^\"]+)\".*", "\\1", b)
      expect_true(grepl("expect_", b, fixed = TRUE),
                  label = sprintf("%s: '%s' asserts something", basename(f), nm))
      if (grepl("\\bnext\\b", b)) {
        # The rule is that a loop which may skip must assert a lower
        # bound on how much it actually looked at. Match the assertion,
        # not the variable name -- the first version required the counter
        # to be called "checked" or "inspected", which is a naming
        # convention rather than the property in question, and it failed
        # a test that counted correctly in a variable called "seen".
        expect_true(grepl("expect_gte?\\(", b),
                    label = sprintf("%s: '%s' asserts how much it inspected",
                                    basename(f), nm))
      }
    }
  }
})
