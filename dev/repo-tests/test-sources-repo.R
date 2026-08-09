# Repository checks for the data-provenance manifest.
#
# These read the source tree (R/, data-raw/), so they cannot run against
# an installed package. They live outside tests/ for that reason: keeping
# them there meant R CMD check and covr kept trying to run them, and
# twice they brought the build down over nothing wrong with the package.
#
# Run them with:  Rscript dev/run-repo-tests.R

test_that("manifest fields are internally consistent", {
  d <- data_sources()

  expect_true(all(d$status %in%
                  c("ssb_api", "ssb_cited", "untraced", "authored")))

  # A file claimed as API-fetched must name the script that fetches it,
  # or the claim cannot be checked by anyone.
  api <- d[d$status == "ssb_api", ]
  expect_true(all(nzchar(api$script)))

  # And that script must exist in the source tree. data-raw/ is excluded
  # from the built package, so this only runs from a source checkout.
  for (s in unique(api$script)) {
    expect_true(file.exists(file.path("../../data-raw", s)),
                label = sprintf("data-raw/%s named in SOURCES.csv", s))
  }
})

test_that("ssb_cited tables really are cited in the R source", {
  # Only `ssb_cited` claims the R source as its evidence. For `ssb_api`
  # the evidence is the fetching script, checked above -- and those table
  # numbers may live only in data-raw/, which .Rbuildignore strips from
  # the built package. Checking both against R/ conflated the two and
  # failed on 07459, which appears only in data-raw/fetch_pop.py.
  src <- paste(unlist(lapply(list.files("../../R", pattern = "\\.[Rr]$",
                                        full.names = TRUE),
                             readLines, warn = FALSE)), collapse = "\n")
  cited <- data_sources()
  tab <- as.character(cited$ssb_table)
  cited <- cited[cited$status == "ssb_cited" &
                 !is.na(tab) & nzchar(tab), ]
  expect_gt(nrow(cited), 0L)
  for (i in seq_len(nrow(cited))) {
    tab <- as.character(cited$ssb_table[i])
    expect_true(grepl(tab, src, fixed = TRUE),
                label = sprintf("SSB %s (%s) cited in R/",
                                tab, cited$file[i]))
  }
})

test_that("ssb_api tables are traceable to their fetching script", {
  # data-raw/ is not in the built package, so this only runs from source.
  dr <- paste(unlist(lapply(
    Filter(function(f) !dir.exists(f),
           list.files("../../data-raw", full.names = TRUE)),
    readLines, warn = FALSE)), collapse = "\n")
  api <- data_sources()
  tab <- as.character(api$ssb_table)
  api <- api[api$status == "ssb_api" & !is.na(tab) & nzchar(tab), ]
  expect_gt(nrow(api), 0L)
  for (i in seq_len(nrow(api))) {
    tab <- as.character(api$ssb_table[i])
    expect_true(grepl(tab, dr, fixed = TRUE),
                label = sprintf("SSB %s (%s) appears in data-raw/",
                                tab, api$file[i]))
  }
})
