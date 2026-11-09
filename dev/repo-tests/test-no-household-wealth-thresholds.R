# Repo-sjekk: ingen husholdningsterskler skal ha sneket seg tilbake inn i koden.
# 12.14e6 og 28.25e6 er Top5- og Top1-grensene fra SSB 10318, en
# husholdningstabell. De hoerer ikke hjemme i en personbasert sampler.
#
# Foerste versjon leste "R/conditional.R" relativt til pakkeroten og stod
# som loes kode utenfor test_that(). test_dir() kjoerer fra dev/repo-tests/,
# saa fila ble aldri funnet, og hele repo-sjekken var roed fra 18. september
# uten at selve paastanden noen gang ble testet.

test_that("ingen husholdningsterskler i R/conditional.R", {
  p <- "../../R/conditional.R"
  expect_true(file.exists(p))
  src <- readLines(p, warn = FALSE)
  code <- src[!grepl("^\\s*#", src)]
  expect_gt(length(code), 1000L)
  for (m in c("133e6", "133.4e6", "28.25e6", "12.14e6")) {
    treff <- grep(m, code, fixed = TRUE, value = TRUE)
    expect_length(treff, 0L)
  }
})
