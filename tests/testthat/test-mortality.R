# Forventet gjenstaende levetid, SSB 14569 (2025).

test_that("datafila dekker alle kombinasjoner", {
  tbl <- CounterfactMe:::.cfm_env$life_expectancy
  expect_false(is.null(tbl))
  # 7 regioner (land + 6 sentralitetsnivaa) x 2 kjonn x 100 aldre
  expect_equal(nrow(tbl), 1400L)
  expect_setequal(unique(tbl$gender), c("M", "F"))
  expect_equal(range(tbl$age), c(0L, 99L))
})

test_that("kjonnsgapet er SSBs 3,2 aar ved fodsel", {
  f <- CounterfactMe:::.life_expectancy(0, "F", NA)
  m <- CounterfactMe:::.life_expectancy(0, "M", NA)
  expect_equal(f - m, 3.2, tolerance = 0.05)
  expect_gt(f, m)
})

test_that("sentralitetsgradienten gaar riktig vei og har riktig storrelse", {
  # 2.2 aar for menn, 1.2 for kvinner (SSB 14569, 2025).
  for (g in c("M", "F")) {
    sentral <- CounterfactMe:::.life_expectancy(0, g, 1)
    distrikt <- CounterfactMe:::.life_expectancy(0, g, 6)
    expect_gt(sentral, distrikt, label = paste("sentralitet", g))
  }
  expect_equal(CounterfactMe:::.life_expectancy(0, "M", 1) -
               CounterfactMe:::.life_expectancy(0, "M", 6), 2.2, tolerance = 0.05)
  expect_equal(CounterfactMe:::.life_expectancy(0, "F", 1) -
               CounterfactMe:::.life_expectancy(0, "F", 6), 1.2, tolerance = 0.05)
})

test_that("gjenstaende levetid faller med alder, men total stiger", {
  # Seleksjon: den som har naadd 80 ventes aa doe senere enn den som er 30.
  ages <- c(0, 30, 60, 80)
  rem <- vapply(ages, function(a) CounterfactMe:::.life_expectancy(a, "F", NA), numeric(1))
  expect_true(all(diff(rem) < 0))
  expect_true(all(diff(ages + rem) > 0))
})

test_that("ugyldige argumenter gir NA, ikke feil", {
  expect_true(is.na(CounterfactMe:::.life_expectancy(NA, "M", 1)))
  expect_true(is.na(CounterfactMe:::.life_expectancy(40, NULL, 1)))
  expect_true(is.na(CounterfactMe:::.life_expectancy(40, "X", 1)))
  # Alder over 99 klippes, ikke feiler.
  expect_false(is.na(CounterfactMe:::.life_expectancy(130, "M", 1)))
  # Ukjent sentralitet faller tilbake paa landstallet.
  expect_equal(CounterfactMe:::.life_expectancy(40, "M", 99),
               CounterfactMe:::.life_expectancy(40, "M", NA))
})

test_that("kjonnsjusteringen gjor kvinner yngre i doedelighetsstigen", {
  # Hele poenget: mor og far leste samme stige med samme alder.
  f <- CounterfactMe:::.mortality_adjusted_age(80, "F")
  m <- CounterfactMe:::.mortality_adjusted_age(80, "M")
  expect_lt(f, m)
  # Justeringen skal vaere beskjeden, ikke dramatisk: gapet er ca 3 aar,
  # halvparten hver vei fra midtpunktet.
  expect_lt(abs(f - m), 5)
  expect_gt(abs(f - m), 0.5)
  expect_equal(CounterfactMe:::.mortality_adjusted_age(NA, "F"), NA)
})

test_that("ego faar forventet levetid, og den er konsistent", {
  set.seed(21)
  for (i in seq_len(20)) {
    x <- counterfact_me(min_age = 20, max_age = 90)
    expect_false(is.null(x$life_expectancy_remaining))
    if (!is.na(x$life_expectancy_remaining)) {
      expect_gt(x$life_expectancy_remaining, 0)
      expect_equal(x$life_expectancy_at,
                   round(x$age + x$life_expectancy_remaining, 1), tolerance = 0.11)
    }
  }
})

test_that("far doer oftere enn mor over mange trekk", {
  # Regresjonstest for selve feilen. Med 300 trekk av ego 60-80 har begge
  # foreldre hoy doedssannsynlighet; kjonnsgapet skal gi far en hoyere andel.
  # Terskelen er 0 fordi retningen er poenget -- storrelsen avhenger av
  # stigen, som er et anslag.
  set.seed(22)
  n <- 300L
  d <- vapply(seq_len(n), function(i) {
    x <- counterfact_me(min_age = 60, max_age = 80)
    c(mor = as.numeric(!is.null(x$mother$death_year) && !is.na(x$mother$death_year)),
      far = as.numeric(!is.null(x$father$death_year) && !is.na(x$father$death_year)))
  }, numeric(2))
  expect_gt(mean(d["far", ]), mean(d["mor", ]))
})

# --- Inntektsgradienten (Kinge et al., JAMA 2019) --------------------------

test_that("gradienten treffer Kinges spenn", {
  g <- CounterfactMe:::.cfm_env$mortality_income_gradient
  expect_false(is.null(g))
  expect_equal(nrow(g), 198L)   # 2 kjonn x 99 persentiler
  for (sex in c("M", "F")) {
    sub <- g[g$gender == sex, ]
    sub <- sub[order(sub$income_percentile), ]
    spenn <- sub$years_offset[99] - sub$years_offset[1]
    forventet <- if (sex == "M") 13.9 else 8.4
    expect_equal(spenn, forventet, tolerance = 0.1,
                 info = paste("ytterpunktspenn", sex))
  }
})

test_that("gradienten er null-sum, saa populasjonssnittet er upaavirket", {
  # Nivaaet kommer fra SSB 14569. Denne fila legger bare til et avvik rundt
  # det, og avviket maa summere til null over fordelingen -- ellers flytter
  # vi hele landets forventede levealder.
  g <- CounterfactMe:::.cfm_env$mortality_income_gradient
  for (sex in c("M", "F")) {
    expect_equal(mean(g$years_offset[g$gender == sex]), 0, tolerance = 0.01)
  }
})

test_that("mennenes gradient er brattere enn kvinnenes", {
  # 13.9 mot 8.4 aar. Det er Kinges hovedfunn og maa overleve konstruksjonen.
  g <- CounterfactMe:::.cfm_env$mortality_income_gradient
  spenn <- function(s) {
    sub <- g[g$gender == s, ]; max(sub$years_offset) - min(sub$years_offset)
  }
  expect_gt(spenn("M"), spenn("F"))
})

test_that("inntektspersentil er monoton og dekker fordelingen", {
  f <- CounterfactMe:::.income_percentile
  p <- vapply(c(50e3, 250e3, 450e3, 650e3, 900e3, 1.5e6, 3e6), f, integer(1))
  expect_true(all(diff(p) >= 0))
  expect_gte(min(p), 1L); expect_lte(max(p), 99L)
  expect_true(is.na(f(NA)))
})

test_that("gradienten avtar med alder", {
  # Kinge gjelder 40+. En som har overlevd til 85 kan ikke baere hele
  # gradienten -- seleksjonen har alt skjedd.
  o40 <- CounterfactMe:::.mortality_income_offset(250e3, "M", 40)
  o85 <- CounterfactMe:::.mortality_income_offset(250e3, "M", 85)
  expect_lt(abs(o85), abs(o40))
  expect_lt(o40, 0)   # lav inntekt -> negativt avvik
  expect_gt(CounterfactMe:::.mortality_income_offset(1.5e6, "M", 40), 0)
})

test_that("forventet levetid varierer med inntekt paa trukne liv", {
  # Hele poenget: en lavtlonnet mann og en hoytlonnet kvinne skal ikke ha
  # samme forventede levealder. Terskelen er lav fordi retningen er poenget.
  set.seed(31)
  le <- function(inc_lo) {
    v <- vapply(seq_len(60), function(i) {
      x <- counterfact_me(gender = if (inc_lo) "M" else "F",
                          min_age = 35, max_age = 55)
      if (is.null(x$life_expectancy_at)) NA_real_ else x$life_expectancy_at
    }, numeric(1))
    mean(v, na.rm = TRUE)
  }
  expect_gt(le(FALSE), le(TRUE))
})

test_that("forventet levetid holder seg innenfor det rimelige", {
  set.seed(32)
  v <- vapply(seq_len(200), function(i) {
    x <- counterfact_me(min_age = 18, max_age = 95)
    if (is.null(x$life_expectancy_at)) NA_real_ else x$life_expectancy_at
  }, numeric(1))
  v <- v[!is.na(v)]
  expect_gt(length(v), 150L)
  # Gulvet paa et halvt aar gjenstaende skal hindre at noen "doer i fortida".
  expect_true(all(v > 18))
  expect_lt(max(v), 120)
})
