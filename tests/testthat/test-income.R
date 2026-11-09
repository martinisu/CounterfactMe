# Personniva inntektsfordeling, SSB 06655 (2024).
#
# Erstattet income_deciles.csv, som var "untraced" og viste seg aa vaere
# 2022-fordelingen med uniform bunn og uniform topp kappet paa 2 mill.

test_that("alle SSB-intervaller kan trekkes, ogsaa det aapne toppintervallet", {
  # Regresjonstest. Forste versjon brukte aggregate() med upper_nok som
  # grupperingsvariabel, og toppintervallet har upper_nok = NA. Formel-
  # aggregering dropper NA i grupperingsvariabel, saa halen forsvant helt:
  # 0 av 40 000 trekk over 2 mill. mot forventet 716.
  set.seed(5)
  n <- 20000L
  d <- sample_income(n)
  tbl <- CounterfactMe:::.cfm_env$income_by_age
  expect_false(is.null(tbl))

  andel_over_2m <- mean(d$nok >= 2e6)
  ssb_andel <- sum(tbl$count[tbl$bracket_code == "73"]) / sum(tbl$count)
  # ssb_andel er ca 0.0179. SE ved n = 20 000 er ca 0.00094, saa et
  # toleransebaand paa 0.006 er over 6 SE -- testen fanger at halen mangler
  # helt, uten aa vaere stoyfolsom.
  expect_gt(andel_over_2m, 0.006)
  expect_lt(abs(andel_over_2m - ssb_andel), 0.006)
})

test_that("hvert intervall trekkes med sin befolkningsvekt", {
  # Intervallene er SSBs egne, ikke desiler -- de er IKKE like sannsynlige.
  set.seed(6)
  n <- 20000L
  d <- sample_income(n)
  tbl <- CounterfactMe:::.cfm_env$income_by_age
  ssb <- tapply(tbl$count, tbl$bracket_label, sum)
  ssb <- ssb / sum(ssb)
  obs <- table(factor(d$bracket, levels = names(ssb))) / n
  # Storste enkeltavvik. SE per andel er hoyst 0.0032 ved n = 20 000, saa
  # 0.02 gir god margin samtidig som en skjev vekting ville blitt fanget.
  expect_lt(max(abs(as.numeric(obs) - as.numeric(ssb))), 0.02)
})

test_that("trukket snitt ligger naer SSBs, korrigert for null-inntekt", {
  set.seed(8)
  d <- sample_income(30000L)
  tbl <- CounterfactMe:::.cfm_env$income_by_age
  ssb_mean <- sum(as.numeric(tbl$sum_mill)) * 1e6 / sum(as.numeric(tbl$count))
  ns <- CounterfactMe:::.cfm_env$income_none_share
  p_none <- sum(ns$none) / sum(ns$population)
  # SSBs snitt teller bare dem MED belop; vaart trekk har null-inntekt med.
  forventet <- ssb_mean * (1 - p_none)
  expect_lt(abs(mean(d$nok) / forventet - 1), 0.06)
})

test_that("halen har alpha over 2, og snittet kan derfor testes", {
  # I motsetning til formueshalen (alpha 1.09-1.55, uendelig varians) har
  # inntektshalen endelig varians, saa et empirisk snitt konvergerer.
  tp <- CounterfactMe:::.cfm_env$income_tail_person
  expect_true(all(tp$pareto_alpha > 1.8))
  expect_true(all(tp$pareto_alpha < 2.5))

  set.seed(9)
  draws <- replicate(20000, CounterfactMe:::.income_tail_draw("45-54"))
  row <- tp[tp$age_band == "45-54", ]
  expect_true(all(draws >= row$pareto_xm[1]))
  expect_lt(abs(mean(draws) / row$mean_nok[1] - 1), 0.10)
})

test_that("andelen uten inntekt faller med alder", {
  ns <- CounterfactMe:::.cfm_env$income_none_share
  expect_equal(nrow(ns), 6L)
  # 3.39 pst ved 17-24 ned til 0.62 pst ved 67+: studenter i den ene enden,
  # pensjon til alle i den andre.
  expect_gt(ns$share_none[ns$age_band == "17-24"],
            ns$share_none[ns$age_band == "67+"])
  expect_true(all(ns$share_none > 0 & ns$share_none < 0.10))
})

test_that("alderskondisjonering endrer fordelingen", {
  set.seed(10)
  ung <- sample_income(4000L, age = 22)
  voksen <- sample_income(4000L, age = 50)
  # 17-24-baandet har langt lavere inntekt enn 45-54.
  expect_lt(stats::median(ung$nok), stats::median(voksen$nok))
})

test_that("part-time position size follows SSB 12542, per occupation", {
  # The stillingsandel used to be runif(0.45, 0.80), capped at 85 pct, for
  # every occupation. Nobody below 45 pct existed. 12542 (4th quarter 2025)
  # puts 29.6 pct of female part-time grunnskolelaerere (2341) below 40 pct
  # and 25.8 pct at 80-99 pct.
  CounterfactMe:::.load_data()
  set.seed(601)
  n <- 4000L
  a <- vapply(seq_len(n), function(i)
    CounterfactMe:::.draw_stillingsandel("2341", "F"), numeric(1))
  expect_length(a, n)
  # SE of a share near 0.3 with n = 4000 is 0.007; 0.03 is over 4 SE.
  expect_lt(abs(mean(a < 0.40) - 0.296), 0.03)
  expect_lt(abs(mean(a >= 0.80) - 0.258), 0.03)
  expect_true(all(a > 0 & a < 1))

  # Occupations differ: part-time helsefagarbeidere (5321) have larger
  # positions than part-time teachers (expected mean 0.62 against 0.56).
  b <- vapply(seq_len(n), function(i)
    CounterfactMe:::.draw_stillingsandel("5321", "A"), numeric(1))
  expect_gt(mean(b), mean(a))

  # Unknown code falls back to the all-occupation distribution, not an error.
  expect_true(is.finite(CounterfactMe:::.draw_stillingsandel("9999", "M")))
})
