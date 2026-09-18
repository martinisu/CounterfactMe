# Personniva-halen i formuesfordelingen.
#
# Bakgrunn: til 2026-09-18 eskalerte .cond_wealth() toppbracketen med terskler
# fra SSB 10318 -- en HUSHOLDNINGSTABELL -- mens kroppen kom fra 08589 per
# person. Husholdninger samler formue, saa tersklene krevde 2-3x for mye
# formue for at en PERSON skulle havne i toppen. Hele formuesskatten bor der.

test_that("alfaene i datafila reproduserer SSBs personsnitt analytisk", {
  # Dette er kalibreringssjekken, og den gjoeres deterministisk. Snittet i en
  # Pareto med alpha < 2 har UENDELIG varians, saa et empirisk snitt kan ikke
  # testes -- det domineres av maksimumet. Parameteren kan derimot sjekkes
  # eksakt mot formelen den ble loest fra.
  tp <- CounterfactMe:::.cfm_env$wealth_tail_person
  expect_false(is.null(tp))
  expect_equal(nrow(tp), 6L)

  trunc_mean <- function(a, xm, cap) {
    (a * xm / (a - 1) * (1 - (xm / cap)^(a - 1))) / (1 - (xm / cap)^a)
  }
  for (i in seq_len(nrow(tp))) {
    got <- trunc_mean(tp$pareto_alpha[i], tp$pareto_xm[i], tp$pareto_cap[i])
    # Tolerance: alpha er avrundet til 4 desimaler i fila.
    expect_equal(got, tp$mean_nok[i], tolerance = 0.002,
                 info = paste("baand", tp$age_band[i]))
  }
})

test_that("SSB-snittet i fila stemmer med sum delt paa antall", {
  tp <- CounterfactMe:::.cfm_env$wealth_tail_person
  expect_equal(tp$mean_nok, round(tp$sum_netto_mill * 1e6 / tp$n_persons),
               tolerance = 1)
})

test_that("halens MEDIAN treffer Pareto-prediksjonen", {
  # Medianen er velkonditionert der snittet ikke er: xm * 2^(1/alpha).
  tp <- CounterfactMe:::.cfm_env$wealth_tail_person
  row <- tp[tp$age_band == "67+", ]
  a <- row$pareto_alpha[1]; xm <- row$pareto_xm[1]; cap <- row$pareto_cap[1]
  forventet <- xm * 2^(1 / a)

  set.seed(11)
  draws <- vapply(seq_len(3000), function(i) {
    min(xm / stats::runif(1)^(1 / a), cap)
  }, numeric(1))
  # Simulering viste medianen stabil innenfor ca 2 pst ved n = 2000.
  expect_equal(stats::median(draws), forventet, tolerance = 0.05)
})

test_that("formuesklasse bruker personterskler, ikke husholdningsterskler", {
  # Regresjonstesten for selve feilen. En person med 15 mill. er i topp 1 pst
  # AV PERSONER (grensa ligger paa ca 12.8 mill.), men den gamle koden brukte
  # husholdningsgrensa 28.25 mill. og ga "top5".
  f <- CounterfactMe:::.wealth_class_from_nok
  expect_equal(f(15e6), "top1")
  expect_equal(f(5e6), "top5")
  expect_equal(f(70e6), "top01")
  expect_true(is.na(f(3e6)))
  expect_true(is.na(f(NA_real_)))
})

test_that("personterskler ligger klart under husholdningstersklene", {
  # Retningen er kjent og maa holde: husholdninger samler formue.
  wc <- CounterfactMe:::.cfm_env$wealth_class_person
  cut <- function(k) wc$cut_nok[wc$class == k]
  expect_lt(cut("top5"), 12.14e6)    # 10318s Top5-grense
  expect_lt(cut("top1"), 28.25e6)    # 10318s Top1-grense
  # ...men de skal fortsatt vaere ordnet og positive.
  expect_lt(cut("top5"), cut("top1"))
  expect_lt(cut("top1"), cut("top01"))
  expect_gt(cut("top5"), 2e6)
})

test_that("trukket formue i toppbracketen respekterer taket", {
  set.seed(3)
  nws <- vapply(seq_len(300), function(i) {
    w <- CounterfactMe:::.cond_wealth(age = 70, income_nok = 600000)
    as.numeric(w$net_wealth_nok)
  }, numeric(1))
  nws <- nws[!is.na(nws)]
  expect_gt(length(nws), 250L)
  expect_true(all(nws <= 5e10))
})
