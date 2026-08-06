# Contracts for the exported functions that had no tests at all:
# counterfact_me_constrained(), counterfact_parallel_lives(), and the
# narration entry points. These are the package's headline features, so
# a silent regression here is worse than one in an internal helper.

# ---------------------------------------------------------------
# counterfact_me_constrained()
# ---------------------------------------------------------------

test_that("numeric and categorical constraints are honoured", {
  set.seed(301)
  for (i in 1:20) {
    x <- counterfact_me_constrained(list(age = 42, gender = "M"))
    expect_equal(as.integer(x$age), 42L)
    expect_equal(x$gender, "M")
  }
})

test_that("a geographic constraint is honoured", {
  set.seed(302)
  for (i in 1:15) {
    x <- counterfact_me_constrained(list(county = "Oslo", age = 40))
    expect_true(grepl("Oslo", paste(x$county, x$municipality)))
  }
})

test_that("constrained draws still fill the unconstrained dimensions", {
  # The point of the feature is that fixing a few things does not empty
  # the rest.
  set.seed(303)
  draws <- lapply(1:40, function(i)
    counterfact_me_constrained(list(age = 45, gender = "F")))
  for (f in c("name", "municipality", "occupation", "education",
              "household", "mother", "father")) {
    share <- mean(vapply(draws, function(d) {
      v <- d[[f]]; !is.null(v) && !all(is.na(v))
    }, logical(1)))
    expect_gt(share, 0.8, label = sprintf("constrained draw lost '%s'", f))
  }
})

test_that("constrained returns a counterfactme object that prints", {
  set.seed(304)
  x <- counterfact_me_constrained(list(age = 30, gender = "F"))
  expect_s3_class(x, "counterfactme")
  expect_output(print(x), "Ditt kontrafaktiske liv")
})

test_that("an empty constraint list behaves like counterfact_me", {
  set.seed(305)
  x <- counterfact_me_constrained(list())
  expect_s3_class(x, "counterfactme")
  expect_true(!is.null(x$age))
})

# ---------------------------------------------------------------
# counterfact_parallel_lives()
# ---------------------------------------------------------------

test_that("parallel lives returns n lives and varies the chosen dimension", {
  set.seed(306)
  lives <- counterfact_parallel_lives(
    givens = list(age = 45, gender = "M"), vary_dim = "county", n = 5)
  expect_length(lives, 5)
  expect_s3_class(lives, "counterfact_parallel")
  counties <- vapply(lives, function(l) as.character(l$county %||% NA),
                     character(1))
  expect_gt(length(unique(counties)), 1L)
})

test_that("parallel lives hold the givens fixed across all lives", {
  # Varying one dimension is only meaningful if the rest stay put.
  set.seed(307)
  lives <- counterfact_parallel_lives(
    givens = list(age = 38, gender = "F"), vary_dim = "county", n = 5)
  expect_true(all(vapply(lives, function(l) as.integer(l$age), integer(1)) == 38L))
  expect_true(all(vapply(lives, function(l) l$gender, character(1)) == "F"))
})

test_that("explicit vary_values are used", {
  set.seed(308)
  lives <- counterfact_parallel_lives(
    givens = list(age = 30, gender = "F"),
    vary_dim = "background",
    vary_values = c("majority", "first_gen"), n = 2)
  expect_length(lives, 2)
  bg <- vapply(lives, function(l) as.character(l$background %||% NA),
               character(1))
  expect_setequal(bg, c("majority", "first_gen"))
})

test_that("every vary_dim option runs", {
  # match.arg accepts six values; each must actually work.
  set.seed(309)
  for (vd in c("county", "occupation", "education", "background",
               "religion", "party")) {
    lives <- counterfact_parallel_lives(
      givens = list(age = 40, gender = "M"), vary_dim = vd, n = 2)
    expect_length(lives, 2)
    expect_s3_class(lives[[1]], "counterfactme")
  }
})

test_that("the parallel print method runs", {
  set.seed(310)
  lives <- counterfact_parallel_lives(
    givens = list(age = 50, gender = "M"), vary_dim = "county", n = 2)
  expect_output(print(lives), "parallelle liv")
})

# ---------------------------------------------------------------
# narrate_life()
# ---------------------------------------------------------------

test_that("all three narration styles produce clean prose", {
  set.seed(311)
  for (i in 1:25) {
    x <- counterfact_me(min_age = 18)
    for (st in c("biography", "compact", "obituary")) {
      txt <- narrate_life(x, style = st)
      expect_type(as.character(txt), "character")
      expect_gt(nchar(txt), 30L)
      # The artefacts that would betray a template hole
      expect_false(grepl("\\bNA\\b", txt), label = sprintf("NA in %s", st))
      expect_false(grepl("\\bNULL\\b", txt), label = sprintf("NULL in %s", st))
      expect_false(grepl("\\\\u\\{", txt), label = sprintf("escape in %s", st))
      expect_false(grepl("  +", txt), label = sprintf("double space in %s", st))
      expect_false(grepl(" \\.", txt), label = sprintf("space before period in %s", st))
    }
  }
})

test_that("narration works for children too", {
  # Children now have several fields suppressed, so the templates have to
  # cope with absent input rather than emitting "NA" or an empty clause.
  set.seed(312)
  for (i in 1:25) {
    x <- counterfact_me(min_age = 0, max_age = 15)
    txt <- narrate_life(x, style = "compact")
    expect_gt(nchar(txt), 20L)
    expect_false(grepl("\\bNA\\b", txt))
    expect_false(grepl("\\bNULL\\b", txt))
  }
})

test_that("narration is reproducible with a seed", {
  x <- counterfact_me(min_age = 30, max_age = 50)
  expect_equal(narrate_life(x, seed = 99), narrate_life(x, seed = 99))
})

test_that("life_factsheet reports only populated fields", {
  set.seed(313)
  x <- counterfact_me(min_age = 0, max_age = 15)
  fs <- life_factsheet(x)
  expect_type(fs, "character")
  expect_gt(nchar(fs), 20L)
  # A gated field must be omitted, not reported as NA
  expect_false(grepl("\\bNA\\b", fs))
})
