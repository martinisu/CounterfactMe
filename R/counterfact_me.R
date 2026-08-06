#' Generate a counterfactual life
#'
#' Draws a random but plausible alternate Norwegian life -- a name, age,
#' municipality, occupation, education, income, household type, and marital
#' status -- all sampled from real population distributions.
#'
#' By default, dimensions are drawn conditionally: age constrains education,
#' education constrains occupation, and so on. Set \code{conditional = FALSE}
#' to draw each dimension independently (v0.1 behaviour).
#'
#' @param dimensions Character vector of dimensions to include. Defaults to
#'   all available (see \code{\link{available_dimensions}}). Pass a subset to
#'   draw only some traits.
#' @param lang Language for labels: \code{"no"} (default, Norwegian) or
#'   \code{"en"}. English is partial by design: occupation titles come from
#'   Statistics Norway's STYRK-98 register, which exists only in Norwegian,
#'   so \code{lang = "en"} translates the frame but leaves occupations,
#'   and a few humorous labels, in Norwegian.
#' @param gender Force a gender for the first name: \code{"M"}, \code{"F"},
#'   or \code{NULL} (default) for random.
#' @param conditional If \code{TRUE} (default), dimensions are sampled with
#'   dependency constraints (age -> education -> occupation -> income, etc.).
#'   If \code{FALSE}, each dimension is drawn independently.
#' @param min_age Minimum age to draw. Default 0 (all ages). Set to e.g. 18 to draw only adults.
#' @param max_age Maximum age to draw. Default 99.
#' @param reject_impossible If \code{TRUE} (default), draws are checked
#'   against \code{\link{find_impossibilities}} and re-drawn when a hard
#'   impossibility (e.g. married under 18) is found. Improbable but possible
#'   combinations are left alone.
#' @param max_reject_attempts Maximum re-draws before returning the cleanest
#'   attempt with post-hoc fixes. Default 20.
#' @return An object of class \code{"counterfactme"} -- a named list of drawn
#'   traits with a pretty-print method.
#' @export
#' @examples
#' counterfact_me()
#' counterfact_me(lang = "en")  # partial: yrker forblir norske
#' counterfact_me(dimensions = c("name", "occupation", "municipality"))
#' counterfact_me(conditional = FALSE)  # independent draws
#' counterfact_me(min_age = 0)          # include children
counterfact_me <- function(dimensions = available_dimensions(),
                           lang = c("no", "en"),
                           gender = NULL,
                           conditional = TRUE,
                           min_age = 0L,
                           max_age = 99L,
                           reject_impossible = TRUE,
                           max_reject_attempts = 20L) {
  lang <- match.arg(lang)

  # Validate dimensions
  valid <- available_dimensions()
  bad <- setdiff(dimensions, valid)
  if (length(bad) > 0) {
    stop("Unknown dimensions: ", paste(bad, collapse = ", "),
         "\nAvailable: ", paste(valid, collapse = ", "),
         call. = FALSE)
  }

  draw_once <- function() {
    if (conditional) {
      .counterfact_conditional(dimensions, lang, gender, min_age, max_age)
    } else {
      .counterfact_independent(dimensions, lang, gender, min_age, max_age)
    }
  }

  if (!isTRUE(reject_impossible)) return(draw_once())

  best <- NULL; best_n <- Inf
  for (i in seq_len(max_reject_attempts)) {
    x <- draw_once()
    probs <- .find_impossibilities(x)
    if (length(probs) == 0L) return(x)
    if (length(probs) < best_n) { best <- x; best_n <- length(probs) }
  }
  # No clean draw — return the cleanest one with post-hoc fixes
  probs <- .find_impossibilities(best)
  .fix_impossibilities(best, probs)
}

# -- Conditional mode (v0.2) --
# Draw order: name -> age -> education -> occupation -> income
#                      age -> marital_status -> household
#             municipality is independent

.counterfact_conditional <- function(dimensions, lang, gender, min_age, max_age) {
  result <- list()
  .load_data()

  # 1. Age (drawn first -- everything else conditions on it)
  age <- sample_age(min_age = min_age, max_age = max_age)
  if ("age" %in% dimensions) {
    result$age <- age
  }

  # 2. Gender (always drawn -- used for yrke x kjonn conditioning)
  if (is.null(gender)) {
    gender_draw <- sample(c("M", "F"), 1)
  } else {
    gender_draw <- toupper(gender)
  }

  # 2c. Immigrant background (drawn early; affects name, geography, income)
  bg <- .cond_immigrant_background(age, lang = lang)
  if ("background" %in% dimensions) {
    result$background <- bg$background
    result$country_background <- bg$country_label
    result$years_in_norway <- bg$years_in_norway
  }

  # 3. Name (cohort-weighted by birth year, modulated by background)
  if ("name" %in% dimensions) {
    birth_year <- .cfm_env$ref_year - age
    bg_name <- .draw_name_by_background(gender_draw, bg$background, bg$name_region)
    if (!is.null(bg_name)) {
      result$name <- bg_name
    } else {
      result$name <- sample_first_name(gender = gender_draw,
                                       birth_year = birth_year)
    }
    result$gender <- gender_draw
  }

  # 4. Municipality (always drawn internally for yrke x bosted conditioning)
  .load_data()
  mun <- .cfm_env$municipalities
  mun_w <- mun$population / sum(mun$population)
  if (!identical(bg$background, "majority")) {
    boost <- .background_oslo_boost(bg$background, bg$name_region)
    is_oslo_area <- mun$county %in% c("Oslo", "Akershus")
    mun_w[is_oslo_area] <- mun_w[is_oslo_area] * boost
    mun_w <- mun_w / sum(mun_w)
  }
  mun_idx <- sample(seq_len(nrow(mun)), 1, prob = mun_w)
  mun_pop <- mun$population[mun_idx]
  county_int <- mun$county[mun_idx]
  mun_name_int <- mun$name[mun_idx]
  sentr <- .cond_sentralitet(mun_name_int, lang = lang)
  if ("sentralitet" %in% dimensions) {
    result$sentralitet <- sentr$score
    result$sentralitet_label <- sentr$label
  }
  if ("municipality" %in% dimensions) {
    result$municipality <- mun$name[mun_idx]
    result$county <- county_int
  }

  # 5. Education (conditional on age, modulated by background)
  edu <- .cond_education(age, gender = gender_draw, lang = lang)
  if (!identical(bg$background, "majority") && !is.null(edu$code) && !is.na(edu$code)) {
    new_code <- .background_edu_shift(edu$code, bg$background, bg$name_region, age)
    if (!identical(new_code, edu$code)) {
      # Re-derive label for the shifted code
      .load_data()
      edu_tbl <- .cfm_env$education
      lcol <- if (identical(lang, "no")) "level_no" else "level"
      ix <- which(edu_tbl$code == new_code)
      if (length(ix) > 0) {
        edu$code <- new_code
        edu$label <- edu_tbl[[lcol]][ix[1]]
      }
    }
  }
  if ("education" %in% dimensions) {
    result$education <- edu$label
  }

  # 5b. Funksjonsnedsettelse — trekkes før yrke (driver uføre-risiko)
  dis <- .cond_disability(age, gender = gender_draw, lang = lang)
  result$has_disability <- dis$has
  if (isTRUE(dis$has)) {
    result$disability_type <- dis$type
    result$disability_severity <- dis$severity
    result$disability <- dis$label
  }

  # 6. Occupation (conditional on age + edu + mun + gender, modulated by background)
  occ <- .cond_occupation(age, edu$code, mun_pop = mun_pop, gender = gender_draw,
                          disabled = dis$has, severity = dis$severity)
  if (!identical(bg$background, "majority") && !is.null(occ$styrk_code) &&
      !is.na(occ$styrk_code) && nzchar(occ$styrk_code)) {
    new_styrk <- .background_occupation_shift(occ$styrk_code, edu$code,
                                              bg$background, bg$name_region)
    new_styrk <- .background_overqualified(edu$code, new_styrk,
                                            bg$background, bg$name_region)
    if (!identical(new_styrk, occ$styrk_code)) {
      # Re-derive occupation label + median_monthly from new STYRK
      .load_data()
      os <- .cfm_env$occupations_salary
      cand <- os[substr(os$code, 1, 2) == new_styrk, , drop = FALSE]
      if (nrow(cand) > 0) {
        pick <- cand[sample.int(nrow(cand), 1), ]
        occ$label <- pick$label
        occ$styrk_code <- pick$code
        occ$median_monthly <- pick$median_nok
      }
    }
  }
  if ("occupation" %in% dimensions) {
    result$occupation <- if (!is.null(occ$schedule) && identical(occ$schedule, "part"))
                          paste0(occ$label, " (deltid)") else occ$label
  }
  # Consistency: uføretrygd implies some funksjonsnedsettelse
  if (!is.null(result$occupation) && !is.na(result$occupation) &&
      grepl("Uf\u00f8retrygd", result$occupation) && !isTRUE(result$has_disability)) {
    result$has_disability <- TRUE
    result$disability_type <- "psykisk eller annet"
    result$disability_severity <- "moderat"
    result$disability <- if (identical(lang, "no")) "psykisk eller annet (moderat)"
                         else "mental or other (moderate)"
  }

  # 6b. Field of study (conditional on age + education + occupation STYRK)
  nus <- .cond_nus_field(age, edu$code, occ$styrk_code, lang = lang)
  if ("field_of_study" %in% dimensions) {
    result$field_of_study <- nus$label
  }
  if ("field_of_study_detail" %in% dimensions) {
    result$field_of_study_detail <- nus$detail_label
    result$field_of_study_code <- nus$detail_code
  }

  # 6c. Parents (conditional on age + education + occupation)
  if ("parents" %in% dimensions) {
    par <- .cond_parents(age, edu$code, occ$styrk_code,
                         gender = gender_draw, lang = lang,
                         background = bg$background,
                         name_region = bg$name_region,
                         country_label = bg$country_label)
    result$mother <- par$mother
    result$father <- par$father
    result$couple_type <- par$couple_type
    result$couple_sex <- par$couple_sex
    pr <- .cond_parents_relationship(age, par$mother, par$father, lang = lang)
    if (!is.null(pr)) {
      result$parents_divorced <- pr$divorced
      result$parents_relationship <- pr$label
    }
    result$parents_capital <- .parents_capital(par$mother, par$father)
    par_for_kids <- par
  } else {
    par_for_kids <- NULL
  }

  # 6d. Siblings (depends on parents)
  if ("siblings" %in% dimensions) {
    mby <- if (!is.null(par_for_kids)) par_for_kids$mother$birth_year else NULL
    sib <- .cond_siblings(age, ego_gender = gender_draw,
                          mother_birth_year = mby,
                          name_region = bg$name_region,
                          background = bg$background, lang = lang)
    result$n_siblings <- sib$count
    result$siblings <- sib$siblings
  }

  # 6e. Grandparents (depends on parents)
  if ("grandparents" %in% dimensions && !is.null(par_for_kids)) {
    mr <- if (!is.null(par_for_kids$mother$origin_region)) par_for_kids$mother$origin_region else "norden"
    fr <- if (!is.null(par_for_kids$father$origin_region)) par_for_kids$father$origin_region else "norden"
    gp <- .cond_grandparents(par_for_kids$mother$birth_year,
                             par_for_kids$father$birth_year,
                             mother_region = mr, father_region = fr,
                             lang = lang)
    result$mormor <- gp$mormor
    result$morfar <- gp$morfar
    result$farmor <- gp$farmor
    result$farfar <- gp$farfar
  }

  # 6f. NEET-flag (16-29, modifies occupation interpretation)
  if ("neet" %in% dimensions) {
    p_neet <- .cond_neet_prob(age, edu$code, bg$background, bg$name_region)
    result$neet <- (p_neet > 0 && stats::runif(1) < p_neet)
    if (isTRUE(result$neet) && "occupation" %in% dimensions) {
      result$occupation <- if (identical(lang, "no"))
                             "Ikke i utdanning eller arbeid" else "Not in education or work"
    }
  }

  # 7. Income (conditional on age + education + occupation, modulated by background)
  inc <- .cond_income(age, edu$code, occ, lang = lang)
  if (!is.null(inc$nok) && !is.na(inc$nok) && inc$nok > 0) {
    bg_factor <- .background_income_factor(bg$background, bg$name_region,
                                            bg$years_in_norway)
    if (!isTRUE(all.equal(bg_factor, 1.0))) {
      inc$nok <- as.integer(round(inc$nok * bg_factor, -3))
    }
  }
  if ("income" %in% dimensions) {
    result$income_bracket <- inc$bracket
    result$income_nok <- inc$nok
    if (!is.null(inc$ukepenger)) result$ukepenger <- inc$ukepenger
  }

  # NEET-override: hvis ego er NEET, sett inntekten til AAP/sosialhjelp-niva
  # NB: NEET kan ha AAP, sosialhjelp, foreldrepenger, eller ingenting
  if (isTRUE(result$neet) && "income" %in% dimensions) {
    # Trekk realistisk ytelse-inntekt
    sub_type <- sample(c("aap", "sosialhjelp", "ingen"), 1,
                       prob = c(0.35, 0.35, 0.30))
    new_nok <- switch(sub_type,
      "aap"        = as.integer(round(stats::runif(1, 180000, 240000), -3)),
      "sosialhjelp"= as.integer(round(stats::runif(1, 130000, 180000), -3)),
      "ingen"      = 0L)
    new_bracket <- switch(sub_type,
      "aap"         = if (identical(lang, "no")) "AAP" else "Work Assessment Allowance",
      "sosialhjelp" = if (identical(lang, "no")) "Sosialhjelp" else "Social assistance",
      "ingen"       = if (identical(lang, "no")) "Ingen inntekt" else "No income")
    result$income_nok <- new_nok
    result$income_bracket <- new_bracket
  }

  # 8. Marital status (conditional on age)
  ms <- .cond_marital_status(age, gender = gender_draw, lang = lang)
  if ("marital_status" %in% dimensions) {
    result$marital_status <- ms$label
  }

  # 9. Household (conditional on age + marital status)
  if ("household" %in% dimensions) {
    result$household <- .cond_household(age, ms$code, gender = gender_draw, lang = lang)
  }

  # 9b. Sexual orientation
  ori <- .cond_orientation(age, gender = gender_draw, lang = lang)
  if ("orientation" %in% dimensions) {
    result$orientation <- ori$label
  }

  # Registrert partnerskap (kode 6) var definisjonsmessig likekjønnet
  # (1993-2009). En hetero som tilfeldig trakk kode 6 konverteres til gift,
  # så orientering og sivilstand ikke motsier hverandre.
  if (!is.null(ms$code) && !is.na(ms$code) && ms$code == 6L &&
      identical(ori$code, "hetero")) {
    ms$code <- 2L
    ms$label <- if (identical(lang, "no")) "Gift" else "Married"
    if ("marital_status" %in% dimensions) result$marital_status <- ms$label
  }

  # 9b2. Partner (conditional on marital status + orientation)
  ptn <- .cond_partner(age, gender_draw, orientation_code = ori$code,
                       marital_code = ms$code, edu_code = edu$code, lang = lang)
  if (!is.null(ptn)) result$partner <- ptn

  # 9c. Religion
  rel <- .cond_religion(age, name_region = bg$name_region,
                       background = bg$background, gender = gender_draw, lang = lang)
  if ("religion" %in% dimensions) {
    result$religion <- rel$label
  }

  # 9d. Party preference
  party <- .cond_party(age, edu_code = edu$code, styrk_code = occ$styrk_code,
                       mun_pop = mun_pop, county = county_int,
                       religion_code = rel$code,
                       background = bg$background, name_region = bg$name_region,
                       income_nok = result$income_nok, gender = gender_draw, lang = lang)
  if ("party" %in% dimensions) {
    result$party <- party$label
  }

  # 10. Housing (conditional on age + income + county + parents_capital)
  if ("housing" %in% dimensions) {
    pc <- result$parents_capital
    if (is.null(pc)) pc <- NA_real_
    inc_nok <- result$income_nok
    if (is.null(inc_nok)) inc_nok <- NA_real_
    h <- .cond_housing(age = age, income_nok = inc_nok,
                       county = county_int, municipality = mun_name_int,
                       parents_capital = pc,
                       gender = gender_draw, lang = lang)
    result$housing_tenure   <- h$tenure
    result$housing_type     <- h$boligtype
    result$housing_area_m2  <- h$area_m2
    result$housing_value_nok <- h$value_nok
    result$housing_debt_nok  <- h$debt_nok
    result$housing_equity_nok <- h$equity_nok
    result$housing_purchase_year <- h$purchase_year
    result$housing_purchase_price_nok <- h$purchase_price_nok
    result$housing_luxury <- h$luxury
    result$has_hytte <- h$has_hytte
    result$hytte_type <- h$hytte_type
    result$hytte_value_nok <- h$hytte_value_nok

    # Konsistens: en boligeier kan ikke samtidig "Bor hos foreldre" eller "Bofellesskap"
    if (isTRUE(h$owner) && !is.null(result$household) && !is.na(result$household)) {
      if (grepl("[Bb]ofellesskap|[Bb]or hos foreldre|[Ll]iving with parents|[Ss]hared",
                result$household)) {
        # Override husholdning til en plausibel eiet-bolig-form
        new_hh <- if (!is.null(ms$code) && ms$code %in% c(2L, 3L)) {
          if (identical(lang, "no")) "Par uten barn" else "Couple without children"
        } else {
          if (identical(lang, "no")) "Enslig uten barn" else "Single without children"
        }
        result$household <- new_hh
      }
    }
  }

  # 11. Wealth (conditional on age + income + housing equity + hytte + parents_capital)
  if ("wealth" %in% dimensions) {
    pc <- result$parents_capital; if (is.null(pc)) pc <- NA_real_
    inc_nok <- result$income_nok; if (is.null(inc_nok)) inc_nok <- NA_real_
    he <- result$housing_equity_nok; if (is.null(he)) he <- 0L
    hv <- result$hytte_value_nok; if (is.null(hv)) hv <- 0L
    # Compute parents_both_dead + n_siblings for inheritance flow
    pbd <- !is.null(result$mother) && !is.null(result$mother$death_year) &&
           !is.na(result$mother$death_year) &&
           !is.null(result$father) && !is.null(result$father$death_year) &&
           !is.na(result$father$death_year)
    nsib <- if (!is.null(result$n_siblings) && !is.na(result$n_siblings))
              as.integer(result$n_siblings) else 0L
    w <- .cond_wealth(age = age, income_nok = inc_nok,
                      housing_equity_nok = he, hytte_value_nok = hv,
                      parents_capital = pc,
                      parents_both_dead = pbd,
                      n_siblings = nsib,
                      gender = gender_draw, lang = lang)
    result$net_wealth_nok       <- w$net_wealth_nok
    result$financial_assets_nok <- w$financial_assets_nok
    result$business_equity_nok  <- w$business_equity_nok
    result$capital_income_nok   <- w$capital_income_nok
    result$inheritance_nok      <- w$inheritance_nok
    result$wealth_class         <- w$wealth_class
  }

  # 11b. Antall barn (ego sin egen reproduksjon)
  if ("children" %in% dimensions) {
    ori_code <- if (exists("ori") && !is.null(ori$code)) ori$code else NA_character_
    nc <- .cond_n_children(age, gender = gender_draw, marital_code = ms$code,
                           orientation_code = ori_code,
                            background = bg$background,
                            name_region = bg$name_region, lang = lang)
    result$n_children <- nc$count
    # Konsistens: husholdning vs antall barn
    if (!is.null(result$household) && !is.na(result$household)) {
      hh <- result$household
      has_kids_label <- grepl("med barn|with children", hh)
      no_kids_label  <- grepl("uten barn|without children|alene$|alone$", hh)
      # Hvis husholdning sier "med barn" men n_children = 0, bump til 1-3 (alder-cap)
      if (has_kids_label && result$n_children == 0L) {
        max_kids <- if (age < 18) 0L
                    else if (age < 22) 1L
                    else if (age < 25) 2L
                    else 3L
        if (max_kids > 0) {
          probs_pick <- c(0.45, 0.40, 0.15)[1:max_kids]
          probs_pick <- probs_pick / sum(probs_pick)
          result$n_children <- as.integer(sample(seq_len(max_kids), 1, prob = probs_pick))
        }
        # NB: hvis max_kids=0 (under 18), beholder vi n_children=0 selv om husholdning
        # sier "med barn" — da er det husholdning-trekningen som er feil, ikke barna
      }
      # Hvis husholdning sier "uten barn" men n_children > 0, sett 0
      if (no_kids_label && result$n_children > 0L) {
        result$n_children <- 0L
      }
    }
  }

  # 11c. Materiell deprivasjon (EU-SILC)
  if ("deprivation" %in% dimensions) {
    nch <- if (!is.null(result$n_children)) result$n_children else 0L
    md <- .cond_material_deprivation(income_nok = result$income_nok, age = age,
                                      background = bg$background,
                                      marital_code = ms$code,
                                      n_children = nch,
                                      net_wealth_nok = result$net_wealth_nok,
                                      lang = lang)
    result$deprivation_count <- md$count
    result$deprivation_label <- md$label
  }

  # 11d. Helse: selvrapportert + kronisk sykdom
  if ("health" %in% dimensions) {
    h_res <- .cond_health(age, edu_code = edu$code,
                          background = bg$background, lang = lang)
    result$self_rated_health <- h_res$self_rated
    result$has_chronic <- h_res$chronic
    result$chronic_type <- h_res$chronic_type
  }

  # 11e. Sosial isolasjon: ensomhet + tillit
  if ("isolation" %in% dimensions) {
    iso <- .cond_social_isolation(age, household = result$household, lang = lang)
    result$loneliness <- iso$loneliness
    result$trust <- iso$trust

    # 11e2. Venner / sosial støtte (betinget på ensomhet)
    sup <- .cond_social_support(age, loneliness = iso$loneliness,
                                household = result$household, lang = lang)
    result$close_friends <- sup$close_friends
    result$has_confidant <- sup$has_confidant
  }

  # 11f. Bourdieu kapitalprofil (FLYTTET OPP — brukes av media + hobbier)
  if ("bourdieu" %in% dimensions) {
    medu <- if (!is.null(result$mother) && !is.null(result$mother$education_code))
              result$mother$education_code else NA_integer_
    fedu <- if (!is.null(result$father) && !is.null(result$father$education_code))
              result$father$education_code else NA_integer_
    party_c <- if (!is.null(result$party) && !is.na(result$party)) result$party else NA_character_
    if (!is.null(party_c) && !is.na(party_c) &&
        grepl("[Ss]temte ikke|[Dd]id not vote|[Ff]yllesyk|[Gg]lemte aa stemme|stemt siden|[Ss]tille flertallet",
              party_c)) {
      party_c <- "STEMTE_IKKE"
    }
    if (age < 18) {
      # Barn: klassebakgrunn fra foreldrene, ikke egen klasseposisjon.
      # Ego-input byttes ut med foreldrenes (hoyeste utdanning + dens yrke,
      # foreldrekapital som formue).
      m_styrk <- if (!is.null(result$mother)) result$mother$styrk_code else NA_character_
      f_styrk <- if (!is.null(result$father)) result$father$styrk_code else NA_character_
      par_edu <- suppressWarnings(max(c(medu, fedu), na.rm = TRUE))
      if (!is.finite(par_edu)) par_edu <- NA_integer_
      par_styrk <- if (!is.na(medu) && (is.na(fedu) || medu >= fedu)) m_styrk else f_styrk
      if (is.null(par_styrk) || is.na(par_styrk) || !nzchar(par_styrk)) {
        par_styrk <- if (!is.null(m_styrk) && !is.na(m_styrk) && nzchar(m_styrk)) m_styrk else f_styrk
      }
      pc <- result$parents_capital
      if (is.null(pc) || is.na(pc)) pc <- NA_real_
      bp <- .cond_bourdieu(net_wealth_nok = pc,
                           capital_income_nok = NA_integer_,
                           housing_equity_nok = NA_integer_,
                           edu_code = par_edu,
                           mother_edu_code = NA_integer_,
                           father_edu_code = NA_integer_,
                           styrk_code = par_styrk,
                           household = result$household,
                           n_siblings = result$n_siblings,
                           party_code = NA_character_,
                           lang = lang)
      result$bourdieu_origin <- TRUE
    } else {
      bp <- .cond_bourdieu(net_wealth_nok = result$net_wealth_nok,
                           capital_income_nok = result$capital_income_nok,
                           housing_equity_nok = result$housing_equity_nok,
                           edu_code = edu$code,
                           mother_edu_code = medu,
                           father_edu_code = fedu,
                           styrk_code = occ$styrk_code,
                           household = result$household,
                           n_siblings = result$n_siblings,
                           party_code = party_c,
                           lang = lang)
    }
    result$bourdieu_okonomisk <- bp$okonomisk
    result$bourdieu_kulturell <- bp$kulturell
    result$bourdieu_sosial    <- bp$sosial
    result$bourdieu_klasse    <- bp$klasse
  }

  # 11g. Kosthold (avhengig av alder + edu)
  if ("diet" %in% dimensions) {
    result$diet <- .cond_diet(age, edu_code = edu$code, gender = gender_draw, lang = lang)$diet
  }

  # 11h. Alkohol (avhengig av diet)
  if ("alcohol" %in% dimensions) {
    result$alcohol_pattern <- .cond_alcohol(age, edu_code = edu$code,
                                             gender = gender_draw,
                                             diet_label = result$diet,
                                             lang = lang)
  }

  # 11i. Søvn (avhengig av n_children + yrke + kronisk helse)
  if ("sleep" %in% dimensions) {
    has_chronic <- isTRUE(result$has_chronic)
    result$sleep_hours <- .cond_sleep(age,
                                       n_children = result$n_children,
                                       styrk_code = occ$styrk_code,
                                       has_chronic = has_chronic,
                                       lang = lang)
  }

  # 11j. Mediadiet (avhengig av parti + Bourdieu)
  if ("media" %in% dimensions) {
    # Hent ut parti-kode (samme logikk som Bourdieu)
    party_c <- if (!is.null(result$party) && !is.na(result$party)) result$party else NA_character_
    if (!is.null(party_c) && !is.na(party_c)) {
      if (grepl("[Ss]temte ikke|[Dd]id not vote|[Ff]yllesyk|[Gg]lemte aa stemme|stemt siden|[Ss]tille flertallet", party_c)) party_c <- "STEMTE_IKKE"
      else if (grepl("Senterpart", party_c)) party_c <- "SP"
      else if (grepl("Sosialist", party_c)) party_c <- "SV"
      else if (grepl("Arbeiderpartiet|^Sier .sosialdem|Stemmer .* AP|Vokste opp paa Furuset|Furuset|sosialdemokrati|stemte AP", party_c)) party_c <- "AP"
      else if (grepl("Hoeyre|H\u{00f8}yre|Civita|Tesla|^Stemmer H ", party_c)) party_c <- "H"
      else if (grepl("Fremskrittspart|Sylvi|bilavgifter|Frp", party_c)) party_c <- "FRP"
      else if (grepl("Venstre|Trine|jeg-er-groenn", party_c)) party_c <- "V"
      else if (grepl("Kristelig|KrF|Hareide|bedehus", party_c)) party_c <- "KRF"
      else if (grepl("Roedt|R\u{00f8}dt|Moxnes|RU paa 90-tallet|kapitalismen", party_c)) party_c <- "R"
      else if (grepl("Miljopartiet|Milj\u{00f8}partiet|MDG|Lan Marie|diesel-Audi|sykler", party_c)) party_c <- "MDG"
    }
    md <- .cond_media(age, edu_code = edu$code,
                       sentralitet = result$sentralitet,
                       party_code = party_c,
                       bourdieu_klasse = result$bourdieu_klasse,
                       lang = lang)
    result$media_paper <- md$paper
    result$media_tv_hours <- md$tv_hours
    result$media_podcast <- md$podcast
    result$media_social <- md$social_media
  }

  # 11k. Hobbier (avhengig av alkohol + parti)
  if ("hobbies" %in% dimensions) {
    owns_h <- !is.null(result$housing_type) && !is.na(result$housing_type) &&
              grepl("[Ee]nebolig|[Ss]maahus|[Dd]etached|[Ss]mall house",
                    result$housing_type)
    # Parti-kode (samme detektering som over)
    party_c <- if (!is.null(result$party) && !is.na(result$party)) result$party else NA_character_
    if (!is.null(party_c) && !is.na(party_c)) {
      if (grepl("[Ss]temte ikke|[Dd]id not vote|[Ff]yllesyk", party_c)) party_c <- "STEMTE_IKKE"
      else if (grepl("Senterpart", party_c)) party_c <- "SP"
      else if (grepl("Sosialist", party_c)) party_c <- "SV"
      else if (grepl("Arbeiderpart|sosialdemokrati", party_c)) party_c <- "AP"
      else if (grepl("Hoeyre|H\u{00f8}yre|Civita", party_c)) party_c <- "H"
      else if (grepl("Fremskritt|Frp|Sylvi", party_c)) party_c <- "FRP"
      else if (grepl("Venstre", party_c)) party_c <- "V"
      else if (grepl("Kristelig|KrF|Hareide", party_c)) party_c <- "KRF"
      else if (grepl("Roedt|R\u{00f8}dt|Moxnes", party_c)) party_c <- "R"
      else if (grepl("MDG|Miljopart|Milj\u{00f8}part", party_c)) party_c <- "MDG"
    }
    hb <- .cond_hobbies(age, gender = gender_draw,
                        sentralitet = result$sentralitet,
                        edu_code = edu$code,
                        owns_house = owns_h,
                        background = bg$background,
                        alcohol_label = result$alcohol_pattern,
                        party_code = party_c,
                        lang = lang)
    result$hobbies <- hb$hobbies
  }

  # 11l. Kriminalitet (utsatthet + trygghet + mindre lovbrudd)
  if ("crime" %in% dimensions) {
    cr <- .cond_crime(age, gender = gender_draw,
                      sentralitet = result$sentralitet, lang = lang)
    result$crime_victimizations <- cr$victimizations
    result$crime_safety <- cr$safety_feeling
    result$minor_offence <- cr$minor_offence
  }

  result$lang <- lang
  result$conditional <- TRUE
  class(result) <- "counterfactme"
  result
}

# -- Independent mode (v0.1 behaviour) --

.counterfact_independent <- function(dimensions, lang, gender, min_age, max_age) {
  result <- list()

  if ("name" %in% dimensions) {
    if (is.null(gender)) {
      gender_draw <- sample(c("M", "F"), 1)
    } else {
      gender_draw <- toupper(gender)
    }
    result$name <- sample_first_name(gender = gender_draw)
    result$gender <- gender_draw
  }

  if ("age" %in% dimensions) {
    result$age <- sample_age(min_age = min_age, max_age = max_age)
  }

  if ("municipality" %in% dimensions) {
    .load_data()
    mun <- .cfm_env$municipalities
    idx <- sample(seq_len(nrow(mun)), 1,
                  prob = mun$population / sum(mun$population))
    result$municipality <- mun$name[idx]
    result$county <- mun$county[idx]
  }

  if ("occupation" %in% dimensions) {
    result$occupation <- sample_occupation()
  }

  if ("education" %in% dimensions) {
    result$education <- sample_education(lang = lang)
  }

  if ("field_of_study" %in% dimensions || "field_of_study_detail" %in% dimensions) {
    .load_data()
    nf <- .cfm_env$nus_fields
    idx <- sample.int(nrow(nf), 1)
    broad_code <- nf$code[idx]
    if ("field_of_study" %in% dimensions) {
      result$field_of_study <- if (identical(lang, "no")) nf$label_no[idx] else nf$label_en[idx]
    }
    if ("field_of_study_detail" %in% dimensions) {
      indep_age <- if (!is.null(result$age)) result$age else NA_integer_
      det <- .cond_nus_detail(broad_code, "default", lang = lang, age = indep_age)
      result$field_of_study_detail <- det$label
      result$field_of_study_code <- det$code
    }
  }

  if ("income" %in% dimensions) {
    inc <- sample_income()
    result$income_bracket <- inc$bracket
    result$income_nok <- inc$nok
  }

  if ("household" %in% dimensions) {
    .load_data()
    hh <- .cfm_env$households
    col <- if (identical(lang, "no")) "type_no" else "type"
    chosen <- sample(seq_len(nrow(hh)), 1, prob = hh$population_share)
    label <- hh[[col]][chosen]
    g <- if (!is.null(result$gender)) result$gender else NULL
    result$household <- .gender_household_label(label, hh$code[chosen], g, lang)
  }

  if ("marital_status" %in% dimensions) {
    result$marital_status <- sample_marital_status(lang = lang)
  }

  if ("parents" %in% dimensions) {
    # Independent mode: draw both parents with no inheritance, ego_age=30 prior
    ego_age <- if (!is.null(result$age)) result$age else 30L
    par <- .cond_parents(ego_age, NA_integer_, NA_character_, lang = lang)
    result$mother <- par$mother
    result$father <- par$father
    result$couple_type <- par$couple_type
    result$couple_sex <- par$couple_sex
    pr <- .cond_parents_relationship(ego_age, par$mother, par$father, lang = lang)
    if (!is.null(pr)) {
      result$parents_divorced <- pr$divorced
      result$parents_relationship <- pr$label
    }
    result$parents_capital <- .parents_capital(par$mother, par$father)
  }

  if ("housing" %in% dimensions) {
    .load_data()
    mun <- .cfm_env$municipalities
    idx <- sample(seq_len(nrow(mun)), 1,
                  prob = mun$population / sum(mun$population))
    county_int <- mun$county[idx]
    mun_name_int <- mun$name[idx]
    pc <- if (!is.null(result$parents_capital)) result$parents_capital else NA_real_
    inc_nok <- if (!is.null(result$income_nok)) result$income_nok else NA_real_
    a <- if (!is.null(result$age)) result$age else 35L
    g <- if (!is.null(result$gender)) result$gender else NULL
    h <- .cond_housing(age = a, income_nok = inc_nok,
                       county = county_int, municipality = mun_name_int,
                       parents_capital = pc,
                       gender = g, lang = lang)
    result$housing_tenure   <- h$tenure
    result$housing_type     <- h$boligtype
    result$housing_area_m2  <- h$area_m2
    result$housing_value_nok <- h$value_nok
    result$housing_debt_nok  <- h$debt_nok
    result$housing_equity_nok <- h$equity_nok
    result$housing_purchase_year <- h$purchase_year
    result$housing_purchase_price_nok <- h$purchase_price_nok
    result$housing_luxury <- h$luxury
    result$has_hytte <- h$has_hytte
    result$hytte_type <- h$hytte_type
    result$hytte_value_nok <- h$hytte_value_nok
  }

  if ("wealth" %in% dimensions) {
    pc <- if (!is.null(result$parents_capital)) result$parents_capital else NA_real_
    inc_nok <- if (!is.null(result$income_nok)) result$income_nok else NA_real_
    he <- if (!is.null(result$housing_equity_nok)) result$housing_equity_nok else 0L
    hv <- if (!is.null(result$hytte_value_nok)) result$hytte_value_nok else 0L
    a <- if (!is.null(result$age)) result$age else 35L
    g <- if (!is.null(result$gender)) result$gender else NULL
    w <- .cond_wealth(age = a, income_nok = inc_nok,
                      housing_equity_nok = he, hytte_value_nok = hv,
                      parents_capital = pc, gender = g, lang = lang)
    result$net_wealth_nok       <- w$net_wealth_nok
    result$financial_assets_nok <- w$financial_assets_nok
    result$business_equity_nok  <- w$business_equity_nok
    result$capital_income_nok   <- w$capital_income_nok
    result$wealth_class         <- w$wealth_class
  }

  result$lang <- lang
  result$conditional <- FALSE
  class(result) <- "counterfactme"
  result
}
