# -- Conditional sampling logic --
#
# Dependency chain:
#   age -> education -> occupation -> income
#   age -> marital_status
#   age + marital_status -> household
#
# HARD RULES (never violated):
#   - Under 16: cannot be married, divorced, separated, widowed, or cohabiting
#   - Under 16: cannot be a parent (no "couple with children" as the person)
#   - Under 16: no income
#   - Under 18: cannot be married (Norwegian law: 18 is minimum)
#   - Under 6: no education at all
#   - Household for children = living with parents (their parents' household)

# Pensioner hobby labels (used as "occupation" for retirees 67+)
.PENSJONIST_LABELS <- c(
  "Hyttepusser", "Turlag-general", "Kaffeslabberas-koordinator",
  "Strikkedronning", "Strikkekonge", "Kryssordmester",
  "Barnebarn-logistiker", "Seniorsv\u{00f8}mmer", "Bridgeekspert",
  "Hageentusiast", "Frimerkesamler", "Modelljernbane-ingeni\u{00f8}r",
  "Sudokumester", "Turg\u{00e5}er", "Fjordfisker", "Vedhogger",
  "Boksirkel-leder", "Kirkekorsanger", "Eldretrim-instrukt\u{00f8}r",
  "Baketeoretiker", "Kolonihage-stolthet", "Hyttebokforfatter",
  "Lotto-veteran", "Kaffekos-ambassad\u{00f8}r"
)

# -- Age -> Education --
#
# For age >= 16, weights come from SSB table 08921 (education × 9 age bands,
# nationally) loaded into .cfm_env$education_by_age. For age < 16, SSB does
# not publish education at all, so we use hand-crafted rules covering school
# progression (barneskole/ungdomsskole). The package keeps a finer 10-level
# taxonomy than SSB's 5 levels; .ssb_to_pkg_edu() bridges them.

.ssb_to_pkg_edu <- function() {
  # Map each SSB level (01, 02a, 11, 03a, 04a, 09a) to package codes 0..9
  # with probability weights. The small leaks from "01" to codes 0 and 1
  # capture "no completed education" and "primary only" which SSB bundles
  # into "Grunnskolenivå" for reporting. PhDs (code 8) are ~3 % of master-
  # level completers in SSB's "Universitets- og høgskolenivå, lang".
  list(
    "01"  = c("0" = 0.02, "1" = 0.05, "2" = 0.93),
    "02a" = c("3" = 0.10, "4" = 0.90),
    "11"  = c("5" = 1.00),
    "03a" = c("6" = 1.00),
    "04a" = c("7" = 0.97, "8" = 0.03),
    "09a" = c("9" = 1.00)
  )
}

.edu_band <- function(age) {
  if (age <= 19)      "16-19 \u{00e5}r"
  else if (age <= 24) "20-24 \u{00e5}r"
  else if (age <= 29) "25-29 \u{00e5}r"
  else if (age <= 39) "30-39 \u{00e5}r"
  else if (age <= 49) "40-49 \u{00e5}r"
  else if (age <= 59) "50-59 \u{00e5}r"
  else if (age <= 66) "60-66 \u{00e5}r"
  else                "67 \u{00e5}r eller eldre"
}

.cond_education <- function(age, gender = NULL, lang = "en") {
  .load_data()
  edu <- .cfm_env$education
  col <- if (lang == "no") "level_no" else "level"
  w <- numeric(nrow(edu))

  if (age < 16) {
    # Under school-leaving age: SSB silent, use school-progression rules.
    if (age <= 5) {
      w[edu$code == 0] <- 1
    } else if (age <= 12) {
      w[edu$code == 1] <- 0.95
      w[edu$code == 0] <- 0.05
    } else {  # 13-15
      w[edu$code == 2] <- 0.90
      w[edu$code == 1] <- 0.08
      w[edu$code == 0] <- 0.02
    }
  } else {
    # 16+: draw from SSB age-band shares, mapped to package codes.
    band <- .edu_band(age)
    tbl  <- .cfm_env$education_by_age
    tbl  <- tbl[tbl$age_band == band, ]
    if (nrow(tbl) == 0) {
      # fallback to overall shares if band somehow missing
      w <- edu$population_share
    } else {
      map <- .ssb_to_pkg_edu()
      for (i in seq_len(nrow(tbl))) {
        ssb_code <- tbl$level_code[i]
        share    <- tbl$share_pct[i] / 100
        distr    <- map[[ssb_code]]
        if (is.null(distr) || share == 0) next
        for (pkg in names(distr)) {
          ix <- edu$code == as.integer(pkg)
          w[ix] <- w[ix] + share * distr[[pkg]]
        }
      }
    }
  }

  # Kjønn × kohort: yngre kvinner har høyere høyere-utdanning enn menn,
  # eldre kvinner lavere (utdanningsrevolusjonen, crossover ca. 1960-kohort).
  if (!is.null(gender) && !is.na(gender) && age >= 19) {
    by <- .cfm_env$ref_year - age
    tert <- if (by >= 1985) 1.35 else if (by >= 1965) 1.18
            else if (by >= 1950) 1.00 else 0.80
    if (!identical(toupper(gender), "F")) tert <- 1 / tert
    hi <- edu$code %in% c(6L, 7L, 8L)
    lo <- edu$code %in% c(0L, 1L, 2L)
    w[hi] <- w[hi] * tert
    w[lo] <- w[lo] / tert
  }

  if (sum(w) == 0) w[edu$code == 0] <- 1
  w <- w / sum(w)

  edu_code <- sample(edu$code, 1, prob = w)
  edu_row  <- edu[edu$code == edu_code, ]

  list(code = edu_code, label = edu_row[[col]])
}

# -- Age + Education -> Occupation --
#
# Returns a list:
#   $label           Norwegian occupation name (string)
#   $styrk_code      4-digit STYRK-08 code (NA for non-worker branches)
#   $median_monthly  SSB median monthly salary in NOK (NA if no SSB match)
#   $kind            "kid" | "teen" | "student" | "pensioner" | "worker" | "unemployed"
#
# For working-age adults, draws from SSB 11418 (`occupations_salary` table,
# schedule == "all"), weighted by headcount and by an education -> STYRK major
# digit mapping so that a person with only compulsory schooling does not end
# up as a brain surgeon.

.edu_to_styrk_majors <- function(edu_code) {
  # Keys are first digit of STYRK-08:
  #   0 Militaere,   1 Ledere,              2 Akademiske yrker,
  #   3 Høyskoleyrker, 4 Kontor,            5 Salg/service,
  #   6 Bonder/fiskere, 7 Handverkere,       8 Prosess/maskin,
  #   9 Renhold/hjelp
  # Weights are multiplicative with headcount. They encode realism, not
  # population shares (SSB would rarely publish those joint distributions).
  if (edu_code <= 2) {
    # Grunnskole or less: manual, service, basic office
    c("0" = 0.5, "1" = 0.1, "2" = 0.01, "3" = 0.2, "4" = 0.8,
      "5" = 1.0, "6" = 1.0, "7" = 1.0, "8" = 1.0, "9" = 1.0)
  } else if (edu_code <= 4) {
    # Videregaende: skilled trades, service, office, some management
    c("0" = 0.8, "1" = 0.3, "2" = 0.05, "3" = 0.6, "4" = 1.0,
      "5" = 1.0, "6" = 0.9, "7" = 1.0, "8" = 0.9, "9" = 0.6)
  } else if (edu_code == 5) {
    # Fagskole: technical specialists
    c("0" = 0.6, "1" = 0.5, "2" = 0.2, "3" = 1.0, "4" = 0.8,
      "5" = 0.6, "6" = 0.4, "7" = 1.0, "8" = 0.9, "9" = 0.2)
  } else if (edu_code == 6) {
    # Bachelor: hoyskoleyrker, some academics, management
    # Hard cap: STYRK 6/7/8 = 0 (bachelor jobber ikke som bonde/handverker/operator)
    c("0" = 0.3, "1" = 1.0, "2" = 0.6, "3" = 1.0, "4" = 0.6,
      "5" = 0.3, "6" = 0.0, "7" = 0.0, "8" = 0.0, "9" = 0.05)
  } else if (edu_code == 7) {
    # Master: akademiske, management
    # Hard cap: STYRK 6/7/8/9 = 0 (master jobber ikke som manuelt arbeid)
    c("0" = 0.2, "1" = 1.0, "2" = 1.0, "3" = 0.5, "4" = 0.3,
      "5" = 0.1, "6" = 0.0, "7" = 0.0, "8" = 0.0, "9" = 0.0)
  } else if (edu_code >= 8) {
    # PhD+: heavily concentrated in akademiske + ledere
    c("0" = 0.05, "1" = 0.6, "2" = 1.0, "3" = 0.2, "4" = 0.1,
      "5" = 0.02, "6" = 0.0, "7" = 0.0, "8" = 0.0, "9" = 0.0)
  } else {
    # fallback: uniform
    setNames(rep(1, 10), as.character(0:9))
  }
}

.disability_prob <- function(age) {
  # From SSB table 11715 (2024): andel av befolkningen med uføretrygd.
  if (age < 18 || age > 67) return(0)
  if (age <= 24) 0.018
  else if (age <= 34) 0.045
  else if (age <= 44) 0.069
  else if (age <= 54) 0.123
  else if (age <= 61) 0.200
  else                0.268
}


.aap_prob <- function(age) {
  # AAP: ca. 3-4 % av 18-67, skewed younger than ufore (peak 30-50)
  if (age < 18 || age > 67) return(0)
  if (age <= 24) 0.015
  else if (age <= 34) 0.035
  else if (age <= 44) 0.045
  else if (age <= 54) 0.040
  else if (age <= 61) 0.030
  else                0.020
}

.dagpenger_prob <- function(age) {
  # Dagpenger (arbeidsledig): ca. 2 % av yrkesaktive, relativt jevnt fordelt
  if (age < 18 || age > 67) return(0)
  if (age <= 24) 0.025
  else if (age <= 34) 0.020
  else if (age <= 54) 0.018
  else                0.015
}

.sosialhjelp_prob <- function(age, edu_code) {
  # Sosialhjelp: < 1 %, overrepresentert blant unge uten utdanning
  if (age < 18 || age > 67) return(0)
  base <- if (age <= 24) 0.012 else if (age <= 34) 0.008 else 0.004
  if (edu_code <= 2) base * 2 else base
}

.bosted_styrk_mult <- function(mun_pop) {
  # Multiplier per STYRK major (0-9) based on municipality population size.
  # Rural areas have more farmers/fishers/craftsmen; cities more office/academic.
  if (mun_pop < 5000) {
    # Rural
    c("0" = 1.5, "1" = 0.4, "2" = 0.5, "3" = 0.7, "4" = 0.3,
      "5" = 0.8, "6" = 5.0, "7" = 2.0, "8" = 2.0, "9" = 0.6)
  } else if (mun_pop < 20000) {
    # Small town
    c("0" = 1.2, "1" = 0.7, "2" = 0.8, "3" = 0.9, "4" = 0.7,
      "5" = 1.0, "6" = 2.0, "7" = 1.5, "8" = 1.5, "9" = 0.8)
  } else if (mun_pop < 50000) {
    # Medium
    c("0" = 1.0, "1" = 1.0, "2" = 1.0, "3" = 1.0, "4" = 1.0,
      "5" = 1.0, "6" = 0.5, "7" = 1.0, "8" = 1.0, "9" = 1.0)
  } else {
    # City (50k+)
    c("0" = 0.5, "1" = 1.5, "2" = 1.5, "3" = 1.3, "4" = 1.5,
      "5" = 1.0, "6" = 0.001, "7" = 0.5, "8" = 0.4, "9" = 1.2)
  }
}

# Occupational structure over time.
#
# .cond_occupation() weights STYRK major groups by today's headcounts,
# which is right for the ego and wrong for a parent who worked in 1955.
# Norway had roughly a quarter of employment in primary industry in 1950
# and has about 2 % now; professional occupations went the other way.
# Without this a parent born in 1905 drew from the 2020s labour market.
#
# Multipliers are relative to the present, keyed on STYRK-08 major group
# and applied at the midpoint of the parent's working life. They are
# approximations of the sectoral shift, not a series -- enough to stop
# the anachronism, not enough to read off as history.
.styrk_cohort_mult <- function(work_year) {
  base <- setNames(rep(1, 10), as.character(0:9))
  if (is.null(work_year) || is.na(work_year) || work_year >= 2005) return(base)
  # anchors: 1955 and 1980; interpolate, flat before 1955
  m1955 <- c("0"=1.2,"1"=0.7,"2"=0.25,"3"=0.5,"4"=0.8,
             "5"=0.6,"6"=8.0,"7"=2.5,"8"=2.0,"9"=2.0)
  m1980 <- c("0"=1.1,"1"=0.85,"2"=0.55,"3"=0.75,"4"=1.1,
             "5"=0.8,"6"=3.0,"7"=1.6,"8"=1.5,"9"=1.4)
  y <- max(1955, min(2005, work_year))
  if (y <= 1980) {
    f <- (y - 1955) / 25
    out <- m1955 * (1 - f) + m1980 * f
  } else {
    f <- (y - 1980) / 25
    out <- m1980 * (1 - f) + base * f
  }
  setNames(as.numeric(out), names(base))
}

.cond_occupation <- function(age, edu_code, mun_pop = NULL, gender = NULL,
                             disabled = NULL, severity = NULL,
                             work_year = NA_integer_) {
  .load_data()

  .wrap <- function(label, kind) {
    list(label = label, styrk_code = NA_character_,
         median_monthly = NA_integer_, kind = kind)
  }

  # Toddlers (0-5): play-based "occupations"
  if (age <= 5) {
    return(.wrap(sample(c(
      "Saftsommelier", "Sandkassesjef", "Lekeklosspilot",
      "Dinosaurekspert", "Puslespillmester", "Tegnefilmkritiker",
      "Kosedyrsjef", "Trehjulsyklist", "Dukkehusarkitekt",
      "Vannpytt-inspekt\u{00f8}r"
    ), 1), "kid"))
  }
  if (age >= 6 && age <= 12) {
    return(.wrap(sample(c(
      "Minecraft-arkitekt", "Fotballentusiast", "Pok\u{00e9}monsamler",
      "Slime-produsent", "Trampolineakrobat", "Lego-ingeni\u{00f8}r",
      "Kiosk-strategist", "Klassens klovn", "Sykkelstuntmann",
      "Friminutts-dirigent", "Leksehjelp-unnviker", "SFO-veteran"
    ), 1), "kid"))
  }
  if (age >= 13 && age <= 15) {
    return(.wrap(sample(c(
      "TikTok-koreograf", "Discord-moderator", "Fortnite-strateg",
      "Snap-streak-vokter", "Minecraft-veteran", "Russebuss-spekulant",
      "Skatepark-stammegjest", "KRLE-filosof", "Kantine-kosmopolitt",
      "Spotify-playlistekurator", "Ungdomsklubb-stamgjest"
    ), 1), "teen"))
  }
  if (age >= 16 && age <= 18) {
    return(.wrap(sample(c(
      "Russeknuter", "Kassa-veteran", "Kino-billettselger",
      "Barista-in-training", "Gymnasiast", "L\u{00e6}rling",
      "Festival-frivillig", "Eksamensangst-kjemper",
      "Kollektivtransport-filosof", "Skolekor-stjerne",
      "Bensinstasjon-nattvakt"
    ), 1), "teen"))
  }

  # Young adults in higher education -- studenter, ofte med deltidsjobb
  # ~60 % av 19-24-aaringer med edu>=5 er studenter.
  # Av disse: ~65 % har en deltidsjobb i tillegg (SSB studielevekaarsundersokelse).
  if (age >= 19 && age <= 24 && edu_code >= 5) {
    if (stats::runif(1) < 0.6) {
      if (stats::runif(1) < 0.65) {
        # Student + deltidsjobb -- kombinert label, en streng.
        part <- .draw_student_deltid(gender = gender)
        if (!is.null(part) && !is.null(part$label) && nzchar(part$label)) {
          combined <- sprintf("Student (deltid: %s)", tolower(part$label))
          return(list(
            label = combined,
            styrk_code = if (!is.null(part$styrk_code)) part$styrk_code else NA_character_,
            median_monthly = if (!is.null(part$median_monthly)) part$median_monthly else NA_integer_,
            kind = "student",
            schedule = NA_character_,    # combined label, do NOT add "(deltid)" suffix
            student_part_label = part$label,
            student_part_styrk = if (!is.null(part$styrk_code)) part$styrk_code else NA_character_
          ))
        }
      }
      return(.wrap("Student", "student"))
    }
  }

  # Uforetrygd (SSB 11715). Driven by funksjonsnedsettelse when known,
  # so the causal arrow runs disability -> uføretrygd (not the reverse).
  if (age >= 18 && age <= 67) {
    p_ufore <- if (!is.null(disabled)) {
      if (isTRUE(disabled))
        switch(severity %||% "moderat",
               mild = 0.10, moderat = 0.35, alvorlig = 0.70, 0.35)
      else 0.01
    } else .disability_prob(age)
    if (stats::runif(1) < p_ufore) {
      return(.wrap("Uf\u{00f8}retrygdet", "disabled"))
    }
  }

  # Elderly: mostly retired (filter gendered labels by ego gender)
  pensioner_pool <- .PENSJONIST_LABELS
  if (!is.null(gender) && identical(gender, "M")) {
    pensioner_pool <- setdiff(pensioner_pool, c("Strikkedronning"))
  } else if (!is.null(gender) && identical(gender, "F")) {
    pensioner_pool <- setdiff(pensioner_pool, c("Strikkekonge"))
  }
  if (age >= 72 && stats::runif(1) < 0.90) {
    return(.wrap(sample(pensioner_pool, 1), "pensioner"))
  }
  if (age >= 67 && age < 72 && stats::runif(1) < 0.55) {
    return(.wrap(sample(pensioner_pool, 1), "pensioner"))
  }

  # -- Working-age adults: SSB-driven draw --
  os <- .cfm_env$occupations_salary
  tbl <- os[os$schedule == "all" & !is.na(os$code) & nzchar(os$code), ]
  tbl <- tbl[tbl$code != "0000" & tbl$headcount > 0, ]

  majors <- .edu_to_styrk_majors(edu_code)
  first_digit <- substr(tbl$code, 1, 1)
  major_w <- unname(majors[first_digit])
  major_w[is.na(major_w)] <- 0
  # Bosted-conditioning: rural/urban multiplier per STYRK major
  bosted_w <- if (!is.null(mun_pop)) {
    bm <- .bosted_styrk_mult(mun_pop)
    unname(bm[first_digit])
  } else {
    rep(1, length(first_digit))
  }
  bosted_w[is.na(bosted_w)] <- 1
  # Gender-conditioning: use SSB female_share per yrke
  gender_w <- rep(1, nrow(tbl))
  if (!is.null(gender)) {
    gen <- .cfm_env$occupations_gender
    fs <- gen$female_share[match(tbl$code, gen$code)]
    fs[is.na(fs)] <- 0.5  # unknown -> neutral
    gender_w <- if (toupper(gender) == "F") fs else (1 - fs)
    # Avoid zero-weights (allow rare crossovers)
    gender_w <- pmax(gender_w, 0.02)
  }
  # Era-conditioning: today's headcounts describe today's labour market.
  cohort_w <- if (!is.na(work_year)) {
    cm <- .styrk_cohort_mult(work_year)
    v <- unname(cm[first_digit]); v[is.na(v)] <- 1; v
  } else rep(1, length(first_digit))
  w <- major_w * bosted_w * gender_w * cohort_w * tbl$headcount
  if (sum(w) == 0) w <- tbl$headcount

  idx  <- sample.int(nrow(tbl), 1, prob = w)
  code <- tbl$code[idx]

  # Schedule: use within-yrke headcount ratio deltid/alle as P(deltid)
  row_full <- os[os$code == code & os$schedule == "full", ]
  row_part <- os[os$code == code & os$schedule == "part", ]
  hc_all   <- tbl$headcount[idx]
  hc_part  <- if (nrow(row_part) == 1) row_part$headcount else 0L
  part_share <- if (hc_all > 0) min(max(hc_part / hc_all, 0), 0.95) else 0

  schedule <- if (stats::runif(1) < part_share) "part" else "full"
  monthly <- if (schedule == "part" && nrow(row_part) == 1 &&
                 !is.na(row_part$median_nok) && row_part$median_nok > 0) {
    as.integer(row_part$median_nok)
  } else if (schedule == "full" && nrow(row_full) == 1 &&
             !is.na(row_full$median_nok) && row_full$median_nok > 0) {
    as.integer(row_full$median_nok)
  } else {
    as.integer(tbl$median_nok[idx])
  }

  # Trekk en spesifikk 7-sifret yrkesbetegnelse innenfor STYRK-08 group
  styrk08_label <- tbl$label[idx]
  detail_label <- .draw_detail_yrke(code, styrk08_label, gender = gender)

  worker <- list(
    label = detail_label,
    styrk_code = code,
    styrk_label = styrk08_label,
    median_monthly = monthly,
    schedule = schedule,
    kind = "worker"
  )

  # -- Roll for AAP / dagpenger / sosialhjelp (19-67, not students) --
  if (age >= 19 && age <= 67) {
    r <- stats::runif(1)
    aap_p  <- .aap_prob(age)
    dag_p  <- .dagpenger_prob(age)
    sosi_p <- .sosialhjelp_prob(age, edu_code)
    if (r < aap_p) {
      return(list(
        label = paste0("AAP (tidl. ", worker$label, ")"),
        styrk_code = worker$styrk_code,
        median_monthly = worker$median_monthly,
        schedule = worker$schedule,
        kind = "aap",
        previous = worker$label
      ))
    } else if (r < aap_p + dag_p) {
      return(list(
        label = paste0("Dagpenger (tidl. ", worker$label, ")"),
        styrk_code = worker$styrk_code,
        median_monthly = worker$median_monthly,
        schedule = worker$schedule,
        kind = "dagpenger",
        previous = worker$label
      ))
    } else if (r < aap_p + dag_p + sosi_p) {
      return(list(
        label = "Sosialhjelp",
        styrk_code = NA_character_,
        median_monthly = NA_integer_,
        schedule = NA_character_,
        kind = "sosialhjelp",
        previous = worker$label
      ))
    }
  }

  worker
}

# -- Education + Occupation -> Income --
#
# For workers, we anchor annual income on the chosen occupation's SSB median
# monthly salary (x 12) with log-normal noise. Age shifts the scale (young
# workers below median, mid-career above). Part-time incidence is not yet
# modelled here (see TODO: "Full yrke x alder x utdanning x deltid").

.cond_income <- function(age, edu_code, occupation, lang = "en") {
  .load_data()
  inc <- .cfm_env$income

  # Accept either the new list form or a bare string (for callers that still
  # pass sample_occupation() output, e.g. sample-many paths).
  occ_label <- if (is.list(occupation)) occupation$label else occupation
  occ_kind  <- if (is.list(occupation)) occupation$kind  else NA_character_
  occ_median_monthly <- if (is.list(occupation)) occupation$median_monthly else NA_integer_

  # Children 0-15: no real income, but fun "ukepenger" labels
  if (age <= 15) {
    is_no <- identical(lang, "no")
    bracket_lbl <- if (is_no) "Ukepenger" else "Pocket money"
    if (age <= 2) {
      return(list(bracket = bracket_lbl, nok = 0L,
                  ukepenger = if (is_no) "Har null peiling p\u00e5 penger"
                              else "Has no clue about money"))
    }
    labels_3_5 <- c(
      "F\u00e5r 10 kr i uka fra bestefar",
      "Har spart 47 kr i sparegrisen",
      "Tror en tikrone er en formue",
      "F\u00e5r l\u00f8nn i is og godteri",
      "Eier tre mynter og en blank knapp"
    )
    labels_3_5_en <- c(
      "Gets 10 kr a week from grandpa",
      "Has saved 47 kr in the piggy bank",
      "Thinks a ten-krone coin is a fortune",
      "Paid in ice cream and sweets",
      "Owns three coins and a shiny button"
    )
    labels_6_9 <- c(
      "50 kr/uke fra mormor",
      "Klipper plenen for 200 kr",
      "Har 387 kr p\u00e5 sparekontoen",
      "Tjener p\u00e5 loppemarked i garasjen",
      "Negativ inntekt \u2013 skylder pappa 80 kr",
      "Selger tegninger for 5 kr stykket",
      "Har funnet 23 kr p\u00e5 bakken hittil i \u00e5r"
    )
    labels_6_9_en <- c(
      "50 kr/week from grandma",
      "Mows the lawn for 200 kr",
      "Has 387 kr in a savings account",
      "Earns money at the garage flea market",
      "Negative income \u2013 owes dad 80 kr",
      "Sells drawings for 5 kr apiece",
      "Has found 23 kr on the ground so far this year"
    )
    labels_10_12 <- c(
      "100 kr/uke for \u00e5 rydde rommet",
      "Vasker bilen for 150 kr",
      "Har 2 847 kr i sparegris",
      "Driver ulovlig godteributikk p\u00e5 skolen",
      "Selger Vipps-tjenester til klassekamerater",
      "Tjener 50 kr p\u00e5 \u00e5 passe naboens katt",
      "Har investert i Pokemon-kort (diversifisert portefolje)"
    )
    labels_10_12_en <- c(
      "100 kr/week for cleaning their room",
      "Washes the car for 150 kr",
      "Has 2 847 kr in the piggy bank",
      "Runs an illegal candy shop at school",
      "Sells Vipps services to classmates",
      "Earns 50 kr looking after the neighbour's cat",
      "Has invested in Pokemon cards (diversified portfolio)"
    )
    labels_13_15 <- c(
      "Sommerjobb: 3 500 kr for hele sommeren",
      "Avisrute: 800 kr/mnd",
      "Har 14 000 kr p\u00e5 BSU (foreldrenes ide)",
      "Tjener p\u00e5 Vipps fra onkel",
      "Negativ inntekt \u2013 skylder kompisen 200 kr",
      "Babysitter-inntekt: 100 kr/kveld",
      "Har l\u00e5nt 500 kr av mamma som aldri blir betalt tilbake",
      "Selger brukte spill p\u00e5 Finn.no",
      "Kj\u00f8per lavt, selger h\u00f8yt i skoleg\u00e5rden"
    )
    labels_13_15_en <- c(
      "Summer job: 3 500 kr for the whole summer",
      "Paper route: 800 kr/month",
      "Has 14 000 kr in a youth savings account (parents' idea)",
      "Earns money via Vipps from an uncle",
      "Negative income \u2013 owes a friend 200 kr",
      "Babysitting income: 100 kr/night",
      "Borrowed 500 kr from mum, never to be repaid",
      "Sells used games on Finn.no",
      "Buys low, sells high in the schoolyard"
    )
    lbl <- if (age <= 5) { if (is_no) sample(labels_3_5, 1) else sample(labels_3_5_en, 1) }
           else if (age <= 9) { if (is_no) sample(labels_6_9, 1) else sample(labels_6_9_en, 1) }
           else if (age <= 12) { if (is_no) sample(labels_10_12, 1) else sample(labels_10_12_en, 1) }
           else { if (is_no) sample(labels_13_15, 1) else sample(labels_13_15_en, 1) }
    return(list(bracket = bracket_lbl, nok = 0L, ukepenger = lbl))
  }

  # Teens 16-18: mostly no income. Those who earn something are weekend/
  # helgejobb-tier: typically 10-80 kNOK, hard-capped at ~120 kNOK (ingen
  # 16-åring tjener 700k som kino-billettselger).
  if (age >= 16 && age <= 18) {
    if (stats::runif(1) < 0.7) {
      return(list(bracket = if (identical(lang, "no")) "Ikke yrkesaktiv"
                            else "Not employed", nok = 0L))
    }
    # Log-normal around ~40 kNOK (med = exp(10.6) ~ 40k), sd = 0.6 in log space
    nok <- as.integer(round(exp(stats::rnorm(1, log(40000), 0.6)), -3))
    nok <- max(5000L, min(nok, 120000L))
    return(list(bracket = inc$label[inc$decile == 1], nok = nok))
  }

  # Helper: pick the bracket whose [lower, upper) contains nok
  .bracket_of <- function(nok) {
    hit <- which(inc$lower_nok <= nok & nok < inc$upper_nok)
    if (length(hit) == 0) hit <- nrow(inc)  # top open bracket
    inc$label[hit[1]]
  }

  # Studenter: studielån + helgejobb, aldri toppinntekt. Hard cap 300k.
  if (identical(occ_kind, "student") || identical(occ_label, "STUDENT") || identical(occ_label, "Student")) {
    w <- c(3, 4, 2, 0.5, 0.1, 0, 0, 0, 0, 0)
    w <- w / sum(w)
    idx <- sample(seq_len(nrow(inc)), 1, prob = w)
    upper <- min(inc$upper_nok[idx], 300000)
    lower <- min(inc$lower_nok[idx], upper - 1)
    nok <- as.integer(round(stats::runif(1, lower, upper), -3))
    return(list(bracket = inc$label[idx], nok = nok))
  }

  # Disabled on uforetrygd: lower-middle deciles, floor around minsteytelse
  if (identical(occ_kind, "disabled") || identical(occ_label, "Uf\u{00f8}retrygdet")) {
    w <- c(1.0, 2.5, 3.0, 2.0, 1.0, 0.4, 0.1, 0.05, 0.02, 0.01)
    w <- w / sum(w)
    idx <- sample(seq_len(nrow(inc)), 1, prob = w)
    upper <- min(inc$upper_nok[idx], 2000000)
    nok <- as.integer(round(stats::runif(1, inc$lower_nok[idx], upper), -3))
    return(list(bracket = inc$label[idx], nok = nok))
  }

  # AAP: 66 % av tidligere lonn (basert pa yrkets median), cappet ved 6G
  if (identical(occ_kind, "aap")) {
    if (!is.null(occ_median_monthly) && !is.na(occ_median_monthly) &&
        occ_median_monthly > 0) {
      annual_basis <- occ_median_monthly * 12 * 0.66
      annual_basis <- min(annual_basis, 711720)  # 6G cap (2024)
      noise <- exp(stats::rnorm(1, 0, 0.15))
      nok <- as.integer(round(annual_basis * noise, -3))
    } else {
      nok <- as.integer(round(stats::runif(1, 200000, 350000), -3))
    }
    return(list(bracket = .bracket_of(nok), nok = nok))
  }

  # Dagpenger: 62.4 % av tidligere lonn, cappet ved 6G
  if (identical(occ_kind, "dagpenger")) {
    if (!is.null(occ_median_monthly) && !is.na(occ_median_monthly) &&
        occ_median_monthly > 0) {
      annual_basis <- occ_median_monthly * 12 * 0.624
      annual_basis <- min(annual_basis, 711720)
      noise <- exp(stats::rnorm(1, 0, 0.15))
      nok <- as.integer(round(annual_basis * noise, -3))
    } else {
      nok <- as.integer(round(stats::runif(1, 180000, 320000), -3))
    }
    return(list(bracket = .bracket_of(nok), nok = nok))
  }

  # Sosialhjelp: flat lavt nivaa, ikke knyttet til tidligere yrke
  if (identical(occ_kind, "sosialhjelp")) {
    nok <- as.integer(round(stats::runif(1, 100000, 180000), -3))
    return(list(bracket = .bracket_of(nok), nok = nok))
  }

  # Pensioners: modest pension, independent of yrke
  if (identical(occ_kind, "pensioner") || occ_label %in% .PENSJONIST_LABELS) {
    w <- c(0.5, 1.5, 2, 2.5, 2, 1.5, 0.5, 0.3, 0.1, 0.05)
    w <- w / sum(w)
    idx <- sample(seq_len(nrow(inc)), 1, prob = w)
    upper <- min(inc$upper_nok[idx], 2000000)
    nok <- as.integer(round(stats::runif(1, inc$lower_nok[idx], upper), -3))
    return(list(bracket = inc$label[idx], nok = nok))
  }

  # Working adults with an SSB-matched occupation: anchor on median salary.
  # For part-time, shrink the annualization (SSB Månedslønn is FTE-equivalent;
  # part-time workers draw their month-lonn on fewer hours).
  occ_schedule <- if (is.list(occupation) && !is.null(occupation$schedule))
                    occupation$schedule else "full"
  if (!is.null(occ_median_monthly) && !is.na(occ_median_monthly) &&
      occ_median_monthly > 0) {
    annual_median <- occ_median_monthly * 12
    age_mult <- if (age <= 24) stats::runif(1, 0.55, 0.85)
                else if (age <= 29) stats::runif(1, 0.80, 1.00)
                else if (age <= 39) stats::runif(1, 0.95, 1.15)
                else if (age <= 54) stats::runif(1, 1.00, 1.20)
                else if (age <= 66) stats::runif(1, 0.95, 1.15)
                else                stats::runif(1, 0.70, 1.00)
    schedule_mult <- if (identical(occ_schedule, "part"))
                       stats::runif(1, 0.45, 0.80) else 1
    # Smaler noise for deltid (mindre rom for over-betaling enn fast-tids)
    noise_sd <- if (identical(occ_schedule, "part")) 0.15 else 0.25
    noise <- exp(stats::rnorm(1, 0, noise_sd))
    nok_raw <- annual_median * age_mult * schedule_mult * noise
    # Cap deltids-inntekt: kan ikke overstige 85% av FTE-aldersjustert median
    if (identical(occ_schedule, "part")) {
      cap <- annual_median * age_mult * 0.85
      if (nok_raw > cap) nok_raw <- cap
    }
    nok <- as.integer(round(nok_raw, -3))
    if (nok < 0) nok <- 0L
    return(list(bracket = .bracket_of(nok), nok = nok))
  }

  # Fallback: no occupation salary available -> old decile-template logic
  if (edu_code <= 2) {
    w <- c(1.5, 2, 2, 1.5, 1.2, 0.8, 0.5, 0.3, 0.15, 0.05)
  } else if (edu_code <= 4) {
    w <- c(0.5, 1, 1.5, 2, 2, 1.5, 1, 0.5, 0.3, 0.1)
  } else if (edu_code <= 6) {
    w <- c(0.2, 0.5, 0.8, 1.2, 1.5, 2, 2, 1.5, 0.8, 0.3)
  } else if (edu_code == 7) {
    w <- c(0.1, 0.2, 0.4, 0.6, 1, 1.5, 2, 2.5, 1.8, 0.8)
  } else {
    w <- c(0.05, 0.1, 0.2, 0.3, 0.5, 1, 1.5, 2.5, 2.5, 1.5)
  }
  if (age <= 29) {
    shift <- c(1.5, 1.3, 1.1, 1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4)
    w <- w * shift
  }
  w <- w / sum(w)
  idx <- sample(seq_len(nrow(inc)), 1, prob = w)
  upper <- min(inc$upper_nok[idx], 2000000)
  nok <- as.integer(round(stats::runif(1, inc$lower_nok[idx], upper), -3))
  list(bracket = inc$label[idx], nok = nok)
}

# -- Age -> Marital status --
# HARD RULES:
#   Under 18: always Ugift/Unmarried (Norwegian marriage age is 18)
#   Under 16: cannot cohabit either
#   18-19: almost always unmarried, tiny chance cohabiting

.cond_marital_status <- function(age, gender = NULL, lang = "en") {
  .load_data()
  ms <- .cfm_env$marital
  col <- if (lang == "no") "status_no" else "status"

  # HARD RULE: children are always unmarried
  if (age < 18) {
    return(list(code = 1L, label = ms[[col]][ms$code == 1]))
  }

  # Order in CSV: 1=Ugift, 2=Gift, 3=Enke, 4=Skilt, 5=Separert,
  #               6=Reg.partner, 7=Samboer

  if (age <= 19) {
    # 18-19: juridisk gift mulig, men ikke enke/skilt/separert
    w <- c(0.95, 0.001, 0, 0, 0, 0, 0.05)
  } else if (age <= 22) {
    # Enke/skilt er juridisk mulig men urealistisk under 23
    w <- c(0.80, 0.03, 0, 0.001, 0.001, 0.001, 0.17)
  } else if (age <= 24) {
    w <- c(0.75, 0.05, 0.001, 0.005, 0.002, 0.002, 0.19)
  } else if (age <= 34) {
    w <- c(0.35, 0.25, 0.002, 0.03, 0.01, 0.005, 0.35)
  } else if (age <= 49) {
    w <- c(0.15, 0.40, 0.005, 0.08, 0.02, 0.005, 0.25)
  } else if (age <= 64) {
    w <- c(0.08, 0.50, 0.03, 0.15, 0.02, 0.005, 0.12)
  } else if (age <= 74) {
    w <- c(0.05, 0.45, 0.10, 0.12, 0.01, 0.005, 0.08)
  } else {
    # 75+: more widowed
    w <- c(0.05, 0.35, 0.25, 0.10, 0.01, 0.005, 0.03)
  }

  # Enkestand er sterkt kjønnet ved høy alder: kvinner lever lenger og
  # gifter seg med eldre menn. Indeks 3 = Enke/enkemann (CSV-koderekkefølge).
  if (!is.null(gender) && !is.na(gender) && age >= 60) {
    wf <- if (age >= 75) 2.4 else 1.8
    if (identical(toupper(gender), "F")) w[3] <- w[3] * wf else w[3] <- w[3] / wf
  }

  w <- w / sum(w)
  idx <- sample(seq_len(nrow(ms)), 1, prob = w)
  result <- list(code = ms$code[idx], label = ms[[col]][idx])

  # ~1.5 % sjanse for en humoristisk sivilstand-joker (aldersbetinget).
  # Kun for IKKE-parforhold-koder (1 ugift, 3 enke, 4 skilt, 5 separert) —
  # ellers ville jokeren motsi en partner som genereres på kode 2/6/7.
  if (!(result$code %in% c(2L, 6L, 7L)) && stats::runif(1) < 0.015) {
    jokes_0_5 <- c(
      "Gift med bamsen sin",
      "Forlova med en badeand",
      "I et seri\u{00f8}st forhold med t\u{00e5}teflaska"
    )
    jokes_6_12 <- c(
      "Gift med bestevennen sin i barnehagen (skilte seg etter lunsj)",
      "Forlova med en Minecraft-landsby",
      "I et komplisert forhold med leksene"
    )
    jokes_13_18 <- c(
      "Det er komplisert (Snapchat-status)",
      "Gift med sofaen",
      "Singel og stolt (if\u{00f8}lge TikTok)"
    )
    jokes_adult <- c(
      "\u{00d8}nsker \u{00e5} bo hos sin mor og ha sin mor i fred",
      "Har forlatt sin mann for en traktor",
      "Gift med jobben (bokstavelig talt)",
      "I et langdistanseforhold med kontoen sin",
      "Separert fra virkeligheten"
    )
    jokes_elder <- c(
      "Gift med kaffen sin siden 1973",
      "Enke etter en sv\u{00e6}rt dyr b\u{00e5}t",
      "I et \u{00e5}pent forhold med hytteboka",
      "Forlovet med kryssordene i Aftenposten"
    )
    joke <- if (age <= 5) sample(jokes_0_5, 1)
            else if (age <= 12) sample(jokes_6_12, 1)
            else if (age <= 18) sample(jokes_13_18, 1)
            else if (age <= 66) sample(jokes_adult, 1)
            else sample(jokes_elder, 1)
    result$label <- joke
  }

  result
}

# -- Age + Marital status -> Household --
# HARD RULES:
#   Under 18: always "Bor hos foreldre" (living with parents)
#
# SOFT RULES (strong multipliers):
#   - "Living with parents" (code 11): zeroed for age 30+
#   - Married/cohabiting: couple categories boosted, living alone near-zero
#   - Divorced/widowed/separated: living alone boosted, couple categories near-zero
#   - Unmarried (ugift, no partner): living alone boosted, couple reduced

.cond_household <- function(age, marital_code, gender = NULL, lang = "en") {
  .load_data()
  hh <- .cfm_env$households
  col <- if (lang == "no") "type_no" else "type"

  # HARD RULE: children live with parents, full stop
  if (age < 18) {
    return(.gender_household_label(hh[[col]][hh$code == 11], 11L, gender, lang))
  }

  # -- Age-biology hard caps for parent/child categories --
  # Minimum believable parent age at child's birth = 16.
  # These hold REGARDLESS of the age-band weighting below.
  ban <- rep(FALSE, nrow(hh))
  # Code 3: par med barn 0-5      -> person 18-50
  if (age < 18 || age > 50) ban[hh$code == 3] <- TRUE
  # Code 4: par med barn 6-17     -> person 22-65
  if (age < 22 || age > 65) ban[hh$code == 4] <- TRUE
  # Code 5: par med voksne barn   -> person 36+
  if (age < 36)             ban[hh$code == 5] <- TRUE
  # Code 6: enslig m/ barn 0-17   -> person 18-60
  if (age < 18 || age > 60) ban[hh$code == 6] <- TRUE
  # Code 7: enslig m/ voksne barn -> person 36+
  if (age < 36)             ban[hh$code == 7] <- TRUE
  # Code 11: bor hos foreldre     -> person 18-45 (foreldre i live)
  if (age > 45)             ban[hh$code == 11] <- TRUE

  # -- Base weights by age band --

  if (age <= 24) {
    w <- rep(0.01, nrow(hh))
    w[hh$code == 11] <- 0.35  # Living with parents
    w[hh$code == 1]  <- 0.25  # Living alone
    w[hh$code == 9]  <- 0.15  # Shared housing
    w[hh$code == 2]  <- 0.10  # Couple without children
    w[hh$code == 3]  <- 0.05  # Couple with small children
    w[hh$code == 6]  <- 0.02  # Single parent
  } else if (age <= 39) {
    w <- rep(0.01, nrow(hh))
    w[hh$code == 3]  <- 0.25  # Couple with children 0-5
    w[hh$code == 4]  <- 0.20  # Couple with children 6-17
    w[hh$code == 2]  <- 0.18  # Couple without children
    w[hh$code == 1]  <- 0.15  # Living alone
    w[hh$code == 6]  <- 0.08  # Single parent
    w[hh$code == 9]  <- 0.03  # Shared housing
  } else if (age <= 59) {
    w <- rep(0.01, nrow(hh))
    w[hh$code == 4]  <- 0.25  # Couple with children 6-17
    w[hh$code == 2]  <- 0.22  # Couple without children
    w[hh$code == 5]  <- 0.15  # Couple with adult children
    w[hh$code == 1]  <- 0.15  # Living alone
    w[hh$code == 7]  <- 0.06  # Single parent adult children
    w[hh$code == 6]  <- 0.04  # Single parent
  } else if (age <= 69) {
    w <- rep(0.01, nrow(hh))
    w[hh$code == 2]  <- 0.35  # Couple without children
    w[hh$code == 1]  <- 0.25  # Living alone
    w[hh$code == 5]  <- 0.15  # Couple with adult children
    w[hh$code == 7]  <- 0.05  # Single parent adult children
  } else {
    # 70+
    w <- rep(0.01, nrow(hh))
    w[hh$code == 1]  <- 0.40  # Living alone
    w[hh$code == 2]  <- 0.35  # Couple without children
    w[hh$code == 5]  <- 0.08  # Couple with adult children
    w[hh$code == 10] <- 0.05  # Other (institution etc.)
  }

  # -- SOFT RULE: "Living with parents" decreases with age --
  # SSB: ~3-4% of 30-34 live at home, <1% for 35+
  if (age >= 40) {
    w[hh$code == 11] <- 0.001  # Near-zero but not impossible
  } else if (age >= 35) {
    w[hh$code == 11] <- 0.005  # Very rare
  } else if (age >= 30) {
    w[hh$code == 11] <- 0.02   # ~3-4% range
  } else if (age >= 25) {
    w[hh$code == 11] <- w[hh$code == 11] * 0.15
  }

  # -- SOFT RULE: Shared housing rare after 30 --
  if (age >= 35) {
    w[hh$code == 9] <- 0
  } else if (age >= 30) {
    w[hh$code == 9] <- w[hh$code == 9] * 0.1
  }

  # -- Marital status adjustments --

  if (marital_code == 2 || marital_code == 6) {
    # MARRIED or REGISTERED PARTNER: almost always couple household
    w[hh$code == 1]  <- w[hh$code == 1]  * 0.02  # Alone: near-zero
    w[hh$code == 9]  <- 0                         # Shared housing: no
    w[hh$code == 11] <- 0                         # With parents: no
    w[hh$code == 6]  <- 0                         # Single parent: no
    w[hh$code == 7]  <- 0                         # Single parent adult: no
    # Boost couple categories
    w[hh$code == 2]  <- w[hh$code == 2]  * 3.0
    w[hh$code == 3]  <- w[hh$code == 3]  * 3.0
    w[hh$code == 4]  <- w[hh$code == 4]  * 3.0
    w[hh$code == 5]  <- w[hh$code == 5]  * 3.0
  } else if (marital_code == 7) {
    # COHABITING: similar to married, couple categories dominate
    w[hh$code == 1]  <- w[hh$code == 1]  * 0.05  # Alone: near-zero
    w[hh$code == 9]  <- 0                         # Shared housing: no
    w[hh$code == 11] <- w[hh$code == 11] * 0.1   # With parents: unlikely
    w[hh$code == 6]  <- 0                         # Single parent: no
    w[hh$code == 7]  <- 0                         # Single parent adult: no
    w[hh$code == 2]  <- w[hh$code == 2]  * 2.5
    w[hh$code == 3]  <- w[hh$code == 3]  * 2.5
    w[hh$code == 4]  <- w[hh$code == 4]  * 2.5
  } else if (marital_code %in% c(3, 4, 5)) {
    # WIDOWED, DIVORCED, or SEPARATED: mostly alone or single parent
    w[hh$code == 1]  <- w[hh$code == 1]  * 3.0   # Alone: very likely
    w[hh$code == 6]  <- w[hh$code == 6]  * 2.0   # Single parent: boosted
    w[hh$code == 7]  <- w[hh$code == 7]  * 2.0   # Single parent adult: boosted
    # Couple categories: near-zero (no longer in couple)
    w[hh$code == 2]  <- w[hh$code == 2]  * 0.02
    w[hh$code == 3]  <- w[hh$code == 3]  * 0.02
    w[hh$code == 4]  <- w[hh$code == 4]  * 0.02
    w[hh$code == 5]  <- w[hh$code == 5]  * 0.02
    w[hh$code == 9]  <- 0                         # Shared housing: no
    w[hh$code == 11] <- 0                         # With parents: no
  } else if (marital_code == 1) {
    # UNMARRIED (ugift): mostly alone, shared, or with parents
    w[hh$code == 1]  <- w[hh$code == 1]  * 2.5   # Alone: boosted
    w[hh$code == 9]  <- w[hh$code == 9]  * 2.0   # Shared: boosted
    w[hh$code == 6]  <- w[hh$code == 6]  * 1.5   # Single parent: possible
    # Couple categories: reduced (ugift can cohabit, but less common)
    w[hh$code == 2]  <- w[hh$code == 2]  * 0.15
    w[hh$code == 3]  <- w[hh$code == 3]  * 0.15
    w[hh$code == 4]  <- w[hh$code == 4]  * 0.15
    w[hh$code == 5]  <- w[hh$code == 5]  * 0.15
  }

  # Apply age-biology hard caps
  w[ban] <- 0
  if (sum(w) == 0) {
    # Last-resort fallback: living alone
    return(.gender_household_label(hh[[col]][hh$code == 1], 1L, gender, lang))
  }
  w <- w / sum(w)
  chosen_code <- sample(hh$code, 1, prob = w)
  label <- hh[[col]][hh$code == chosen_code]
  .gender_household_label(label, chosen_code, gender, lang)
}


# -- Age + Education + Occupation -> NUS field of study --
#
# NUS2000 1-digit broad field (0-9) and 3-digit detailed field (~177 codes)
# from the official SSB classification (Barrabes & Ostli 2017, NOT2017-02).
#
# Two-stage draw:
#   1. Broad field: weighted by STYRK 2-digit sub-major group
#      (inst/extdata/nus_by_styrk.csv).
#   2. Detailed field: within the drawn broad field, weighted by a hand-
#      calibrated STYRK 2-digit -> NUS 3-digit mapping
#      (inst/extdata/nus_detail_by_styrk.csv, top 3-5 codes per STYRK).
#      Fallback when STYRK has no plausible detailed code in the drawn
#      broad field: uniform draw across all detailed codes in that broad
#      field (from inst/extdata/nus_detailed.csv).
#
# HARD RULES:
#   Under 16: returns NA/NA (no field of study yet).
#   Grunnskole only (edu_code <= 2): broad = 9 "Uoppgitt", detail = 999.
#   VGS (edu_code 3-4): boost weight on broad 0 (allmenne) and 5 (yrkesfag).
#   PhD (edu_code >= 8): broad 9 "Uoppgitt" is zeroed.

.cond_nus_field <- function(age, edu_code, styrk_code, lang = "en") {
  .load_data()

  nf <- .cfm_env$nus_fields

  # Children under 16 have no field of study.
  if (is.null(age) || is.na(age) || age < 16) {
    return(list(code = NA_integer_, label = NA_character_,
                detail_code = NA_character_, detail_label = NA_character_))
  }

  # Grunnskole eller mindre: ingen fagfelt/studieretning gir mening
  # (ingen utdanning, barneskole, ungdomsskole). Returner NA -- print.R
  # hopper over NA-felt automatisk.
  if (is.null(edu_code) || is.na(edu_code) || edu_code <= 2) {
    return(list(code = NA_integer_, label = NA_character_,
                detail_code = NA_character_, detail_label = NA_character_))
  }

  # Look up STYRK 2-digit group. Fall back to "default" if unknown.
  mp <- .cfm_env$nus_by_styrk
  sk2 <- if (is.null(styrk_code) || is.na(styrk_code) || !nzchar(styrk_code)) {
    "default"
  } else {
    substr(as.character(styrk_code), 1, 2)
  }
  row <- mp[mp$styrk2 == sk2, , drop = FALSE]
  if (nrow(row) == 0) {
    row <- mp[mp$styrk2 == "default", , drop = FALSE]
  }

  # --- Stage 1: broad field ---
  weights <- if (nrow(row) == 0) rep(1, 10) else as.numeric(row[1, paste0("w", 0:9)])
  weights[is.na(weights)] <- 0
  if (edu_code <= 4) {
    weights[1] <- weights[1] + 0.5   # w0 allmenne
    weights[6] <- weights[6] + 0.3   # w5 yrkesfag/tekniske
  }
  if (edu_code == 5) {
    weights[6] <- weights[6] + 0.2
  }
  if (edu_code >= 8) {
    weights[10] <- 0  # w9 uoppgitt
  }
  if (sum(weights) == 0) weights <- rep(1, 10)
  broad_idx <- sample.int(10, 1, prob = weights) - 1L   # 0-9

  row_b <- nf[nf$code == broad_idx, , drop = FALSE]
  broad_label <- if (identical(lang, "no")) row_b$label_no[1] else row_b$label_en[1]
  if (is.null(broad_label) || length(broad_label) == 0 || is.na(broad_label)) {
    broad_label <- "Unspecified"
  }

  # --- Stage 2: detailed field within broad ---
  detail <- .cond_nus_detail(broad_idx, sk2, lang = lang, age = age)
  # Skip detail if identical to broad (avoid "Allmenne fag == Allmenne fag" duplikat)
  if (!is.null(detail$label) && !is.na(detail$label) &&
      identical(as.character(detail$label), as.character(broad_label))) {
    detail$code <- NA_character_
    detail$label <- NA_character_
  }

  list(
    code = broad_idx,
    label = broad_label,
    detail_code = detail$code,
    detail_label = detail$label
  )
}

# Helper: draw 3-digit detailed field given broad digit + STYRK 2-digit group.
# Uses nus_detail_by_styrk.csv first, filtered to codes whose first digit
# matches `broad`. Falls back to uniform across nus_detailed.csv rows with
# matching broad.
# NUS-koder for utdanninger som knapt fantes i Norge for 1980-tallet.
# Brukes til a dempe anakronistiske studieretninger for eldre kohorter.
.modern_nus_codes <- c(
  "161", "165", "166",          # design (bruks-, interior-, kles-)
  "351", "353", "359",          # medier og kommunikasjon
  "481", "482", "489",          # informatikk
  "513", "514",                 # mikrobiologi, miljostudier
  "541", "542", "549"           # informasjons- og datateknologi
)

.cond_nus_detail <- function(broad, sk2, lang = "en", age = NULL) {
  dm <- .cfm_env$nus_detail_by_styrk
  dl <- .cfm_env$nus_detailed
  broad_chr <- as.character(broad)
  lbl_col <- if (identical(lang, "no") && "label_no" %in% names(dl)) "label_no" else "label_en"
  unspec  <- if (identical(lang, "no")) "Uoppgitt" else "Unspecified"

  # Uoppgitt broad -> 999
  if (broad_chr == "9") {
    return(list(code = "999", label = unspec))
  }

  # First pass: STYRK-specific entries that nest in this broad field
  cand <- dm[dm$styrk2 == sk2, , drop = FALSE]
  if (nrow(cand) > 0) {
    cand <- cand[substr(cand$nus_code, 1, 1) == broad_chr, , drop = FALSE]
  }

  # Second pass: default STYRK entries in this broad
  if (nrow(cand) == 0) {
    dcand <- dm[dm$styrk2 == "default", , drop = FALSE]
    cand <- dcand[substr(dcand$nus_code, 1, 1) == broad_chr, , drop = FALSE]
  }

  if (nrow(cand) == 0) {
    # Uniform fallback across all detailed codes with this broad
    pool <- dl[dl$broad == broad_chr, , drop = FALSE]
    if (nrow(pool) == 0) {
      return(list(code = paste0(broad_chr, "99"), label = unspec))
    }
    idx <- sample.int(nrow(pool), 1)
    return(list(code = pool$code[idx], label = pool[[lbl_col]][idx]))
  }

  # Weighted draw from candidates, justert for kohort
  w <- cand$weight
  w[is.na(w) | w < 0] <- 0
  # Moderne NUS-koder (IT, design, media, miljo, mikrobiologi) skal vaere
  # sjeldne for ego fodt for 1980 -- utdanningene fantes knapt.
  if (!is.null(age) && !is.na(age)) {
    birth_year <- .cfm_env$ref_year - age
    is_modern <- cand$nus_code %in% .modern_nus_codes
    cohort_mult <- if (birth_year < 1960) 0.05
                   else if (birth_year < 1970) 0.15
                   else if (birth_year < 1980) 0.45
                   else 1.0

    # En multiplikator kan bare flytte vekt til noe annet. Er alle
    # kandidatene moderne -- STYRK 35 (IKT-teknikere) har bare slike, og
    # STYRK 25 er 90 % -- endrer x0.05 ingenting relativt, og en 78-aring
    # fikk fortsatt IKT-utdanning. Da er det riktigere a la
    # studieretningen sta apen enn a pasta en grad som ikke fantes:
    # det brede fagfeltet vises fortsatt.
    if (cohort_mult < 1 && all(is_modern)) {
      return(list(code = NA_character_, label = NA_character_))
    }
    w[is_modern] <- w[is_modern] * cohort_mult
  }
  if (sum(w) == 0) w <- rep(1, nrow(cand))
  idx <- sample.int(nrow(cand), 1, prob = w)
  nus_code <- cand$nus_code[idx]
  lbl_row <- dl[dl$code == nus_code, , drop = FALSE]
  lbl <- if (nrow(lbl_row) > 0) lbl_row[[lbl_col]][1] else unspec
  list(code = nus_code, label = lbl)
}

# -- Age + Education + Occupation -> Parents (mother + father) --
#
# Draws both biological parents with realistic fodealder, cohort-appropriate
# education (correlated with ego via r ~= 0.4), and occupation that may inherit
# ego's STYRK major group (soft yrkesarv, P ~= 0.3).
#
# HARD RULES:
#   - Mother fodealder 18-45 (skewed right, median ~29).
#   - Father fodealder 18-50 (slightly older, median ~31).
#   - Parent's current age = ego's age + fodealder; capped at 99.
#   - Parent is flagged is_alive = FALSE if implied current age > 90, with
#     probability rising sharply above 85. (Deceased parents keep their last
#     known occupation/education labels for transparency.)
#   - Ego under 16 still gets parents (they exist regardless of ego's age).


# --- Couple type for parents (heterosexual / mixed / same-sex) ----------
# Decides composition of ego's parents based on ego's age + background.
# Returns list(type, sex, mother_region, father_region).
# 'sex' is "FM" (mother+father), "FF" (two mothers), "MM" (two fathers).
.draw_couple_type <- function(ego_age, ego_background, ego_name_region, ref_year) {
  birth_year <- ref_year - ego_age

  # Same-sex parent probability — cohort-conditional.
  # Norway legalized samkjonnsekteskap in 2009; assistert befruktning for kvinnelige
  # par fra 2009; registrert partnerskap fra 1993. Adoption har vaert mulig laenge.
  p_samesex <- if (birth_year >= 2010) stats::runif(1, 0.020, 0.030)
               else if (birth_year >= 1995) 0.005
               else if (birth_year >= 1980) 0.001
               else 0.0005

  if (stats::runif(1) < p_samesex) {
    # Female couples more common (assistert reproduksjon), male couples adopt
    sex <- if (stats::runif(1) < 0.70) "FF" else "MM"
    return(list(type = "samesex", sex = sex,
                mother_region = ego_name_region,
                father_region = ego_name_region))
  }

  # Heterosexual couples — composition by ego background
  pick_other_region <- function(exclude) {
    pool <- setdiff(c("ost_europa","mena_sor_asia","afrika_sub",
                      "ost_asia","vesteuropa","latam_filippin","norden"),
                    exclude)
    if (length(pool) == 0) return("norden")
    sample(pool, 1)
  }

  if (identical(ego_background, "majority")) {
    r <- stats::runif(1)
    if (r < 0.96) {
      return(list(type = "both_norwegian", sex = "FM",
                  mother_region = "norden", father_region = "norden"))
    }
    immigrant_region <- sample(
      c("ost_europa","vesteuropa","mena_sor_asia","afrika_sub","ost_asia","latam_filippin","norden"),
      1, prob = c(0.40, 0.22, 0.16, 0.07, 0.08, 0.05, 0.02))
    if (stats::runif(1) < 0.5) {
      return(list(type = "mixed", sex = "FM",
                  mother_region = immigrant_region, father_region = "norden"))
    }
    return(list(type = "mixed", sex = "FM",
                mother_region = "norden", father_region = immigrant_region))
  }

  if (identical(ego_background, "second_gen")) {
    r <- stats::runif(1)
    if (r < 0.85) {
      return(list(type = "both_origin", sex = "FM",
                  mother_region = ego_name_region,
                  father_region = ego_name_region))
    }
    if (r < 0.97) {
      if (stats::runif(1) < 0.5) {
        return(list(type = "mixed", sex = "FM",
                    mother_region = ego_name_region, father_region = "norden"))
      }
      return(list(type = "mixed", sex = "FM",
                  mother_region = "norden", father_region = ego_name_region))
    }
    other <- pick_other_region(ego_name_region)
    if (stats::runif(1) < 0.5) {
      return(list(type = "two_origin_mixed", sex = "FM",
                  mother_region = ego_name_region, father_region = other))
    }
    return(list(type = "two_origin_mixed", sex = "FM",
                mother_region = other, father_region = ego_name_region))
  }

  # first_gen
  r <- stats::runif(1)
  if (r < 0.92) {
    return(list(type = "both_origin", sex = "FM",
                mother_region = ego_name_region,
                father_region = ego_name_region))
  }
  if (r < 0.98) {
    if (stats::runif(1) < 0.5) {
      return(list(type = "mixed", sex = "FM",
                  mother_region = ego_name_region, father_region = "norden"))
    }
    return(list(type = "mixed", sex = "FM",
                mother_region = "norden", father_region = ego_name_region))
  }
  other <- pick_other_region(ego_name_region)
  if (stats::runif(1) < 0.5) {
    return(list(type = "two_origin_mixed", sex = "FM",
                mother_region = ego_name_region, father_region = other))
  }
  return(list(type = "two_origin_mixed", sex = "FM",
              mother_region = other, father_region = ego_name_region))
}

# Educational expansion across the twentieth century.
#
# education_by_age.csv tops out at "67 aar eller eldre", which describes
# everyone alive today above that age -- mostly born 1940-1958, and
# 27.5 % of them hold a tertiary degree. Applying that band to a parent
# born in 1905 gave a 93-year-old ego two parents with master's degrees.
# In that cohort a few per cent had any higher education at all.
#
# Approximate share with any tertiary education by birth cohort, scaled
# against the 27.5 % the band implies:
.edu_cohort_tertiary_mult <- function(birth_year) {
  if (is.null(birth_year) || is.na(birth_year)) return(1)
  if (birth_year < 1920) 0.11        # ~3 %
  else if (birth_year < 1935) 0.22   # ~6 %
  else if (birth_year < 1945) 0.36   # ~10 %
  else if (birth_year < 1955) 0.65   # ~18 %
  else if (birth_year < 1965) 0.91   # ~25 %
  else 1
}

# Demote a tertiary code when the cohort makes it implausible. Long
# degrees (7) and doctorates (8) are cut harder than short ones: the
# pre-war gap between "some higher education" and "embetseksamen" was
# wider than it is now.
.demote_edu_for_cohort <- function(code, birth_year) {
  if (is.null(code) || is.na(code) || code < 6L) return(code)
  m <- .edu_cohort_tertiary_mult(birth_year)
  if (code >= 7L) m <- m * 0.6
  if (stats::runif(1) < m) return(code)
  sample(c(2L, 3L, 4L, 5L), 1, prob = c(0.42, 0.20, 0.30, 0.08))
}

.cond_parents <- function(age, edu_code, styrk_code, gender = NULL, lang = "en",
                          background = "majority", name_region = "norden",
                          country_label = NULL) {
  .load_data()

  # --- 1. Fodealder distributions (piecewise triangular, calibrated to SSB
  #         fertility tables 04232). Mother mode ~29, father mode ~31.
  draw_mother_age <- function() {
    # Sample from discretised distribution over 18-45.
    ages <- 18:45
    # Peaks around 28-32, right-skewed tail.
    w <- dnorm(ages, mean = 30, sd = 5)
    # Small uplift for 20-24 (traditional Nordic pattern historically).
    w[ages >= 20 & ages <= 24] <- w[ages >= 20 & ages <= 24] * 1.1
    sample(ages, 1, prob = w)
  }

  draw_father_age <- function(mother_age) {
    # SSB: median far-mor-aldersforskjell ~2.4 år, sd ~5.5 år.
    # Wider spread than v0.5.10. ~7% yngre far, 50% innen 0-3 eldre,
    # 25% 4-7 eldre, 12% 8-14 eldre, 6% 15-25 eldre (spennende kombinasjoner).
    delta <- sample(c(-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,12,14,16,18,22),
                    1,
                    prob = c(0.005,0.010,0.015,0.020,0.020,
                             0.060,0.130,0.150,0.140,0.110,
                             0.080,0.065,0.050,0.035,0.025,
                             0.025,0.018,0.015,0.012,0.010,0.005))
    fa <- mother_age + delta
    if (fa < 16) fa <- 16L
    if (fa > 65) fa <- 65L
    fa
  }

  mother_birth_age <- draw_mother_age()
  father_birth_age <- draw_father_age(mother_birth_age)
  # Implied current age — UNCAPPED (used for birth_year + death calc)
  mother_age_implied <- age + mother_birth_age
  father_age_implied <- age + father_birth_age

  # --- 2. Death year. Uses implied (uncapped) age. For implied_age >= 105
  #         death is certain. Death age sampled triangular over [60, implied_age].
  .death_year <- function(implied_age, birth_year) {
    if (implied_age < 65) return(NA_integer_)
    p_dead <- if (implied_age >= 105) 1.00
              else if (implied_age >= 95) 0.95
              else if (implied_age >= 90) 0.80
              else if (implied_age >= 85) 0.55
              else if (implied_age >= 75) 0.25
              else 0.05
    if (runif(1) > p_dead) return(NA_integer_)
    # Dead: pick death_age weighted toward 75-90, but cap at 105 to avoid silly ages.
    span_max <- min(105L, as.integer(implied_age))
    span <- 60:span_max
    if (length(span) == 0) return(birth_year + as.integer(implied_age))
    w <- dnorm(span, mean = 80, sd = 8)
    death_age <- sample(span, 1, prob = w)
    as.integer(birth_year + death_age)
  }

  # birth_year uses UNCAPPED implied age (so very old ego have parents born e.g. 1900)
  mother_birth_year <- .cfm_env$ref_year - mother_age_implied
  father_birth_year <- .cfm_env$ref_year - father_age_implied
  mother_death_year <- .death_year(mother_age_implied, mother_birth_year)
  father_death_year <- .death_year(father_age_implied, father_birth_year)
  # mother_age / father_age (used downstream by edu/occ helpers) cap at 99 — ok since
  # we just need a plausible "if alive" age.
  mother_age <- as.integer(min(99L, mother_age_implied))
  father_age <- as.integer(min(99L, father_age_implied))

  # --- 3. Education: cohort-appropriate, pulled toward ego.
  #         With prob 0.4, copy ego's edu_code (or one step off). Otherwise
  #         draw independently from parent's age-band.
  parent_edu <- function(parent_age, gender_str, parent_birth_year = NA_integer_) {
    row <- .cfm_env$education
    relabel <- function(cd) {
      lb <- if (identical(lang, "no")) row$level_no[row$code == cd][1]
            else row$level[row$code == cd][1]
      list(code = cd, label = lb)
    }
    # Inheritance only makes sense when ego is adult (>=18). For children,
    # parent edu shouldn't mirror child's grade level.
    if (!is.null(age) && !is.na(age) && age >= 18 &&
        !is.null(edu_code) && !is.na(edu_code) && runif(1) < 0.4) {
      jitter <- sample(c(-1L, 0L, 0L, 0L, 1L), 1)
      code <- max(2L, min(9L, as.integer(edu_code) + jitter))  # min 2 (no adult barneskole only)
      # Correlation with the child does not suspend the cohort: an ego
      # with a master's born in 1933 does not thereby give her parents,
      # born around 1905, master's degrees of their own.
      code <- .demote_edu_for_cohort(code, parent_birth_year)
      return(relabel(code))
    }
    # Draw from parent's own cohort via existing .cond_education.
    e <- .cond_education(parent_age, lang = lang)
    if (!is.null(e$code) && !is.na(e$code)) {
      dc <- .demote_edu_for_cohort(as.integer(e$code), parent_birth_year)
      if (!identical(dc, as.integer(e$code))) e <- relabel(dc)
    }
    # Floor: adults shouldn't have edu_code = 1 (kun barneskole 1-7)
    if (!is.null(e$code) && !is.na(e$code) && e$code < 2) {
      e$code <- 2L
      row <- .cfm_env$education
      e$label <- if (identical(lang, "no"))
                   row$level_no[row$code == 2L][1] else row$level[row$code == 2L][1]
    }
    e
  }

  mother_edu <- parent_edu(mother_age, "F", mother_birth_year)
  father_edu <- parent_edu(father_age, "M", father_birth_year)

  # --- Decide couple type early (used for parent_occupation gender) ---
  couple <- .draw_couple_type(age, background, name_region, .cfm_env$ref_year)
  m_gender_str <- if (identical(couple$sex, "MM")) "M" else "F"
  f_gender_str <- if (identical(couple$sex, "FF")) "F" else "M"

  # --- 4. Occupation: soft yrkesarv. 30% chance parent's STYRK major ==
  #         ego's STYRK major (via edu_code override route).
  # Use actual gender per couple type (avoids "Andrius (M): Hjemmeværende" bug)
  mother_occ <- .parent_occupation(mother_birth_year, m_gender_str, mother_edu$code)
  mother_occ <- .try_yrkesarv(mother_occ, styrk_code, 50L, mother_edu$code, m_gender_str)
  father_occ <- .parent_occupation(father_birth_year, f_gender_str, father_edu$code)
  father_occ <- .try_yrkesarv(father_occ, styrk_code, 50L, father_edu$code, f_gender_str)

  # --- Override for parents who likely lived in origin country (not Norway) ---
  # If ego is non-majority and parent's birth_year predates the immigration window
  # (with some slack), parent likely lived their working life abroad, not in Norway.
  # Replace SSB-Norwegian STYRK label with a generic "Bodde i [land]" label.
  if (!identical(background, "majority") &&
      !is.null(name_region) && !is.na(name_region) && nzchar(name_region)) {
    isy <- .cfm_env$immigration_start_year
    cd  <- .cfm_env$immigrant_country_dist
    parent_in_norway <- function(parent_birth_year) {
      if (is.null(isy) || is.null(cd)) return(TRUE)
      # Use first country in name_region as proxy if country not passed
      if (!is.null(country_label) && !is.na(country_label)) {
        sy_row <- cd[cd$label == country_label, , drop = FALSE]
        if (nrow(sy_row) > 0) {
          isy_row <- isy[isy$code == sy_row$code[1], , drop = FALSE]
          sy <- if (nrow(isy_row) > 0) as.integer(isy_row$start_year[1]) else 1970L
        } else sy <- 1970L
      } else sy <- 1970L
      # Parent born >= sy - 30 could plausibly have migrated as young adult
      parent_birth_year >= (sy - 30L)
    }
    abroad_label <- if (!is.null(country_label) && !is.na(country_label)) {
      sprintf(if (identical(lang, "no")) "Bodde i %s" else "Lived in %s", country_label)
    } else {
      if (identical(lang, "no")) "Bodde i opprinnelseslandet" else "Lived in country of origin"
    }
    if (!parent_in_norway(mother_birth_year)) {
      mother_occ$label <- abroad_label
      mother_occ$styrk_code <- NA_character_
      mother_occ$median_monthly <- NA_integer_
    }
    if (!parent_in_norway(father_birth_year)) {
      father_occ$label <- abroad_label
      father_occ$styrk_code <- NA_character_
      father_occ$median_monthly <- NA_integer_
    }
  }

  # --- 5. Names per parent's origin region (couple already drawn at start of step 4) ---
  draw_parent_name <- function(gender_str, birth_year, region) {
    if (!is.null(region) && !is.na(region) && !identical(region, "norden")) {
      .load_data()
      nbr <- .cfm_env$names_by_region
      if (!is.null(nbr)) {
        pool <- nbr[nbr$region == region & nbr$gender == gender_str, , drop = FALSE]
        if (nrow(pool) > 0) {
          return(pool$name[sample.int(nrow(pool), 1)])
        }
      }
    }
    tryCatch(
      sample_first_name(gender = gender_str, birth_year = birth_year),
      error = function(e) NA_character_
    )
  }
  mother_name <- draw_parent_name(m_gender_str, mother_birth_year, couple$mother_region)
  father_name <- draw_parent_name(f_gender_str, father_birth_year, couple$father_region)

  list(
    mother = list(
      name = mother_name,
      gender = m_gender_str,
      birth_year = as.integer(mother_birth_year),
      death_year = mother_death_year,
      birth_age = mother_birth_age,
      education = mother_edu$label,
      education_code = mother_edu$code,
      occupation = mother_occ$label,
      styrk_code = mother_occ$styrk_code,
      median_monthly = mother_occ$median_monthly,
      origin_region = couple$mother_region
    ),
    father = list(
      name = father_name,
      gender = f_gender_str,
      birth_year = as.integer(father_birth_year),
      death_year = father_death_year,
      birth_age = father_birth_age,
      education = father_edu$label,
      education_code = father_edu$code,
      occupation = father_occ$label,
      styrk_code = father_occ$styrk_code,
      median_monthly = father_occ$median_monthly,
      origin_region = couple$father_region
    ),
    couple_type = couple$type,
    couple_sex = couple$sex
  )
}

# -- Parents capital estimate (bridge to housing + wealth) --
#
# Rough log-normal estimate in NOK of the couple's combined net assets
# (housing equity + savings + pension, minus debt). Used downstream by
# .cond_housing (egenkapital prior) and .cond_wealth (arvsannsynlighet).
#
# Model per parent:
#   base(age)    400k at 40 -> 2 MNOK at 65 -> 3 MNOK at 80 (piecewise linear)
#   occ_mult     STYRK 1-digit major lookup (directors 2.5x, elementary 0.6x)
#   edu_mult     0.6 (< VGS) to 1.6 (PhD)
#   noise        log-normal noise sd = 0.7
# Couple total = mother_capital + father_capital.

.parents_capital <- function(mother, father) {
  occ_mult <- c(
    "0" = 1.2, "1" = 2.5, "2" = 1.8, "3" = 1.3, "4" = 1.0,
    "5" = 0.75, "6" = 1.0, "7" = 1.0, "8" = 0.85, "9" = 0.6
  )
  edu_mult <- c(
    "0" = 0.5, "1" = 0.6, "2" = 0.7, "3" = 0.85, "4" = 0.95,
    "5" = 1.10, "6" = 1.20, "7" = 1.35, "8" = 1.60, "9" = 0.6
  )

  .base <- function(a) {
    if (is.null(a) || is.na(a)) return(800000)
    if (a < 25)  return(100000)
    if (a < 40)  return(100000 + (a - 25) * (400000 - 100000) / 15)
    if (a < 65)  return(400000 + (a - 40) * (2000000 - 400000) / 25)
    if (a < 80)  return(2000000 + (a - 65) * (3000000 - 2000000) / 15)
    if (a < 95)  return(3000000 - (a - 80) * (3000000 - 2400000) / 15)
    return(2400000)
  }

  .per_parent <- function(p) {
    if (is.null(p)) return(0)
    # Snapshot age: age at death if dead, else current implied age.
    is_dead <- !is.null(p$death_year) && !is.na(p$death_year)
    snap_age <- if (is_dead && !is.null(p$birth_year)) {
      p$death_year - p$birth_year
    } else if (!is.null(p$birth_year)) {
      .cfm_env$ref_year - p$birth_year
    } else NA_integer_
    base <- .base(snap_age)
    occd <- if (!is.null(p$styrk_code) && !is.na(p$styrk_code) &&
                nzchar(p$styrk_code)) substr(p$styrk_code, 1, 1) else NA_character_
    om <- if (!is.na(occd) && occd %in% names(occ_mult)) occ_mult[[occd]] else 0.5
    em <- if (!is.null(p$education_code) && !is.na(p$education_code)) {
      key <- as.character(p$education_code)
      if (key %in% names(edu_mult)) edu_mult[[key]] else 1.0
    } else 1.0
    # Deceased parents: their wealth has likely been distributed; model at 30 %
    # of what they would have had (ego hasn't necessarily inherited yet).
    alive_mult <- if (is_dead) 0.3 else 1.0
    noise <- exp(rnorm(1, mean = 0, sd = 0.7))
    base * om * em * alive_mult * noise
  }

  cap <- .per_parent(mother) + .per_parent(father)
  as.integer(round(cap))
}

# -- Parent's occupation at yrkesaktiv age (not current age) --
#
# Calls .cond_occupation() with a fixed age of 50 so parents never end up
# tagged with pensjonisthobby, STUDENT, Uforetrygdet, or kid/teen labels.
# Adds a cohort-conditional "Hjemmeværende" branch for women born before
# ~1970 (falling from ~55 % for 1920s births to ~1 % for post-1975).
# Reduces the husmor share for highly educated women (edu >= 6 -> x0.3).
#
# NOTE: the underlying STYRK distribution is still today's yrkesstruktur.
# A full cohort recalibration (more industry/sjofart/landbruk for older
# male cohorts, e.g.) is a follow-up.

.parent_occupation <- function(parent_birth_year, gender, edu_code) {
  # Women pre-1970: probability of being a homemaker in active years.
  if (!is.null(gender) && identical(toupper(gender), "F") &&
      !is.null(parent_birth_year) && !is.na(parent_birth_year)) {
    p_hus <- if (parent_birth_year < 1930) 0.55
             else if (parent_birth_year < 1945) 0.45
             else if (parent_birth_year < 1955) 0.30
             else if (parent_birth_year < 1965) 0.15
             else if (parent_birth_year < 1975) 0.05
             else 0.01
    if (!is.null(edu_code) && !is.na(edu_code) && edu_code >= 6L) {
      p_hus <- p_hus * 0.3
    }
    if (runif(1) < p_hus) {
      return(list(
        label = "Hjemmev\u{00e6}rende",
        styrk_code = NA_character_,
        median_monthly = NA_integer_,
        kind = "homemaker"
      ))
    }
  }
  # Default: draw as a 50-year-old (yrkesaktiv) with parent's edu + gender.
  # Reject benefit-status outputs (we want the last active yrke for parents).
  .is_benefit <- function(o) {
    if (is.null(o)) return(FALSE)
    k <- if (!is.null(o$kind)) as.character(o$kind) else ""
    if (k %in% c("disabled", "unemployed")) return(TRUE)
    lbl <- if (!is.null(o$label)) as.character(o$label) else ""
    grepl("^AAP|^Sosialhjelp|^Dagpenger|^Uforetrygdet|^Uf\u{00f8}retrygdet", lbl)
  }
  # Midpoint of the parent's working life, not today.
  wy <- if (!is.null(parent_birth_year) && !is.na(parent_birth_year))
          as.integer(parent_birth_year) + 40L else NA_integer_
  occ <- .cond_occupation(age = 50L, edu_code = edu_code, gender = gender,
                          work_year = wy)
  for (i in seq_len(8)) {
    if (!.is_benefit(occ)) break
    occ <- .cond_occupation(age = 50L, edu_code = edu_code,
                            gender = gender, work_year = wy)
  }
  occ
}


# Soft yrkesarv: with 30 % probability, try up to 5 resamples to match
# ego's STYRK major (first digit). Accepts whatever comes out after budget.
# Only triggers if parent's drawn yrke actually has a STYRK code (skips
# homemakers, etc.).
.try_yrkesarv <- function(occ, ego_styrk, active_age, parent_edu_code, parent_gender) {
  if (is.null(ego_styrk) || is.na(ego_styrk) || !nzchar(ego_styrk)) return(occ)
  if (is.null(occ$styrk_code) || is.na(occ$styrk_code) ||
      !nzchar(occ$styrk_code)) return(occ)
  if (runif(1) >= 0.30) return(occ)
  target <- substr(as.character(ego_styrk), 1, 1)
  for (i in seq_len(5)) {
    if (substr(as.character(occ$styrk_code), 1, 1) == target) break
    occ <- .cond_occupation(age = active_age, edu_code = parent_edu_code,
                            gender = parent_gender)
    if (is.null(occ$styrk_code) || is.na(occ$styrk_code) ||
        !nzchar(occ$styrk_code)) break
  }
  occ
}

# -- Husholdning: erstatt "mor/far" med faktisk kjønn (M/F) --
#
# Brukes på kode 6 (enslig forelder med barn 0-17) og 7 (enslig forelder
# med voksne barn). Andre koder returneres uendret. Hvis gender ikke er
# oppgitt, returneres labelen som den er.
.gender_household_label <- function(label, code, gender, lang) {
  if (is.null(label) || length(label) == 0 || is.na(label)) return(label)
  if (is.null(gender) || length(gender) == 0 || is.na(gender) ||
      !nzchar(gender)) return(label)
  if (!(code %in% c(6L, 7L))) return(label)
  g <- toupper(as.character(gender))
  if (identical(lang, "no")) {
    if (g == "F") return(sub("mor/far", "mor", label, fixed = TRUE))
    if (g == "M") return(sub("mor/far", "far", label, fixed = TRUE))
  } else {
    if (g == "F") return(sub("Single parent", "Single mother", label, fixed = TRUE))
    if (g == "M") return(sub("Single parent", "Single father", label, fixed = TRUE))
  }
  label
}

# -- Trekk en typisk student-deltidsjobb --
#
# Vekter STYRK 5 (servicearbeid) og 9 (renhold/post/kjokken) tungt; legger
# inn litt 3 (kontorhjelp) og 2 (studentassistent ved UH). Gender-conditioning
# hvis oppgitt. Returnerer label, styrk_code, median_monthly.
.draw_student_deltid <- function(gender = NULL) {
  os <- .cfm_env$occupations_salary
  tbl <- os[os$schedule == "part" & !is.na(os$code) & nzchar(os$code), ]
  tbl <- tbl[tbl$code != "0000" & tbl$headcount > 0, ]
  if (nrow(tbl) == 0) return(NULL)
  fd <- substr(tbl$code, 1, 1)
  w <- ifelse(fd == "5", 5,
        ifelse(fd == "9", 4,
         ifelse(fd == "3", 1.5,
          ifelse(fd == "2", 0.8, 0.3))))
  w <- w * tbl$headcount
  # Gender-skew using occupations_gender
  if (!is.null(gender) && nzchar(gender)) {
    gen <- .cfm_env$occupations_gender
    fs <- gen$female_share[match(tbl$code, gen$code)]
    fs[is.na(fs)] <- 0.5
    g_w <- if (toupper(as.character(gender)) == "F") fs else (1 - fs)
    g_w <- pmax(g_w, 0.05)
    w <- w * g_w
  }
  if (sum(w) == 0) w <- tbl$headcount
  idx <- sample.int(nrow(tbl), 1, prob = w)
  list(
    label = tbl$label[idx],
    styrk_code = tbl$code[idx],
    median_monthly = as.integer(tbl$median_nok[idx])
  )
}


# --- Housing (bolig) conditional draw --------------------------------------
# Conditional on age + income + county + parents_capital.
# Returns list(owner, tenure, boligtype, area_m2, value_nok, debt_nok, equity_nok).

.cond_housing <- function(age,
                          income_nok = NULL,
                          county = NULL,
                          municipality = NULL,
                          parents_capital = NULL,
                          gender = NULL,
                          lang = "en",
                          ref_year = NULL) {
  na_result <- list(
    owner = NA,
    tenure = NA_character_,
    boligtype = NA_character_,
    area_m2 = NA_integer_,
    value_nok = NA_integer_,
    debt_nok = NA_integer_,
    equity_nok = NA_integer_,
    purchase_year = NA_integer_,
    purchase_price_nok = NA_integer_,
    luxury = NA,
    has_hytte = NA,
    hytte_type = NA_character_,
    hytte_value_nok = NA_integer_
  )
  if (is.null(age) || is.na(age) || age < 18) return(na_result)
  if (is.null(county) || is.na(county) || !nzchar(county)) return(na_result)

  .load_data()
  hp <- .cfm_env$housing_prices
  if (is.null(hp)) return(na_result)
  if (is.null(ref_year)) ref_year <- .cfm_env$ref_year

  p_own <- .housing_own_prob(age, income_nok, parents_capital)
  is_owner <- stats::runif(1) < p_own

  if (!is_owner) {
    return(list(
      owner = FALSE,
      tenure = if (identical(lang, "no")) "Leier" else "Rents",
      boligtype = NA_character_,
      area_m2 = NA_integer_,
      value_nok = NA_integer_,
      debt_nok = NA_integer_,
      equity_nok = NA_integer_,
      purchase_year = NA_integer_,
      purchase_price_nok = NA_integer_,
      luxury = FALSE,
      has_hytte = .draw_hytte_owner(age, income_nok, county, owns_primary = FALSE),
      hytte_type = NA_character_,
      hytte_value_nok = NA_integer_
    ))
  }

  bt <- .draw_boligtype(age, income_nok, county, parents_capital = parents_capital)
  area <- .draw_housing_area(bt$code, age, income_nok)

  hp_sub <- hp[hp$fylke == county & hp$boligtype_code == bt$code, , drop = FALSE]
  if (nrow(hp_sub) == 0) {
    hp_sub <- hp[hp$fylke == "Hele landet" & hp$boligtype_code == bt$code, , drop = FALSE]
  }
  if (nrow(hp_sub) == 0) return(na_result)
  latest_year <- max(hp_sub$year)
  base_price_m2 <- hp_sub$price_m2_nok[hp_sub$year == latest_year][1]

  # Apply kommune-level multiplier if available (urban/rural within fylke)
  km <- .cfm_env$kommune_price_multiplier
  if (!is.null(km) && !is.null(municipality) && !is.na(municipality) && nzchar(municipality)) {
    krow <- km[km$kommune == municipality & km$boligtype_code == bt$code, , drop = FALSE]
    if (nrow(krow) > 0) {
      mult <- as.numeric(krow$multiplier[1])
      if (!is.na(mult) && mult > 0) base_price_m2 <- base_price_m2 * mult
    }
  }

  # Adjust to ref_year
  idx_now <- .housing_index_factor(county, bt$code,
                                   from_year = latest_year,
                                   to_year = ref_year)
  price_m2_now <- base_price_m2 * idx_now
  # Per-individual variability (location within fylke, condition, etc.)
  price_m2_ind <- price_m2_now * stats::runif(1, 0.85, 1.15)

  # --- Luxury tail: 1.5% probability, 3-8x value ---
  # Higher in Oslo + Akershus + with very high income / parents_capital.
  luxury <- .luxury_draw(age, income_nok, county, parents_capital)
  if (luxury) {
    price_m2_ind <- price_m2_ind * stats::runif(1, 3.0, 8.0)
    # Luxury homes tend to be larger
    area <- as.integer(round(area * stats::runif(1, 1.3, 2.2)))
  }

  value <- round(area * price_m2_ind)

  # --- Purchase year: older owners likely bought longer ago ---
  purchase_year <- .draw_purchase_year(age, ref_year)
  years_held <- max(0, ref_year - purchase_year)

  # Historical price at purchase year
  pidx <- .housing_index_factor(county, bt$code,
                                from_year = ref_year,
                                to_year = purchase_year)
  # purchase_price ~ value * (index_purchase / index_now), with small noise
  purchase_price <- round(value * pidx * stats::runif(1, 0.92, 1.08))
  if (purchase_price < 1) purchase_price <- value  # safety

  # --- Original LTV: function of age-at-purchase, parents_capital, luxury ---
  age_at_purchase <- age - years_held
  initial_ltv <- .initial_ltv(age_at_purchase, parents_capital, luxury)
  original_loan <- round(purchase_price * initial_ltv)

  # --- Amortization: linear over 25 years (extra payments captured by noise) ---
  amort_years <- 25
  amort_share <- min(1.0, years_held / amort_years)
  remaining_debt <- round(original_loan * (1 - amort_share))
  # Some owners refinance / take out equity; add modest noise
  remaining_debt <- round(remaining_debt * stats::runif(1, 0.85, 1.15))
  remaining_debt <- max(0L, min(as.integer(value * 0.95), as.integer(remaining_debt)))
  equity <- as.integer(value - remaining_debt)

  # --- Hytte / fritidsbolig ---
  has_hytte <- .draw_hytte_owner(age, income_nok, county, owns_primary = TRUE,
                                  parents_capital = parents_capital,
                                  luxury_primary = luxury)
  hytte_type <- NA_character_
  hytte_value <- NA_integer_
  if (isTRUE(has_hytte)) {
    h <- .draw_hytte(income_nok, county, parents_capital, luxury_primary = luxury,
                     ref_year = ref_year, lang = lang)
    hytte_type <- h$type
    hytte_value <- h$value
    # Cap: hytte > 3x primaer er urealistisk unntatt ved luksusbolig eller arvet rikdom.
    # Selv med luksus/arvet rikdom er hytte > 5x primaer ekstremt sjeldent.
    pc_safe <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital
    max_ratio <- if (isTRUE(luxury) || pc_safe > 25e6) 5
                 else if (pc_safe > 10e6) 4
                 else 3
    max_hytte <- as.numeric(value) * max_ratio
    if (hytte_value > max_hytte) hytte_value <- as.integer(max_hytte)
  }

  list(
    owner = TRUE,
    tenure = if (identical(lang, "no")) "Eier" else "Owns",
    boligtype = if (identical(lang, "no")) bt$label_no else bt$label_en,
    area_m2 = as.integer(area),
    value_nok = as.integer(value),
    debt_nok = as.integer(remaining_debt),
    equity_nok = as.integer(equity),
    purchase_year = as.integer(purchase_year),
    purchase_price_nok = as.integer(purchase_price),
    luxury = luxury,
    has_hytte = isTRUE(has_hytte),
    hytte_type = hytte_type,
    hytte_value_nok = hytte_value
  )
}


.housing_own_prob <- function(age, income_nok = NULL, parents_capital = NULL) {
  base <- if (age < 25) 0.12
          else if (age < 30) 0.35
          else if (age < 35) 0.55
          else if (age < 45) 0.75
          else if (age < 55) 0.82
          else if (age < 65) 0.85
          else if (age < 75) 0.82
          else 0.72
  inc <- if (is.null(income_nok) || is.na(income_nok)) 400000 else income_nok
  inc_mult <- if (inc < 150000) 0.40
              else if (inc < 300000) 0.80
              else if (inc < 500000) 1.00
              else if (inc < 800000) 1.10
              else 1.15
  pc <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital
  pc_mult <- if (pc > 10e6) 1.25
             else if (pc > 5e6) 1.15
             else if (pc > 2e6) 1.05
             else 1.00
  p <- base * inc_mult * pc_mult
  max(0.02, min(0.97, p))
}

.draw_boligtype <- function(age, income_nok = NULL, county = "",
                            parents_capital = NULL) {
  urban_score <- if (identical(county, "Oslo")) 0.85
                 else if (county %in% c("Akershus", "Vestland", "Rogaland", "Tr\u{00f8}ndelag", "Tr\u{00f8}ndelag")) 0.45
                 else if (county %in% c("\u{00d8}stfold", "Vestfold", "Buskerud", "Agder", "M\u{00f8}re og Romsdal")) 0.25
                 else 0.12
  age_score <- if (age < 30) 0.70 else if (age < 45) 0.30 else 0.15
  inc <- if (is.null(income_nok) || is.na(income_nok)) 400000 else income_nok
  pc  <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital

  p_blokk   <- min(0.85, urban_score * 0.60 + age_score * 0.40)
  p_smahus  <- 0.35 * (1 - p_blokk)
  p_enebolig <- 1 - p_blokk - p_smahus

  if (inc > 800000 && urban_score < 0.8) {
    p_enebolig <- p_enebolig + 0.15
    p_blokk    <- p_blokk    - 0.10
    p_smahus   <- p_smahus   - 0.05
  }

  # Geografi-bevisst aldersjustering:
  # - Oslo/Akershus (urban_score >= 0.5): under 25 nesten bare blokk; 25-29 ingen enebolig
  #   med mindre rike foreldre (pc > 5M) som kan ha overdratt eller hjulpet til
  # - Distrikt (urban_score < 0.5): under 25 mulig enebolig (arvet/distrikt-priser),
  #   men dempet sannsynlighet
  if (age < 25) {
    if (urban_score >= 0.5 && pc < 5e6) {
      # Urban + ingen rik forelder: kun blokk
      p_blokk <- 1.0
      p_smahus <- 0; p_enebolig <- 0
    } else if (urban_score >= 0.5) {
      # Urban + rik forelder: tillat småhus, ikke enebolig
      p_smahus <- p_smahus + p_enebolig * 0.3
      p_enebolig <- 0
    } else {
      # Distrikt: enebolig mulig men dempet (typisk arvet eller billig distrikt-hus)
      p_enebolig <- p_enebolig * 0.4
      p_smahus <- p_smahus * 1.2
    }
  } else if (age < 30) {
    if (urban_score >= 0.5 && pc < 5e6) {
      # Urban under 30 uten rik forelder: ingen enebolig
      p_enebolig <- 0
    } else if (urban_score < 0.5) {
      # Distrikt: tillat enebolig, men dempet
      p_enebolig <- p_enebolig * 0.7
    }
  }

  probs <- c(p_enebolig, p_smahus, p_blokk)
  probs <- pmax(probs, 0.01)
  probs <- probs / sum(probs)
  pick <- sample(c("01", "02", "03"), 1, prob = probs)
  if (identical(pick, "01")) {
    list(code = "01", label_en = "Detached house", label_no = "Enebolig")
  } else if (identical(pick, "02")) {
    list(code = "02", label_en = "Small house",    label_no = "Sm\u{00e5}hus")
  } else {
    list(code = "03", label_en = "Apartment",      label_no = "Blokkleilighet")
  }
}

.draw_housing_area <- function(boligtype_code, age, income_nok = NULL) {
  inc <- if (is.null(income_nok) || is.na(income_nok)) 400000 else income_nok
  base <- if (identical(boligtype_code, "01")) stats::rnorm(1, 145, 35)
          else if (identical(boligtype_code, "02")) stats::rnorm(1, 105, 25)
          else stats::rnorm(1, 62, 15)
  inc_mult <- 0.85 + min(0.30, (inc / 1e6) * 0.15)
  age_mult <- if (age < 30) 0.90 else if (age < 50) 1.00 else 1.05
  area <- base * inc_mult * age_mult
  minA <- if (identical(boligtype_code, "03")) 25 else 55
  maxA <- if (identical(boligtype_code, "01")) 300
          else if (identical(boligtype_code, "02")) 220 else 130
  round(max(minA, min(maxA, area)))
}

.housing_debt_ratio <- function(age) {
  r <- if (age < 30) stats::rnorm(1, 0.82, 0.10)
       else if (age < 40) stats::rnorm(1, 0.68, 0.12)
       else if (age < 50) stats::rnorm(1, 0.52, 0.15)
       else if (age < 60) stats::rnorm(1, 0.35, 0.15)
       else if (age < 70) stats::rnorm(1, 0.20, 0.12)
       else stats::rnorm(1, 0.10, 0.08)
  max(0, min(0.95, r))
}

.housing_index_factor <- function(county, boligtype_code,
                                  from_year, to_year) {
  if (is.null(.cfm_env$housing_index) ||
      is.null(.cfm_env$fylke_index_region)) return(1.0)
  if (is.na(from_year) || is.na(to_year) || from_year == to_year) return(1.0)

  fxr <- .cfm_env$fylke_index_region
  hi  <- .cfm_env$housing_index
  row <- fxr[fxr$fylke == county, , drop = FALSE]
  if (nrow(row) == 0) row <- fxr[fxr$fylke == "Hele landet", , drop = FALSE]
  if (nrow(row) == 0) return(1.0)
  region_code <- row$index_region_code[1]

  # Try region+bt; require the sub to actually span [from_year, to_year]
  # so we don't extrapolate into negative territory for years where SSB has no data.
  needed_yr <- min(from_year, to_year)
  pick_sub <- function(rc, bt) {
    s <- hi[hi$region_code == rc & hi$boligtype_code == bt, , drop = FALSE]
    s <- s[!is.na(s$index_2015) & s$index_2015 > 0, , drop = FALSE]
    s
  }
  sub <- pick_sub(region_code, boligtype_code)
  if (nrow(sub) == 0 || min(sub$year) > needed_yr + 5L) {
    cand <- pick_sub(region_code, "00")
    if (nrow(cand) > 0 && min(cand$year) <= needed_yr + 2L) sub <- cand
  }
  if (nrow(sub) == 0 || min(sub$year) > needed_yr + 5L) {
    cand <- pick_sub("TOTAL", boligtype_code)
    if (nrow(cand) > 0 && min(cand$year) <= needed_yr + 2L) sub <- cand
  }
  if (nrow(sub) == 0 || min(sub$year) > needed_yr + 5L) {
    cand <- pick_sub("TOTAL", "00")
    if (nrow(cand) > 0) sub <- cand
  }
  if (nrow(sub) == 0) return(1.0)

  years <- sort(sub$year)
  idx_of <- function(y) {
    if (y %in% years) return(sub$index_2015[sub$year == y][1])
    if (y < min(years)) {
      y1 <- years[1]; y2 <- years[2]
      i1 <- sub$index_2015[sub$year == y1][1]
      i2 <- sub$index_2015[sub$year == y2][1]
      return(i1 + (i2 - i1) * (y - y1) / (y2 - y1))
    }
    if (y > max(years)) {
      n <- length(years)
      y1 <- years[n - 1]; y2 <- years[n]
      i1 <- sub$index_2015[sub$year == y1][1]
      i2 <- sub$index_2015[sub$year == y2][1]
      return(i2 + (i2 - i1) * (y - y2) / (y2 - y1))
    }
    lo <- max(years[years < y]); hi <- min(years[years > y])
    il <- sub$index_2015[sub$year == lo][1]; iu <- sub$index_2015[sub$year == hi][1]
    il + (iu - il) * (y - lo) / (hi - lo)
  }
  # Before 1992 SSB has no house price index -- 07230 starts there. For
  # earlier years we splice on housing_index_prewar.csv, which is the
  # consumer price index (SSB 08981, real, back to 1920) multiplied by an
  # estimated real house price level. The estimate is the documented
  # shape of the series, not a transcription of one: flat-to-falling
  # through the war, slow postwar growth, the deregulation boom peaking
  # around 1987 and the banking-crisis trough in 1992.
  #
  # Deflating by CPI alone would have been wrong in a knowable
  # direction. A house worth 5 MNOK today comes out at about 123,000 in
  # 1965 with the real adjustment and 367,000 without; the 1965 figure
  # was nearer 100,000.
  first_year <- min(years)
  prewar_ratio <- function(y) {
    pw <- .cfm_env$housing_index_prewar
    if (is.null(pw) || !nrow(pw)) return(NA_real_)
    if (y >= first_year) return(1.0)
    v_y <- pw$nominal_index_1992_100[pw$year == max(min(pw$year), y)]
    v_b <- pw$nominal_index_1992_100[pw$year == max(pw$year)]
    if (!length(v_y) || !length(v_b) || v_b <= 0) return(NA_real_)
    v_y[1] / v_b[1]
  }
  idx_or_prewar <- function(y) {
    if (y >= first_year) return(idx_of(y))
    r <- prewar_ratio(y)
    if (is.na(r)) return(idx_of(first_year))
    idx_of(first_year) * r
  }

  i_from <- idx_or_prewar(from_year)
  i_to   <- idx_or_prewar(to_year)
  if (is.na(i_from) || i_from <= 0) return(1.0)
  if (is.na(i_to) || i_to <= 0) return(1.0)
  i_to / i_from
}


# --- Housing v2 helpers --------------------------------------------------

.draw_purchase_year <- function(age, ref_year) {
  # Years held: older owners average longer hold; first-time buyers ~28-32
  years_held <- if (age < 30) stats::runif(1, 0, max(1, age - 22))
                else if (age < 40) stats::runif(1, 1, min(15, age - 24))
                else if (age < 55) stats::runif(1, 3, min(25, age - 25))
                else if (age < 70) stats::runif(1, 5, min(35, age - 25))
                else                stats::runif(1, 10, min(45, age - 30))
  py <- ref_year - round(years_held)
  # The floor used to be 1992, which is where SSB's index starts -- so a
  # 93-year-old "bought" at 59. housing_index_prewar.csv now reaches back
  # to 1920, so the year can be what the holding period implies.
  max(1920L, as.integer(py))
}

.initial_ltv <- function(age_at_purchase, parents_capital = NULL, luxury = FALSE) {
  base <- if (age_at_purchase < 25) stats::rnorm(1, 0.88, 0.06)
          else if (age_at_purchase < 30) stats::rnorm(1, 0.85, 0.07)
          else if (age_at_purchase < 40) stats::rnorm(1, 0.78, 0.10)
          else if (age_at_purchase < 50) stats::rnorm(1, 0.70, 0.12)
          else if (age_at_purchase < 60) stats::rnorm(1, 0.55, 0.15)
          else                            stats::rnorm(1, 0.40, 0.18)
  pc <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital
  pc_adj <- if (pc > 20e6) -0.30
            else if (pc > 10e6) -0.22
            else if (pc > 5e6)  -0.15
            else if (pc > 2e6)  -0.07
            else 0
  ltv <- base + pc_adj
  if (luxury) ltv <- ltv * stats::runif(1, 0.5, 0.85)  # luxury buyers usually have own equity
  max(0.05, min(0.95, ltv))
}

.luxury_draw <- function(age, income_nok = NULL, county = "",
                         parents_capital = NULL) {
  inc <- if (is.null(income_nok) || is.na(income_nok)) 400000 else income_nok
  pc  <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital
  base <- 0.005
  if (county %in% c("Oslo", "Akershus")) base <- base * 4
  else if (county %in% c("Vestland", "Rogaland")) base <- base * 1.5
  if (inc > 1.5e6) base <- base * 4
  else if (inc > 1.0e6) base <- base * 2
  if (pc > 25e6) base <- base * 5
  else if (pc > 10e6) base <- base * 2.5
  if (age < 35) base <- base * 0.4  # rare to own luxury this young
  base <- min(base, 0.10)
  stats::runif(1) < base
}

.draw_hytte_owner <- function(age, income_nok = NULL, county = "",
                              owns_primary = TRUE,
                              parents_capital = NULL,
                              luxury_primary = FALSE) {
  if (is.null(age) || is.na(age) || age < 25) return(FALSE)
  inc <- if (is.null(income_nok) || is.na(income_nok)) 400000 else income_nok
  pc  <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital

  # Baseline: ~17% av norske husholdninger eier fritidsbolig
  base <- 0.17
  if (!owns_primary) base <- base * 0.4

  # Income gradient (sterk)
  inc_mult <- if (inc < 250000) 0.20
              else if (inc < 450000) 0.55
              else if (inc < 700000) 1.05
              else if (inc < 1.0e6) 1.85
              else if (inc < 1.5e6) 2.80
              else 3.60

  # Age gradient (peaker 50-70)
  age_mult <- if (age < 35) 0.35
              else if (age < 45) 0.75
              else if (age < 55) 1.10
              else if (age < 70) 1.30
              else 1.05

  # Geographic: oslofolk har "rad-hytte"-tradisjon, kystfylker har sjokk
  geo_mult <- if (county %in% c("Oslo", "Akershus")) 1.20
              else if (county %in% c("Innlandet", "Buskerud", "Vestfold", "Telemark", "Tr\u{00f8}ndelag")) 1.10
              else 0.95

  # Parents_capital — arvet hytte
  pc_mult <- if (pc > 10e6) 1.40 else if (pc > 5e6) 1.20 else if (pc > 2e6) 1.05 else 1.0

  # Luxury primary eier ofte hytte også
  if (luxury_primary) pc_mult <- pc_mult * 1.8

  p <- base * inc_mult * age_mult * geo_mult * pc_mult
  p <- min(p, 0.92)
  stats::runif(1) < p
}

.draw_hytte <- function(income_nok = NULL, county = "", parents_capital = NULL,
                        luxury_primary = FALSE, ref_year = 2026, lang = "en") {
  inc <- if (is.null(income_nok) || is.na(income_nok)) 400000 else income_nok
  pc  <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital

  # Type-fordeling betinget pa primaerfylke + inntekt
  # Typer: "fjell", "innland", "kyst", "kyst_luksus"
  if (county %in% c("Oslo", "Akershus")) {
    # Stor andel fjell-hytter (Norefjell/Hemsedal/Geilo) + kysthytte
    type_probs <- c(fjell = 0.45, innland = 0.10, kyst = 0.30, kyst_luksus = 0.15)
  } else if (county %in% c("Buskerud", "Innlandet", "Telemark")) {
    type_probs <- c(fjell = 0.55, innland = 0.30, kyst = 0.13, kyst_luksus = 0.02)
  } else if (county %in% c("Vestfold", "Agder")) {
    type_probs <- c(fjell = 0.15, innland = 0.10, kyst = 0.55, kyst_luksus = 0.20)
  } else if (county %in% c("Vestland", "Rogaland", "M\u{00f8}re og Romsdal")) {
    type_probs <- c(fjell = 0.30, innland = 0.10, kyst = 0.50, kyst_luksus = 0.10)
  } else if (county %in% c("Nordland", "Troms", "Finnmark")) {
    type_probs <- c(fjell = 0.20, innland = 0.30, kyst = 0.45, kyst_luksus = 0.05)
  } else if (county %in% c("Tr\u{00f8}ndelag")) {
    type_probs <- c(fjell = 0.40, innland = 0.25, kyst = 0.30, kyst_luksus = 0.05)
  } else {
    type_probs <- c(fjell = 0.30, innland = 0.30, kyst = 0.35, kyst_luksus = 0.05)
  }
  # Inntekt skyver mot luksus
  if (inc > 1.0e6 || pc > 10e6 || luxury_primary) {
    type_probs["kyst_luksus"] <- type_probs["kyst_luksus"] + 0.15
    type_probs <- type_probs / sum(type_probs)
  }
  # Lav-inntekt + lav-formue ego skal ikke kunne trekke kyst_luksus
  # (strandeiendom på 10-50 MNOK kan ikke realistisk eies på 400k inntekt)
  if (inc < 700000 && (is.null(pc) || pc < 5e6) && !isTRUE(luxury_primary)) {
    type_probs["kyst_luksus"] <- 0
    sm <- sum(type_probs)
    if (sm > 0) type_probs <- type_probs / sm
  }
  type <- sample(names(type_probs), 1, prob = type_probs)

  # Verdi: lognormal, sterk regional spredning
  val_mu <- switch(type,
    "fjell"       = log(2.8e6),   # median ~2.8 MNOK
    "innland"     = log(1.4e6),   # median ~1.4 MNOK
    "kyst"        = log(3.5e6),   # median ~3.5 MNOK
    "kyst_luksus" = log(15e6),    # median ~15 MNOK, hale opp til 80 MNOK+
    log(2e6))
  val_sd <- switch(type,
    "fjell" = 0.55, "innland" = 0.55, "kyst" = 0.65, "kyst_luksus" = 0.85, 0.55)
  value <- round(exp(stats::rnorm(1, val_mu, val_sd)))

  # Inntekts-skvis: rikere kjoper bedre versjon
  if (inc > 1.5e6) value <- round(value * stats::runif(1, 1.3, 2.0))

  # Lavinntekt-eiere: dempet hytteverdi (typisk arvet hytte uten oppgradering)
  # Unntak: hvis pc > 5e6 (rik forelder gav dyr hytte) eller luxury_primary
  if (inc < 500000 && (is.null(pc) || pc < 5e6) && !isTRUE(luxury_primary)) {
    value <- round(value * stats::runif(1, 0.40, 0.70))
  } else if (inc < 700000 && (is.null(pc) || pc < 3e6)) {
    value <- round(value * stats::runif(1, 0.65, 0.90))
  }

  # Cap
  cap <- if (identical(type, "kyst_luksus")) 150e6 else if (identical(type, "kyst")) 25e6 else 12e6
  value <- min(as.integer(value), as.integer(cap))
  # Hytte > 3x primaer er urealistisk uten luxury_primary eller pc > 10M
  # (denne info propageres ikke direkte hit — implementer cap i kallende kode)

  type_label <- if (identical(lang, "no")) {
    switch(type, "fjell" = "Fjellhytte", "innland" = "Innlandshytte",
           "kyst" = "Kysthytte", "kyst_luksus" = "Strandeiendom", type)
  } else {
    switch(type, "fjell" = "Mountain cabin", "innland" = "Inland cabin",
           "kyst" = "Coastal cabin", "kyst_luksus" = "Waterfront property", type)
  }

  list(type = type_label, value = as.integer(value))
}


# --- Wealth (formue) conditional draw -------------------------------------
# Returns list(net_wealth_nok, financial_assets_nok, business_equity_nok,
#              capital_income_nok, wealth_class).
# Conditional on age + income + parents_capital + (optional) housing_equity & hytte_value.
# Built from SSB 08589 (age x bracket) + 10318 (deciles + top 5%/1%/0.1%).

.cond_wealth <- function(age,
                         income_nok = NULL,
                         housing_equity_nok = 0,
                         hytte_value_nok = 0,
                         parents_capital = NULL,
                         parents_both_dead = FALSE,
                         n_siblings = 0,
                         gender = NULL,
                         lang = "en") {
  # NA_real_, ikke NA_integer_: formuesfeltene er double fordi kronebelop
  # i toppsjiktet overstiger R sitt heltallstak.
  na_result <- list(
    net_wealth_nok = NA_real_,
    financial_assets_nok = NA_real_,
    business_equity_nok = NA_real_,
    capital_income_nok = NA_real_,
    inheritance_nok = NA_real_,
    wealth_class = NA_character_
  )
  if (is.null(age) || is.na(age)) return(na_result)
  .load_data()
  wba <- .cfm_env$wealth_by_age
  wd  <- .cfm_env$wealth_deciles
  if (is.null(wba) || is.null(wd)) return(na_result)

  # 1. Pick age-conditional bracket from SSB 08589 (per person, 17+)
  band <- .wealth_age_band(age)
  if (is.na(band)) return(na_result)
  sub <- wba[wba$age_band == band, , drop = FALSE]
  if (nrow(sub) == 0) return(na_result)

  # Adjust bracket weights for income + parents_capital (push high-inc + rich-parent up)
  inc <- if (is.null(income_nok) || is.na(income_nok)) 400000 else income_nok
  pc  <- if (is.null(parents_capital) || is.na(parents_capital)) 0 else parents_capital
  w <- as.numeric(sub$count)
  brackets <- sub$bracket_code
  # Indices of top brackets
  top_idx <- which(brackets %in% c("09", "10+"))
  mid_idx <- which(brackets %in% c("07", "08"))
  low_idx <- which(brackets %in% c("00-", "01"))

  if (inc > 800000)  { w[top_idx] <- w[top_idx] * 1.6; w[mid_idx] <- w[mid_idx] * 1.2 }
  if (inc > 1.5e6)   { w[top_idx] <- w[top_idx] * 2.0 }
  if (inc < 250000)  { w[low_idx] <- w[low_idx] * 1.4; w[top_idx] <- w[top_idx] * 0.5 }
  if (pc > 5e6)      { w[top_idx] <- w[top_idx] * 1.8 }
  if (pc > 25e6)     { w[top_idx] <- w[top_idx] * 3.0 }
  w <- pmax(w, 0)
  w <- w / sum(w)
  bracket <- sample(brackets, 1, prob = w)

  # 2. Sample within bracket
  bracket_range <- .wealth_bracket_range(bracket)
  lo <- bracket_range[1]; hi <- bracket_range[2]

  # 3. If top bracket "10+" (2M+), escalate to top 5/1/0.1 % using 10318
  wealth_class <- bracket
  if (identical(bracket, "10+")) {
    # Probability of being in each top tier given bracket 10+:
    # In 67+ band, 28.9% are 2M+; top 1% of all is 1% so within 2M+ ~3.5%.
    # We use a stylized escalation:
    # 50% stay in 2M-12M (top 10-5%), 35% in 12-28M (top 5-1%),
    # 12% in 28-133M (top 1-0.1%), 3% in 133M+ (top 0.1%)
    tier_p <- c(rest = 0.68, top5 = 0.25, top1 = 0.05, top01 = 0.007)
    # Income/pc shift (smaller bumps)
    if (inc > 1.5e6) { tier_p["top5"] <- tier_p["top5"] + 0.03; tier_p["top1"] <- tier_p["top1"] + 0.015 }
    if (inc > 3e6)   { tier_p["top1"] <- tier_p["top1"] + 0.03; tier_p["top01"] <- tier_p["top01"] + 0.008 }
    if (pc > 25e6)   { tier_p["top1"] <- tier_p["top1"] + 0.03; tier_p["top01"] <- tier_p["top01"] + 0.015 }
    if (pc > 100e6)  { tier_p["top01"] <- tier_p["top01"] + 0.07 }
    tier_p <- tier_p / sum(tier_p)
    tier <- sample(names(tier_p), 1, prob = tier_p)
    wealth_class <- tier
    cuts <- .wealth_top_tier(tier)
    lo <- cuts[1]; hi <- cuts[2]
  }

  # Lognormal within range, parameterized so mode sits ~0.3-0.4 through range
  if (identical(bracket, "00-")) {
    nw <- NA_real_  # negativ formue: settes i egen blokk nedenfor
  } else if (is.finite(hi)) {
    # Truncated lognormal-ish: just sample uniformly in log space
    log_lo <- log(max(1, lo))
    log_hi <- log(hi)
    nw <- exp(stats::runif(1, log_lo, log_hi))
    # Push slightly toward bottom of bracket (most are near floor)
    nw <- nw * stats::runif(1, 0.85, 1.05)
  } else {
    # Top tier 0.1% — Pareto with alpha ~1.2
    alpha <- 1.6
    u <- stats::runif(1)
    nw <- lo / (u^(1 / alpha))
    nw <- min(nw, 3e9)  # cap at 3 mrd
  }
  # Compute real estate wealth first (used to gate negative-wealth bracket)
  re_wealth <- (if (is.null(housing_equity_nok) || is.na(housing_equity_nok)) 0L
                else as.numeric(housing_equity_nok)) +
               (if (is.null(hytte_value_nok) || is.na(hytte_value_nok)) 0L
                else as.numeric(hytte_value_nok))

  # Bracket "00-" (≤0): negative, typically student loans / billån / kreditt.
  # But if ego has substantial real estate equity, pure negative wealth is
  # implausible — they have collateral. Replace with: equity minus moderate debt.
  if (identical(bracket, "00-")) {
    if (re_wealth > 200000L) {
      # Boligeier: nettoformue = real estate equity - extra forbruksgjeld
      extra_debt <- exp(stats::runif(1, log(50000), log(500000)))
      nw <- as.numeric(re_wealth) - extra_debt
      if (nw < 0) nw <- as.numeric(re_wealth) * stats::runif(1, 0.3, 0.7)
    } else {
      nw <- -1 * exp(stats::runif(1, log(50000), log(1500000)))
    }
  }

  net_wealth <- round(nw)
  # Formue holdes som double, ikke integer: for topp 0,1 % overstiger
  # kronebelopet R sitt heltallstak (2 147 483 647), og as.integer() ga da
  # NA med advarsel -- som i neste linje ble til "missing value where
  # TRUE/FALSE needed". Vakten her fanger enhver annen vei til NA.
  if (!is.finite(net_wealth)) net_wealth <- as.numeric(re_wealth)

  # 4. Decompose: housing equity + hytte are given, residual is financial+business
  residual <- net_wealth - re_wealth

  # If residual is negative but net_wealth is moderate, treat re_wealth as the anchor
  # and shift up net_wealth a bit.
  if (residual < 0 && net_wealth > 0) {
    net_wealth <- round(re_wealth + max(0, residual + abs(residual) * 0.3))
    residual <- net_wealth - re_wealth
  }

  business_equity <- 0L
  financial <- max(0, round(residual))

  # Inheritance: if both parents dead, ego received share of parents_capital
  # (Norway has no inheritance tax since 2014, full transfer)
  inheritance <- 0L
  if (isTRUE(parents_both_dead) && !is.null(parents_capital) &&
      !is.na(parents_capital) && parents_capital > 0) {
    n_kids <- max(1, as.integer(n_siblings) + 1L)
    inheritance <- round(parents_capital / n_kids)
    # Add to financial assets
    financial <- financial + inheritance
    net_wealth <- round(net_wealth + inheritance)
    # Inheritance can push ego into a higher wealth class
    if (net_wealth >= 133e6) wealth_class <- "top01"
    else if (net_wealth >= 28e6 && !(wealth_class %in% c("top01"))) wealth_class <- "top1"
    else if (net_wealth >= 12e6 && !(wealth_class %in% c("top1", "top01"))) wealth_class <- "top5"
  }

  # Top-tier business equity: most of the wealth is næringsformue
  if (wealth_class %in% c("top1", "top01")) {
    biz_share <- if (wealth_class == "top01") stats::runif(1, 0.65, 0.85)
                 else stats::runif(1, 0.45, 0.75)
    business_equity <- round(residual * biz_share)
    financial <- max(0, round(residual - business_equity))
  } else if (wealth_class == "top5" && residual > 5e6) {
    biz_share <- stats::runif(1, 0.20, 0.45)
    business_equity <- round(residual * biz_share)
    financial <- max(0, round(residual - business_equity))
  }

  # 5. Capital income: utbytte + renter
  # Non-business: ~2-4% of financial assets (interest + small dividends)
  # Business equity: ~3-7% utbytte (highly variable, often re-invested)
  cap_income <- 0
  if (financial > 0) {
    cap_income <- cap_income + financial * stats::runif(1, 0.015, 0.040)
  }
  if (business_equity > 0) {
    # Some years 0, some years large. Median ~3% of equity.
    if (stats::runif(1) < 0.55) {
      cap_income <- cap_income + business_equity * stats::runif(1, 0.020, 0.080)
    }
  }
  cap_income <- round(cap_income, -3)

  list(
    net_wealth_nok = net_wealth,
    financial_assets_nok = financial,
    business_equity_nok = business_equity,
    capital_income_nok = cap_income,
    inheritance_nok = inheritance,
    wealth_class = wealth_class
  )
}

.wealth_age_band <- function(age) {
  if (is.na(age)) return(NA_character_)
  if (age < 17) return(NA_character_)
  if (age <= 24) "17-24"
  else if (age <= 34) "25-34"
  else if (age <= 44) "35-44"
  else if (age <= 54) "45-54"
  else if (age <= 66) "55-66"
  else "67+"
}

.wealth_bracket_range <- function(code) {
  switch(code,
    "00-" = c(-2e6, 0),
    "01"  = c(1, 99999),
    "02"  = c(100000, 199999),
    "03"  = c(200000, 299999),
    "04"  = c(300000, 399999),
    "05"  = c(400000, 499999),
    "07"  = c(500000, 749999),
    "08"  = c(750000, 999999),
    "09"  = c(1e6, 1.999999e6),
    "10+" = c(2e6, 12e6),  # default if not escalated
    c(0, 1)
  )
}

.wealth_top_tier <- function(tier) {
  switch(tier,
    "rest"  = c(2e6, 12.14e6),       # top 10-5 %
    "top5"  = c(12.14e6, 28.25e6),   # top 5-1 %
    "top1"  = c(28.25e6, 133.4e6),   # top 1-0.1 %
    "top01" = c(133.4e6, Inf),       # top 0.1 %
    c(2e6, 12e6)
  )
}

# --- Immigrant background + country -------------------------------------
# Returns list(background, country, country_label, name_region, years_in_norway).
# background is one of: "majority", "second_gen", "first_gen".
# Norway 2026 (SSB 09817): innvandrere ~17.5%, andre_gen ~4.2%, majority ~78.3%.

.cond_immigrant_background <- function(age, lang = "en") {
  na_result <- list(
    background = "majority",
    country = NA_character_,
    country_label = NA_character_,
    name_region = "norden",   # majority => Norwegian names
    years_in_norway = NA_integer_
  )
  # Sample background — slightly age-conditioned (younger = more 2nd gen)
  age_2g_mult <- if (age < 18) 1.6
                 else if (age < 30) 1.4
                 else if (age < 50) 0.9
                 else 0.3
  age_1g_mult <- if (age < 18) 0.5
                 else if (age < 65) 1.1
                 else 0.6

  p_majority <- 0.783
  p_first <- 0.175 * age_1g_mult
  p_second <- 0.042 * age_2g_mult
  s <- p_majority + p_first + p_second
  probs <- c(majority = p_majority, first_gen = p_first, second_gen = p_second) / s
  bg <- sample(names(probs), 1, prob = probs)
  if (identical(bg, "majority")) return(na_result)

  .load_data()
  cd <- .cfm_env$immigrant_country_dist
  if (is.null(cd) || nrow(cd) == 0) return(na_result)

  col <- if (identical(bg, "first_gen")) "innvandrere" else "andre_gen"
  w <- cd[[col]]
  if (is.null(w) || sum(w, na.rm = TRUE) == 0) return(na_result)

  # Historical plausibility filter: ego's age must fit the immigration window
  # for the country. Person needs to have been able to arrive >= start_year and
  # be alive today. Allow arrival age up to ~50.
  isy <- .cfm_env$immigration_start_year
  if (!is.null(isy)) {
    ref_year <- .cfm_env$ref_year
    ego_birth_year <- ref_year - age
    plausible <- vapply(seq_len(nrow(cd)), function(i) {
      cc <- cd$code[i]
      sy_row <- isy[isy$code == cc, , drop = FALSE]
      sy <- if (nrow(sy_row) > 0) as.integer(sy_row$start_year[1]) else 1900L
      if (identical(bg, "first_gen")) {
        # Ego must have been able to arrive in [sy, ref_year] at a
        # plausible age. Earliest possible arrival = max(sy, ego_birth_year),
        # latest = ref_year - 1. Arrival age must fall in [0, 45]: migration
        # after the mid-forties is rare enough that allowing it up to 60
        # produced people who "immigrated" at 49 from countries whose flow
        # to Norway only began in the 2000s.
        earliest_arrival <- max(sy, ego_birth_year)
        latest_arrival <- ref_year - 1L
        if (earliest_arrival > latest_arrival) return(FALSE)
        # Arrival age constraint
        earliest_age <- max(0L, earliest_arrival - ego_birth_year)
        if (earliest_age > 45L) return(FALSE)
        return(TRUE)
      } else {  # second_gen — parents arrived >= start_year, ego born in Norway
        # Ego (born in Norway) must have been born after parents arrived.
        # Parents arrived at age >= 16, so earliest ego birth = sy + 16
        # Allow some slack — sy+10
        return(ego_birth_year >= sy + 10L)
      }
    }, logical(1))
    w <- w * as.numeric(plausible)
    if (sum(w, na.rm = TRUE) == 0) return(na_result)  # no plausible country
  }

  idx <- sample(seq_len(nrow(cd)), 1, prob = w)
  country_code <- cd$code[idx]
  country_label <- cd$label[idx]
  name_region <- cd$name_region[idx]

  # Botid: only meaningful for first_gen
  yrs_in_norway <- NA_integer_
  if (identical(bg, "first_gen")) {
    ref_year <- .cfm_env$ref_year
    # start_year bounds how long anyone can have been here.
    sy_row <- if (!is.null(.cfm_env$immigration_start_year))
                .cfm_env$immigration_start_year[
                  .cfm_env$immigration_start_year$code == country_code, , drop = FALSE]
              else NULL
    start_year <- if (!is.null(sy_row) && nrow(sy_row) > 0)
                    as.integer(sy_row$start_year[1]) else 1970L

    # Botid bounded by min(age, ref_year - start_year)
    max_botid <- min(as.integer(age), ref_year - start_year)
    if (max_botid < 1) max_botid <- 1L
    # peak_year is no longer used to centre the draw. It shaped residence
    # length directly, which is what forced implausible arrival ages; the
    # hard constraint that matters -- nobody can have lived here longer
    # than the flow has existed -- is already carried by max_botid.
    # Arrival age is the quantity that has to be plausible; residence
    # length follows from it. The earlier version drew residence from the
    # country's peak year and checked arrival age afterwards, which meant
    # the check could only clamp -- Pakistani first-gen piled up on the
    # floor of the window at exactly 15.
    #
    # The flow is a property of the country, not the region:
    # mena_sor_asia holds both 1960s labour migration (Pakistan, Turkey,
    # Morocco) and refugee movements (Syria, Iraq, Afghanistan).
    #
    #   labour   young adults, a fifth arriving as accompanied children
    #   mixed    labour wave first, then family reunification, which
    #            brought spouses and teenagers but few small children
    #   refugee  whole families, infants to middle age
    profile <- if (!is.null(sy_row) && nrow(sy_row) > 0 &&
                   "arrival_profile" %in% names(sy_row)) {
      as.character(sy_row$arrival_profile[1])
    } else if (name_region %in% c("norden", "vesteuropa", "ost_europa")) {
      "labour"
    } else "refugee"

    .draw_arrival_age <- function() {
      if (identical(profile, "labour")) {
        if (stats::runif(1) < 0.18) {
          max(0L, as.integer(round(stats::rnorm(1, 10, 6))))
        } else {
          min(40L, max(18L, as.integer(round(stats::rnorm(1, 27, 6)))))
        }
      } else if (identical(profile, "mixed")) {
        if (stats::runif(1) < 0.55) {
          min(40L, max(18L, as.integer(round(stats::rnorm(1, 26, 6)))))
        } else {
          min(40L, max(0L, as.integer(round(stats::rnorm(1, 14, 7)))))
        }
      } else {
        min(45L, max(0L, as.integer(round(stats::rnorm(1, 24, 12)))))
      }
    }

    age_i <- as.integer(age)
    yrs_in_norway <- NA_integer_
    for (attempt in seq_len(12L)) {
      cand <- age_i - .draw_arrival_age()
      if (cand >= 1L && cand <= max_botid) { yrs_in_norway <- as.integer(cand); break }
    }
    if (is.na(yrs_in_norway)) {
      # No arrival age satisfies both the window and the country's flow
      # history. Take the nearest feasible residence length rather than
      # an implausible arrival age.
      cand <- age_i - .draw_arrival_age()
      yrs_in_norway <- as.integer(max(1L, min(max_botid, cand)))
    }
  }

  list(
    background = bg,
    country = country_code,
    country_label = country_label,
    name_region = name_region,
    years_in_norway = yrs_in_norway
  )
}

# Background-aware first name: returns a name string or NULL to fall back to default
.draw_name_by_background <- function(gender, background, name_region) {
  if (is.null(background) || identical(background, "majority")) return(NULL)
  .load_data()
  nbr <- .cfm_env$names_by_region
  if (is.null(nbr) || nrow(nbr) == 0) return(NULL)
  # For second_gen: 70% region, 30% Norwegian
  if (identical(background, "second_gen") && stats::runif(1) < 0.30) return(NULL)
  pool <- nbr[nbr$region == name_region & nbr$gender == gender, , drop = FALSE]
  if (nrow(pool) == 0) return(NULL)
  pool$name[sample.int(nrow(pool), 1)]
}

# Background-aware modifiers — applied to existing draws
.background_income_factor <- function(background, name_region, years_in_norway = NA) {
  if (is.null(background) || identical(background, "majority")) return(1.0)
  if (identical(background, "second_gen")) {
    # Andre-generasjon nesten lik majoritet, fortsatt liten gap (~95-98%)
    return(if (name_region %in% c("norden","vesteuropa")) 1.00 else 0.95)
  }
  # first_gen: stronger gap, modulated by botid + region
  base <- switch(name_region,
    "norden" = 1.00,
    "vesteuropa" = 1.05,           # often skilled migrants
    "ost_europa" = 0.78,
    "mena_sor_asia" = 0.70,
    "afrika_sub" = 0.62,
    "ost_asia" = 0.78,
    "latam_filippin" = 0.78,
    0.80
  )
  # Botid effect: longer = closer to majority
  if (!is.na(years_in_norway)) {
    botid_boost <- min(0.20, years_in_norway / 100)
    base <- base + botid_boost
  }
  max(0.4, min(1.10, base))
}

.background_oslo_boost <- function(background, name_region) {
  if (is.null(background) || identical(background, "majority")) return(1.0)
  # Strong Oslo+Akershus concentration for non-Western
  switch(name_region,
    "norden" = 1.10,
    "vesteuropa" = 1.30,
    "ost_europa" = 1.40,
    "mena_sor_asia" = 2.20,
    "afrika_sub" = 2.50,
    "ost_asia" = 1.40,
    "latam_filippin" = 1.40,
    1.20
  )
}

# --- Background-aware adjustments (innvandrerbakgrunn v2) ----------------
# Stylized adjustments capturing aggregate patterns from SSB.
# Sensitivity note: these are POPULATION-level shifts, not individual
# predictions. Each draw can land anywhere in the resulting distribution.

# Education shift: bumps edu_code up or down based on region of origin.
# Captures: refugee origin tends lower formal edu (with high variance);
# Western/India IT migration tends higher; second-gen mobility ("paradox effect").
.background_edu_shift <- function(edu_code, background, name_region, age) {
  if (is.null(background) || identical(background, "majority")) return(edu_code)
  if (is.null(edu_code) || is.na(edu_code) || edu_code < 0) return(edu_code)
  if (is.null(name_region) || is.na(name_region)) return(edu_code)

  if (identical(background, "first_gen")) {
    region_shift <- switch(name_region,
      "afrika_sub"     = -1.6,    # mostly refugee origins
      "mena_sor_asia"  = -0.6,    # mixed: India IT high, Syria/Afghanistan low
      "ost_europa"     = -0.4,    # mostly labor migration
      "vesteuropa"     = +0.6,    # often skilled migration
      "ost_asia"       = +0.1,
      "latam_filippin" = -0.1,
      "norden"         =  0.0,
      0
    )
    # Higher variance for refugee regions
    sd <- if (name_region %in% c("afrika_sub", "mena_sor_asia")) 1.1
          else if (name_region == "ost_europa") 0.8
          else 0.7
  } else {  # second_gen — innvandrerparadokset
    region_shift <- switch(name_region,
      "afrika_sub"     = -0.2,    # less mobility than other groups
      "mena_sor_asia"  = +0.5,    # paradox: pakistanske/lankiske 2.gen høy
      "ost_asia"       = +0.6,    # vietnamesiske/kinesiske 2.gen høy
      "ost_europa"     =  0.0,
      "vesteuropa"     = +0.3,
      "latam_filippin" = +0.1,
      "norden"         =  0.0,
      0
    )
    sd <- 0.6
  }

  raw_shift <- region_shift + stats::rnorm(1, 0, sd)
  new_code <- round(edu_code + raw_shift)
  max(0L, min(7L, as.integer(new_code)))
}

# Occupation segregation: with some probability, swap to a stereotypically
# overrepresented STYRK for the origin region. Captures real labour market patterns.
.background_occupation_shift <- function(styrk_code, edu_code, background, name_region) {
  if (is.null(background) || !identical(background, "first_gen")) return(styrk_code)
  if (is.null(name_region) || is.na(name_region)) return(styrk_code)
  if (is.null(styrk_code) || is.na(styrk_code) || !nzchar(styrk_code)) return(styrk_code)

  # Probability of shifting to a typical occupation
  prob <- switch(name_region,
    "ost_europa"     = 0.30,
    "mena_sor_asia"  = 0.30,
    "afrika_sub"     = 0.40,
    "ost_asia"       = 0.25,
    "latam_filippin" = 0.30,
    "vesteuropa"     = 0.10,
    "norden"         = 0.05,
    0.10
  )
  if (stats::runif(1) > prob) return(styrk_code)

  typical <- switch(name_region,
    "ost_europa"     = c("71","72","83","91","51"),  # håndverk, sjåfør, renhold, salg
    "mena_sor_asia"  = c("83","51","91","53"),       # sjåfør, salg, renhold, pleie
    "afrika_sub"     = c("83","53","91","51"),       # sjåfør, pleie, renhold, salg
    "ost_asia"       = c("53","51","91","83"),       # pleie, salg, renhold
    "latam_filippin" = c("53","51","91","83"),
    "vesteuropa"     = c("21","25","26","31","23"),  # ingeniør, IT, akademiker
    "norden"         = c("33","34","53","31"),
    NULL
  )
  if (is.null(typical)) return(styrk_code)

  # Edu gating: if edu >= 6, "vesteuropa" stays high; others rarely demoted unless overkvalifisert-effekt
  if (!is.null(edu_code) && !is.na(edu_code) && edu_code >= 6 &&
      name_region %in% c("vesteuropa", "norden")) {
    return(styrk_code)
  }
  # Pick from typical
  pick <- sample(typical, 1)
  pick
}

# Overqualified ("brain waste") — high-edu first_gen non-Western
# may end up in lower-status occupations
.background_overqualified <- function(edu_code, styrk_code, background, name_region) {
  if (is.null(background) || !identical(background, "first_gen")) return(styrk_code)
  if (is.null(edu_code) || is.na(edu_code) || edu_code < 5) return(styrk_code)
  if (!name_region %in% c("mena_sor_asia", "afrika_sub")) return(styrk_code)
  if (is.null(styrk_code) || !nzchar(styrk_code)) return(styrk_code)
  # High-skill STYRK already? leave alone
  if (substr(styrk_code, 1, 1) %in% c("1","2","3")) {
    if (stats::runif(1) < 0.30) {
      # Brain waste demotion
      return(sample(c("51","53","83","91"), 1))
    }
  }
  styrk_code
}

# --- Sexual orientation (LHBT+) -----------------------------------------
# ~7% LHBT+ in Norwegian surveys (Bufdir / SSB Levekårsundersøkelse).
# Younger cohorts report higher (more openness, not necessarily more prevalence).
.cond_orientation <- function(age, gender = NULL, lang = "en") {
  # Skip for children — orientation is not meaningful before adolescence
  if (is.null(age) || is.na(age) || age < 16) {
    return(list(code = NA_character_, label = NA_character_))
  }
  birth_year <- .cfm_env$ref_year - age
  # Cohort: younger = more reported LHBT+
  p_lhbt <- if (birth_year >= 2000) 0.12
            else if (birth_year >= 1985) 0.08
            else if (birth_year >= 1970) 0.05
            else 0.03

  if (stats::runif(1) >= p_lhbt) {
    return(list(code = "hetero",
                label = if (identical(lang, "no")) "Heterofil" else "Heterosexual"))
  }
  # Within LHBT+ — split into concrete identities, gender-conditioned:
  # menn rapporterer oftere homofil, kvinner oftere bifil (SSB/levekår).
  fem <- identical(toupper(gender %||% ""), "F")
  man <- identical(toupper(gender %||% ""), "M")
  gay_cut <- if (man) 0.55 else if (fem) 0.22 else 0.40
  bi_cut  <- if (man) 0.78 else if (fem) 0.82 else 0.80
  r <- stats::runif(1)
  if (r < gay_cut) {
    return(list(code = "gay",
                label = if (identical(lang, "no")) "Homofil" else "Gay/Lesbian"))
  }
  if (r < bi_cut) {
    return(list(code = "bi",
                label = if (identical(lang, "no")) "Bifil" else "Bisexual"))
  }
  if (r < bi_cut + 0.08) {
    return(list(code = "queer",
                label = if (identical(lang, "no")) "Queer" else "Queer"))
  }
  if (r < bi_cut + 0.13) {
    return(list(code = "pan",
                label = if (identical(lang, "no")) "Panseksuell" else "Pansexual"))
  }
  if (r < bi_cut + 0.17) {
    return(list(code = "ace",
                label = if (identical(lang, "no")) "Aseksuell" else "Asexual"))
  }
  list(code = "uavklart",
       label = if (identical(lang, "no")) "Usikker / utforskende" else "Unsure / questioning")
}

# --- Religion ------------------------------------------------------------
# Trekker fra region-betinget fordeling hvis ikke majority, ellers baseline.
# Religion weights for a specific country of origin.
#
# The region taxonomy is too coarse for this one purpose. `mena_sor_asia`
# runs from Morocco to Nepal, so its 8 % Hindu share -- which comes from
# India, Nepal and Sri Lanka -- was being applied to Lebanon. And
# `afrika_sub` carries 55 % Islam, which makes Ethiopia, Eritrea, Kenya,
# Uganda, Ghana, Rwanda and Angola Muslim-majority when every one of them
# is Christian-majority.
#
# The taxonomy itself is deliberately left alone: `name_region` is the key
# into names_by_region.csv and drives the income, occupation and turnout
# adjustments. Only religion is conditioned on the country, and only when
# the country is known; otherwise the region distribution still applies.
.religion_country_weights <- function(country_label) {
  if (is.null(country_label) || length(country_label) != 1 ||
      is.na(country_label) || !nzchar(country_label)) return(NULL)
  .load_data()
  rbc <- .cfm_env$religion_by_country
  if (is.null(rbc) || !nrow(rbc)) return(NULL)
  hit <- which(tolower(trimws(rbc$label)) == tolower(trimws(country_label)))
  if (!length(hit)) return(NULL)
  codes <- setdiff(names(rbc), c("code", "label"))
  w <- as.numeric(rbc[hit[1], codes])
  names(w) <- codes
  if (!any(is.finite(w)) || sum(w, na.rm = TRUE) <= 0) return(NULL)
  w
}

.cond_religion <- function(age, name_region = NULL, background = "majority",
                           gender = NULL, country_label = NULL, lang = "en") {
  .load_data()
  rb <- .cfm_env$religion_baseline
  rbr <- .cfm_env$religion_by_region
  if (is.null(rb)) return(list(code = NA_character_, label = NA_character_))

  # Pick weights: country when we know it, region when we do not,
  # majority baseline otherwise.
  w <- NULL
  if (!identical(background, "majority")) {
    w <- .religion_country_weights(country_label)
    if (is.null(w) && !is.null(name_region) && !is.na(name_region) &&
        !is.null(rbr) && name_region %in% rbr$region) {
      row <- rbr[rbr$region == name_region, , drop = FALSE]
      codes <- setdiff(names(row), "region")
      w <- as.numeric(row[1, codes])
      names(w) <- codes
    }
  }
  if (is.null(w)) w <- setNames(rb$share_majority, rb$code)

  # Age effect: younger more secular
  birth_year <- .cfm_env$ref_year - age
  if (birth_year >= 2000) {
    if ("INGEN" %in% names(w)) w["INGEN"] <- w["INGEN"] * 1.6
    if ("DnK" %in% names(w))   w["DnK"]   <- w["DnK"] * 0.7
  } else if (birth_year < 1940) {
    # Church of Norway membership was close to universal in the pre-war
    # cohorts. x1.2 against a ~50 % baseline gave 60 %, which made a
    # 93-year-old look like a 2020s Norwegian.
    if ("DnK" %in% names(w))   w["DnK"]   <- w["DnK"] * 6.0
    if ("INGEN" %in% names(w)) w["INGEN"] <- w["INGEN"] * 0.12
    if ("HUM" %in% names(w))   w["HUM"]   <- w["HUM"] * 0.2
  } else if (birth_year < 1955) {
    if ("DnK" %in% names(w))   w["DnK"]   <- w["DnK"] * 2.6
    if ("INGEN" %in% names(w)) w["INGEN"] <- w["INGEN"] * 0.3
    if ("HUM" %in% names(w))   w["HUM"]   <- w["HUM"] * 0.5
  }
  # Kjønn: kvinner litt oftere religiøst tilknyttet, menn oftere uten.
  if (!is.null(gender) && !is.na(gender)) {
    if (identical(toupper(gender), "F")) {
      if ("INGEN" %in% names(w)) w["INGEN"] <- w["INGEN"] * 0.85
      for (cc in c("DnK","KAT","ANN_KRIS")) if (cc %in% names(w)) w[cc] <- w[cc] * 1.12
    } else {
      if ("INGEN" %in% names(w)) w["INGEN"] <- w["INGEN"] * 1.18
      if ("HUM" %in% names(w))   w["HUM"]   <- w["HUM"]   * 1.20
      for (cc in c("DnK","KAT","ANN_KRIS")) if (cc %in% names(w)) w[cc] <- w[cc] * 0.92
    }
  }
  w <- pmax(w, 0)
  w <- w / sum(w)
  pick <- sample(names(w), 1, prob = w)

  # Find label
  row <- rb[rb$code == pick, , drop = FALSE]
  lbl <- if (nrow(row) > 0) {
    if (identical(lang, "no")) row$label_no[1] else row$label_en[1]
  } else pick
  # For children (under 14), reframe label as parental affiliation rather than active practice
  if (!is.null(age) && !is.na(age) && age < 14) {
    lbl <- .child_religion_label(pick, lang)
  } else if (identical(lang, "no") && stats::runif(1) < 0.12) {
    # ~12% sannsynlighet for humoristisk variant (kun voksne, kun norsk)
    rh <- .cfm_env$religion_humor
    if (!is.null(rh)) {
      pool <- rh[rh$code == pick, , drop = FALSE]
      if (nrow(pool) > 0) lbl <- pool$label[sample.int(nrow(pool), 1)]
    }
  }
  list(code = pick, label = lbl)
}

.child_religion_label <- function(code, lang = "en") {
  no <- identical(lang, "no")
  switch(code,
    "DnK" = if (no) "D\u{00f8}pt i Den norske kirke" else "Baptized in Church of Norway",
    "KAT" = if (no) "D\u{00f8}pt katolsk" else "Baptized Catholic",
    "ANN_KRIS" = if (no) "D\u{00f8}pt i annet kristent samfunn" else "Baptized other Christian",
    "ISL" = if (no) "Tilh\u{00f8}rer islam (foreldrenes valg)" else "Muslim (parents' affiliation)",
    "HUM" = if (no) "Navnefest i Human-Etisk Forbund" else "Naming ceremony, Humanist",
    "BUD" = if (no) "Tilh\u{00f8}rer buddhisme (foreldrenes valg)" else "Buddhist (parents' affiliation)",
    "HIN" = if (no) "Tilh\u{00f8}rer hinduisme (foreldrenes valg)" else "Hindu (parents' affiliation)",
    "JOD" = if (no) "Tilh\u{00f8}rer mosaisk trossamfunn" else "Jewish (parents' affiliation)",
    "ANN" = if (no) "Foreldrenes tradisjon" else "Parents' tradition",
    "INGEN" = if (no) "Ingen registrert tilhorighet" else "No registered religion",
    code
  )
}

# --- Party preference ---------------------------------------------------
# Trekker fra base 2025-fordeling, modifisert av: yrke (klasse), geografi
# (urban/rural via mun_pop), alder, religion (KrF for kristne).
.cond_party <- function(age, edu_code = NULL, styrk_code = NULL, mun_pop = NULL,
                        county = NULL, religion_code = NULL,
                        background = "majority", name_region = NULL,
                        income_nok = NULL, gender = NULL, lang = "en") {
  .load_data()
  pb <- .cfm_env$party_baseline
  if (is.null(pb)) return(list(code = NA_character_, label = NA_character_))
  if (age < 18) return(list(code = NA_character_, label = NA_character_))

  # --- Turnout: hill-climb-kalibrert mot SSB 2021 (MAE 0.0125 over 17 profiler).
  # 14/17 innen 2pp; 3 outliers (grunn/hoy kort/hoy lang) skyldes at SSB
  # marginale tall ikke kan matches eksakt med multiplikative parametere
  # uten joint distribusjon. Akseptert tradeoff.
  p_vote <- if (age < 22) 0.743
            else if (age < 30) 0.703
            else if (age < 45) 0.761
            else if (age < 55) 0.796
            else if (age < 67) 0.797
            else if (age < 80) 0.886
            else 0.851
  if (!is.null(edu_code) && !is.na(edu_code)) {
    p_vote <- p_vote * (
      if (edu_code <= 1) 0.893
      else if (edu_code == 2) 0.948
      else if (edu_code <= 4) 0.997
      else if (edu_code <= 6) 1.091
      else 1.180)
  }
  if (!is.null(income_nok) && !is.na(income_nok)) {
    if (income_nok < 200000) p_vote <- p_vote * 0.930
    else if (income_nok > 800000) p_vote <- p_vote * 1.040
  }
  if (!is.null(background) && !identical(background, "majority")) {
    if (identical(background, "first_gen")) {
      p_vote <- p_vote * (
        if (identical(name_region, "norden")) 0.922
        else if (identical(name_region, "vesteuropa")) 0.782
        else if (identical(name_region, "ost_europa")) 0.707
        else if (identical(name_region, "mena_sor_asia")) 0.620
        else if (identical(name_region, "afrika_sub")) 0.637
        else 0.780)
    } else {
      p_vote <- p_vote * 0.928
    }
  }
  p_vote <- max(0.10, min(0.97, p_vote))
  if (stats::runif(1) >= p_vote) {
    si_lbl <- if (identical(lang, "no")) "Stemte ikke" else "Did not vote"
    if (identical(lang, "no") && stats::runif(1) < 0.40) {
      # Higher humor rate for non-voters (more interesting stories there)
      ph <- .cfm_env$party_humor
      if (!is.null(ph)) {
        pool <- ph[ph$code == "STEMTE_IKKE", , drop = FALSE]
        if (nrow(pool) > 0) si_lbl <- pool$label[sample.int(nrow(pool), 1)]
      }
    }
    return(list(code = "STEMTE_IKKE", label = si_lbl))
  }

  w <- setNames(pb$share, pb$code)
  styrk1 <- if (!is.null(styrk_code) && !is.na(styrk_code) && nzchar(styrk_code))
              substr(as.character(styrk_code), 1, 1) else NA

  # Class/occupation effect
  if (!is.na(styrk1)) {
    if (styrk1 %in% c("1","2")) {
      # Ledere/akademikere: H, V, MDG, AP up; Frp, SP down
      w["H"]   <- w["H"]   * 1.50
      w["V"]   <- w["V"]   * 1.80
      w["MDG"] <- w["MDG"] * 1.40
      w["AP"]  <- w["AP"]  * 1.10
      w["FRP"] <- w["FRP"] * 0.55
      w["SP"]  <- w["SP"]  * 0.50
    } else if (styrk1 %in% c("7","8")) {
      # Faglaerte arbeidere: AP, Frp opp; H, V ned
      w["AP"]  <- w["AP"]  * 1.40
      w["FRP"] <- w["FRP"] * 1.40
      w["H"]   <- w["H"]   * 0.70
      w["V"]   <- w["V"]   * 0.50
      w["R"]   <- w["R"]   * 1.20
    } else if (styrk1 %in% c("5","9")) {
      # Service / ufaglaerte: AP, R, Frp
      w["AP"]  <- w["AP"]  * 1.30
      w["R"]   <- w["R"]   * 1.30
      w["FRP"] <- w["FRP"] * 1.20
      w["H"]   <- w["H"]   * 0.70
    } else if (styrk1 %in% c("6")) {
      # Bonder/fiskere: SP, Frp
      w["SP"]  <- w["SP"]  * 4.0
      w["FRP"] <- w["FRP"] * 1.4
      w["H"]   <- w["H"]   * 0.7
      w["R"]   <- w["R"]   * 0.5
    }
  }

  # Geographic effect — urban -> H/V/MDG/R; rural -> SP/Frp
  if (!is.null(mun_pop) && !is.na(mun_pop)) {
    if (mun_pop > 100000) {
      w["H"]   <- w["H"]   * 1.30
      w["V"]   <- w["V"]   * 1.40
      w["MDG"] <- w["MDG"] * 1.50
      w["R"]   <- w["R"]   * 1.30
      w["SP"]  <- w["SP"]  * 0.30
    } else if (mun_pop < 5000) {
      w["SP"]  <- w["SP"]  * 3.0
      w["FRP"] <- w["FRP"] * 1.2
      w["H"]   <- w["H"]   * 0.7
      w["MDG"] <- w["MDG"] * 0.4
    }
  }

  # County (Oslo bias)
  if (!is.null(county) && identical(county, "Oslo")) {
    w["H"]   <- w["H"]   * 1.30
    w["MDG"] <- w["MDG"] * 1.60
    w["V"]   <- w["V"]   * 1.50
    w["SP"]  <- w["SP"]  * 0.10
  }

  # Age effect
  if (age < 30) {
    w["MDG"] <- w["MDG"] * 1.6
    w["R"]   <- w["R"]   * 1.5
    w["SV"]  <- w["SV"]  * 1.4
    w["KRF"] <- w["KRF"] * 0.4
    w["FRP"] <- w["FRP"] * 0.8
  } else if (age >= 65) {
    w["KRF"] <- w["KRF"] * 1.5
    w["AP"]  <- w["AP"]  * 1.2
    w["SP"]  <- w["SP"]  * 1.3
    w["MDG"] <- w["MDG"] * 0.4
    w["R"]   <- w["R"]   * 0.5
  }

  # Religion effect — KrF strongly tied to active Christians
  if (!is.null(religion_code) && !is.na(religion_code)) {
    if (religion_code %in% c("DnK","KAT","ANN_KRIS")) w["KRF"] <- w["KRF"] * 2.5
    if (identical(religion_code, "INGEN")) w["KRF"] <- w["KRF"] * 0.05
    if (identical(religion_code, "ISL")) {
      w["AP"]  <- w["AP"]  * 1.5
      w["SV"]  <- w["SV"]  * 1.4
      w["R"]   <- w["R"]   * 1.3
      w["FRP"] <- w["FRP"] * 0.2
      w["KRF"] <- w["KRF"] * 0.05
    }
    if (identical(religion_code, "HUM")) {
      w["V"]   <- w["V"]   * 1.5
      w["MDG"] <- w["MDG"] * 1.3
      w["KRF"] <- w["KRF"] * 0.02
    }
  }

  # Kjønn — moderate, veldokumenterte forskjeller (SSB valgundersøkelser):
  # kvinner noe mer SV/Rødt/MDG/KrF, menn noe mer FrP/Høyre.
  if (!is.null(gender) && !is.na(gender)) {
    if (identical(toupper(gender), "F")) {
      w["SV"]  <- w["SV"]  * 1.30; w["R"]   <- w["R"]   * 1.20
      w["MDG"] <- w["MDG"] * 1.20; w["KRF"] <- w["KRF"] * 1.15
      w["AP"]  <- w["AP"]  * 1.05
      w["FRP"] <- w["FRP"] * 0.70; w["H"]   <- w["H"]   * 0.90
    } else {
      w["FRP"] <- w["FRP"] * 1.25; w["H"]   <- w["H"]   * 1.08
      w["SV"]  <- w["SV"]  * 0.80; w["R"]   <- w["R"]   * 0.85
      w["MDG"] <- w["MDG"] * 0.85
    }
  }

  w <- pmax(w, 0)
  w <- w / sum(w)
  pick <- sample(names(w), 1, prob = w)
  row <- pb[pb$code == pick, , drop = FALSE]
  lbl <- if (nrow(row) > 0) row$label[1] else pick
  if (identical(lang, "no") && stats::runif(1) < 0.12) {
    ph <- .cfm_env$party_humor
    if (!is.null(ph)) {
      pool <- ph[ph$code == pick, , drop = FALSE]
      if (nrow(pool) > 0) lbl <- pool$label[sample.int(nrow(pool), 1)]
    }
  }
  list(code = pick, label = lbl)
}

# --- Siblings (søsken) -----------------------------------------------------
# Count + brief info per sibling. Cohort-dependent: large families (2-4) for
# parents born 1920-50, smaller (1-2) for parents born 1970+.
.cond_siblings <- function(ego_age, ego_gender = NULL,
                           mother_birth_year = NULL, name_region = "norden",
                           background = "majority", lang = "en") {
  if (is.null(mother_birth_year) || is.na(mother_birth_year)) {
    return(list(count = NA_integer_, siblings = list()))
  }
  # Distribution of TOTAL kids (incl. ego) — varies by mother cohort + region
  # Norway TFR: ~3 in 1960s, 1.7 in 2024
  mc_factor <- if (mother_birth_year < 1940) 1.4
               else if (mother_birth_year < 1960) 1.0
               else if (mother_birth_year < 1980) 0.7
               else 0.5
  # Region adjustment (immigrant families often larger)
  region_factor <- switch(name_region,
    "mena_sor_asia" = 1.4,
    "afrika_sub"    = 1.6,
    "ost_europa"    = 0.9,
    "latam_filippin"= 1.2,
    1.0)

  # Probabilities for total kids (1, 2, 3, 4, 5, 6+)
  # Base for mother born 1960-80, majority Norwegian:
  base_p <- c(`1` = 0.25, `2` = 0.45, `3` = 0.20, `4` = 0.07, `5` = 0.02, `6+` = 0.01)
  # Apply factor: shift mass toward larger families if factor > 1
  # Simple approach: weight by k * factor
  ks <- c(1, 2, 3, 4, 5, 6.5)
  total_factor <- mc_factor * region_factor
  shifted_w <- base_p * (ks ^ ((total_factor - 1) * 1.2))
  shifted_w <- shifted_w / sum(shifted_w)
  total_kids <- sample(c(1L, 2L, 3L, 4L, 5L, 6L), 1, prob = shifted_w)
  n_siblings <- total_kids - 1L

  if (n_siblings <= 0L) return(list(count = 0L, siblings = list()))

  # Generate brief info per sibling
  # Konvensjon: delta > 0 = søsken er X år yngre, delta < 0 = X år eldre
  # Søsken-alder = ego_age - delta (eldre søsken har høyere alder)
  # Klipper: søsken kan ikke være foedt etter referanseaar (delta <= ego_age)
  # eller foedt før mor sin reproduktive alder (delta >= -(45 - mother_birth_age))
  # Approksimerer: max yngre = ego_age, max eldre = 15 (typisk søsken-spread)
  siblings <- list()
  for (i in seq_len(n_siblings)) {
    raw_delta <- round(stats::rnorm(1, 0, 4))
    # Klipping: yngre søsken kan ikke være ufoedt
    max_younger <- as.integer(ego_age)  # søsken kan max være ego_age år yngre
    max_older <- 15L  # søsken sjelden mer enn 15 år eldre
    delta <- as.integer(max(-max_older, min(max_younger, raw_delta)))
    sib_age <- as.integer(max(0L, ego_age - delta))
    if (sib_age > 95) sib_age <- 95L
    sib_birth_year <- .cfm_env$ref_year - sib_age
    sib_gender <- if (stats::runif(1) < 0.5) "M" else "F"
    # Name from same region
    sib_name <- tryCatch({
      if (!identical(background, "majority") &&
          !is.null(name_region) && nzchar(name_region) &&
          !identical(name_region, "norden")) {
        nbr <- .cfm_env$names_by_region
        pool <- nbr[nbr$region == name_region & nbr$gender == sib_gender, , drop = FALSE]
        if (nrow(pool) > 0) pool$name[sample.int(nrow(pool), 1)]
        else sample_first_name(gender = sib_gender, birth_year = sib_birth_year)
      } else {
        sample_first_name(gender = sib_gender, birth_year = sib_birth_year)
      }
    }, error = function(e) NA_character_)
    siblings[[length(siblings) + 1L]] <- list(
      name = sib_name, gender = sib_gender,
      birth_year = as.integer(sib_birth_year),
      age_delta = as.integer(delta)
    )
  }
  list(count = as.integer(n_siblings), siblings = siblings)
}

# --- Grandparents (besteforeldre) -----------------------------------------
# 4 of them: mormor, morfar, farmor, farfar.
# Born ~25-30 years before each parent. Most are deceased given typical ego age.
.cond_grandparents <- function(mother_birth_year, father_birth_year,
                               mother_region = "norden", father_region = "norden",
                               lang = "en") {
  if (is.null(mother_birth_year) || is.na(mother_birth_year) ||
      is.null(father_birth_year) || is.na(father_birth_year)) {
    return(list(mormor = NULL, morfar = NULL, farmor = NULL, farfar = NULL))
  }
  ref_year <- .cfm_env$ref_year

  draw_gp <- function(parent_birth_year, gp_gender, region) {
    gp_birth_age <- if (gp_gender == "F")
                      as.integer(round(stats::rnorm(1, 27, 5)))
                    else as.integer(round(stats::rnorm(1, 30, 6)))
    if (gp_birth_age < 16) gp_birth_age <- 16L
    if (gp_birth_age > 50) gp_birth_age <- 50L
    gp_birth_year <- parent_birth_year - gp_birth_age
    implied_age <- ref_year - gp_birth_year
    # Death probability — most grandparents deceased
    p_dead <- if (implied_age >= 105) 1.0
              else if (implied_age >= 95) 0.97
              else if (implied_age >= 90) 0.85
              else if (implied_age >= 85) 0.65
              else if (implied_age >= 80) 0.45
              else if (implied_age >= 75) 0.25
              else if (implied_age >= 70) 0.15
              else if (implied_age >= 65) 0.05
              else 0
    death_year <- if (stats::runif(1) < p_dead) {
      death_age_max <- min(105L, as.integer(implied_age))
      span <- 60:death_age_max
      if (length(span) > 0) {
        w <- dnorm(span, mean = 80, sd = 12)
        as.integer(gp_birth_year + sample(span, 1, prob = w))
      } else NA_integer_
    } else NA_integer_

    # Name from origin region (grandparents of first/second_gen ego are origin)
    name <- tryCatch({
      if (!identical(region, "norden")) {
        nbr <- .cfm_env$names_by_region
        pool <- nbr[nbr$region == region & nbr$gender == gp_gender, , drop = FALSE]
        if (nrow(pool) > 0) pool$name[sample.int(nrow(pool), 1)]
        else sample_first_name(gender = gp_gender, birth_year = gp_birth_year)
      } else {
        sample_first_name(gender = gp_gender, birth_year = gp_birth_year)
      }
    }, error = function(e) NA_character_)

    list(name = name, gender = gp_gender,
         birth_year = as.integer(gp_birth_year),
         death_year = death_year)
  }

  list(
    mormor = draw_gp(mother_birth_year, "F", mother_region),
    morfar = draw_gp(mother_birth_year, "M", mother_region),
    farmor = draw_gp(father_birth_year, "F", father_region),
    farfar = draw_gp(father_birth_year, "M", father_region)
  )
}

# --- NEET (Not in Education, Employment, or Training) ----------------------
# For ages 16-29. ~10% baseline overall in Norway. Higher among low-edu,
# first-gen non-Western. Returns a flag — interpretation in .cond_occupation.
.cond_neet_prob <- function(age, edu_code = NULL, background = "majority",
                            name_region = NULL) {
  if (age < 16 || age > 29) return(0)
  base <- if (age < 19) 0.07 else if (age < 25) 0.12 else 0.10
  if (!is.null(edu_code) && !is.na(edu_code)) {
    if (edu_code <= 1) base <- base * 2.0
    else if (edu_code == 2) base <- base * 1.3
    else if (edu_code >= 5) base <- base * 0.4
  }
  if (!is.null(background) && !identical(background, "majority")) {
    if (identical(background, "first_gen")) {
      base <- base * (
        if (identical(name_region, "norden")) 0.9
        else if (identical(name_region, "vesteuropa")) 0.8
        else if (identical(name_region, "ost_europa")) 1.1
        else if (identical(name_region, "mena_sor_asia")) 1.6
        else if (identical(name_region, "afrika_sub")) 1.8
        else 1.2)
    } else {
      base <- base * 1.2
    }
  }
  min(0.55, base)
}

# --- Bourdieu kapitalprofil ----------------------------------------------
# Beregner tre kapital-indekser (0-100) + klasseposisjon basert pa allerede
# trukne dimensjoner. Bygger pa Bourdieu sin Distinction (1979) og nyere
# norsk Bourdieu-applikasjon.
#
# Økonomisk kapital: net_wealth + capital_income + housing_equity
# Kulturell kapital: ego edu + foreldreutdanning + yrke-type + religion-tilpassing
# Sosial kapital: husholdning + søsken + parti + couple-type

.cond_bourdieu <- function(net_wealth_nok = NULL, capital_income_nok = NULL,
                           housing_equity_nok = NULL,
                           edu_code = NULL, mother_edu_code = NULL,
                           father_edu_code = NULL,
                           styrk_code = NULL,
                           household = NULL, n_siblings = NULL,
                           party_code = NULL,
                           lang = "en") {

  # --- Økonomisk kapital (0-100) ---
  # Log-skalert: 0 = under 0, 50 = ~3 MNOK, 90 = ~50 MNOK, 99 = top 0.1%
  nw <- if (is.null(net_wealth_nok) || is.na(net_wealth_nok)) 0 else net_wealth_nok
  ci <- if (is.null(capital_income_nok) || is.na(capital_income_nok)) 0 else capital_income_nok
  he <- if (is.null(housing_equity_nok) || is.na(housing_equity_nok)) 0 else housing_equity_nok
  total_econ <- nw + (ci * 5) + he * 0.5  # cap-inntekt × 5 (vekt mer), housing 50% vekt
  econ_score <- if (total_econ <= 0) 0
                else min(100, max(0, 50 + 15 * (log10(max(1, total_econ)) - 6.5)))
  econ_score <- round(econ_score, 1)

  # --- Kulturell kapital (0-100) ---
  # Edu (0-10 -> 0-50), foreldreutdanning (0-10 -> 0-25), yrke-type (0-25)
  edu_pts <- if (!is.null(edu_code) && !is.na(edu_code)) min(50, edu_code * 6) else 20
  par_edu <- mean(c(if (!is.null(mother_edu_code) && !is.na(mother_edu_code)) mother_edu_code else 3,
                    if (!is.null(father_edu_code) && !is.na(father_edu_code)) father_edu_code else 3))
  par_pts <- min(25, par_edu * 3)
  styrk1 <- if (!is.null(styrk_code) && !is.na(styrk_code) && nzchar(styrk_code))
              substr(as.character(styrk_code), 1, 1) else NA
  styrk_pts <- if (is.na(styrk1)) 8
               else if (styrk1 %in% c("2", "3")) 25  # akademisk, hoeyskoleyrker
               else if (styrk1 %in% c("1", "4")) 18  # ledere, kontor
               else if (styrk1 %in% c("5")) 10        # service
               else if (styrk1 %in% c("6", "7", "8")) 8  # bonder, handverk, prosess
               else 12
  cult_score <- round(min(100, edu_pts + par_pts + styrk_pts), 1)

  # --- Sosial kapital (0-100) ---
  # Husholdning (parforhold = +25, enslig = +10, bofellesskap = +20, alone = +5)
  # Søsken (0 = -5, 1-2 = +10, 3+ = +15)
  # Parti (engagert = +20, ingen/stemte_ikke = -10)
  hh <- if (is.null(household) || is.na(household)) "" else as.character(household)
  hh_pts <- if (grepl("[Pp]ar med barn|[Cc]ouple with", hh)) 30
            else if (grepl("[Pp]ar uten|[Cc]ouple without", hh)) 25
            else if (grepl("[Bb]ofellesskap|[Ss]hared", hh)) 20
            else if (grepl("[Ee]nslig|[Ss]ingle", hh)) 12
            else 15
  sib_pts <- if (is.null(n_siblings) || is.na(n_siblings)) 5
             else if (n_siblings == 0) 5
             else if (n_siblings <= 2) 12
             else 18
  party_pts <- if (is.null(party_code) || is.na(party_code)) 8
               else if (identical(party_code, "STEMTE_IKKE")) 3
               else 20
  soc_score <- round(min(100, hh_pts + sib_pts + party_pts), 1)

  # --- Klasseposisjon ---
  # Norsk Bourdieu-tilpasset typologi (Skarpenes, Skogen, Mangset)
  klasse <- .bourdieu_klasse(econ_score, cult_score, soc_score, lang = lang)

  list(
    okonomisk = econ_score,
    kulturell = cult_score,
    sosial = soc_score,
    klasse = klasse
  )
}

.bourdieu_klasse <- function(econ, cult, soc, lang = "en") {
  no <- identical(lang, "no")
  # Etablert overklasse: hoy økonomisk + hoy kulturell
  if (econ >= 70 && cult >= 70) return(if (no) "Etablert overklasse" else "Established upper class")
  # Økonomisk elite: hoy økonomisk, lav-medium kulturell
  if (econ >= 75 && cult < 65) return(if (no) "\u{00d8}konomisk elite" else "Economic elite")
  # Kulturell elite: hoy kulturell, lav-medium økonomisk
  if (cult >= 75 && econ < 65) return(if (no) "Kulturell elite" else "Cultural elite")
  # Etablert middelklasse: medium-hoy begge
  if (econ >= 50 && cult >= 50) return(if (no) "Etablert middelklasse" else "Established middle class")
  # Ny middelklasse / kulturell mellomlag
  if (cult >= 55 && econ >= 30) return(if (no) "Kulturell middelklasse" else "Cultural middle class")
  if (econ >= 55 && cult >= 30) return(if (no) "\u{00d8}konomisk middelklasse" else "Economic middle class")
  # Tradisjonell arbeiderklasse: medium-low begge
  if (econ >= 30 && cult >= 25) return(if (no) "Tradisjonell arbeiderklasse" else "Traditional working class")
  # Ny arbeiderklasse / service
  if (cult >= 25) return(if (no) "Ny arbeiderklasse" else "New working class")
  # Prekariat: lav-lav-lav
  if (no) "Prekariat" else "Precariat"
}

# --- Urbanitet (sentralitetsindeks 1-6) ---------------------------------
.cond_sentralitet <- function(municipality_name, lang = "en") {
  if (is.null(municipality_name) || is.na(municipality_name) || !nzchar(municipality_name)) {
    return(list(score = NA_integer_, label = NA_character_))
  }
  ks <- .cfm_env$kommune_sentralitet
  if (is.null(ks)) return(list(score = NA_integer_, label = NA_character_))
  row <- ks[ks$kommune == municipality_name, , drop = FALSE]
  if (nrow(row) == 0) return(list(score = NA_integer_, label = NA_character_))
  lbl_col <- if (identical(lang, "no")) "sentralitet_label_no" else "sentralitet_label_en"
  list(score = as.integer(row$sentralitet[1]),
       label = row[[lbl_col]][1])
}

# --- Antall barn ----------------------------------------------------------
.cond_n_children <- function(age, gender = NULL, marital_code = NULL,
                             background = "majority", name_region = NULL,
                             orientation_code = NULL,
                             lang = "en") {
  if (is.null(age) || is.na(age) || age < 16) return(list(count = 0L))
  birth_year <- .cfm_env$ref_year - age
  ncc <- .cfm_env$n_children_by_cohort
  if (is.null(ncc)) return(list(count = 0L))

  # Find cohort row
  row <- NULL
  for (i in seq_len(nrow(ncc))) {
    if (birth_year >= ncc$cohort_min[i] && birth_year < ncc$cohort_max[i]) {
      row <- ncc[i, ]
      break
    }
  }
  if (is.null(row)) row <- ncc[nrow(ncc), ]

  # Probabilities for 0,1,2,3,4+
  probs <- c(row$p0, row$p1, row$p2, row$p3, row$p4plus)

  # Background adjustment: MENA/Afrika +30% til 2-4 barn, dempet 0-1
  if (!identical(background, "majority") && !is.null(name_region)) {
    if (name_region %in% c("mena_sor_asia", "afrika_sub")) {
      probs <- probs * c(0.6, 0.7, 1.2, 1.4, 1.5)
      probs <- probs / sum(probs)
    } else if (name_region == "ost_europa") {
      probs <- probs * c(0.9, 1.0, 1.1, 1.0, 0.9)
      probs <- probs / sum(probs)
    }
  }

  # Orientering: ikke umulig, men annerledes fordeling.
  # Eldre kohorter LHB: mange fikk barn i tidligere heterofile forhold.
  # Yngre homofile menn: lav fertilitet. Lesbiske: donor/tidligere forhold,
  # hoyere enn homofile menn, typisk planlagte 1-2-barnsfamilier.
  # Bifile: nær befolkningssnittet (flertallet i ulikekjonnede par).
  if (!is.null(orientation_code) && !is.na(orientation_code) &&
      orientation_code %in% c("gay", "bi")) {
    if (identical(orientation_code, "gay")) {
      is_f <- !is.null(gender) && identical(toupper(gender), "F")
      if (birth_year < 1965) {
        # Kom ut i voksen alder, ofte etter heterofilt ekteskap
        probs <- probs * if (is_f) c(1.8, 1.1, 0.7, 0.40, 0.25)
                         else      c(2.0, 1.0, 0.6, 0.35, 0.20)
      } else if (birth_year < 1985) {
        probs <- probs * if (is_f) c(1.7, 1.0, 0.8, 0.30, 0.15)
                         else      c(3.0, 0.8, 0.5, 0.20, 0.10)
      } else {
        probs <- probs * if (is_f) c(1.6, 1.0, 1.0, 0.30, 0.10)
                         else      c(4.0, 0.6, 0.35, 0.10, 0.05)
      }
      probs <- probs / sum(probs)
    }
    # bi: ingen justering
  }

  # Age-based dampening: under 22 har sjelden barn
  if (age < 22) probs <- probs * c(3.0, 1.2, 0.5, 0.2, 0.05)
  else if (age < 26) probs <- probs * c(1.5, 1.3, 0.9, 0.4, 0.2)
  probs <- probs / sum(probs)

  # Menn under 28 også sjeldnere far
  if (!is.null(gender) && identical(gender, "M") && age < 28) {
    probs <- probs * c(2.5, 1.0, 0.6, 0.3, 0.1)
    probs <- probs / sum(probs)
  }

  pick <- sample(c(0L, 1L, 2L, 3L, 4L), 1, prob = probs)
  if (pick == 4L) pick <- sample(4:7, 1, prob = c(0.55, 0.25, 0.13, 0.07))

  # Hard cap by age — biologisk + sosialt umulig
  if (age < 17) pick <- 0L  # under 17 = ingen barn
  else if (age < 19) pick <- min(pick, 1L)
  else if (age < 22) pick <- min(pick, 2L)
  else if (age < 25) pick <- min(pick, 3L)

  list(count = as.integer(pick))
}

# --- Materiell deprivasjon (EU-SILC) ------------------------------------
.cond_material_deprivation <- function(income_nok = NULL, age = NULL,
                                        background = "majority",
                                        marital_code = NULL,
                                        n_children = 0,
                                        net_wealth_nok = NULL,
                                        lang = "en") {
  # Approximate income decile from income_nok (Norwegian individual after-tax 2024)
  decile <- if (is.null(income_nok) || is.na(income_nok)) 5L
            else if (income_nok < 200000) 1L
            else if (income_nok < 280000) 2L
            else if (income_nok < 340000) 3L
            else if (income_nok < 410000) 4L
            else if (income_nok < 480000) 5L
            else if (income_nok < 560000) 6L
            else if (income_nok < 660000) 7L
            else if (income_nok < 810000) 8L
            else if (income_nok < 1100000) 9L
            else 10L

  md <- .cfm_env$material_deprivation
  if (is.null(md)) return(list(count = 0L))
  row <- md[md$income_decile == decile, , drop = FALSE]
  if (nrow(row) == 0) row <- md[md$income_decile == 5, , drop = FALSE]
  probs <- c(row$p0, row$p1, row$p2, row$p3, row$p4_or_more)

  # Adjustments: enslig forelder + ikke-vestlig +
  if (!is.null(n_children) && n_children > 0 && !is.null(marital_code) &&
      marital_code %in% c(1L, 4L, 5L)) {  # ugift / skilt / enke (single parent)
    probs <- probs * c(0.6, 0.9, 1.3, 1.6, 2.0)
    probs <- probs / sum(probs)
  }
  if (!is.null(background) && identical(background, "first_gen")) {
    probs <- probs * c(0.7, 1.0, 1.2, 1.4, 1.6)
    probs <- probs / sum(probs)
  }

  cat_pick <- sample(0:4, 1, prob = probs)
  count <- if (cat_pick == 4) sample(4:9, 1, prob = c(0.40, 0.25, 0.15, 0.10, 0.06, 0.04))
           else cat_pick

  # Wealth-based cap: rikfolk har ikke materielle ulemper (de kan velge bort)
  if (!is.null(net_wealth_nok) && !is.na(net_wealth_nok)) {
    if (net_wealth_nok > 10e6) count <- min(count, 1L)
    else if (net_wealth_nok > 3e6) count <- min(count, 2L)
  }
  count <- as.integer(count)

  label <- if (identical(lang, "no")) {
    if (count == 0) "Ingen materielle ulemper"
    else if (count <= 2) sprintf("Mild deprivasjon (%d/9 ulemper)", count)
    else if (count <= 4) sprintf("Moderat deprivasjon (%d/9 ulemper)", count)
    else sprintf("Alvorlig deprivasjon (%d/9 ulemper)", count)
  } else {
    if (count == 0) "No material deprivation"
    else sprintf("Material deprivation (%d/9 indicators)", count)
  }
  list(count = as.integer(count), label = label)
}

# --- Helse: selvrapportert + kronisk sykdom ----------------------------
# --- Aldersgrenser per dimensjon ---------------------------------------
#
# To grunner til at et felt ikke gjelder barn:
#
#   (a) Sporreskjema-grense. Levekarsundersokelsen spor 16+. A trekke en
#       tillitsscore for en firearing er ikke en tilnaerming, men en
#       oppdiktet observasjon presentert med samme autoritet som
#       registertallene.
#
#   (b) Manglende kilde. Radene for 0-15 i sleep_hours, tv_hours og
#       chronic_illness_prob, og barnesannsynligheten i .cond_disability,
#       har ingen provenans: ingen skript i data-raw/ henter dem, og
#       verdiene er runde tall som summerer til nayaktig 1. De ble satt
#       fordi oppslaget trengte noe a finne.
#
# Begge klasser gates her. print() hopper over NA, sa feltene vises ikke.
# Barn beholder det som enten er SSB-basert (navn, alder, geografi,
# familie, husholdning) eller apenbart skrevet (barnevarianter av yrke,
# inntekt og kosthold) -- der er det ingen fare for a forveksle dikt med
# maling.
.dimension_min_age <- c(
  # (a) sporreskjema-grense
  trust             = 16L,  # generell tillit 0-10, Levekar/ESS 16+
  loneliness        = 16L,  # Levekar 16+
  close_friends     = 16L,  # Levekar 16+
  has_confidant     = 16L,  # Levekar 16+
  self_rated_health = 16L,  # Levekar 16+
  media_paper       = 13L,  # avisvalg betinges av parti
  media_podcast     = 13L,
  media_social      = 13L,  # plattformene har 13-arsgrense
  # (b) anslag uten kilde
  sleep_hours       = 16L,
  media_tv_hours    = 16L,
  has_chronic       = 16L,
  has_disability    = 16L
)


.cond_health <- function(age, edu_code = NULL, background = "majority",
                         lang = "en") {
  if (is.null(age) || is.na(age)) {
    return(list(self_rated = NA_character_, chronic = FALSE))
  }
  # Bade selvrapportert helse (sporreskjema, 16+) og kronisk sykdom
  # (0-15-raden er et anslag uten kilde) gates for barn.
  if (age < .dimension_min_age[["self_rated_health"]] &&
      age < .dimension_min_age[["has_chronic"]]) {
    return(list(self_rated = NA_character_, chronic = FALSE,
                chronic_type = NA_character_))
  }
  gate_srh <- age < .dimension_min_age[["self_rated_health"]]

  band <- if (age < 16) "0-15"
          else if (age < 25) "16-24"
          else if (age < 45) "25-44"
          else if (age < 65) "45-64"
          else if (age < 80) "65-79"
          else "80+"

  srh <- .cfm_env$self_rated_health
  cip <- .cfm_env$chronic_illness_prob
  if (is.null(srh) || is.null(cip)) {
    return(list(self_rated = NA_character_, chronic = FALSE))
  }

  # Self-rated health
  row <- srh[srh$age_band == band, , drop = FALSE]
  if (nrow(row) == 0) {
    # Fant ikke aldersbandet. Ikke fall tilbake pa forste rad -- det ville
    # gitt et barn en voksenverdi uten a si fra. Bedre a returnere NA.
    return(list(self_rated = NA_character_, chronic = FALSE,
                chronic_type = NA_character_))
  }
  probs <- c(row$p_meget_god, row$p_god, row$p_saa_som, row$p_daarlig, row$p_meget_daarlig)
  # Edu adjustment: høyere utdanning → bedre selvrapportert helse
  if (!is.null(edu_code) && !is.na(edu_code)) {
    if (edu_code >= 6) {
      probs <- probs * c(1.3, 1.1, 0.8, 0.6, 0.5)
    } else if (edu_code <= 1) {
      probs <- probs * c(0.7, 0.95, 1.2, 1.4, 1.5)
    }
    probs <- probs / sum(probs)
  }
  pick <- sample(1:5, 1, prob = probs)
  labels_no <- c("Meget god", "God", "S\u{00e5} som s\u{00e5}", "D\u{00e5}rlig", "Meget d\u{00e5}rlig")
  labels_en <- c("Excellent", "Good", "Fair", "Poor", "Very poor")
  srh_label <- if (identical(lang, "no")) labels_no[pick] else labels_en[pick]

  # Chronic illness
  prow <- cip[cip$age_band == band, , drop = FALSE]
  if (nrow(prow) == 0) {
    return(list(self_rated = srh_label, chronic = FALSE,
                chronic_type = NA_character_))
  }
  p_chr <- prow$p_chronic[1]
  # Edu adjustment: lavere edu → mer kronisk
  if (!is.null(edu_code) && !is.na(edu_code)) {
    if (edu_code <= 2) p_chr <- p_chr * 1.3
    else if (edu_code >= 6) p_chr <- p_chr * 0.7
  }
  chronic <- stats::runif(1) < min(1, p_chr)

  # Hvis kronisk, trekk type
  chronic_type <- NA_character_
  if (chronic) {
    types_no <- c("Hjerte-/karsykdom", "Diabetes", "Astma/KOLS", "Revmatisme/leddsykdom",
                  "Psykisk lidelse", "Kreft (tidligere)", "Annen kronisk lidelse")
    types_en <- c("Cardiovascular disease", "Diabetes", "Asthma/COPD", "Rheumatic/joint disease",
                  "Mental health condition", "Cancer (history)", "Other chronic condition")
    weights <- if (age < 12) c(0.01, 0.05, 0.55, 0.04, 0.05, 0.01, 0.29)  # barn: astma/eksema dominerer
               else if (age < 30) c(0.05, 0.05, 0.30, 0.10, 0.40, 0.02, 0.08)
               else if (age < 50) c(0.15, 0.10, 0.20, 0.20, 0.20, 0.05, 0.10)
               else if (age < 70) c(0.30, 0.15, 0.10, 0.20, 0.10, 0.10, 0.05)
               else c(0.40, 0.15, 0.10, 0.15, 0.05, 0.10, 0.05)
    idx <- sample.int(7, 1, prob = weights)
    chronic_type <- if (identical(lang, "no")) types_no[idx] else types_en[idx]
  }

  if (isTRUE(gate_srh)) srh_label <- NA_character_
  list(self_rated = srh_label, chronic = chronic, chronic_type = chronic_type)
}

# --- Sosial isolasjon (ensomhet + tillit) -------------------------------
.cond_social_isolation <- function(age, household = NULL, lang = "en") {
  if (is.null(age) || is.na(age)) return(list(loneliness = NA_character_, trust = NA_integer_))
  # Ensomhet og generell tillit er Levekar-sporsmal (16+). Under grensa
  # finnes det ikke noe svar a rapportere.
  if (age < .dimension_min_age[["loneliness"]])
    return(list(loneliness = NA_character_, trust = NA_integer_))
  band <- if (age < 16) "0-15"
          else if (age < 25) "16-24"
          else if (age < 45) "25-44"
          else if (age < 65) "45-64"
          else if (age < 80) "65-79"
          else "80+"

  si <- .cfm_env$social_isolation
  if (is.null(si)) return(list(loneliness = NA_character_, trust = NA_integer_))
  row <- si[si$age_band == band, , drop = FALSE]
  if (nrow(row) == 0) {
    return(list(loneliness = NA_character_, trust = NA_integer_))
  }

  probs <- c(row$p_lonely_often, row$p_lonely_sometimes,
             row$p_lonely_rarely, row$p_lonely_never)

  # Husholdning-justering: enslig + bofellesskap → mer ensom; familie sterkt mindre
  if (!is.null(household) && !is.na(household)) {
    if (grepl("[Ee]nslig|[Ss]ingle$|[Aa]lone", household)) {
      probs <- probs * c(1.5, 1.3, 0.85, 0.7)
    } else if (grepl("med barn|with children", household)) {
      probs <- probs * c(0.15, 0.50, 1.20, 1.50)
    } else if (grepl("[Pp]ar|[Cc]ouple", household)) {
      probs <- probs * c(0.30, 0.70, 1.20, 1.40)
    } else if (grepl("[Ff]amili|[Ff]amily", household)) {
      probs <- probs * c(0.30, 0.70, 1.20, 1.40)
    }
    probs <- probs / sum(probs)
  }

  pick <- sample(1:4, 1, prob = probs)
  labels_no <- c("F\u{00f8}ler seg ofte ensom", "F\u{00f8}ler seg av og til ensom",
                 "F\u{00f8}ler seg sjelden ensom", "F\u{00f8}ler seg aldri ensom")
  labels_en <- c("Often lonely", "Sometimes lonely", "Rarely lonely", "Never lonely")
  lon_label <- if (identical(lang, "no")) labels_no[pick] else labels_en[pick]

  # Generell tillit (0-10): norsk gjennomsnitt ~6.7 (ESS)
  # Lavere tillit blant ensom + lav-edu
  trust_mean <- 6.7
  if (pick == 1) trust_mean <- trust_mean - 1.5
  else if (pick == 2) trust_mean <- trust_mean - 0.5
  trust <- max(0L, min(10L, as.integer(round(stats::rnorm(1, trust_mean, 1.5)))))

  list(loneliness = lon_label, trust = trust)
}


# --- Hobby/fritidsinteresse-dimensjon ------------------------------------
# Trekker 1-3 hobbier per ego basert pa kjonn x aldersband (M_under30, etc.),
# justert for sentralitet (friluftsliv ↑ distrikt), edu (kulturell ↑ for hoy edu),
# og boligeierskap (hagearbeid for eiere).
.cond_hobbies <- function(age, gender = NULL, sentralitet = NULL,
                          edu_code = NULL, owns_house = FALSE,
                          background = "majority", alcohol_label = NULL,
                          party_code = NULL, lang = "en") {
  if (is.null(age) || is.na(age) || age < 6) return(list(hobbies = list()))
  .load_data()
  hb <- .cfm_env$hobbies
  if (is.null(hb) || nrow(hb) == 0) return(list(hobbies = list()))

  # Choose weight column
  g <- if (!is.null(gender) && identical(toupper(gender), "M")) "M" else "F"
  band <- if (age < 30) "under30"
          else if (age < 50) "30_50"
          else if (age < 67) "50_67"
          else "over67"
  col <- paste0(g, "_", band)
  if (!col %in% names(hb)) return(list(hobbies = list()))

  weights <- as.numeric(hb[[col]])
  weights[is.na(weights)] <- 0

  # --- Skjerp kjønnskontrasten -------------------------------------------
  # Deltakelsestallene er kjønnsbetinget, men kontrasten er for myk: når
  # man trekker 4-5 hobbyer fra ~190, dukker lavvekt-hobbyer fra det andre
  # kjønnet opp for ofte. w^gamma gjør sterkt kjønnede hobbyer dominante og
  # presser kryss-kjønn-halen ned, uten å nulle den (fortsatt mulig, bare
  # sjeldnere). gamma justerbar.
  weights <- weights ^ 1.6

  # --- Aldersgate: hobbyer med min_age over ego-alder er utelukket ---
  if ("min_age" %in% names(hb)) {
    ma <- suppressWarnings(as.numeric(hb$min_age))
    ma[is.na(ma)] <- 0
    weights[ma > age] <- 0
  }

  # --- Sentralitet-justering ---
  if (!is.null(sentralitet) && !is.na(sentralitet)) {
    if (sentralitet >= 5) {
      # Distrikt: friluft + jakt-/transport-hobbier, klubb (Rotary/Lions)
      weights[hb$category == "friluft"] <- weights[hb$category == "friluft"] * 1.6
      weights[hb$category == "kulturell"] <- weights[hb$category == "kulturell"] * 0.4
      weights[hb$category == "digital"] <- weights[hb$category == "digital"] * 0.6
      weights[hb$category == "wellness"] <- weights[hb$category == "wellness"] * 0.5
      weights[hb$category == "klubb"] <- weights[hb$category == "klubb"] * 1.4
      weights[hb$category == "transport"] <- weights[hb$category == "transport"] * 1.4
      weights[hb$category == "hjem"] <- weights[hb$category == "hjem"] * 1.4
    } else if (sentralitet <= 2) {
      # Storby: kulturell, digital, wellness, mat-spesial, weird
      weights[hb$category == "friluft"] <- weights[hb$category == "friluft"] * 0.6
      weights[hb$category == "kulturell"] <- weights[hb$category == "kulturell"] * 1.6
      weights[hb$category == "digital"] <- weights[hb$category == "digital"] * 1.4
      weights[hb$category == "wellness"] <- weights[hb$category == "wellness"] * 1.7
      weights[hb$category == "mat"] <- weights[hb$category == "mat"] * 1.4
      weights[hb$category == "weird"] <- weights[hb$category == "weird"] * 1.3
      weights[hb$category == "klubb"] <- weights[hb$category == "klubb"] * 0.6
    }
  }

  # --- Edu-justering ---
  if (!is.null(edu_code) && !is.na(edu_code)) {
    if (edu_code >= 6) {
      weights[hb$category == "kulturell"] <- weights[hb$category == "kulturell"] * 1.5
      weights[hb$category == "frivillig"] <- weights[hb$category == "frivillig"] * 1.4
      weights[hb$category == "wellness"] <- weights[hb$category == "wellness"] * 1.3
      weights[hb$category == "politikk"] <- weights[hb$category == "politikk"] * 1.5
      weights[hb$category == "mat"] <- weights[hb$category == "mat"] * 1.3
    } else if (edu_code <= 2) {
      weights[hb$category == "kulturell"] <- weights[hb$category == "kulturell"] * 0.5
      weights[hb$category == "politikk"] <- weights[hb$category == "politikk"] * 0.5
      weights[hb$category == "kampsport"] <- weights[hb$category == "kampsport"] * 1.3
      weights[hb$category == "transport"] <- weights[hb$category == "transport"] * 1.3
    }
  }

  # --- Boligeier-justering ---
  if (isTRUE(owns_house)) {
    weights[hb$category == "hjem"] <- weights[hb$category == "hjem"] * 1.6
    weights[hb$category == "transport"] <- weights[hb$category == "transport"] * 1.3
    is_carpentry <- grepl("[Ss]nekring|[Cc]arpentry|[Vv]erksted", hb$hobby_no)
    weights[is_carpentry] <- weights[is_carpentry] * 1.6
  }

  # Alkohol-justering: avholdsmann/sjelden → fjern drikke-hobbier
  if (!is.null(alcohol_label) && !is.na(alcohol_label)) {
    drinking_hobby_pattern <- "[Vv]inkurs|[Vv]insmaking|[\u{00d8}\u{00f8}]lbrygging|[Ww]hisky|[Vv]inmaking|[Ww]ine"
    is_drinking <- grepl(drinking_hobby_pattern, hb$hobby_no)
    if (grepl("[Aa]vhold|[Tt]eetot", alcohol_label)) {
      weights[is_drinking] <- 0
    } else if (grepl("[Ss]jelden|[Rr]arely", alcohol_label)) {
      weights[is_drinking] <- weights[is_drinking] * 0.2
    }
  }

  # Parti-justering: noen hobbier korrelerer sterkt med parti
  if (!is.null(party_code) && !is.na(party_code)) {
    hunt_idx <- grepl("[Jj]akt|[Hh]unting|[Ff]iske|[Ff]ishing", hb$hobby_no)
    opera_idx <- grepl("[Oo]pera|[Kk]lassisk konsert|[Tt]eater|[Kk]unstutst", hb$hobby_no)
    if (party_code == "SP") {
      weights[hunt_idx] <- weights[hunt_idx] * 1.8
    } else if (party_code %in% c("H", "V")) {
      weights[opera_idx] <- weights[opera_idx] * 1.5
    } else if (party_code %in% c("SV", "R", "MDG")) {
      weights[hunt_idx] <- weights[hunt_idx] * 0.5
    }
  }

  weights <- pmax(weights, 0)
  if (sum(weights) == 0) return(list(hobbies = list()))

  # Antall hobbier: 2-5 for voksne (med flere alternativer)
  n_hobbies <- if (age < 12) sample(1:3, 1, prob = c(0.3, 0.45, 0.25))
               else if (age < 18) sample(2:4, 1, prob = c(0.30, 0.45, 0.25))
               else if (age < 30) sample(2:5, 1, prob = c(0.20, 0.40, 0.30, 0.10))
               else if (age < 60) sample(2:5, 1, prob = c(0.20, 0.40, 0.30, 0.10))
               else sample(1:4, 1, prob = c(0.20, 0.35, 0.30, 0.15))

  # Sample without replacement
  picks <- sample.int(nrow(hb), min(n_hobbies, sum(weights > 0)),
                      prob = weights, replace = FALSE)

  labels <- vapply(picks, function(i) {
    if (identical(lang, "no")) hb$hobby_no[i] else hb$hobby_en[i]
  }, character(1))

  list(hobbies = as.list(labels))
}

# --- Detail-yrke lookup (STYRK-4 → 7-sifret yrkesbetegnelse) -------------
# Yrkestitler i STYRK-98 som beskriver ARBEIDERENS kjonn. Disse finnes i
# par, og et kvinnelig ego skal ikke ende som "Fosterfar".
#
# NB: DAMEFRISOR/HERREFRISOR og DAMESKREDDER/HERRESKREDDER er bevisst IKKE
# med. De beskriver KUNDENS kjonn, ikke arbeiderens - en damefrisor kan
# utmerket godt vaere mann. Et generisk MOR/FAR-bytte over hele registeret
# ville innfort en ny feil mens det fikset denne.
.gendered_yrke_titles <- list(
  female = c("FOSTERMOR", "INTERNATHUSMOR", "BYSSEPIKE",
             "LUGARPIKE", "MESSEPIKE", "STALLPIKE"),
  male   = c("FOSTERFAR", "INTERNATHUSFAR", "BYSSEGUTT",
             "LUGARGUTT", "MESSEGUTT", "STALLGUTT")
)

.draw_detail_yrke <- function(styrk4_code, fallback_label = NA, gender = NULL) {
  if (is.null(styrk4_code) || is.na(styrk4_code) || !nzchar(styrk4_code)) {
    return(fallback_label)
  }
  if (identical(styrk4_code, "0000")) return(fallback_label)
  occ <- .cfm_env$occupations
  if (is.null(occ) || nrow(occ) == 0) return(fallback_label)
  # Match via styrk08-kolonnen (STYRK-98 detail-koder → STYRK-08 4-sifret target)
  # Korrespondance hentet fra SSB Klass 145 → Klass 7
  if ("styrk08" %in% names(occ)) {
    cand <- occ[!is.na(occ$styrk08) & occ$styrk08 == styrk4_code, , drop = FALSE]
  } else {
    # Fallback: prefix-match (gammel STYRK-98 logikk)
    cand <- occ[substr(occ$code, 1, 4) == styrk4_code, , drop = FALSE]
  }
  if (nrow(cand) == 0) return(fallback_label)

  # Drop titler som motsier egos kjonn. Faller tilbake pa ufiltrert liste
  # hvis filteret skulle tomme kandidatsettet.
  if (!is.null(gender) && !is.na(gender)) {
    drop <- if (identical(toupper(gender), "F")) {
      .gendered_yrke_titles$male
    } else {
      .gendered_yrke_titles$female
    }
    keep <- !(toupper(cand$name) %in% drop)
    if (any(keep)) cand <- cand[keep, , drop = FALSE]
  }

  pick <- cand$name[sample.int(nrow(cand), 1)]
  .titlecase_yrke(pick)
}

# Convert ALL-CAPS Norwegian yrkesnavn til sentence case.
.titlecase_yrke <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(s)
  out <- tolower(s)
  # Cap first letter
  out <- sub("^([a-z\u{00e6}\u{00f8}\u{00e5}])", "\\U\\1", out, perl = TRUE)
  # Cap letter after "(" or ", " or ": " or " - "
  out <- gsub("([(,:\\-] ?)([a-z\u{00e6}\u{00f8}\u{00e5}])", "\\1\\U\\2", out, perl = TRUE)
  # Restore common acronyms
  acronyms <- c("LIS1", "LIS", "NAV", "KRLE", "ICT", "IKT", "BJJ", "MMA",
                "VVS", "HMS", "KOLS", "ADHD", "AAP", "FOU", "PR",
                "EU", "USA", "UK", "FN", "WTO", "DPS", "TV")
  for (a in acronyms) {
    pattern <- paste0("\\b", tolower(a), "\\b")
    out <- gsub(pattern, a, out, perl = TRUE)
  }
  out
}

# --- Mediadiet ------------------------------------------------------------
.cond_media <- function(age, edu_code = NULL, sentralitet = NULL,
                        party_code = NULL, bourdieu_klasse = NULL, lang = "en") {
  .load_data()
  mp <- .cfm_env$media_papers
  tv <- .cfm_env$tv_hours
  pa <- .cfm_env$podcast_activity
  sm <- .cfm_env$social_media_use
  if (is.null(mp)) return(list(paper = NA_character_))

  band <- if (age < 16) "0-15"
          else if (age < 25) "16-24"
          else if (age < 45) "25-44"
          else if (age < 65) "45-64"
          else if (age < 80) "65-79"
          else "80+"

  # Avis-favoritt
  age_mult_col <- if (age < 30) "under30_mult"
                  else if (age < 50) "age30_50_mult"
                  else if (age < 67) "age50_67_mult"
                  else "over67_mult"
  w <- as.numeric(mp$base_weight) * as.numeric(mp[[age_mult_col]])
  if (!is.null(edu_code) && !is.na(edu_code)) {
    if (edu_code <= 2) w <- w * as.numeric(mp$low_edu_mult)
    else if (edu_code >= 6) w <- w * as.numeric(mp$high_edu_mult)
  }
  if (!is.null(sentralitet) && !is.na(sentralitet)) {
    if (sentralitet <= 2) w <- w * as.numeric(mp$storby_mult)
    else if (sentralitet >= 5) w <- w * as.numeric(mp$distrikt_mult)
  }

  # Partilojal-justering (sterk korrelasjon avis ↔ parti i Norge)
  if (!is.null(party_code) && !is.na(party_code)) {
    idx_klassekamp <- which(mp$paper == "Klassekampen")
    idx_morgenbl   <- which(mp$paper == "Morgenbladet")
    idx_vart_land  <- which(mp$paper == "V\u{00e5}rt Land")
    idx_nationen   <- which(mp$paper == "Nationen")
    idx_dn         <- which(mp$paper == "Dagens N\u{00e6}ringsliv")
    idx_aften      <- which(mp$paper == "Aftenposten")
    idx_vg         <- which(mp$paper == "VG")
    idx_dag        <- which(mp$paper == "Dagbladet")
    idx_lokal      <- which(mp$paper == "Lokalavis")
    if (party_code %in% c("SV", "R", "MDG")) {
      w[idx_klassekamp] <- w[idx_klassekamp] * 6
      w[idx_morgenbl]   <- w[idx_morgenbl]   * 3
      w[idx_dn]         <- w[idx_dn]         * 0.3
    } else if (party_code == "KRF") {
      w[idx_vart_land] <- w[idx_vart_land] * 10
    } else if (party_code == "SP") {
      w[idx_nationen] <- w[idx_nationen] * 7
      w[idx_lokal]    <- w[idx_lokal]    * 1.5
    } else if (party_code == "H") {
      w[idx_aften] <- w[idx_aften] * 1.8
      w[idx_dn]    <- w[idx_dn]    * 2.5
    } else if (party_code == "V") {
      w[idx_aften]    <- w[idx_aften]    * 1.7
      w[idx_morgenbl] <- w[idx_morgenbl] * 2.0
    } else if (party_code == "FRP") {
      w[idx_vg]  <- w[idx_vg]  * 1.6
      w[idx_dag] <- w[idx_dag] * 1.3
      w[idx_klassekamp] <- w[idx_klassekamp] * 0.1
      w[idx_morgenbl]   <- w[idx_morgenbl]   * 0.2
    } else if (party_code == "AP") {
      w[idx_vg]    <- w[idx_vg]    * 1.3
      w[idx_lokal] <- w[idx_lokal] * 1.2
    }
  }

  # Bourdieu-justering (kulturell elite ↔ "kvalitetspresse")
  if (!is.null(bourdieu_klasse) && !is.na(bourdieu_klasse)) {
    idx_aften    <- which(mp$paper == "Aftenposten")
    idx_morgenbl <- which(mp$paper == "Morgenbladet")
    idx_dn       <- which(mp$paper == "Dagens N\u{00e6}ringsliv")
    idx_vg       <- which(mp$paper == "VG")
    idx_dag      <- which(mp$paper == "Dagbladet")
    idx_ingen    <- which(mp$paper == "Ingen avis")
    if (grepl("[Kk]ulturell elite|[Ee]tablert overklasse", bourdieu_klasse)) {
      w[idx_aften]    <- w[idx_aften]    * 2.0
      w[idx_morgenbl] <- w[idx_morgenbl] * 3.5
      w[idx_vg]       <- w[idx_vg]       * 0.3
      w[idx_dag]      <- w[idx_dag]      * 0.2
      w[idx_ingen]    <- w[idx_ingen]    * 0.2
    } else if (grepl("[\u{00d8}\u{00f8}]konomisk elite", bourdieu_klasse)) {
      w[idx_dn]    <- w[idx_dn]    * 4.0
      w[idx_aften] <- w[idx_aften] * 1.5
    } else if (grepl("[Pp]rekariat|[Nn]y arbeiderklasse", bourdieu_klasse)) {
      w[idx_vg]    <- w[idx_vg]    * 1.4
      w[idx_dag]   <- w[idx_dag]   * 1.2
      w[idx_ingen] <- w[idx_ingen] * 1.5
      w[idx_morgenbl] <- w[idx_morgenbl] * 0.1
      w[idx_dn]    <- w[idx_dn]    * 0.2
    }
  }

  w <- pmax(w, 0); w <- w / sum(w)
  paper <- sample(mp$paper, 1, prob = w)

  # TV-timer
  tv_row <- tv[tv$age_band == band, ]
  tv_h <- if (nrow(tv_row) > 0)
            max(0, round(stats::rnorm(1, tv_row$mean_hours[1], tv_row$sd[1]), 1))
          else NA_real_

  # Podcast
  age_cat <- if (age < 30) "under30_mult"
             else if (age < 50) "age30_50_mult"
             else if (age < 67) "age50_67_mult"
             else "over67_mult"
  if (!is.null(pa)) {
    pw <- as.numeric(pa$share) * as.numeric(pa[[age_cat]])
    pw <- pw / sum(pw)
    pod_idx <- sample.int(nrow(pa), 1, prob = pw)
    podcast_lbl <- if (identical(lang, "no")) pa$level_no[pod_idx] else pa$level_en[pod_idx]
  } else podcast_lbl <- NA_character_

  # Sosiale medier
  if (!is.null(sm)) {
    sw <- as.numeric(sm$share) * as.numeric(sm[[age_cat]])
    sw <- sw / sum(sw)
    sm_idx <- sample.int(nrow(sm), 1, prob = sw)
    sm_lbl <- if (identical(lang, "no")) sm$level_no[sm_idx] else sm$level_en[sm_idx]
  } else sm_lbl <- NA_character_

  # --- Aldersgating for barn ---
  if (age < 16) paper <- NA_character_  # ingen avisfavoritt for barn
  if (age < 10 && !is.null(pa)) {
    i0 <- which(pa$level_no == "Aldri")[1]
    if (!is.na(i0)) podcast_lbl <- if (identical(lang, "no")) pa$level_no[i0] else pa$level_en[i0]
  }
  if (age < 9 && !is.null(sm)) {
    i0 <- which(sm$level_no == "Ikke aktiv")[1]
    if (!is.na(i0)) sm_lbl <- if (identical(lang, "no")) sm$level_no[i0] else sm$level_en[i0]
  }

  # Avis, podkast og sosiale medier gates ved 13; TV-tid ved 16 fordi
  # 0-15-raden i tv_hours.csv er et anslag uten kilde.
  if (age < .dimension_min_age[["media_tv_hours"]]) tv_h       <- NA_real_
  if (age < .dimension_min_age[["media_paper"]])   paper       <- NA_character_
  if (age < .dimension_min_age[["media_podcast"]]) podcast_lbl <- NA_character_
  if (age < .dimension_min_age[["media_social"]])  sm_lbl      <- NA_character_
  list(paper = paper, tv_hours = tv_h, podcast = podcast_lbl, social_media = sm_lbl)
}

# --- Daglig tidsbruk: søvn ------------------------------------------------
.cond_sleep <- function(age, n_children = 0, styrk_code = NULL,
                        has_chronic = FALSE, lang = "en") {
  # 0-15-raden i sleep_hours.csv er et anslag uten kilde.
  if (is.null(age) || is.na(age) ||
      age < .dimension_min_age[["sleep_hours"]]) return(NA_real_)
  .load_data()
  sh <- .cfm_env$sleep_hours
  if (is.null(sh)) return(NA_real_)
  band <- if (age < 16) "0-15"
          else if (age < 25) "16-24"
          else if (age < 45) "25-44"
          else if (age < 65) "45-64"
          else if (age < 80) "65-79"
          else "80+"
  row <- sh[sh$age_band == band, ]
  if (nrow(row) == 0) return(NA_real_)
  base <- stats::rnorm(1, row$mean_hours[1], row$sd[1])

  # Småbarnsforeldre sover mindre (under 30 år ekstra, 30-44 mest, 45+ litt)
  if (!is.null(n_children) && !is.na(n_children) && n_children > 0) {
    if (age >= 25 && age <= 44) base <- base - stats::runif(1, 0.5, 1.5)
    else if (age >= 20 && age < 25) base <- base - stats::runif(1, 0.3, 1.0)
  }

  # Skiftarbeidere: sykepleiere, sjåfører, sikkerhetsvakter — mer varians + ~30 min mindre
  if (!is.null(styrk_code) && !is.na(styrk_code) && nzchar(styrk_code)) {
    sj2 <- substr(as.character(styrk_code), 1, 2)
    # 32 (helsearbeidere), 51 (service), 83 (transport), 54 (sikkerhet/vakt)
    if (sj2 %in% c("32", "33", "51", "53", "54", "83", "93")) {
      base <- base + stats::rnorm(1, -0.3, 0.8)  # varians ↑
    }
  }

  # Kronisk sykdom → litt dårligere søvn
  if (isTRUE(has_chronic)) base <- base - stats::runif(1, 0.0, 0.6)

  max(3.5, min(12, round(base, 1)))
}

# --- Kosthold -------------------------------------------------------------
.cond_diet <- function(age, edu_code = NULL, gender = NULL, lang = "en") {
  # Barn under 13: kostholdet bestemmes hjemme, ingen egen "diett"
  if (!is.null(age) && !is.na(age) && age < 13) {
    return(list(diet = if (identical(lang, "no"))
                  "Spiser det som blir servert hjemme"
                else "Eats whatever is served at home"))
  }
  .load_data()
  dp <- .cfm_env$diet_patterns
  if (is.null(dp)) return(list(diet = NA_character_))
  w <- as.numeric(dp$share)
  age_col <- if (age < 30) "under30_mult"
             else if (age < 50) "age30_50_mult"
             else if (age < 67) "age50_67_mult"
             else "over67_mult"
  w <- w * as.numeric(dp[[age_col]])
  if (!is.null(edu_code) && !is.na(edu_code)) {
    if (edu_code <= 2) w <- w * as.numeric(dp$low_edu_mult)
    else if (edu_code >= 6) w <- w * as.numeric(dp$high_edu_mult)
  }
  # Kjønnsbetinging: vegetar/vegan/fleksitar/glutenfri kvinnedominert,
  # kjøtt-tungt + lavkarbo mannsdominert (SSB/Helsedirektoratet-mønstre).
  if (!is.null(gender) && !is.na(gender)) {
    fem <- identical(toupper(gender), "F")
    gm <- rep(1, nrow(dp))
    veg  <- grepl("[Vv]egetar|[Vv]egan", dp$diet_no)
    flex <- grepl("[Ff]leksitar|[Ff]lexitar", dp$diet_no)
    meat <- grepl("kj\u00f8tt-tung|meat-heavy|[Tt]radisjonell", dp$diet_no)
    lowc <- grepl("[Ll]avkarbo|keto|[Ll]ow-carb", dp$diet_no)
    glut <- grepl("[Gg]lutenfri|[Gg]luten", dp$diet_no)
    if (fem) { gm[veg] <- 1.9; gm[flex] <- 1.4; gm[glut] <- 1.5; gm[meat] <- 0.6; gm[lowc] <- 0.85 }
    else     { gm[veg] <- 0.55; gm[flex] <- 0.7; gm[glut] <- 0.7; gm[meat] <- 1.3; gm[lowc] <- 1.15 }
    w <- w * gm
  }
  w <- pmax(w, 0); w <- w / sum(w)
  idx <- sample.int(nrow(dp), 1, prob = w)
  diet_lbl <- if (identical(lang, "no")) dp$diet_no[idx] else dp$diet_en[idx]
  list(diet = diet_lbl)
}

# --- Alkoholmønster -------------------------------------------------------
.cond_alcohol <- function(age, edu_code = NULL, gender = NULL,
                          diet_label = NULL, lang = "en") {
  # HARD RULE: under 16 -> no alcohol pattern at all
  if (is.null(age) || is.na(age) || age < 16) return(NA_character_)
  .load_data()
  ap <- .cfm_env$alcohol_patterns
  if (is.null(ap)) return(NA_character_)
  w <- as.numeric(ap$share)
  age_col <- if (age < 30) "under30_mult"
             else if (age < 50) "age30_50_mult"
             else if (age < 67) "age50_67_mult"
             else "over67_mult"
  w <- w * as.numeric(ap[[age_col]])
  if (!is.null(edu_code) && !is.na(edu_code)) {
    if (edu_code <= 2) w <- w * as.numeric(ap$low_edu_mult)
    else if (edu_code >= 6) w <- w * as.numeric(ap$high_edu_mult)
  }
  if (!is.null(gender) && identical(toupper(gender), "F")) {
    w <- w * as.numeric(ap$F_mult)
  }

  # Diet-justering: veganer/vegetar drikker mindre, lavkarbo/keto også (helse-fokus)
  if (!is.null(diet_label) && !is.na(diet_label)) {
    idx_avhold <- which(grepl("Avhold|Teetot", ap$pattern_no) | grepl("Avhold|Teetot", ap$pattern_en))
    idx_sjelden <- which(grepl("Sjelden|Rarely", ap$pattern_no) | grepl("Sjelden|Rarely", ap$pattern_en))
    idx_helger <- which(grepl("Helger|Weekend", ap$pattern_no) | grepl("Helger|Weekend", ap$pattern_en))
    idx_flere <- which(grepl("Flere ganger|Several", ap$pattern_no) | grepl("Flere|Several", ap$pattern_en))
    idx_daglig <- which(grepl("Daglig|Daily", ap$pattern_no) | grepl("Daglig|Daily", ap$pattern_en))
    if (grepl("[Vv]egan", diet_label)) {
      w[idx_avhold] <- w[idx_avhold] * 4
      w[idx_sjelden] <- w[idx_sjelden] * 2.5
      w[idx_helger] <- w[idx_helger] * 0.7
      w[idx_flere] <- w[idx_flere] * 0.3
      w[idx_daglig] <- w[idx_daglig] * 0.1
    } else if (grepl("[Vv]egetar", diet_label)) {
      w[idx_avhold] <- w[idx_avhold] * 2
      w[idx_sjelden] <- w[idx_sjelden] * 1.5
      w[idx_flere] <- w[idx_flere] * 0.7
      w[idx_daglig] <- w[idx_daglig] * 0.3
    } else if (grepl("[Ll]avkarbo|[Kk]eto|[Gg]lutenfri", diet_label)) {
      w[idx_sjelden] <- w[idx_sjelden] * 1.5
      w[idx_daglig] <- w[idx_daglig] * 0.5
    }
  }

  w <- pmax(w, 0); w <- w / sum(w)
  idx <- sample.int(nrow(ap), 1, prob = w)
  if (identical(lang, "no")) ap$pattern_no[idx] else ap$pattern_en[idx]
}


# --- Kriminalitet: utsatthet (victimization) ----------------------------
.cond_crime <- function(age, gender = NULL, sentralitet = NULL,
                        lang = "en") {
  if (is.null(age) || is.na(age) || age < 12) {
    return(list(victimizations = character(0),
                safety_feeling = NA_character_,
                minor_offence = NA_character_))
  }
  .load_data()
  cv <- .cfm_env$crime_victimization
  sf <- .cfm_env$crime_safety_feeling
  mo <- .cfm_env$minor_offences

  victims <- character(0)
  if (!is.null(cv)) {
    for (i in seq_len(nrow(cv))) {
      p <- as.numeric(cv$base_rate[i])
      if (!is.null(gender) && identical(toupper(gender), "M")) {
        p <- p * as.numeric(cv$male_mult[i])
      } else if (!is.null(gender) && identical(toupper(gender), "F")) {
        p <- p * as.numeric(cv$female_mult[i])
      }
      age_col <- if (age < 30) "under30_mult"
                 else if (age < 50) "age30_50_mult"
                 else if (age < 67) "age50_67_mult"
                 else "over67_mult"
      p <- p * as.numeric(cv[[age_col]][i])
      if (!is.null(sentralitet) && !is.na(sentralitet)) {
        if (sentralitet <= 2) p <- p * as.numeric(cv$storby_mult[i])
        else if (sentralitet >= 5) p <- p * as.numeric(cv$distrikt_mult[i])
      }
      # Skip seksualkrenkelse for under 18
      if (age < 18 && grepl("[Ss]eksualkrenkelse|[Ss]exual", cv$type_no[i])) next
      # Aldersgate fra CSV (min_age per type)
      if ("min_age" %in% names(cv)) {
        ma <- suppressWarnings(as.numeric(cv$min_age[i]))
        if (!is.na(ma) && age < ma) next
      }
      if (stats::runif(1) < min(1, p)) {
        victims <- c(victims, if (identical(lang, "no")) cv$type_no[i] else cv$type_en[i])
      }
    }
  }

  # Trygghet
  safety_lbl <- NA_character_
  if (!is.null(sf)) {
    w <- as.numeric(sf$base_share)
    if (!is.null(sentralitet) && !is.na(sentralitet)) {
      if (sentralitet <= 2) w <- w * as.numeric(sf$storby_mult)
      else if (sentralitet >= 5) w <- w * as.numeric(sf$distrikt_mult)
    }
    if (!is.null(gender) && identical(toupper(gender), "F")) {
      w <- w * as.numeric(sf$F_mult)
    }
    if (age < 30) w <- w * as.numeric(sf$under30_mult)
    else if (age >= 67) w <- w * as.numeric(sf$over67_mult)
    w <- pmax(w, 0); w <- w / sum(w)
    idx <- sample.int(nrow(sf), 1, prob = w)
    safety_lbl <- if (identical(lang, "no")) sf$level_no[idx] else sf$level_en[idx]
  }

  # Mindre lovbrudd — trekk én tilfeldig "merittert" hendelse
  minor_lbl <- NA_character_
  if (!is.null(mo)) {
    w <- as.numeric(mo$base_share)
    if ("min_age" %in% names(mo)) {
      ma <- suppressWarnings(as.numeric(mo$min_age))
      ma[is.na(ma)] <- 0
      w[ma > age] <- 0
    }
    if (!is.null(gender) && identical(toupper(gender), "M")) {
      w <- w * as.numeric(mo$male_mult)
    }
    age_col <- if (age < 30) "under30_mult"
               else if (age < 50) "age30_50_mult"
               else if (age < 67) "age30_50_mult"  # bruker samme som 30-50
               else "over67_mult"
    if (age_col %in% names(mo)) w <- w * as.numeric(mo[[age_col]])
    w <- pmax(w, 0)
    if (sum(w) > 0) {
      w <- w / sum(w)
      idx <- sample.int(nrow(mo), 1, prob = w)
      minor_lbl <- if (identical(lang, "no")) mo$offence_no[idx] else mo$offence_en[idx]
    }
  }

  list(victimizations = victims, safety_feeling = safety_lbl,
       minor_offence = minor_lbl)
}

# ============================================================
# Partner + parental relationship (added v0.9.20)
# ============================================================

.edu_label_by_code <- function(code, lang = "en") {
  if (is.null(code) || is.na(code)) return(NA_character_)
  ed <- .cfm_env$education
  if (is.null(ed)) return(NA_character_)
  row <- ed[ed$code == as.integer(code), , drop = FALSE]
  if (!nrow(row)) return(NA_character_)
  if (identical(lang, "no")) row$level_no[1] else row$level[1]
}

# Partner — only for partnered marital codes (2 gift, 6 partnerskap, 7 samboer).
# Gender follows orientation; age + education are assortative.
.cond_partner <- function(age, gender, orientation_code = NULL,
                          marital_code = NULL, edu_code = NULL, lang = "en") {
  no <- identical(lang, "no")
  if (is.null(age) || is.na(age) || age < 18) return(NULL)
  if (is.null(marital_code) || is.na(marital_code) ||
      !(marital_code %in% c(2L, 6L, 7L))) return(NULL)

  ego_g <- if (identical(gender, "M")) "M" else "F"
  opp <- if (identical(ego_g, "M")) "F" else "M"
  pg <- if (identical(as.integer(marital_code), 6L)) ego_g
        else if (identical(orientation_code, "gay")) ego_g
        else if (is.null(orientation_code) || is.na(orientation_code) ||
                 orientation_code %in% c("hetero", "ace", "uavklart")) opp
        else if (orientation_code %in% c("bi", "pan", "queer"))
          (if (stats::runif(1) < 0.5) ego_g else opp)
        else opp

  shift <- if (identical(ego_g, "M") && identical(pg, "F")) -1.5
           else if (identical(ego_g, "F") && identical(pg, "M")) 1.5 else 0
  pa <- as.integer(round(age + shift + stats::rnorm(1, 0, 3)))
  if (is.na(pa)) pa <- age
  pa <- max(18L, min(pa, 95L))

  ego_lvl <- suppressWarnings(as.integer(substr(as.character(edu_code %||% ""), 1, 1)))
  if (is.na(ego_lvl)) ego_lvl <- 4L
  delta <- sample(c(-2L, -1L, 0L, 0L, 0L, 1L, 2L), 1)
  plvl <- max(1L, min(8L, ego_lvl + delta))
  p_edu_label <- .edu_label_by_code(plvl, lang = lang)

  p_occ <- tryCatch(.cond_occupation(pa, as.character(plvl), gender = pg),
                    error = function(e) NULL)
  p_occ_label <- if (!is.null(p_occ)) p_occ$label else NA_character_

  by <- .cfm_env$ref_year - pa
  p_name <- tryCatch(sample_first_name(gender = pg, birth_year = by),
                     error = function(e) NA_character_)

  rel_word <- if (identical(as.integer(marital_code), 7L))
                (if (no) "samboer" else "cohabiting partner")
              else (if (no) "ektefelle" else "spouse")

  list(name = p_name, gender = pg, age = pa,
       education = p_edu_label, education_code = plvl,
       occupation = p_occ_label, relation = rel_word)
}

# Parents' relationship while ego grew up. Divorce probability rises by
# cohort; an early parental death is framed as loss rather than divorce.
.cond_parents_relationship <- function(age, mother = NULL, father = NULL, lang = "en") {
  no <- identical(lang, "no")
  if (is.null(age) || is.na(age)) return(NULL)
  birth_year <- .cfm_env$ref_year - age

  early_loss <- FALSE
  for (p in list(mother, father)) {
    if (!is.null(p) && !is.null(p$death_year) && !is.na(p$death_year) &&
        p$death_year < birth_year + 18L) early_loss <- TRUE
  }
  if (early_loss) {
    return(list(status = "loss", divorced = FALSE,
                label = if (no) "Mistet en forelder tidlig" else "Lost a parent early"))
  }

  p_div <- if (birth_year >= 2000) 0.35
           else if (birth_year >= 1985) 0.30
           else if (birth_year >= 1970) 0.20
           else if (birth_year >= 1955) 0.11
           else 0.06
  if (stats::runif(1) < p_div) {
    list(status = "divorced", divorced = TRUE,
         label = if (no) "Foreldrene er skilt" else "Parents are divorced")
  } else {
    list(status = "together", divorced = FALSE,
         label = if (no) "Foreldrene er fortsatt sammen" else "Parents still together")
  }
}

# ============================================================
# Funksjonsnedsettelse + sosial støtte / venner (added v0.9.21)
# Levekårsundersøkelsen om funksjonsnedsettelse + Levekår generelt.
# ============================================================

# Funksjonsnedsettelse — uavhengig av jobbstatus. Trekkes FØR yrke i
# counterfact_me(), og mates inn i .cond_occupation for å heve uføre-risiko.
.cond_disability <- function(age, gender = NULL, lang = "en") {
  no <- identical(lang, "no")
  none <- list(has = FALSE, type = NA_character_, severity = NA_character_, label = NA_character_)
  if (is.null(age) || is.na(age)) return(none)
  # Barnesannsynligheten var hardkodet 0.06 med hardkodede typevekter, uten
  # kilde. disability_by_age.csv hjelper ikke: den gjelder uforetrygd, ikke
  # funksjonsnedsettelse, og starter pa 18-24.
  if (age < .dimension_min_age[["has_disability"]]) return(none)

  # Andel med funksjonsnedsettelse, grovt etter SSB-aldersgradient
  p <- if (age < 25) 0.14
       else if (age < 45) 0.13
       else if (age < 67) 0.20
       else if (age < 80) 0.30
       else 0.45
  if (stats::runif(1) >= p) return(none)

  fem <- identical(toupper(gender %||% ""), "F")
  man <- identical(toupper(gender %||% ""), "M")
  if (age < 18) {
    types <- c("l\u{00e6}revansker", "ADHD", "autismespekter", "bevegelse", "syn", "h\u{00f8}rsel")
    tw <- c(0.30, 0.22, 0.12, 0.18, 0.09, 0.09)
    # ADHD/autisme/lærevansker diagnostiseres klart oftere hos gutter
    if (man) tw <- tw * c(1.3, 1.8, 2.5, 1.0, 1.0, 1.0)
    else if (fem) tw <- tw * c(0.8, 0.55, 0.4, 1.0, 1.0, 1.0)
  } else {
    types <- c("bevegelse", "psykisk", "h\u{00f8}rsel", "syn", "kognitiv", "annet")
    tw <- c(0.40, 0.22, 0.14, 0.10, 0.07, 0.07)
    # psykiske lidelser oftere registrert hos kvinner; hørsel/kognitiv litt oftere menn
    if (fem) tw <- tw * c(1.0, 1.5, 0.8, 1.0, 0.8, 1.0)
    else if (man) tw <- tw * c(1.0, 0.75, 1.2, 1.0, 1.25, 1.0)
  }
  ty <- sample(types, 1, prob = tw)
  sev <- sample(c("mild", "moderat", "alvorlig"), 1, prob = c(0.45, 0.38, 0.17))
  lbl <- if (no) sprintf("%s (%s)", ty, sev) else sprintf("%s (%s)", ty, sev)
  list(has = TRUE, type = ty, severity = sev, label = lbl)
}

# Venner / sosial støtte — betinget på ensomhet (trekkes ETTER isolasjon).
.cond_social_support <- function(age, loneliness = NULL, household = NULL, lang = "en") {
  no <- identical(lang, "no")
  if (is.null(age) || is.na(age))
    return(list(close_friends = NA_character_, n_band = NA_integer_, has_confidant = NA))
  # Naere venner / fortrolig er Levekar-sporsmal (16+).
  if (age < .dimension_min_age[["close_friends"]])
    return(list(close_friends = NA_character_, n_band = NA_integer_, has_confidant = NA))

  lon <- tolower(loneliness %||% "")
  lonely_often <- grepl("ofte|often", lon)
  lonely_some  <- grepl("av og til|sometimes", lon)

  # band: 1 ingen, 2 = 1-2, 3 = 3-5, 4 = 6+
  base <- c(0.06, 0.24, 0.45, 0.25)
  if (lonely_often) base <- base * c(4.0, 1.6, 0.7, 0.4)
  else if (lonely_some) base <- base * c(1.6, 1.2, 1.0, 0.8)
  if (age >= 67) base <- base * c(1.4, 1.2, 1.0, 0.8)
  base <- base / sum(base)
  k <- sample(1:4, 1, prob = base)

  bands_no <- c("Ingen n\u{00e6}re venner", "1\u{2013}2 n\u{00e6}re venner",
                "3\u{2013}5 n\u{00e6}re venner", "6 eller flere n\u{00e6}re venner")
  bands_en <- c("No close friends", "1-2 close friends",
                "3-5 close friends", "6+ close friends")
  friends <- if (no) bands_no[k] else bands_en[k]

  # A confidant need not be a friend. For most people it is a partner, a
  # sibling or an adult child, which is why Levekar asks the two
  # questions separately. So "no close friends" and "has a confidant" is
  # a real combination -- but only if there is somebody in the household.
  # Living alone with no close friends and still having someone to
  # confide in is possible and rare; the flat 0.35 here ignored the
  # household entirely, and `household` was accepted but never used.
  # Enumerating the cohabiting categories is brittle -- it missed both
  # "Enslig mor/far med voksne barn" and "Flerfamiliehusholdning" on the
  # first attempt. Only living alone is unambiguous, so test for that.
  lives_with_others <- TRUE
  if (!is.null(household) && !is.na(household)) {
    hh <- tolower(as.character(household))
    lives_with_others <- !grepl("aleneboende|enslig$|living alone|^single$", hh)
  }

  p_conf <- if (lonely_often) 0.45 else if (lonely_some) 0.80 else 0.90
  if (k == 1L) {
    p_conf <- if (lives_with_others) min(p_conf, 0.55) else min(p_conf, 0.12)
  } else if (!lives_with_others) {
    p_conf <- p_conf * 0.9
  }
  has_conf <- stats::runif(1) < p_conf

  list(close_friends = friends, n_band = k, has_confidant = has_conf)
}
