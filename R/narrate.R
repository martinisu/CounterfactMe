# ============================================================
# Template-based biography narration (story engine v2).
# Pure R, no API, no network. Norwegian.
#
# Design goals over v1:
#  (1) Salient lead   — open on the single most distinctive fact.
#  (2) Trajectory     — connect parental class/education to own outcome.
#  (3) Combined prose — fold related fields into one sentence + connectives.
#  (4) Fewer artefacts— no lowercasing of proper nouns, no double "På fritiden",
#                       varied sentence openings, concrete numbers over buckets.
# ============================================================

#' Generate a biographical narrative from a counterfactual life
#'
#' Takes a \code{counterfactme} object and produces a multi-paragraph
#' biography in Norwegian, written as natural prose. Template-based —
#' no LLM, no API, no network. For an LLM-written version (local Ollama),
#' see \code{\link{narrate_life_llm}}.
#'
#' @param x A \code{counterfactme} object.
#' @param style One of \code{"biography"} (default), \code{"obituary"},
#'   or \code{"compact"}.
#' @param seed Optional integer for reproducible narration.
#' @return A \code{counterfactme_narrative} object.
#' @export
narrate_life <- function(x, style = c("biography", "obituary", "compact"),
                         seed = NULL) {
  style <- match.arg(style)
  if (!is.null(seed)) set.seed(seed)

  is_no <- is.null(x$lang) || identical(x$lang, "no")
  if (!is_no) warning("English narration not yet implemented; using Norwegian.")

  if (identical(style, "compact")) {
    text <- .narrate_compact(x)
  } else if (identical(style, "obituary")) {
    text <- .narrate_obituary(x)
  } else {
    lead <- .narrate_lead(x)
    has_lead <- !is.na(lead) && nzchar(lead)
    paragraphs <- c(
      lead,
      .narrate_opening(x, has_lead = has_lead),
      .narrate_origins(x),
      .narrate_education_career(x),
      .narrate_family_life(x),
      .narrate_material(x),
      .narrate_cultural(x),
      .narrate_wellbeing(x)
    )
    paragraphs <- paragraphs[!is.na(paragraphs) & nzchar(paragraphs)]
    text <- paste(paragraphs, collapse = "\n\n")
  }

  class(text) <- c("counterfactme_narrative", "character")
  text
}

#' Print a counterfactual life narrative
#'
#' @param x A \code{counterfactme_narrative} object.
#' @param ... Ignored.
#' @return Invisibly returns \code{x}.
#' @export
print.counterfactme_narrative <- function(x, ...) {
  cat(x, "\n")
  invisible(x)
}

# ---- small helpers -----------------------------------------------------

.pick <- function(v) {
  v <- v[!is.na(v) & nzchar(v)]
  if (!length(v)) return(NA_character_)
  if (length(v) == 1L) return(v)
  sample(v, 1L)
}

.pronouns <- function(g, case = "subj") {
  if (is.null(g) || is.na(g)) g <- "F"
  if (identical(g, "M")) {
    switch(case, subj = "han", obj = "ham", poss = "hans", refl = "seg")
  } else {
    switch(case, subj = "hun", obj = "henne", poss = "hennes", refl = "seg")
  }
}

.cap <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(s)
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# lowercase ONLY the first letter (for mid-sentence common nouns); leaves
# the rest — and any internal proper nouns — untouched.
.decap <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return(s)
  paste0(tolower(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

.birthyear <- function(x) {
  if (!is.null(x$birth_year) && !is.na(x$birth_year)) return(as.integer(x$birth_year))
  if (is.null(x$age) || is.na(x$age)) return(NA_integer_)
  2026L - as.integer(x$age)
}

.no_count <- function(n) {
  if (is.null(n) || is.na(n)) return(NA_character_)
  w <- c("ett", "to", "tre", "fire", "fem", "seks", "sju", "åtte", "ni", "ti")
  if (n >= 1 && n <= 10) w[n] else as.character(n)
}

.join_no <- function(v) {
  v <- v[!is.na(v) & nzchar(v)]
  n <- length(v)
  if (n == 0) return(NA_character_)
  if (n == 1) return(v)
  if (n == 2) return(paste(v[1], "og", v[2]))
  paste0(paste(v[-n], collapse = ", "), " og ", v[n])
}

.edu_level <- function(code) {
  if (is.null(code) || all(is.na(code))) return(NA_integer_)
  suppressWarnings(as.integer(substr(as.character(code)[1], 1, 1)))
}

.edu_level_label_no <- function(l) {
  if (is.null(l) || is.na(l)) return(NA_character_)
  switch(as.character(l),
    "0" = "ingen fullført utdanning",
    "1" = "grunnskole", "2" = "grunnskole",
    "3" = "videregående", "4" = "videregående",
    "5" = "fagskole",
    "6" = "bachelorgrad", "7" = "mastergrad", "8" = "doktorgrad",
    NA_character_)
}

.income_phrase <- function(income, neet = FALSE) {
  if (isTRUE(neet)) return("står utenfor arbeidslivet")
  if (is.null(income) || is.na(income)) return(NA_character_)
  if (income < 50000) return(NA_character_)
  r <- round(income / 10000) * 10000
  sprintf("en årsinntekt på rundt %s kroner",
          formatC(r, format = "d", big.mark = " "))
}

.qualitative_wealth <- function(nw) {
  if (is.null(nw) || is.na(nw)) return(NA_character_)
  if (nw < -500000) "negativ formue"
  else if (nw < 0) "litt gjeld"
  else if (nw < 500000) "lite formue"
  else if (nw < 2e6) "middels formue"
  else if (nw < 5e6) "god formue"
  else if (nw < 15e6) "betydelig formue"
  else "stor formue"
}

.nsiblings <- function(x) {
  if (!is.null(x$n_siblings) && !is.na(x$n_siblings)) return(as.integer(x$n_siblings))
  if (!is.null(x$siblings)) return(length(x$siblings))
  NA_integer_
}

# educational mobility relative to parents (adults only)
.mobility <- function(x) {
  age <- x$age
  if (is.null(age) || is.na(age) || age < 25) return(NULL)
  ego <- .edu_level(x$edu_code)
  if (is.na(ego)) return(NULL)
  par <- suppressWarnings(max(c(.edu_level(x$mother_edu_code),
                                .edu_level(x$father_edu_code)), na.rm = TRUE))
  if (!is.finite(par)) return(NULL)
  d <- ego - par
  dir <- if (d >= 2) "sterk_opp" else if (d == 1) "opp" else if (d == 0) "lik"
         else if (d == -1) "ned" else "sterk_ned"
  list(dir = dir, ego = ego, par = par,
       ego_lbl = .edu_level_label_no(ego), par_lbl = .edu_level_label_no(par))
}

.is_not_religious <- function(rel) {
  grepl("ingen|ateis|ikke|sekul|livssyn|human", tolower(rel %||% ""))
}

# ---- salient lead (the hook) ------------------------------------------

.narrate_lead <- function(x) {
  name <- x$name %||% "Personen"
  g <- x$gender
  pron <- .pronouns(g, "subj")
  occ <- tolower(x$occupation %||% "")
  muni <- x$municipality %||% x$county %||% "Norge"
  party <- tolower(x$party %||% "")
  rel <- x$religion %||% ""
  nw <- x$net_wealth_nok
  sent <- x$sentralitet
  bg <- x$background %||% "majority"

  # 1) primary producer in a central municipality
  if (grepl("bonde|gårdbruk|gardbruk|jordbruk|fisker|skogbruk", occ) &&
      !is.null(sent) && !is.na(sent) && sent <= 2) {
    return(.pick(c(
      sprintf("Det er ikke mange igjen som livnærer seg slik %s gjør — som %s, midt i %s.",
              name, occ, muni),
      sprintf("%s er %s i %s, der de fleste andre jobber med noe helt annet.",
              name, occ, muni))))
  }
  # 2) Christian-Democrat but not a believer
  if (grepl("kristelig|krf", party) && .is_not_religious(rel)) {
    return(sprintf("%s stemmer kristelig, men regner seg ikke som troende — det henger ikke alltid sammen.",
                   name))
  }
  # 3) far-left with a fortune
  if (grepl("rødt|roedt|moxnes|kapitalismen", party) &&
      !is.null(nw) && !is.na(nw) && nw > 5e6) {
    return(sprintf("%s stemmer Rødt og sitter samtidig på en formue de fleste bare drømmer om.",
                   name))
  }
  # 4) immigrant background voting FrP
  if (bg %in% c("first_gen", "second_gen") &&
      grepl("fremskritt|frp|sylvi", party)) {
    cb <- x$country_background %||% x$country_label %||% NA
    if (!is.null(cb) && !is.na(cb)) {
      return(sprintf("%s, med røtter i %s, stemmer Fremskrittspartiet.", name, cb))
    }
  }
  # 5) strong upward educational mobility
  mob <- .mobility(x)
  if (!is.null(mob) && identical(mob$dir, "sterk_opp") &&
      !is.na(mob$par_lbl) && !is.na(mob$ego_lbl)) {
    return(sprintf("%s klatret lenger enn de fleste: fra %s i barndomshjemmet til %s.",
                   name, mob$par_lbl, mob$ego_lbl))
  }
  NA_character_
}

# ---- opening -----------------------------------------------------------

.narrate_opening <- function(x, has_lead = FALSE) {
  name <- x$name %||% "Personen"
  age <- x$age %||% NA
  by <- .birthyear(x)
  place <- x$municipality %||% x$county %||% "Norge"
  g <- x$gender
  pron <- .pronouns(g, "subj")

  bg_clause <- ""
  if (!is.null(x$background) && !is.na(x$background) &&
      !identical(x$background, "majority")) {
    cb <- x$country_background %||% x$country_label %||% ""
    if (identical(x$background, "first_gen")) {
      yrs <- x$years_in_norway
      bg_clause <- if (!is.null(yrs) && !is.na(yrs)) {
        sprintf(" %s kom til Norge fra %s for %d år siden.", .cap(pron), cb, yrs)
      } else if (nzchar(cb)) {
        sprintf(" %s er født i %s.", .cap(pron), cb)
      } else ""
    } else if (identical(x$background, "second_gen") && nzchar(cb)) {
      bg_clause <- sprintf(" Foreldrene kom fra %s.", cb)
    }
  }

  if (has_lead) {
    # avoid repeating the name; lead already used it
    base <- .pick(c(
      sprintf("%s er %s år og bor i %s.", .cap(pron), age, place),
      sprintf("I dag er %s %s år gammel, bosatt i %s.", pron, age, place),
      sprintf("%s år, %s.", age, place)))
  } else {
    base <- .pick(c(
      sprintf("%s er %s år gammel og bor i %s.", name, age, place),
      sprintf("%s, født i %s, bor i %s.", name, by %||% "?", place),
      sprintf("%s er %s år og holder til i %s.", name, age, place)))
  }
  paste0(base, bg_clause)
}

# ---- origins: parents, siblings, mobility -----------------------------

.narrate_origins <- function(x) {
  if (is.null(x$mother) && is.null(x$father)) return(NA_character_)
  g <- x$gender
  pron <- .pronouns(g, "subj")
  child_noun <- if (identical(g, "M")) "sønn" else "datter"

  m_occ <- tolower(x$mother$occupation %||% "")
  f_occ <- tolower(x$father$occupation %||% "")
  m_name <- x$mother$name %||% "moren"
  f_name <- x$father$name %||% "faren"

  parts <- character(0)
  if (nzchar(m_occ) || nzchar(f_occ)) {
    if (nzchar(m_occ) && nzchar(f_occ)) {
      parts <- c(parts, .pick(c(
        sprintf("%s vokste opp som %s av %s og %s.", .cap(pron), child_noun, m_occ, f_occ),
        sprintf("Moren %s jobbet som %s, faren %s som %s.", m_name, m_occ, f_name, f_occ),
        sprintf("Hjemme var det en %s og en %s som forsørget familien.", m_occ, f_occ))))
    } else {
      one <- if (nzchar(m_occ)) m_occ else f_occ
      parts <- c(parts, sprintf("%s vokste opp med en forelder som var %s.", .cap(pron), one))
    }
  }

  nsib <- .nsiblings(x)
  if (!is.na(nsib)) {
    if (nsib == 0L) {
      parts <- c(parts, .pick(c(
        sprintf("%s var enebarn.", .cap(pron)),
        "Søsken ble det ikke.")))
    } else {
      total <- nsib + 1L
      parts <- c(parts, .pick(c(
        sprintf("%s var ett av %s barn.", .cap(pron), .no_count(total)),
        sprintf("Søskenflokken talte %s.", .no_count(total)))))
    }
  }

  # parental relationship
  if (isTRUE(x$parents_divorced)) {
    parts <- c(parts, .pick(c(
      "Foreldrene skilte seg.",
      sprintf("%s vokste opp mellom to hjem etter at foreldrene gikk fra hverandre.", .cap(pron)))))
  } else if (!is.null(x$parents_relationship) && !is.na(x$parents_relationship) &&
             grepl("mistet|lost", tolower(x$parents_relationship))) {
    parts <- c(parts, sprintf("%s mistet en av foreldrene tidlig i livet.", .cap(pron)))
  }

  # trajectory clause
  mob <- .mobility(x)
  if (!is.null(mob) && !is.na(mob$ego_lbl) && !is.na(mob$par_lbl)) {
    if (mob$dir %in% c("sterk_opp", "opp")) {
      cl <- if (mob$dir == "sterk_opp" && mob$par <= 2 && mob$ego >= 6) {
        sprintf("Ingen hjemme hadde gått på universitetet; selv tok %s %s.", pron, mob$ego_lbl)
      } else {
        .pick(c(
          sprintf("Der foreldrene stoppet ved %s, gikk %s videre til %s.",
                  mob$par_lbl, pron, mob$ego_lbl),
          sprintf("%s tok lengre utdanning enn noen før i familien.", .cap(pron))))
      }
      parts <- c(parts, cl)
    } else if (mob$dir %in% c("ned", "sterk_ned")) {
      parts <- c(parts, sprintf("Til forskjell fra foreldrene, som hadde %s, endte det med %s for %s.",
                                mob$par_lbl, mob$ego_lbl, .pronouns(g, "obj")))
    } else if (identical(mob$dir, "lik") && runif(1) < 0.25) {
      parts <- c(parts, sprintf("Som foreldrene før %s ble utdanningen på %s-nivå.",
                                .pronouns(g, "obj"), mob$ego_lbl))
    }
  }

  paste(parts, collapse = " ")
}

# ---- education + career -----------------------------------------------

.narrate_education_career <- function(x) {
  parts <- character(0)
  g <- x$gender
  pron <- .pronouns(g, "subj")
  age_n <- if (!is.null(x$age) && !is.na(x$age)) as.integer(x$age) else NA_integer_

  # children / youth: school + play, no job/income judgement
  if (!is.na(age_n) && age_n < 16) {
    skole <- if (age_n < 6) "i barnehagen"
             else if (age_n <= 12) "på barneskolen"
             else "på ungdomsskolen"
    parts <- c(parts, .pick(c(
      sprintf("%s går %s.", .cap(pron), skole),
      sprintf("Til daglig går %s %s.", pron, skole))))
    occ <- x$occupation
    if (!is.null(occ) && !is.na(occ)) {
      parts <- c(parts, .pick(c(
        sprintf("På fritiden er %s %s.", pron, tolower(occ)),
        sprintf("Hjemme er tittelen %s.", tolower(occ)))))
    }
    if (!is.null(x$ukepenger) && !is.na(x$ukepenger)) {
      parts <- c(parts, sprintf("Økonomien: %s.", .decap(x$ukepenger)))
    }
    return(paste(parts, collapse = " "))
  }

  edu <- x$education
  field <- x$field_of_study
  field_d <- x$field_of_study_detail
  occ <- x$occupation
  income <- x$income_nok

  edu_str  <- if (!is.null(edu) && !is.na(edu)) tolower(edu) else NA
  field_str <- if (!is.null(field) && !is.na(field)) tolower(field) else NA
  fd_str   <- if (!is.null(field_d) && !is.na(field_d)) tolower(field_d) else NA
  occ_str  <- if (!is.null(occ) && !is.na(occ)) occ else NA

  # combined education + first job arc
  edu_full <- if (!is.na(edu_str)) {
    if (!is.na(fd_str)) sprintf("%s med fordypning i %s", edu_str, fd_str)
    else if (!is.na(field_str)) sprintf("%s innen %s", edu_str, field_str)
    else edu_str
  } else NA

  if (!is.na(edu_full) && !is.na(occ_str)) {
    inc <- if (!is.na(age_n) && age_n < 19) NA_character_
           else .income_phrase(income, neet = isTRUE(x$neet))
    if (!is.na(inc) && !grepl("utenfor", inc)) {
      parts <- c(parts, .pick(c(
        sprintf("Med %s tok %s veien til jobben som %s, og har i dag %s.",
                edu_full, pron, tolower(occ_str), inc),
        sprintf("Etter %s jobber %s som %s — %s.",
                edu_full, pron, tolower(occ_str), inc))))
    } else if (!is.na(inc) && grepl("utenfor", inc)) {
      parts <- c(parts, sprintf("%s har %s, men %s for tiden.",
                                .cap(pron), edu_full, inc))
    } else {
      parts <- c(parts, .pick(c(
        sprintf("Med %s i bagasjen er %s %s.", edu_full, pron, tolower(occ_str)),
        sprintf("%s har %s og jobber som %s.", .cap(pron), edu_full, tolower(occ_str)))))
    }
  } else if (!is.na(edu_full)) {
    parts <- c(parts, sprintf("%s har %s.", .cap(pron), edu_full))
  } else if (!is.na(occ_str)) {
    inc <- if (!is.na(age_n) && age_n < 19) NA_character_
           else .income_phrase(income, neet = isTRUE(x$neet))
    if (!is.na(inc) && !grepl("utenfor", inc)) {
      parts <- c(parts, sprintf("%s jobber som %s og har %s.",
                                .cap(pron), tolower(occ_str), inc))
    } else {
      parts <- c(parts, sprintf("%s jobber som %s.", .cap(pron), tolower(occ_str)))
    }
  }

  paste(parts, collapse = " ")
}

# ---- family life -------------------------------------------------------

.narrate_family_life <- function(x) {
  g <- x$gender
  pron <- .pronouns(g, "subj")
  ms <- tolower(x$marital_status %||% "")
  nc <- x$n_children
  nc_n <- if (!is.null(nc) && !is.na(nc)) as.integer(nc) else NA_integer_
  has_kids <- !is.na(nc_n) && nc_n > 0L
  kids_str <- if (has_kids) sprintf("%s barn", .no_count(nc_n)) else NA

  marital <- NA_character_
  if (grepl("^gift", ms)) marital <- "gift"
  else if (grepl("samboer", ms)) marital <- "samboer"
  else if (grepl("^skilt", ms)) marital <- "skilt"
  else if (grepl("^enke", ms)) marital <- if (identical(g, "M")) "enkemann" else "enke"
  else if (grepl("^ugift", ms)) marital <- "ugift"

  part <- NA_character_
  if (!is.na(marital)) {
    if (marital %in% c("gift", "samboer")) {
      ptn <- x$partner
      if (!is.null(ptn)) {
        pname <- ptn$name %||% (if (marital == "gift") "ektefellen" else "samboeren")
        pocc <- if (!is.null(ptn$occupation) && !is.na(ptn$occupation))
                  sprintf(", som er %s", tolower(ptn$occupation)) else ""
        verb <- if (marital == "gift") "er gift med" else "bor sammen med"
        base <- sprintf("%s %s %s%s", .cap(pron), verb, pname, pocc)
        part <- if (has_kids) sprintf("%s, og sammen har de %s.", base, kids_str)
                else paste0(base, ".")
      } else {
        verb <- if (marital == "gift") "er gift" else "bor med samboer"
        part <- if (has_kids) sprintf("%s %s og har %s.", .cap(pron), verb, kids_str)
                else sprintf("%s %s.", .cap(pron), verb)
      }
    } else if (marital == "skilt") {
      part <- if (has_kids) sprintf("Etter et samlivsbrudd har %s %s.", pron, kids_str)
              else sprintf("%s er skilt.", .cap(pron))
    } else if (marital %in% c("enke", "enkemann")) {
      part <- if (has_kids) sprintf("%s er %s og har %s.", .cap(pron), marital, kids_str)
              else sprintf("%s er %s.", .cap(pron), marital)
    } else if (marital == "ugift") {
      if (has_kids) {
        part <- sprintf("%s er ugift, men har %s.", .cap(pron), kids_str)
      } else if (!is.null(x$age) && !is.na(x$age) && x$age >= 30) {
        part <- .pick(c(sprintf("%s er fortsatt ugift og uten barn.", .cap(pron)),
                        sprintf("%s lever alene.", .cap(pron))))
      }
    }
  } else if (has_kids) {
    part <- sprintf("%s har %s.", .cap(pron), kids_str)
  }

  hh <- tolower(x$household %||% "")
  hh_part <- NA_character_
  if (grepl("foreldre", hh)) {
    fortsatt <- if (!is.null(x$age) && !is.na(x$age) && x$age >= 20) "fortsatt " else ""
    hh_part <- sprintf("%s bor %shjemme hos foreldrene.", .cap(pron), fortsatt)
  } else if (grepl("bofellesskap|kollektiv", hh)) {
    hh_part <- sprintf("%s bor i kollektiv.", .cap(pron))
  }

  paste(c(part, hh_part)[!is.na(c(part, hh_part))], collapse = " ")
}

# ---- material conditions ----------------------------------------------

.narrate_material <- function(x) {
  g <- x$gender; pron <- .pronouns(g, "subj")
  ht <- x$housing_tenure %||% ""
  htype <- x$housing_type
  hval <- x$housing_value_nok
  hytte <- isTRUE(x$has_hytte)
  nw <- x$net_wealth_nok

  part <- NA_character_
  if (grepl("^Eier|^Owns|eier", ht, ignore.case = TRUE)) {
    type_str <- if (!is.null(htype) && !is.na(htype)) tolower(htype) else "bolig"
    val_str <- if (!is.null(hval) && !is.na(hval) && hval > 0)
                 sprintf(" verdt rundt %.1f millioner", hval / 1e6) else ""
    part <- if (hytte)
      sprintf("Familien eier en %s%s, og har i tillegg hytte.", type_str, val_str)
    else
      sprintf("%s eier en %s%s.", .cap(pron), type_str, val_str)
  } else if (grepl("^Leier|^Rents|leier", ht, ignore.case = TRUE)) {
    part <- if (hytte) sprintf("%s leier bolig, men har hytte.", .cap(pron))
            else sprintf("%s leier bolig.", .cap(pron))
  } else if (hytte) {
    part <- "Familien har hytte."
  }

  wq <- .qualitative_wealth(nw)
  wpart <- if (!is.na(wq) && wq %in% c("betydelig formue", "stor formue", "negativ formue"))
             sprintf("Samlet sett: %s.", wq) else NA

  paste(c(part, wpart)[!is.na(c(part, wpart))], collapse = " ")
}

# ---- cultural / political ---------------------------------------------

.narrate_cultural <- function(x) {
  g <- x$gender; pron <- .pronouns(g, "subj")
  age_n <- if (!is.null(x$age) && !is.na(x$age)) as.integer(x$age) else NA_integer_
  parts <- character(0)

  # religion + party, combined where possible
  rel <- x$religion
  rel_clause <- NA_character_
  if (!is.null(rel) && !is.na(rel)) {
    if (.is_not_religious(rel)) rel_clause <- "regner seg ikke som troende"
    else if (!grepl("uoppgi|na$|udefiner", tolower(rel)))
      rel_clause <- sprintf("tilhører %s", rel)
  }

  party <- x$party
  party_clause <- NA_character_
  if (!is.null(party) && !is.na(party) &&
      !grepl("stemmer ikke|sofa|ikke stemme|stemte ikke|fyllesyk|glemte", tolower(party))) {
    is_sentence <- grepl("[a-zæøå] [a-zæøå]", party) && nchar(party) > 18
    if (is_sentence) {
      party_clause <- NA_character_  # handled as standalone below
      parts <- c(parts, paste0(.cap(party), "."))
    } else {
      party_clause <- sprintf("stemmer %s", party)
    }
  }

  if (!is.na(party_clause) && !is.na(rel_clause)) {
    parts <- c(parts, sprintf("Politisk %s %s, og religiøst %s.", party_clause, pron, rel_clause))
  } else if (!is.na(party_clause)) {
    parts <- c(parts, sprintf("Politisk %s %s.", party_clause, pron))
  } else if (!is.na(rel_clause)) {
    parts <- c(parts, sprintf("%s %s.", .cap(pron), rel_clause))
  }

  # hobbies — dedup the "På fritiden" opener for children
  hobs <- x$hobbies
  if (!is.null(hobs) && length(hobs) > 0) {
    hob_labels <- vapply(hobs, function(h) {
      if (is.list(h)) { if (!is.null(h$label)) as.character(h$label) else NA_character_ }
      else as.character(h)
    }, character(1))
    hob_labels <- hob_labels[!is.na(hob_labels) & nzchar(hob_labels)]
    if (length(hob_labels) > 0) {
      sel <- if (length(hob_labels) > 2) sample(hob_labels, 2) else hob_labels
      hob_str <- tolower(.join_no(sel))
      lead <- if (!is.na(age_n) && age_n < 16)
        .pick(c(sprintf("Ellers går tiden til %s.", hob_str),
                sprintf("%s liker også %s.", .cap(pron), hob_str)))
      else
        .pick(c(sprintf("På fritiden driver %s med %s.", pron, hob_str),
                sprintf("Fritiden går til %s.", hob_str),
                sprintf("I helgene blir det %s.", hob_str)))
      parts <- c(parts, lead)
    }
  }

  # media (which paper) — distinctive enough to keep
  if (!is.null(x$media_paper) && !is.na(x$media_paper) &&
      !grepl("ingen|none", tolower(x$media_paper))) {
    parts <- c(parts, .pick(c(
      sprintf("Nyhetene kommer fra %s.", x$media_paper),
      sprintf("%s leser %s.", .cap(pron), x$media_paper))))
  }

  # diet only when distinctive
  if (!is.null(x$diet) && !is.na(x$diet) &&
      !grepl("blandet|vanlig|alminnel|servert hjemme|served at home", tolower(x$diet))) {
    parts <- c(parts, sprintf("Kostholdet er %s.", tolower(x$diet)))
  }

  paste(parts, collapse = " ")
}

# ---- wellbeing ---------------------------------------------------------

.narrate_wellbeing <- function(x) {
  parts <- character(0)
  if (!is.null(x$self_rated_health) && !is.na(x$self_rated_health)) {
    h <- tolower(x$self_rated_health)
    if (grepl("daarlig|dårlig|poor", h)) parts <- c(parts, sprintf("Helsen oppleves som %s.", h))
    else if (grepl("meget god|excellent", h)) parts <- c(parts, "Helsen er god.")
  }
  if (isTRUE(x$has_chronic) && !is.null(x$chronic_type) && !is.na(x$chronic_type)) {
    parts <- c(parts, sprintf("Lever med %s.", tolower(x$chronic_type)))
  }
  if (isTRUE(x$has_disability) && !is.null(x$disability) && !is.na(x$disability)) {
    parts <- c(parts, sprintf("Har en funksjonsnedsettelse (%s).", tolower(x$disability)))
  }
  if (!is.null(x$close_friends) && !is.na(x$close_friends) &&
      grepl("ingen", tolower(x$close_friends))) {
    parts <- c(parts, "Har ingen nære venner.")
  }
  if (identical(x$has_confidant, FALSE)) {
    parts <- c(parts, "Mangler en fortrolig å snakke med.")
  }
  if (!is.null(x$loneliness) && !is.na(x$loneliness) &&
      grepl("ofte|often", tolower(x$loneliness))) {
    parts <- c(parts, "Føler seg ofte ensom.")
  }
  if (!is.null(x$deprivation_count) && !is.na(x$deprivation_count) &&
      x$deprivation_count >= 4L) {
    parts <- c(parts, "Hverdagen er preget av materielle vansker.")
  }
  paste(parts, collapse = " ")
}

# ---- compact -----------------------------------------------------------

.narrate_compact <- function(x) {
  pieces <- sprintf("%s, %s år, bor i %s.",
                    x$name %||% "Personen", x$age %||% 0,
                    x$municipality %||% x$county %||% "Norge")
  age_n <- if (!is.null(x$age) && !is.na(x$age)) as.integer(x$age) else NA_integer_
  if (!is.na(age_n) && age_n < 16) {
    skole <- if (age_n < 6) "i barnehagen" else if (age_n <= 12) "på barneskolen" else "på ungdomsskolen"
    pieces <- c(pieces, sprintf("Går %s.", skole))
    if (!is.null(x$occupation) && !is.na(x$occupation))
      pieces <- c(pieces, sprintf("Er %s.", tolower(x$occupation)))
  } else {
    if (!is.null(x$occupation) && !is.na(x$occupation))
      pieces <- c(pieces, sprintf("Jobber som %s.", tolower(x$occupation)))
    if (!is.null(x$education) && !is.na(x$education))
      pieces <- c(pieces, sprintf("Har %s.", tolower(x$education)))
  }
  if (!is.null(x$marital_status) && !is.na(x$marital_status) &&
      !grepl("^ugift", tolower(x$marital_status)))
    pieces <- c(pieces, sprintf("Sivilstatus: %s.", tolower(x$marital_status)))
  if (!is.null(x$n_children) && !is.na(x$n_children) && x$n_children > 0L)
    pieces <- c(pieces, sprintf("Har %s barn.", .no_count(x$n_children)))
  if (!is.null(x$party) && !is.na(x$party) && nchar(x$party) < 18)
    pieces <- c(pieces, sprintf("Stemmer %s.", x$party))
  paste(pieces, collapse = " ")
}

# ---- obituary ----------------------------------------------------------

.narrate_obituary <- function(x) {
  by <- .birthyear(x)
  name <- x$name %||% "Personen"
  parts <- sprintf("%s (f. %s) — et liv i korte trekk.", name, by %||% "?")
  parts <- c(parts, .narrate_origins(x), .narrate_education_career(x),
             .narrate_family_life(x), .narrate_material(x))
  parts <- parts[nzchar(parts) & !is.na(parts)]
  paste(parts, collapse = "\n\n")
}

# %||% is defined in print.R
