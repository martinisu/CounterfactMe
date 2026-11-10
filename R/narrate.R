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
      # The lead may already have said the mobility or the party; do not
      # say it again two paragraphs later.
      .narrate_origins(x, skip_mobility = has_lead && grepl("klatret", lead)),
      .narrate_education_career(x),
      .narrate_family_life(x),
      .narrate_material(x),
      .narrate_cultural(x, skip_party = has_lead && grepl("stemmer", lead)),
      .narrate_wellbeing(x)
    )
    paragraphs <- paragraphs[!is.na(paragraphs) & nzchar(paragraphs)]
    text <- paste(paragraphs, collapse = "\n\n")
  }

  # Last line of defence for label residue, whatever path it came by:
  # register qualifiers about firm size, and doubled spaces.
  text <- gsub("\\s*\\([^()]*(ansatte|medarbeidere)[^()]*\\)", "", text)
  text <- gsub("[ \t]{2,}", " ", text)
  text <- gsub(" +([.,])", "\\1", text)

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
  w <- c("ett", "to", "tre", "fire", "fem", "seks", "sju", "\u{00e5}tte", "ni", "ti")
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


# SSB's register terms are exact but unreadable in prose:
# "Universitets- og hogskoleutdanning kort/lang" is a classification, not
# something anyone says. These map a register label to the everyday word
# for the same thing, for narration only. The labels in
# education_levels.csv and every other output are untouched.
#
# The mapping goes label -> code -> prose, reusing .edu_level_label_no()
# so there is one vocabulary rather than two that can drift apart.
.edu_code_from_label <- function(lbl) {
  if (is.null(lbl) || all(is.na(lbl)) || !nzchar(as.character(lbl)[1]))
    return(NA_integer_)
  .load_data()
  el <- .cfm_env$education
  if (is.null(el)) return(NA_integer_)
  target <- tolower(trimws(as.character(lbl)[1]))
  for (col in c("level_no", "level")) {
    if (!col %in% names(el)) next
    hit <- which(tolower(trimws(el[[col]])) == target)
    if (length(hit)) return(as.integer(el$code[hit[1]]))
  }
  NA_integer_
}

.edu_prose <- function(lbl) {
  if (is.null(lbl) || all(is.na(lbl))) return(NA_character_)
  code <- .edu_code_from_label(lbl)
  if (is.na(code)) return(tolower(as.character(lbl)[1]))   # unknown: pass through
  if (code == 9L) return(NA_character_)                    # "Uoppgitt": say nothing
  out <- .edu_level_label_no(code)
  if (is.na(out)) tolower(as.character(lbl)[1]) else out
}

.edu_level_label_no <- function(l) {
  if (is.null(l) || is.na(l)) return(NA_character_)
  switch(as.character(l),
    "0" = "ingen fullf\u{00f8}rt utdanning",
    "1" = "grunnskole", "2" = "grunnskole",
    "3" = "videreg\u{00e5}ende", "4" = "videreg\u{00e5}ende",
    "5" = "fagskole",
    "6" = "bachelorgrad", "7" = "mastergrad", "8" = "doktorgrad",
    NA_character_)
}

.income_phrase <- function(income, neet = FALSE) {
  if (isTRUE(neet)) return("st\u{00e5}r utenfor arbeidslivet")
  if (is.null(income) || is.na(income)) return(NA_character_)
  if (income < 50000) return(NA_character_)
  r <- round(income / 10000) * 10000
  sprintf("en \u{00e5}rsinntekt p\u{00e5} rundt %s kroner",
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
  # This read x$edu_code, x$mother_edu_code and x$father_edu_code. None
  # of those fields exist: the ego carries only the education label, and
  # the parents' codes are nested under x$mother / x$father. So
  # .edu_level() always got NULL, the function always returned NULL, and
  # the whole educational-mobility strand of the biography -- "Der
  # foreldrene stoppet ved videregaende, gikk hun videre til
  # mastergrad" -- never once appeared in any output.
  age <- x$age
  if (is.null(age) || is.na(age) || age < 25) return(NULL)
  ego <- .edu_code_from_label(x$education)
  if (is.na(ego)) return(NULL)
  if (ego == 9L) return(NULL)          # "Uoppgitt" is not a level
  par_codes <- suppressWarnings(as.integer(c(
    x$mother$education_code %||% NA, x$father$education_code %||% NA)))
  par_codes <- par_codes[!is.na(par_codes) & par_codes != 9L]
  if (!length(par_codes)) return(NULL)
  par <- max(par_codes)
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

# ---- labels -> prose ---------------------------------------------------
#
# The data carry SSB register labels and authored humour labels. Those are
# right on the card, but dropped verbatim into a sentence they break it:
#   "Hege, som er dagpenger (tidl. lektor (universitet og hoyskole))"
#   "Han eier en smahus"            (smahus is neuter)
#   "Eier en Tesla og to leiligheter -- derfor H."   (humour label, and it
#                                    contradicted the life it was printed in)
#   "Han tilhorer Annet kristent trossamfunn."
#   "Kostholdet er fleksitarianer"
# These helpers turn a label into prose. The labels themselves are not
# touched.

# Occupation label -> its parts. Recognises the forms .cond_occupation()
# produces: "X (deltid)", "AAP (tidl. X)", "Dagpenger (tidl. X)",
# "Sosialhjelp", "Uføretrygdet".
.occ_parse <- function(occ) {
  if (is.null(occ) || is.na(occ) || !nzchar(occ)) return(NULL)
  o <- trimws(as.character(occ))
  # Older labels could carry "(deltid)" after a benefit; strip it first.
  o_nopart <- sub(" \\(deltid\\)$", "", o)
  if (grepl("^(AAP|Dagpenger) \\(tidl\\.", o_nopart, ignore.case = TRUE)) o <- o_nopart
  m <- regmatches(o, regexec("^(AAP|Dagpenger) \\(tidl\\. (.*)\\)$", o,
                             ignore.case = TRUE))[[1]]
  if (length(m) == 3L) {
    return(list(kind = if (tolower(m[2]) == "aap") "aap" else "dagpenger",
                prev = tolower(m[3])))
  }
  if (grepl("^sosialhjelp$", o, ignore.case = TRUE))
    return(list(kind = "sosialhjelp"))
  if (o %in% .TEEN_OCCUPATIONS && !grepl("^l\u00e6rling$", tolower(o))) return(list(kind = "teen"))
  ms <- regmatches(o, regexec("^Student(?: \\(deltid: (.*)\\))?$", o, perl = TRUE))[[1]]
  if (length(ms) >= 1L && nzchar(ms[1])) {
    pj <- if (length(ms) == 2L && nzchar(ms[2])) tolower(ms[2]) else NA_character_
    return(list(kind = "student", job = pj))
  }
  if (grepl("^hjemmev", o, ignore.case = TRUE)) return(list(kind = "home"))
  if (grepl("^ikke i utdanning eller arbeid", o, ignore.case = TRUE)) return(list(kind = "neet"))
  if (grepl("^uoppgitt", o, ignore.case = TRUE)) return(list(kind = "unknown"))
  if (grepl("^uf\u00f8retrygd", o, ignore.case = TRUE))
    return(list(kind = "ufor"))
  part <- grepl(" \\(deltid\\)$", o)
  job <- tolower(sub(" \\(deltid\\)$", "", o))
  # Register residue in titles: "(offentlig administrasjon - uspesifisert)".
  job <- sub(" - uspesifisert\\)", ")", job)
  # Register size/sector qualifiers: "(over 9 ansatte)", "(jordbruk, ... -
  # faerre enn 10 ansatte - privat virksomhet)".
  job <- gsub("\\s*\\([^()]*(ansatte|medarbeidere)[^()]*\\)", "", job)
  job <- trimws(job)
  # A 4-digit group label is plural ("offiserer fra fenrik og hoeyere grad").
  os <- .cfm_env$occupations_salary
  if (!is.null(os) && job %in% tolower(os$label)) job <- tolower(.singular_styrk(job))
  list(kind = "job", job = job, part = part)
}

# ", som er sykepleier" / ", som jobber deltid som ..." / ", som gar pa
# dagpenger etter a ha vaert ..." -- for a partner.
.occ_relative_clause <- function(occ) {
  o <- .occ_parse(occ)
  if (is.null(o)) return("")
  switch(o$kind,
    aap = sprintf(", som mottar arbeidsavklaringspenger etter \u00e5 ha jobbet som %s", o$prev),
    dagpenger = sprintf(", som g\u00e5r p\u00e5 dagpenger etter \u00e5 ha jobbet som %s", o$prev),
    sosialhjelp = ", som mottar sosialhjelp",
    ufor = ", som er uf\u00f8retrygdet",
    student = if (!is.na(o$job)) sprintf(", som studerer og jobber deltid som %s", o$job)
              else ", som studerer",
    neet = ", som st\u00e5r utenfor utdanning og arbeid",
    teen = ", som g\u00e5r p\u00e5 videreg\u00e5ende",
    home = ", som er hjemmev\u00e6rende",
    unknown = "",
    job = if (isTRUE(o$part)) sprintf(", som jobber deltid som %s", o$job)
          else sprintf(", som er %s", o$job))
}

# Housing type with the right indefinite article.
.housing_np <- function(htype) {
  t <- if (!is.null(htype) && !is.na(htype)) tolower(htype) else "bolig"
  art <- if (grepl("hus$", t)) "et" else "en"
  paste(art, t)
}

# "4.2" -> "4,2": Norwegian decimal comma.
.mill_no <- function(v) sub(".", ",", sprintf("%.1f", v / 1e6), fixed = TRUE)

# Party label (plain or humour) -> the party's name, or NA for non-voters
# and anything unrecognised. The humour label stays on the card.
.party_name <- function(label) {
  if (is.null(label) || is.na(label)) return(NA_character_)
  .load_data()
  pb <- .cfm_env$party_baseline
  if (!is.null(pb) && label %in% pb$label) {
    return(if (identical(pb$code[pb$label == label][1], "ANDRE")) "p\u00e5 et sm\u00e5parti" else label)
  }
  ph <- .cfm_env$party_humor
  if (!is.null(ph) && !is.null(pb) && label %in% ph$label) {
    cd <- ph$code[ph$label == label][1]
    if (cd %in% pb$code) {
      return(if (identical(cd, "ANDRE")) "p\u00e5 et sm\u00e5parti" else pb$label[pb$code == cd][1])
    }
    return(NA_character_)
  }
  # Unknown and sentence-like: do not print it as if it were a party.
  if (grepl(" ", label) && nchar(label) > 18) return(NA_character_)
  label
}

# Religion label (plain or humour) -> a verb phrase, or NA.
.religion_clause <- function(label) {
  if (is.null(label) || is.na(label)) return(NA_character_)
  .load_data()
  rb <- .cfm_env$religion_baseline
  code <- NA_character_
  if (!is.null(rb) && label %in% rb$label_no) code <- rb$code[rb$label_no == label][1]
  rh <- .cfm_env$religion_humor
  if (is.na(code) && !is.null(rh) && label %in% rh$label) code <- rh$code[rh$label == label][1]
  if (is.na(code)) {
    if (.is_not_religious(label)) return("regner seg ikke som troende")
    if (grepl("uoppgi|na$|udefiner", tolower(label))) return(NA_character_)
    if (grepl(" ", label) && nchar(label) > 25) return(NA_character_)
    l <- if (grepl("^Den ", label)) label else .decap(label)
    return(sprintf("tilh\u00f8rer %s", l))
  }
  switch(code,
    DnK = "er medlem av Den norske kirke",
    KAT = "er katolikk",
    ANN_KRIS = "tilh\u00f8rer et annet kristent trossamfunn",
    ISL = "er muslim",
    BUD = "er buddhist",
    HIN = "er hindu",
    JOD = "er j\u00f8de",
    ANN = "tilh\u00f8rer et annet tros- eller livssynssamfunn",
    HUM = "regner seg ikke som troende",
    INGEN = "regner seg ikke som troende",
    NA_character_)
}

# Diet label -> sentence.
.diet_sentence <- function(diet, pron) {
  d <- tolower(diet)
  P <- .cap(pron)
  if (grepl("^fleksitarianer|^vegetarianer|^pescetarianer", d))
    return(sprintf("%s er %s.", P, d))
  if (grepl("^vegan", d)) return(sprintf("%s er veganer.", P))
  if (grepl("lavkarbo|keto", d)) return(sprintf("%s spiser lavkarbo.", P))
  if (grepl("glutenfri", d)) return(sprintf("%s spiser glutenfritt.", P))
  if (grepl("^tradisjonell norsk", d))
    return("Kostholdet er tradisjonelt norsk og kj\u00f8ttrikt.")
  sprintf("Kostholdet er %s.", d)
}

# Children's religion labels from .child_religion_label() -> verb phrase.
.child_religion_clause <- function(label) {
  if (is.null(label) || is.na(label)) return(NA_character_)
  l <- tolower(label)
  # .decap, not tolower: "Den norske kirke" keeps its capitals.
  if (grepl("^d\u00f8pt", l)) return(paste("er", sub(" i annet ", " i et annet ", .decap(label))))
  if (grepl("navnefest", l)) return("hadde navnefest i Human-Etisk Forbund")
  fam <- c(islam = "muslimsk", buddhisme = "buddhistisk", hinduisme = "hinduistisk",
           mosaisk = "j\u00f8disk")
  for (k in names(fam)) if (grepl(k, l)) return(sprintf("vokser opp i en %s familie", fam[[k]]))
  if (grepl("foreldrenes tradisjon", l)) return("vokser opp i foreldrenes tradisjon")
  if (grepl("ingen|ikke", l)) return(NA_character_)
  .religion_clause(label)
}

# Hobby labels -> nouns that fit after "driver med" / "gaar til".
.hobby_noun <- function(h) {
  m <- c("lese b\u00f8ker" = "lesing", "spille gitar" = "gitarspill",
         "spille piano" = "pianospill", "spille trommer" = "trommespill",
         "synge i kor" = "korsang", "spille i band" = "band",
         "frivillig: r\u00f8de kors" = "frivillig arbeid i R\u00f8de Kors",
         "frivillig: idrettslag-trener" = "trenerjobb i idrettslaget",
         "frivillig: speiderleder" = "speiderarbeid",
         "frivillig: matsentral" = "frivillig arbeid p\u00e5 matsentralen",
         "frivillig: dyrebeskyttelsen" = "frivillig arbeid i Dyrebeskyttelsen")
  k <- tolower(h)
  if (k %in% names(m)) unname(m[k]) else if (grepl("^magic", k)) h else k
}

# Disability type + severity -> phrase. "funksjonsnedsettelse (psykisk
# (alvorlig))" nested two sets of brackets.
.disability_phrase <- function(type, severity, label = NA) {
  ty <- tolower(type %||% ""); sev <- tolower(severity %||% "")
  grad <- switch(sev, mild = "lett", moderat = "moderat", alvorlig = "sterkt", "")
  adj  <- switch(sev, mild = "lett", moderat = "moderat", alvorlig = "alvorlig", "")
  sq <- function(z) gsub("\\s+", " ", z)
  if (ty == "bevegelse") return(sq(sprintf("har %s nedsatt bevegelsesevne", grad)))
  if (ty == "syn") return(sq(sprintf("har %s nedsatt syn", grad)))
  if (ty == "h\u00f8rsel") return(sq(sprintf("har %s nedsatt h\u00f8rsel", grad)))
  if (ty == "psykisk") return(sq(sprintf("har en %s psykisk funksjonsnedsettelse", adj)))
  if (ty == "kognitiv") return(sq(sprintf("har en %s kognitiv funksjonsnedsettelse", adj)))
  if (ty == "adhd") return("har ADHD")
  if (ty == "l\u00e6revansker") return("har l\u00e6revansker")
  if (ty == "autismespekter") return("har en diagnose p\u00e5 autismespekteret")
  if (nzchar(adj)) sprintf("har en %s funksjonsnedsettelse", adj)
  else sprintf("har en funksjonsnedsettelse")
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
  if (grepl("bonde|g\u{00e5}rdbruk|gardbruk|jordbruk|fisker|skogbruk", occ) &&
      !is.null(sent) && !is.na(sent) && sent <= 2) {
    return(.pick(c(
      sprintf("Det er ikke mange igjen som livn\u{00e6}rer seg slik %s gj\u{00f8}r \u{2014} som %s, midt i %s.",
              name, occ, muni),
      sprintf("%s er %s i %s, der de fleste andre jobber med noe helt annet.",
              name, occ, muni))))
  }
  # 2) Christian-Democrat but not a believer
  if (grepl("kristelig|krf", party) && .is_not_religious(rel)) {
    return(sprintf("%s stemmer kristelig, men regner seg ikke som troende \u{2014} det henger ikke alltid sammen.",
                   name))
  }
  # 3) far-left with a fortune
  if (grepl("r\u{00f8}dt|roedt|moxnes|kapitalismen", party) &&
      !is.null(nw) && !is.na(nw) && nw > 5e6) {
    return(sprintf("%s stemmer R\u{00f8}dt og sitter samtidig p\u{00e5} en formue de fleste bare dr\u{00f8}mmer om.",
                   name))
  }
  # 4) immigrant background voting FrP
  if (bg %in% c("first_gen", "second_gen") &&
      grepl("fremskritt|frp|sylvi", party)) {
    cb <- x$country_background %||% x$country_label %||% NA
    if (!is.null(cb) && !is.na(cb)) {
      return(sprintf("%s, med r\u{00f8}tter i %s, stemmer Fremskrittspartiet.", name, cb))
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
        sprintf(" %s kom til Norge fra %s for %d \u{00e5}r siden.", .cap(pron), cb, yrs)
      } else if (nzchar(cb)) {
        sprintf(" %s er f\u{00f8}dt i %s.", .cap(pron), cb)
      } else ""
    } else if (identical(x$background, "second_gen") && nzchar(cb)) {
      bg_clause <- sprintf(" Foreldrene kom fra %s.", cb)
    }
  }

  # "Malin er 0 aar" -> "under ett aar".
  if (!is.na(age) && identical(as.integer(age), 0L)) age <- "under ett"
  if (has_lead) {
    # avoid repeating the name; lead already used it
    base <- .pick(c(
      sprintf("%s er %s \u{00e5}r og bor i %s.", .cap(pron), age, place),
      sprintf("I dag er %s %s \u{00e5}r gammel, bosatt i %s.", pron, age, place),
      sprintf("%s \u{00e5}r, %s.", age, place)))
  } else {
    base <- .pick(c(
      sprintf("%s er %s \u{00e5}r gammel og bor i %s.", name, age, place),
      sprintf("%s, f\u{00f8}dt i %s, bor i %s.", name, by %||% "?", place),
      sprintf("%s er %s \u{00e5}r og holder til i %s.", name, age, place)))
  }
  paste0(base, bg_clause)
}

# ---- origins: parents, siblings, mobility -----------------------------

# .cond_parents() stores "Bodde i <land>" / "Lived in <country>" in the
# occupation field for parents who were not in Norway during their
# working life. It is a marker, not a job title.
.is_abroad_label <- function(s) {
  if (is.null(s) || length(s) != 1 || is.na(s) || !nzchar(s)) return(FALSE)
  grepl("^bodde i |^lived in ", tolower(s))
}

.narrate_origins <- function(x, skip_mobility = FALSE) {
  if (is.null(x$mother) && is.null(x$father)) return(NA_character_)
  g <- x$gender
  pron <- .pronouns(g, "subj")
  child_noun <- if (identical(g, "M")) "s\u{00f8}nn" else "datter"

  # For parents who lived their working life in the country of origin,
  # .cond_parents() deliberately replaces the STYRK title with a marker
  # like "Bodde i Serbia" -- we do not know what they did. The narration
  # treated that marker as a job title and lowercased it, producing
  # "Hjemme var det en bodde i serbia og en bodde i serbia som forsorget
  # familien." Detect the marker and say the thing it actually means.
  m_raw <- as.character(x$mother$occupation %||% "")
  f_raw <- as.character(x$father$occupation %||% "")
  if (is.na(m_raw)) m_raw <- ""
  if (is.na(f_raw)) f_raw <- ""
  m_abroad <- .is_abroad_label(m_raw)
  f_abroad <- .is_abroad_label(f_raw)

  # Job titles are common nouns and belong in lower case; the abroad
  # marker contains a country name and must keep its capital.
  m_occ <- if (m_abroad) m_raw else tolower(m_raw)
  f_occ <- if (f_abroad) f_raw else tolower(f_raw)
  m_name <- x$mother$name %||% "moren"
  f_name <- x$father$name %||% "faren"

  # A child who still lives with living parents is described in the
  # present tense. "Moren jobbet som sykepleier" about a six-year-old's
  # mother reads as if she were dead or retired. Every .pick() below keeps
  # the same number of alternatives in both tenses, so the random stream
  # is unchanged.
  age_n <- if (!is.null(x$age) && !is.na(x$age)) as.integer(x$age) else NA_integer_
  alive <- function(p) is.null(p) || is.null(p$death_year) || is.na(p$death_year)
  lost <- !is.null(x$parents_relationship) && !is.na(x$parents_relationship) &&
          grepl("mistet|lost", tolower(x$parents_relationship))
  at_home <- grepl("foreldre", tolower(x$household %||% ""))
  young <- !is.na(age_n) && (age_n < 18 || at_home) &&
           alive(x$mother) && alive(x$father) && !lost

  # Parents' occupation labels can be benefit labels ("Dagpenger (tidl.
  # lektor)") or carry "(deltid)". As nouns they only work for jobs.
  m_p <- if (m_abroad) NULL else .occ_parse(m_raw)
  f_p <- if (f_abroad) NULL else .occ_parse(f_raw)
  # "uoppgitt / yrker som ikke kan identifiseres" is a register category,
  # not something to say about someone's mother.
  if (!is.null(m_p) && m_p$kind == "unknown") { m_p <- NULL; m_occ <- "" }
  if (!is.null(f_p) && f_p$kind == "unknown") { f_p <- NULL; f_occ <- "" }
  as_noun <- function(p, fallback) {
    if (is.null(p) || p$kind != "job") return(fallback)
    if (isTRUE(p$part)) paste(p$job, "p\u{00e5} deltid") else p$job
  }
  m_occ <- as_noun(m_p, m_occ)
  f_occ <- as_noun(f_p, f_occ)
  as_clause <- function(p) {
    k <- p$kind
    if (k == "job") {
      j <- if (isTRUE(p$part)) paste(p$job, "p\u{00e5} deltid") else p$job
      return(sprintf(if (young) "jobber som %s" else "jobbet som %s", j))
    }
    switch(k,
      home = if (young) "er hjemmev\u{00e6}rende" else "var hjemmev\u{00e6}rende",
      student = if (young) "studerer" else "studerte",
      aap = if (young) "mottar arbeidsavklaringspenger" else "mottok arbeidsavklaringspenger",
      dagpenger = if (young) "g\u{00e5}r p\u{00e5} dagpenger" else "gikk p\u{00e5} dagpenger",
      sosialhjelp = if (young) "mottar sosialhjelp" else "mottok sosialhjelp",
      ufor = if (young) "er uf\u{00f8}retrygdet" else "var uf\u{00f8}retrygdet")
  }
  nonjob <- (!is.null(m_p) && m_p$kind != "job") || (!is.null(f_p) && f_p$kind != "job")

  parts <- character(0)
  if (nzchar(m_occ) || nzchar(f_occ)) {
    if (m_abroad && f_abroad) {
      # Same marker twice reads as a stutter; say it once.
      where <- sub("^[Bb]odde i ", "", m_occ)
      parts <- c(parts, .pick(c(
        sprintf("Foreldrene bodde i %s mens de var i arbeidsf\u{00f8}r alder.", where),
        sprintf("Begge foreldrene levde arbeidslivet sitt i %s.", where))))
    } else if (m_abroad || f_abroad) {
      abroad_occ <- if (m_abroad) m_occ else f_occ
      home_occ   <- if (m_abroad) f_occ else m_occ
      where <- sub("^[Bb]odde i ", "", abroad_occ)
      parts <- c(parts, if (nzchar(home_occ)) {
        sprintf("Den ene forelderen bodde i %s, den andre var %s.", where, home_occ)
      } else {
        sprintf("Foreldrene bodde i %s.", where)
      })
    } else if (nzchar(m_occ) && nzchar(f_occ) && nonjob) {
      cm <- as_clause(m_p); cf <- as_clause(f_p)
      # "Moren var uforetrygdet, og faren var uforetrygdet" -> say it once.
      sent <- if (identical(cm, cf))
        sprintf("B\u{00e5}de moren %s og faren %s %s.", m_name, f_name, cm)
      else sprintf("Moren %s %s, og faren %s %s.", m_name, cm, f_name, cf)
      parts <- c(parts, .pick(rep(sent, 3L)))
    } else if (nzchar(m_occ) && nzchar(f_occ)) {
      parts <- c(parts, .pick(if (young) c(
        if (!is.na(age_n) && age_n >= 18)
          sprintf("%s er %s av %s og %s.", .cap(pron), child_noun, m_occ, f_occ)
        else sprintf("%s vokser opp som %s av %s og %s.", .cap(pron), child_noun, m_occ, f_occ),
        sprintf("Moren %s jobber som %s, faren %s som %s.", m_name, m_occ, f_name, f_occ),
        sprintf("Hjemme er det en %s og en %s som fors\u{00f8}rger familien.", m_occ, f_occ))
      else c(
        sprintf("%s vokste opp som %s av %s og %s.", .cap(pron), child_noun, m_occ, f_occ),
        sprintf("Moren %s jobbet som %s, faren %s som %s.", m_name, m_occ, f_name, f_occ),
        sprintf("Hjemme var det en %s og en %s som fors\u{00f8}rget familien.", m_occ, f_occ))))
    } else {
      # One parent's occupation is known. "vokste opp med en forelder som
      # var X" implied a single parent; name the one we know about.
      one_p <- if (nzchar(m_occ)) m_p else f_p
      who <- if (nzchar(m_occ)) sprintf("Moren %s", m_name) else sprintf("Faren %s", f_name)
      parts <- c(parts, if (!is.null(one_p)) sprintf("%s %s.", who, as_clause(one_p))
                        else sprintf("%s var %s.", who, if (nzchar(m_occ)) m_occ else f_occ))
    }
  }

  nsib <- .nsiblings(x)
  if (!is.na(nsib)) {
    if (nsib == 0L) {
      parts <- c(parts, .pick(if (young) c(
        sprintf("%s er enebarn.", .cap(pron)),
        sprintf("%s har ingen s\u{00f8}sken.", .cap(pron)))
      else c(
        sprintf("%s var enebarn.", .cap(pron)),
        "S\u{00f8}sken ble det ikke.")))
    } else {
      total <- nsib + 1L
      parts <- c(parts, .pick(if (young) c(
        sprintf("%s er ett av %s barn.", .cap(pron), .no_count(total)),
        sprintf("S\u{00f8}skenflokken teller %s.", .no_count(total)))
      else c(
        sprintf("%s var ett av %s barn.", .cap(pron), .no_count(total)),
        sprintf("S\u{00f8}skenflokken talte %s.", .no_count(total)))))
    }
  }

  # parental relationship
  if (isTRUE(x$parents_divorced)) {
    parts <- c(parts, .pick(if (young) c(
      "Foreldrene er skilt.",
      sprintf("%s vokser opp mellom to hjem etter at foreldrene gikk fra hverandre.", .cap(pron)))
    else c(
      "Foreldrene skilte seg.",
      sprintf("%s vokste opp mellom to hjem etter at foreldrene gikk fra hverandre.", .cap(pron)))))
  } else if (!is.null(x$parents_relationship) && !is.na(x$parents_relationship) &&
             grepl("mistet|lost", tolower(x$parents_relationship))) {
    parts <- c(parts, sprintf("%s mistet en av foreldrene tidlig i livet.", .cap(pron)))
  }

  # trajectory clause
  mob <- if (isTRUE(skip_mobility)) NULL else .mobility(x)
  # Codes 3 and 4 both read "videregaende": "Der foreldrene stoppet ved
  # videregaende, gikk hun videre til videregaende."
  if (!is.null(mob) && identical(mob$ego_lbl, mob$par_lbl)) mob <- NULL
  if (!is.null(mob) && !is.na(mob$ego_lbl) && !is.na(mob$par_lbl)) {
    if (mob$dir %in% c("sterk_opp", "opp")) {
      cl <- if (mob$dir == "sterk_opp" && mob$par <= 2 && mob$ego >= 6) {
        sprintf("Ingen hjemme hadde g\u{00e5}tt p\u{00e5} universitetet; selv tok %s %s.", pron, mob$ego_lbl)
      } else {
        .pick(c(
          sprintf("Der foreldrene stoppet ved %s, gikk %s videre til %s.",
                  mob$par_lbl, pron, mob$ego_lbl),
          sprintf("%s tok lengre utdanning enn noen f\u{00f8}r i familien.", .cap(pron))))
      }
      parts <- c(parts, cl)
    } else if (mob$dir %in% c("ned", "sterk_ned")) {
      parts <- c(parts, sprintf("Til forskjell fra foreldrene, som hadde %s, endte det med %s for %s.",
                                mob$par_lbl, mob$ego_lbl, .pronouns(g, "obj")))
    } else if (identical(mob$dir, "lik") && runif(1) < 0.25) {
      parts <- c(parts, sprintf("Som foreldrene f\u{00f8}r %s ble utdanningen p\u{00e5} %s-niv\u{00e5}.",
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
  if (!is.na(age_n) && age_n < 1) {
    return(sprintf("%s har ikke begynt i barnehagen enn\u{00e5}.", .cap(pron)))
  }
  if (!is.na(age_n) && age_n < 16) {
    skole <- if (age_n < 6) "i barnehagen"
             else if (age_n <= 12) "p\u{00e5} barneskolen"
             else "p\u{00e5} ungdomsskolen"
    parts <- c(parts, .pick(c(
      sprintf("%s g\u{00e5}r %s.", .cap(pron), skole),
      sprintf("Til daglig g\u{00e5}r %s %s.", pron, skole))))
    occ <- x$occupation
    if (!is.null(occ) && !is.na(occ)) {
      parts <- c(parts, .pick(c(
        sprintf("P\u{00e5} fritiden er %s %s.", pron, tolower(occ)),
        sprintf("Hjemme er tittelen %s.", tolower(occ)))))
    }
    if (!is.null(x$ukepenger) && !is.na(x$ukepenger)) {
      parts <- c(parts, sprintf("\u{00d8}konomien: %s.", .decap(x$ukepenger)))
    }
    return(paste(parts, collapse = " "))
  }

  edu <- x$education
  field <- x$field_of_study
  field_d <- x$field_of_study_detail
  occ <- x$occupation
  income <- x$income_nok

  edu_str  <- .edu_prose(edu)
  # NUS residual categories end in ", annet"/", andre"; drop the tail.
  .clean_field <- function(f) sub("(,\\s*(annet|andre|uoppgitt))+$", "", tolower(f))
  field_str <- if (!is.null(field) && !is.na(field)) .clean_field(field) else NA
  fd_str   <- if (!is.null(field_d) && !is.na(field_d)) .clean_field(field_d) else NA
  occ_str  <- if (!is.null(occ) && !is.na(occ)) occ else NA

  # combined education + first job arc
  edu_full <- if (!is.na(edu_str)) {
    # "mastergrad i filosofi" is what people say; "med fordypning i" stays
    # for levels that are not degrees ("videregaende med fordypning i ...").
    if (!is.na(fd_str) && grepl("grad$", edu_str)) sprintf("%s i %s", edu_str, fd_str)
    else if (!is.na(fd_str)) sprintf("%s med fordypning i %s", edu_str, fd_str)
    else if (!is.na(field_str)) sprintf("%s innen %s", edu_str, field_str)
    else edu_str
  } else NA

  # Retirees: the "occupation" is a pastime label, not a job, and the
  # income is a pension. Without this a 89-year-old was described as
  # having "tok veien til jobben som hyttebokforfatter og har i dag en
  # arsinntekt pa ...".
  # 67-71 draws a real job 45 pct of the time. Those people still work:
  # "Yrkeslivet ligger bak henne ... Pensjonen ligger paa rundt 60 000" was
  # a part-time wage narrated as a pension.
  op_pre <- .occ_parse(occ_str)
  still_working <- !is.null(op_pre) && op_pre$kind == "job" &&
                   !(occ_str %in% .PENSJONIST_LABELS) && !is.na(age_n) && age_n < 72
  is_pens <- !still_working &&
             ((!is.na(age_n) && age_n >= 67) ||
              (!is.na(occ_str) && occ_str %in% .PENSJONIST_LABELS))
  if (isTRUE(is_pens)) {
    if (!is.na(edu_full)) {
      parts <- c(parts, .pick(c(
        sprintf("Yrkeslivet ligger bak %s; utdanningen var %s.",
                .pronouns(g, "obj"), edu_full),
        sprintf("%s har %s bak seg, og er n\u{00e5} pensjonist.", .cap(pron), edu_full))))
    } else {
      parts <- c(parts, sprintf("%s er pensjonist.", .cap(pron)))
    }
    if (!is.na(occ_str) && occ_str %in% .PENSJONIST_LABELS) {
      parts <- c(parts, .pick(c(
        sprintf("Dagene g\u{00e5}r med til rollen som %s.", tolower(occ_str)),
        sprintf("I n\u{00e6}rmilj\u{00f8}et er %s kjent som %s.", pron, tolower(occ_str)))))
    } else if (!is.na(occ_str)) {
      op_r <- .occ_parse(occ_str)
      if (!is.null(op_r) && op_r$kind == "job")
        parts <- c(parts, sprintf("Yrket var %s.", op_r$job))
    }
    if (!is.null(income) && !is.na(income) && income >= 50000) {
      r <- round(income / 10000) * 10000
      parts <- c(parts, sprintf("Pensjonen ligger p\u{00e5} rundt %s kroner i \u{00e5}ret.",
                                formatC(r, format = "d", big.mark = " ")))
    }
    return(paste(parts, collapse = " "))
  }

  # 16-18: the occupation is a humour label from .TEEN_OCCUPATIONS, not a
  # job. "Med grunnskole i bagasjen er hun kassa-veteran" read as a career.
  if (!is.na(age_n) && age_n <= 18 && !is.na(occ_str) && occ_str %in% .TEEN_OCCUPATIONS) {
    P <- .cap(pron)
    if (grepl("^l\u{00e6}rling$", tolower(occ_str))) {
      parts <- c(parts, sprintf("%s er l\u{00e6}rling.", P))
    } else {
      parts <- c(parts, sprintf("%s g\u{00e5}r p\u{00e5} videreg\u{00e5}ende.", P))
      if (!grepl("^gymnasiast$", tolower(occ_str)))
        parts <- c(parts, sprintf("Ved siden av skolen er %s %s.", pron, tolower(occ_str)))
    }
    return(paste(parts, collapse = " "))
  }

  # Benefit and part-time labels from .cond_occupation() ("Dagpenger (tidl.
  # lektor)", "revisormedarbeider (deltid)") read badly after "jobben som".
  op <- .occ_parse(occ_str)
  if (isTRUE(x$neet) || (!is.null(op) && op$kind == "neet")) {
    P <- .cap(pron)
    parts <- c(parts, if (!is.na(edu_full))
      sprintf("%s har %s, men st\u00e5r utenfor b\u00e5de utdanning og arbeid.", P, edu_full)
    else sprintf("%s st\u00e5r utenfor b\u00e5de utdanning og arbeid.", P))
    return(paste(parts, collapse = " "))
  }
  if (!is.null(op) && op$kind == "student") {
    P <- .cap(pron)
    side <- if (!is.na(op$job)) sprintf(", og jobber deltid som %s ved siden av", op$job) else ""
    parts <- c(parts, if (!is.na(edu_full))
      sprintf("%s har %s og studerer videre%s.", P, edu_full, side)
    else sprintf("%s er student%s.", P, side))
    return(paste(parts, collapse = " "))
  }
  if (!is.null(op) && op$kind != "job") {
    P <- .cap(pron)
    now <- switch(op$kind,
      aap = "mottar n\u{00e5} arbeidsavklaringspenger",
      dagpenger = "g\u{00e5}r n\u{00e5} p\u{00e5} dagpenger",
      sosialhjelp = "mottar n\u{00e5} sosialhjelp",
      ufor = "er uf\u{00f8}retrygdet")
    worked <- if (!is.null(op$prev)) sprintf("jobbet som %s", op$prev) else NA_character_
    s1 <- if (!is.na(edu_full) && !is.na(worked)) sprintf("%s har %s og %s, men %s.", P, edu_full, worked, now)
          else if (!is.na(edu_full)) sprintf(if (op$kind %in% c("ufor", "sosialhjelp"))
                                               "%s har %s og %s." else "%s har %s, men %s.",
                                             P, edu_full, sub(" n\u{00e5} ", " ", now))
          else if (!is.na(worked)) sprintf("%s %s, men %s.", P, worked, now)
          else sprintf("%s %s.", P, sub("^(mottar|g\u{00e5}r) n\u{00e5}", "\\1", now))
    parts <- c(parts, s1)
    inc <- .income_phrase(income, neet = isTRUE(x$neet))
    if (!is.na(inc) && !grepl("utenfor", inc)) parts <- c(parts, sprintf("%s har %s.", P, inc))
    return(paste(parts, collapse = " "))
  }
  occ_disp <- if (is.null(op)) NA_character_
              else if (isTRUE(op$part)) paste(op$job, "p\u{00e5} deltid") else op$job

  if (!is.na(edu_full) && !is.na(occ_str)) {
    inc <- if (!is.na(age_n) && age_n < 19) NA_character_
           else .income_phrase(income, neet = isTRUE(x$neet))
    if (!is.na(inc) && !grepl("utenfor", inc)) {
      parts <- c(parts, .pick(c(
        # "Med videregaende med fordypning i ..." doubles the "med".
        sprintf("%s %s tok %s veien til jobben som %s, og har i dag %s.",
                if (grepl(" med ", edu_full)) "Etter" else "Med",
                edu_full, pron, occ_disp, inc),
        sprintf("Etter %s jobber %s som %s \u{2014} %s.",
                edu_full, pron, occ_disp, inc))))
    } else if (!is.na(inc) && grepl("utenfor", inc)) {
      parts <- c(parts, sprintf("%s har %s, men %s for tiden.",
                                .cap(pron), edu_full, inc))
    } else {
      parts <- c(parts, .pick(c(
        if (grepl(" med ", edu_full)) sprintf("Etter %s er %s %s.", edu_full, pron, occ_disp)
        else sprintf("Med %s i bagasjen er %s %s.", edu_full, pron, occ_disp),
        sprintf("%s har %s og jobber som %s.", .cap(pron), edu_full, occ_disp))))
    }
  } else if (!is.na(edu_full)) {
    parts <- c(parts, sprintf("%s har %s.", .cap(pron), edu_full))
  } else if (!is.na(occ_str)) {
    inc <- if (!is.na(age_n) && age_n < 19) NA_character_
           else .income_phrase(income, neet = isTRUE(x$neet))
    if (!is.na(inc) && !grepl("utenfor", inc)) {
      parts <- c(parts, sprintf("%s jobber som %s og har %s.",
                                .cap(pron), occ_disp, inc))
    } else {
      parts <- c(parts, sprintf("%s jobber som %s.", .cap(pron), occ_disp))
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
        pocc <- .occ_relative_clause(ptn$occupation)
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
      part <- if (has_kids) sprintf("%s er skilt og har %s.", .cap(pron), kids_str)
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

  age_n <- if (!is.null(x$age) && !is.na(x$age)) as.integer(x$age) else NA_integer_
  hh <- tolower(x$household %||% "")
  # Someone living with their parents does not own or rent the home; the
  # tenure is the parents'. And "Leier" contains "eier": the unanchored
  # pattern used to narrate every renter as an owner.
  if (grepl("foreldre", hh) || (!is.na(age_n) && age_n < 18)) return(NA_character_)
  in_coll <- grepl("bofellesskap|kollektiv", hh)
  alone <- grepl("alene|enslig", hh) && !grepl("med barn", hh)
  who <- if (alone || !nzchar(hh)) .cap(pron) else "Familien"

  part <- NA_character_
  if (grepl("^Eier|^Owns", ht, ignore.case = TRUE)) {
    type_np <- .housing_np(htype)
    val_str <- if (!is.null(hval) && !is.na(hval) && hval > 0)
                 sprintf(" verdt rundt %s millioner", .mill_no(hval)) else ""
    part <- if (hytte)
      sprintf("%s eier %s%s, og har i tillegg hytte.", who, type_np, val_str)
    else
      sprintf("%s eier %s%s.", .cap(pron), type_np, val_str)
  } else if (in_coll) {
    part <- NA_character_      # "Hun bor i kollektiv" is already said
  } else if (grepl("^Leier|^Rents", ht, ignore.case = TRUE)) {
    part <- if (hytte) sprintf("%s leier bolig, men har hytte.", .cap(pron))
            else sprintf("%s leier bolig.", .cap(pron))
  } else if (hytte) {
    part <- "Familien har hytte."
  }

  wq <- .qualitative_wealth(nw)
  wpart <- if (!is.na(wq) && wq %in% c("betydelig formue", "stor formue", "negativ formue"))
             (if (wq == "negativ formue") sprintf("%s har mer gjeld enn formue.", .cap(pron))
              else sprintf("%s har %s.", .cap(pron), wq)) else NA

  paste(c(part, wpart)[!is.na(c(part, wpart))], collapse = " ")
}

# ---- cultural / political ---------------------------------------------

.narrate_cultural <- function(x, skip_party = FALSE) {
  g <- x$gender; pron <- .pronouns(g, "subj")
  age_n <- if (!is.null(x$age) && !is.na(x$age)) as.integer(x$age) else NA_integer_
  parts <- character(0)

  # religion + party, combined where possible
  rel_clause <- .religion_clause(x$religion)
  # Children: the label is the parents' choice ("D\u00f8pt i Den norske
  # kirke", "Foreldrenes tradisjon"). "tilh\u00f8rer d\u00f8pt i ..." and a
  # five-year-old who "regner seg ikke som troende" both came from treating
  # it as the child's own belief.
  if (!is.na(age_n) && age_n < 16) rel_clause <- .child_religion_clause(x$religion)

  # Humour labels used to be printed as a standalone sentence. They read
  # well on the card, but in prose they could contradict the life ("Eier
  # en Tesla og to leiligheter -- derfor H." for a man with one small
  # house and 270 000 a year). The prose now names the party.
  party <- x$party
  party_clause <- NA_character_
  if (!is.null(party) && !is.na(party) &&
      !grepl("stemmer ikke|sofa|ikke stemme|stemte ikke|fyllesyk|glemte", tolower(party))) {
    party_clause <- .party_name(party)
  }
  if (isTRUE(skip_party)) party_clause <- NA_character_

  # Norwegian is V2: after a fronted adverbial the finite verb comes
  # second and the subject follows it. The old template appended the
  # pronoun to a ready-made verb phrase --
  #   sprintf("Politisk %s %s.", "stemmer SV", "hun")
  # -- which put the subject after the object: "Politisk stemmer SV hun."
  # The conjoined form was worse still, leaving the second clause with no
  # subject at all: "..., og religiost tilhorer Den norske kirke."
  #
  # The party clause is now the object alone, so the pronoun can be
  # placed where it belongs.
  if (!is.na(party_clause) && !is.na(rel_clause)) {
    parts <- c(parts, .pick(c(
      sprintf("%s stemmer %s og %s.", .cap(pron), party_clause, rel_clause),
      sprintf("Politisk stemmer %s %s, og %s %s.",
              pron, party_clause, pron, rel_clause))))
  } else if (!is.na(party_clause)) {
    parts <- c(parts, .pick(c(
      sprintf("%s stemmer %s.", .cap(pron), party_clause),
      sprintf("Politisk stemmer %s %s.", pron, party_clause))))
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
    # "regner seg ikke som troende ... I helgene blir det religioes aktivitet"
    if (identical(rel_clause, "regner seg ikke som troende") || is.na(rel_clause))
      hob_labels <- hob_labels[!grepl("^religi", tolower(hob_labels))]
    if (length(hob_labels) > 0) {
      sel <- if (length(hob_labels) > 2) sample(hob_labels, 2) else hob_labels
      hob_str <- .join_no(vapply(sel, .hobby_noun, character(1)))
      lead <- if (!is.na(age_n) && age_n < 16)
        .pick(c(sprintf("Ellers g\u{00e5}r tiden til %s.", hob_str),
                sprintf("%s liker %s.", .cap(pron), hob_str)))
      else
        .pick(c(sprintf("P\u{00e5} fritiden driver %s med %s.", pron, hob_str),
                sprintf("Fritiden g\u{00e5}r til %s.", hob_str),
                sprintf("I helgene blir det %s.", hob_str)))
      parts <- c(parts, lead)
    }
  }

  # media (which paper) — distinctive enough to keep
  if (!is.null(x$media_paper) && !is.na(x$media_paper) &&
      !grepl("ingen|none", tolower(x$media_paper))) {
    paper <- if (identical(x$media_paper, "Lokalavis")) "lokalavisen" else x$media_paper
    parts <- c(parts, .pick(c(
      sprintf("Nyhetene kommer fra %s.", paper),
      sprintf("%s leser %s.", .cap(pron), paper))))
  }

  # diet only when distinctive
  if (!is.null(x$diet) && !is.na(x$diet) &&
      !grepl("blandet|vanlig|alminnel|servert hjemme|served at home", tolower(x$diet))) {
    parts <- c(parts, .diet_sentence(x$diet, pron))
  }

  paste(parts, collapse = " ")
}

# ---- wellbeing ---------------------------------------------------------

.narrate_wellbeing <- function(x) {
  parts <- character(0)
  P <- .cap(.pronouns(x$gender, "subj"))
  # "Helsen er god. Lever med psykisk lidelse." read as two claims that
  # contradict each other. Self-rated good health alongside a chronic
  # condition is common and real; the sentence now says so.
  chronic <- NA_character_
  if (isTRUE(x$has_chronic) && !is.null(x$chronic_type) && !is.na(x$chronic_type)) {
    ct <- tolower(x$chronic_type)
    chronic <- if (grepl("(lidelse|sykdom)$", ct) && !grepl("^(en|ei|et) ", ct)) paste("en", ct) else ct
  }
  good <- FALSE
  if (!is.null(x$self_rated_health) && !is.na(x$self_rated_health)) {
    h <- tolower(x$self_rated_health)
    good <- grepl("meget god|excellent", h)
    if (grepl("daarlig|d\u{00e5}rlig|poor", h)) parts <- c(parts, sprintf("Helsen oppleves som %s.", h))
    else if (good && is.na(chronic)) parts <- c(parts, "Helsen er god.")
  }
  if (!is.na(chronic)) {
    parts <- c(parts, if (good)
      sprintf("Helsen oppleves som god, selv om %s lever med %s.", tolower(P), chronic)
    else sprintf("%s lever med %s.", P, chronic))
  }
  if (isTRUE(x$has_disability) && !is.null(x$disability) && !is.na(x$disability)) {
    parts <- c(parts, sprintf("%s %s.", P, .disability_phrase(x$disability_type, x$disability_severity,
                                                         x$disability)))
  }
  if (!is.null(x$close_friends) && !is.na(x$close_friends) &&
      grepl("ingen", tolower(x$close_friends))) {
    parts <- c(parts, sprintf("%s har ingen n\u{00e6}re venner.", P))
  }
  if (identical(x$has_confidant, FALSE)) {
    parts <- c(parts, sprintf("%s mangler en fortrolig \u{00e5} snakke med.", P))
  }
  if (!is.null(x$loneliness) && !is.na(x$loneliness) &&
      grepl("ofte|often", tolower(x$loneliness))) {
    parts <- c(parts, sprintf("%s f\u{00f8}ler seg ofte ensom.", P))
  }
  if (!is.null(x$deprivation_count) && !is.na(x$deprivation_count) &&
      x$deprivation_count >= 4L) {
    parts <- c(parts, "Hverdagen er preget av materielle vansker.")
  }
  paste(parts, collapse = " ")
}

# ---- compact -----------------------------------------------------------

.narrate_compact <- function(x) {
  pieces <- sprintf("%s, %s \u{00e5}r, bor i %s.",
                    x$name %||% "Personen", x$age %||% 0,
                    x$municipality %||% x$county %||% "Norge")
  age_n <- if (!is.null(x$age) && !is.na(x$age)) as.integer(x$age) else NA_integer_
  if (!is.na(age_n) && age_n < 16) {
    skole <- if (age_n < 6) "i barnehagen" else if (age_n <= 12) "p\u{00e5} barneskolen" else "p\u{00e5} ungdomsskolen"
    pieces <- c(pieces, sprintf("G\u{00e5}r %s.", skole))
    if (!is.null(x$occupation) && !is.na(x$occupation))
      pieces <- c(pieces, sprintf("Er %s.", tolower(x$occupation)))
  } else if ((!is.na(age_n) && age_n >= 67) ||
             (!is.null(x$occupation) && !is.na(x$occupation) &&
              x$occupation %in% .PENSJONIST_LABELS)) {
    pieces <- c(pieces, "Er pensjonist.")
    if (!is.null(x$occupation) && !is.na(x$occupation))
      pieces <- c(pieces, sprintf("Kjent som %s.", tolower(x$occupation)))
    edu_c <- .edu_prose(x$education)
    if (!is.na(edu_c)) pieces <- c(pieces, sprintf("Har %s.", edu_c))
  } else {
    op <- .occ_parse(x$occupation)
    if (isTRUE(x$neet)) op <- list(kind = "neet")
    if (!is.null(op)) {
      s_occ <- switch(op$kind,
        job = sprintf("Jobber som %s%s.", op$job, if (isTRUE(op$part)) " p\u{00e5} deltid" else ""),
        aap = "Mottar arbeidsavklaringspenger.",
        dagpenger = "G\u{00e5}r p\u{00e5} dagpenger.",
        sosialhjelp = "Mottar sosialhjelp.",
        ufor = "Er uf\u{00f8}retrygdet.",
        student = if (!is.na(op$job)) sprintf("Studerer og jobber deltid som %s.", op$job) else "Studerer.",
        neet = "St\u{00e5}r utenfor utdanning og arbeid.",
        home = "Er hjemmev\u{00e6}rende.",
        teen = "G\u{00e5}r p\u{00e5} videreg\u{00e5}ende.",
        NA_character_)
      if (!is.na(s_occ)) pieces <- c(pieces, s_occ)
    }
    edu_c <- .edu_prose(x$education)
    if (!is.na(edu_c)) pieces <- c(pieces, sprintf("Har %s.", edu_c))
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
  parts <- sprintf("%s (f. %s) \u{2014} et liv i korte trekk.", name, by %||% "?")
  parts <- c(parts, .narrate_origins(x), .narrate_education_career(x),
             .narrate_family_life(x), .narrate_material(x))
  parts <- parts[nzchar(parts) & !is.na(parts)]
  paste(parts, collapse = "\n\n")
}

# %||% is defined in print.R
