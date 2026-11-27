# ============================================================
# Requirements per job title (November 2026)
#
# SSB publishes education, age and wages per four-digit STYRK code, but
# the package narrates one of about 7 000 seven-digit titles inside the
# code. A code such as 2310 holds "universitetslektor" next to titles
# that need far less, so the data cannot say what a title requires. Until
# now that was patched one title at a time, after a reader had seen it go
# wrong ("universitetslektor" with a bachelor's degree, "solvertinne" for
# a man, a 54-year-old "praktikant").
#
# This file classifies the whole title register once, by rule, so that
# every title carrying a regulated or self-describing name gets its
# requirement whether or not anyone has seen it misused yet. Each rule is
# a regular expression on the upper-case title and one or more of:
#
#   min_edu   lowest education code (NUS level: 4 videregaaende/fagbrev,
#             5 fagskole, 6 bachelor, 7 master)
#   min_age   lowest age
#   max_age   highest age
#   gender    "F" or "M" when the title names the holder's sex
#
# A title matched by several rules gets the strictest of each. The rules
# describe Norwegian titles and authorisation law (helsepersonelloven,
# domstolloven, advokatforskriften, opplaeringslova's lektor/adjunkt
# levels), not SSB data; they are assumptions and are listed here so that
# they can be read and argued with. test-title-rules.R checks them
# against named titles in both directions.
# ============================================================

.TITLE_NOTRAIN <- "^(?!.*(L\u00c6RLING|PRAKTIKANT|ASPIRANT|\\bELEV\\b))"

.TITLE_RULES <- data.frame(
  stringsAsFactors = FALSE,
  rule = c(
    "master_profesjon", "master_akademia", "master_sivil",
    "bachelor_helse", "bachelor_skole_sosial", "bachelor_revisjon_politi",
    "fagskole_ingenior", "mesterbrev", "fagbrev",
    "ledelse", "toppledelse", "laering",
    "kvinnetittel", "manntittel"),
  pattern = c(
    # Doctors, dentists, psychologists, lawyers, judges, clergy. Not lay
    # judges (MEDDOMMER) or competition judges (DOMMER (IDRETT)).
    paste0("(?<!KOL)LEGE\\b|PSYKOLOG\\b|ADVOKAT\\b|(?<!MED)(?<!SMAKS)DOMMER(?!.*\\((FERDIG|IDRETT|MAT))|",
           "PREST\\b|BISKOP|^PROST\\b|JURIST\\b|LOGOPED|ERN\u00c6RINGSFYSIOLOG|VETERIN\u00c6R|PROVISOR"),
    # Academic posts. Lektor in school is the master-level title (adjunkt
    # is the bachelor one). ETTERFORSKER is police, handled below.
    "LEKTOR|AMANUENSIS|PROFESSOR|DOSENT|STIPENDIAT|POSTDOK|(?<!ETTER)FORSKER\\b",
    paste0("SIVILINGENI\u00d8R|SIVIL\u00d8KONOM|SIVILARKITEKT|^(LANDSKAPS)?ARKITEKT\\b|",
           "(?<!IKKE )STATSAUTORISERT"),
    # Authorised health professions at bachelor level.
    paste0(.TITLE_NOTRAIN, ".*(SYKEPLEIER\\b|ERGOTERAPEUT|ARBEIDSTERAPEUT|FYSIOTERAPEUT|JORDMOR|",
           "RADIOGRAF|BIOINGENI|VERNEPLEIER\\b|TANNPLEIER|OPTIKER\\b|AUDIOGRAF|PARAMEDISINER|",
           "FARMAS\u00d8YT\\b|ORTOPEDIINGENI)"),
    # Teachers in school (not course teachers), preschool teachers,
    # social workers, librarians, deacons.
    paste0(.TITLE_NOTRAIN, ".*(ADJUNKT|^(ALLMENN|GRUNNSKOLE|F\u00d8RSKOLE|BARNEHAGE|FAG|SPESIAL|SOSIAL|KONTAKT)?",
           "L\u00c6RER\\b(?!.*KURS)|^PEDAGOG\\b(?!.*FORSKER)|PEDAGOGISK LEDER|BARNEVERNSPEDAGOG|",
           "SOSIONOM|SPESIALPEDAGOG|BIBLIOTEKAR\\b|^DIAKON\\b|^SOKNEDIAKON|^KLOKKERDIAKON|KATEKET)"),
    paste0("^REVISOR\\b|REGISTRERT REVISOR|AUTORISERT REGNSKAPSF\u00d8RER|",
           "^POLITI(BETJENT|OVERBETJENT|F\u00d8RSTEBETJENT|INSPEKT\u00d8R)|^ETTERFORSKER|INTERI\u00d8RARKITEKT"),
    paste0(.TITLE_NOTRAIN, ".*(INGENI\u00d8R|INSTALLAT\u00d8R)"),
    # Mesterbrev: fagbrev, practice and the master course. Not ship or
    # stage machinists (MASKINMESTER).
    paste0("(MALER|T\u00d8MRER|MURER|R\u00d8RLEGGER|BAKER|KONDITOR|FRIS\u00d8R|SNEKKER|BLIKKENSLAGER|",
           "SMED|URMAKER|SKREDDER|SLAKTER|BYGG|ELEKTRO)MESTER\\b|^MESTER\\b"),
    # Titles that say the holder has a trade certificate or authorisation.
    paste0(.TITLE_NOTRAIN, ".*(HJELPEPLEIER|HELSEFAGARBEIDER|OVERPLEIER|ELEKTRIKER|ELVERKSMONT|",
           "ENERGIMONT|AUTOMATIKER|FAGARBEIDER|SVENN\\b|BARNE- OG UNGDOMSARBEIDER|",
           "AMBULANSEARBEIDER|OMSORGSARBEIDER)"),
    # Senior and leading titles. OVER- but not OVERSETTER, OVERFLATE-.
    paste0("^(OVER(?!SETT|FLATE|V\u00c5K|GANG|BYGN|SIKT|NATT|HALING|F\u00d8RING)|LEDENDE|SENIOR|HOVED|SJEFS?)|",
           "SJEF\\b|DIREKT\u00d8R|(?<!PEDAGOGISK )\\bLEDER\\b"),
    "ADMINISTRERENDE DIREKT\u00d8R|KONSERNSJEF|GENERALDIREKT\u00d8R|DEPARTEMENTSR\u00c5D|ADM\\.? ?DIR",
    "L\u00c6RLING|PRAKTIKANT|ASPIRANT|TRAINEE|\\bELEV\\b|KADETT",
    # Titles naming the holder's sex. A slash ("B&B-VERT/VERTINNE") names
    # both. -MANN titles and JORDMOR are official for both sexes.
    "^[^/]*(INNE|PIKE|DAME|JOMFRU|OLDFRUE|HUSMOR|FOSTERMOR|DIAKONISSE|S\u00d8STER)( \\(|$)",
    "^[^/]*(GUTT|FOSTERFAR|HUSFAR)( \\(|$)"),
  min_edu = c(7L, 7L, 7L, 6L, 6L, 6L, 5L, 5L, 4L, NA, NA, NA, NA, NA),
  min_age = c(NA, NA, NA, NA, NA, NA, NA, 25L, NA, 30L, 35L, NA, NA, NA),
  max_age = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 29L, NA, NA),
  gender  = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "F", "M")
)

# Requirements for every row of .cfm_env$occupations, computed once.
.title_requirements <- function() {
  tr <- .cfm_env$title_requirements
  occ <- .cfm_env$occupations
  if (!is.null(tr) && !is.null(occ) && nrow(tr) == nrow(occ)) return(tr)
  nm <- toupper(occ$name)
  n <- length(nm)
  min_edu <- rep(0L, n); min_age <- rep(0L, n); max_age <- rep(200L, n)
  gender <- rep(NA_character_, n); rule <- rep("", n)
  for (i in seq_len(nrow(.TITLE_RULES))) {
    hit <- grepl(.TITLE_RULES$pattern[i], nm, perl = TRUE)
    if (!any(hit)) next
    if (!is.na(.TITLE_RULES$min_edu[i])) min_edu[hit] <- pmax(min_edu[hit], .TITLE_RULES$min_edu[i])
    if (!is.na(.TITLE_RULES$min_age[i])) min_age[hit] <- pmax(min_age[hit], .TITLE_RULES$min_age[i])
    if (!is.na(.TITLE_RULES$max_age[i])) max_age[hit] <- pmin(max_age[hit], .TITLE_RULES$max_age[i])
    if (!is.na(.TITLE_RULES$gender[i]))  gender[hit]  <- .TITLE_RULES$gender[i]
    rule[hit] <- ifelse(nzchar(rule[hit]), paste(rule[hit], .TITLE_RULES$rule[i], sep = "+"),
                        .TITLE_RULES$rule[i])
  }
  tr <- data.frame(min_edu = min_edu, min_age = min_age, max_age = max_age,
                   gender = gender, rule = rule, stringsAsFactors = FALSE)
  .cfm_env$title_requirements <- tr
  tr
}

# Which of the occupation rows `rows` a person of this age, education and
# sex can hold. Unknown education (NA, or 9 "uoppgitt") counts as
# videregaaende, the modal level, rather than as no constraint.
.title_fits <- function(rows, age = NA, edu_code = NA, gender = NULL) {
  tr <- .title_requirements()[rows, , drop = FALSE]
  ok <- rep(TRUE, length(rows))
  e <- suppressWarnings(as.integer(edu_code))
  if (!length(e) || is.na(e) || e == 9L) e <- 4L
  ok <- ok & tr$min_edu <= e
  a <- suppressWarnings(as.numeric(age))
  if (length(a) && !is.na(a)) ok <- ok & a >= tr$min_age & a <= tr$max_age
  if (!is.null(gender) && length(gender) && !is.na(gender)) {
    g <- toupper(gender)
    if (g == "F") ok <- ok & !(tr$gender %in% "M")
    if (g == "M") ok <- ok & !(tr$gender %in% "F")
  }
  ok
}

# Does a finished title (as narrated) fit the person? Used by the final
# check on a whole life. Looks the title up in the register; titles not
# found there (pensioner labels, benefit labels, teen jobs) pass.
.title_label_fits <- function(label, age = NA, edu_code = NA, gender = NULL) {
  if (is.null(label) || is.na(label) || !nzchar(label)) return(TRUE)
  occ <- .cfm_env$occupations
  if (is.null(occ)) return(TRUE)
  key <- .cfm_env$occupation_title_keys
  if (is.null(key) || length(key) != nrow(occ)) {
    key <- tolower(vapply(occ$name, .titlecase_yrke, character(1), USE.NAMES = FALSE))
    .cfm_env$occupation_title_keys <- key
  }
  lab <- tolower(sub(" \\(deltid\\)$", "", label))
  m <- regmatches(lab, regexec("^(aap|dagpenger) \\(tidl\\. (.*)\\)$", lab))[[1]]
  if (length(m) == 3L) lab <- m[3]
  rows <- which(key == lab)
  if (!length(rows)) return(TRUE)
  any(.title_fits(rows, age, edu_code, gender))
}
