# data-raw/first_names.R
#
# (Re)build cohort-weighted first-name frequencies from SSB table 10467:
#   "Fødte, etter fornavn, statistikkvariabel og år" (1880-present).
#
# Output: inst/extdata/first_names_cohort.csv
#   columns: name, gender, cohort, frequency
#   cohort  = decade of birth (e.g. 1990 covers 1990-1999)
#   gender  = "F" or "M" (encoded in the first char of the Fornavn code)
#
# Provenance:
#   https://www.ssb.no/statbank/table/10467
#   API:  https://data.ssb.no/api/v0/no/table/10467
#
# To regenerate:
#   setwd("CounterfactMe"); source("data-raw/_ssb.R"); source("data-raw/first_names.R")

source("data-raw/_ssb.R")

TABLE_ID <- "10467"

# -- 1. Metadata -------------------------------------------------------------

meta <- ssb_metadata(TABLE_ID)
name_codes <- vapply(
  meta$variables[[match("Fornavn", vapply(meta$variables, `[[`, "", "code"))]]$values,
  identity, character(1)
)
year_codes <- vapply(
  meta$variables[[match("Tid", vapply(meta$variables, `[[`, "", "code"))]]$values,
  identity, character(1)
)
years <- as.integer(year_codes)

# -- 2. Chunked fetch --------------------------------------------------------
# Keep each request well under SSB's cell-count ceiling by fetching one decade
# at a time. 1974 names x 10 years x 1 stat ~ 20k cells per request.

decade_buckets <- split(year_codes, (as.integer(year_codes) %/% 10) * 10)

chunks <- lapply(names(decade_buckets), function(dec) {
  message(sprintf("[SSB %s] fetching decade %s (%d years)...",
                  TABLE_ID, dec, length(decade_buckets[[dec]])))
  q <- ssb_build_query(
    selections = list(
      Fornavn = name_codes,
      Tid     = decade_buckets[[dec]]
    ),
    contents_code = "Personer",
    meta = meta
  )
  df <- ssb_fetch(TABLE_ID, q)
  Sys.sleep(1.6)  # stay under 40 req/min
  df
})
raw <- do.call(rbind, chunks)
raw$value[is.na(raw$value)] <- 0
raw$year <- as.integer(raw$Tid)

# -- 3. Decode Fornavn codes ------------------------------------------------
# First char:  "1" = female, "2" = male.
# Rest encodes the name with these substitutions observed in the API:
#   Z1 -> Æ, Z2 -> Ø, Z3 -> Å,  _  -> "-"   (hyphenated compound names)

decode_name <- function(code) {
  stopifnot(is.character(code))
  gender <- ifelse(substr(code, 1, 1) == "1", "F",
            ifelse(substr(code, 1, 1) == "2", "M", NA_character_))
  body <- substr(code, 2, nchar(code))
  body <- gsub("Z1", "\u00C6", body, fixed = TRUE)
  body <- gsub("Z2", "\u00D8", body, fixed = TRUE)
  body <- gsub("Z3", "\u00C5", body, fixed = TRUE)
  body <- gsub("_", "-", body, fixed = TRUE)
  # Title-case each hyphen-separated part
  parts <- strsplit(body, "-", fixed = TRUE)
  pretty <- vapply(parts, function(p) {
    p <- tolower(p)
    substr(p, 1, 1) <- toupper(substr(p, 1, 1))
    paste(p, collapse = "-")
  }, character(1))
  data.frame(name = pretty, gender = gender, stringsAsFactors = FALSE)
}

decoded <- decode_name(raw$Fornavn)
raw$name   <- decoded$name
raw$gender <- decoded$gender

# Drop rows with unknown gender prefix (should not occur, but be safe)
raw <- raw[!is.na(raw$gender), ]

# -- 4. Aggregate into decade cohorts ---------------------------------------

raw$cohort <- (raw$year %/% 10) * 10L

agg <- aggregate(value ~ name + gender + cohort, data = raw, FUN = sum)
names(agg)[names(agg) == "value"] <- "frequency"
agg <- agg[agg$frequency > 0, ]
agg <- agg[order(agg$cohort, agg$gender, -agg$frequency, agg$name), ]

# -- 5. Write CSV ------------------------------------------------------------

out <- file.path("inst", "extdata", "first_names_cohort.csv")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write.csv(agg, out, row.names = FALSE, fileEncoding = "UTF-8")

message(sprintf("Wrote %d rows to %s (cohorts %d-%d, %d unique names).",
                nrow(agg), out,
                min(agg$cohort), max(agg$cohort),
                length(unique(agg$name))))
