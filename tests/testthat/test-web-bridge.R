# Nettleserlaget (web/) har flere hardkodede lister som MÅ speile noe på
# R-sida: pin-nøkler, reroll-dimensjoner, navn på interne trekk-funksjoner.
# Den klassen feil har slått til fire ganger i dette prosjektet -- CSV_FILES
# og R_FILES i cfm-webr.js, source()-lista i cfm-bootstrap.R, male_only/
# female_only-kopiene, og expect_length(tbl$female, 6) i test-counterfact-me.R
# -- og hver gang stille: lista blir utdatert, koden kjører videre, og
# resultatet er feil uten at noe feiler.
#
# Konkret eksempel som motiverte denne fila: broen sendte ikke brukerens
# låser videre til R, så ALLE låste trekk falt ned i sampler.js -- en
# JS-speiling av pakken v0.9.1, altså uten dødelighet, utdanningsbånd,
# formuehale og aldersbetinget inntekt. Ingenting feilet. Appen viste
# "Kilde · R-pakke" hele tida.
#
# web/ ligger i .Rbuildignore, så disse testene kjører under
# devtools::test() og hoppes over i R CMD check på en bygget tarball.

web_dir <- function() testthat::test_path("..", "..", "web")

read_web <- function(file) {
  p <- file.path(web_dir(), file)
  skip_if_not(file.exists(p), paste("web/", file, "er ikke tilgjengelig her"))
  readLines(p, warn = FALSE, encoding = "UTF-8")
}

# Nøklene i .cfm_rec_fns-blokka: "  nøkkel = \"funksjon\","
parse_rec_fns <- function(lines) {
  i <- grep("^\\.cfm_rec_fns\\s*<-\\s*c\\(", lines)
  if (length(i) == 0) return(character(0))
  j <- i[1] + which(grepl("^\\)", lines[(i[1] + 1):length(lines)]))[1]
  body <- lines[(i[1] + 1):(j - 1)]
  m <- regmatches(body, regexec('^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*"([^"]+)"', body))
  m <- m[vapply(m, length, integer(1)) == 3L]
  stats::setNames(vapply(m, `[`, character(1), 3), vapply(m, `[`, character(1), 2))
}

# Alle pin-nøkler R godtar = .cfm_rec_fns-nøklene + spesialnøklene.
parse_pin_keys <- function(lines) {
  rec <- names(parse_rec_fns(lines))
  i <- grep("^\\.cfm_all_pin_keys\\s*<-", lines)
  expect_length(i, 1L)
  extra <- regmatches(lines[i[1]], gregexpr('"[^"]+"', lines[i[1]]))[[1]]
  c(rec, gsub('"', "", extra))
}

# Verdiene i et JS-objektliteral over N linjer: { a: 'x', b: 'y' }
parse_js_map_values <- function(lines, const_name) {
  i <- grep(paste0("^const ", const_name, "\\s*="), lines)
  expect_length(i, 1L)
  j <- i[1] + which(grepl("\\};", lines[i[1]:length(lines)]))[1] - 1L
  body <- paste(lines[i[1]:j], collapse = " ")
  m <- gregexpr("[A-Za-z_][A-Za-z0-9_]*\\s*:\\s*'[^']*'", body)
  pairs <- regmatches(body, m)[[1]]
  vals <- sub(".*'([^']*)'.*", "\\1", pairs)
  keys <- sub("\\s*:.*", "", pairs)
  stats::setNames(vals, keys)
}


test_that("hver funksjon broen pinner finnes faktisk i pakken", {
  lines <- read_web("cfm-reroll.R")
  ns <- asNamespace("CounterfactMe")

  # Trekk-funksjonene i recorder-tabellen.
  fns <- unname(parse_rec_fns(lines))
  expect_gt(length(fns), 20L)

  # Pluss følge-pinnene, som bare står som strenger i pin_one()-kallene.
  extra <- c(".draw_name_by_background", ".cond_neet_prob",
             ".background_edu_shift", ".background_occupation_shift",
             ".background_overqualified", ".background_income_factor")
  # Hvis en følge-pinne blir borte fra fila, skal denne lista også krympe --
  # ellers tester vi noe som ikke lenger pinnes.
  for (f in extra)
    expect_true(any(grepl(f, lines, fixed = TRUE)),
                label = sprintf("%s nevnes i cfm-reroll.R", f))

  for (f in c(fns, extra)) {
    expect_true(exists(f, envir = ns, inherits = FALSE),
                label = sprintf("%s finnes i CounterfactMe", f))
  }
})

test_that("hver laas i app.jsx mapper til en pin-noekkel R godtar", {
  pin_keys <- parse_pin_keys(read_web("cfm-reroll.R"))
  app <- read_web("app.jsx")

  m <- parse_js_map_values(app, "R_PIN_FOR_LOCK")
  expect_gt(length(m), 8L)
  expect_true(all(m %in% pin_keys),
              label = paste("ukjente pin-noekler:",
                            paste(setdiff(m, pin_keys), collapse = ", ")))

  # Og hver laasbare dimensjon i UI-et maa ha en oppfoering i den mappen.
  # Ellers vil laasen se ut som den virker, men ikke bli sendt til R --
  # som var nettopp feilen denne fila finnes for aa fange.
  i <- grep("^function lockKeyFor", app)
  expect_length(i, 1L)
  body <- paste(app[i[1]:(i[1] + 3L)], collapse = " ")
  lock_keys <- unique(regmatches(body, gregexpr(':"[^"]+"', body))[[1]])
  lock_keys <- gsub('"', "", sub("^:", "", lock_keys))
  expect_gt(length(lock_keys), 8L)
  expect_true(all(lock_keys %in% names(m)),
              label = paste("laaser uten R-pin:",
                            paste(setdiff(lock_keys, names(m)), collapse = ", ")))
})

test_that("reroll-dimensjonene i cfm-webr.js finnes i R-ens redraw-sett", {
  reroll <- read_web("cfm-reroll.R")
  i <- grep("^\\.cfm_redraw_sets\\s*<-\\s*list\\(", reroll)
  expect_length(i, 1L)
  dims <- regmatches(reroll, regexec("^  ([a-z_]+)\\s+=\\s+c\\(", reroll))
  dims <- vapply(dims[vapply(dims, length, integer(1)) == 2L], `[`, character(1), 2)
  expect_gt(length(dims), 5L)

  webr <- read_web("cfm-webr.js")
  j <- grep("^  const REROLL_DIMS", webr)
  expect_length(j, 1L)
  # Stopp ved "]);" i stedet for et fast linjevindu -- ellers ville en ny
  # kommentar rett under settet kunne bli lest som en dimensjon.
  k <- j[1] + which(grepl("\\]\\);", webr[j[1]:length(webr)]))[1] - 1L
  body <- paste(webr[j[1]:k], collapse = " ")
  js_dims <- gsub("'", "", regmatches(body, gregexpr("'[a-z_]+'", body))[[1]])
  expect_gt(length(js_dims), 5L)

  expect_setequal(js_dims, dims)
})

test_that("redraw-settene inneholder bare gyldige pin-noekler", {
  # Et stavefeil-element her ville ikke feile: setdiff() dropper det bare,
  # dimensjonen blir pinnet i stedet for trukket paa nytt, og rerollen
  # returnerer samme verdi uten aa si noe.
  reroll <- read_web("cfm-reroll.R")
  pin_keys <- parse_pin_keys(reroll)

  i <- grep("^\\.cfm_redraw_sets\\s*<-\\s*list\\(", reroll)
  j <- i[1] + which(grepl("^\\)", reroll[(i[1] + 1):length(reroll)]))[1]
  body <- paste(reroll[i[1]:j], collapse = " ")
  used <- unique(gsub('"', "", regmatches(body, gregexpr('"[a-z_]+"', body))[[1]]))
  expect_gt(length(used), 15L)

  expect_true(all(used %in% pin_keys),
              label = paste("ukjente noekler i redraw-sett:",
                            paste(setdiff(used, pin_keys), collapse = ", ")))
})

test_that("JS-reserven er ikke lenger ruten for laaste trekk", {
  # Regresjonstest paa den faktiske feilen: useR krevde NULL laaser, saa
  # hver laas sendte trekket til sampler.js.
  app <- read_web("app.jsx")
  expect_false(
    any(grepl("Object.keys(STATE.locks || {}).length === 0", app, fixed = TRUE)),
    label = "app.jsx diskvalifiserer ikke R naar det finnes laaser"
  )
  expect_true(any(grepl("drawLockedLife", app, fixed = TRUE)))

  # Og rammeverksvelgeren skal ikke ligge igjen som en valgmulighet noe sted.
  sampler <- read_web("sampler.js")
  expect_false(any(grepl("opts.frame", sampler, fixed = TRUE)))
})
