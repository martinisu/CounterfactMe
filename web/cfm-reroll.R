# cfm-reroll.R — betinget om-trekning av ÉN dimensjon, helt i R.
#
# Problemet: counterfact_me() trekker hele livet som én betinget kjede
# (alder → bakgrunn → utdanning → yrke → inntekt → ...). Å bytte én
# variabel og beholde resten ukritisk bryter den betingede strukturen.
#
# Løsningen her:
#   1. RECORDERE: tynne wrappere rundt alle .cond_*/sample_*-funksjoner
#      fanger returverdien av hver interne trekning (med koder, ikke bare
#      labels) i .cfm_cap under en full counterfact_me()-kjøring.
#   2. KONTEKST-LAGER: hver trekning lagres med en id (_ctx_id i JSON).
#   3. cfm_reroll_json(dim, ctx_id): pinner alt UNNTATT måldimensjonen og
#      dens faktiske nedstrøms-avhengige (pin = funksjonen returnerer den
#      lagrede verdien i stedet for å trekke), og kjører counterfact_me()
#      på nytt. Oppstrøms reproduseres eksakt; målet + avhengige trekkes
#      friskt, betinget på det bevarte. Gibbs-aktig betinget om-trekning.
#
# Sources av cfm-bootstrap.R ETTER pakkefilene og patch-cond-wealth.R,
# slik at recorderne wrapper de patchede versjonene.

.cfm_cap       <- new.env(parent = emptyenv())  # fangst for inneværende trekning
.cfm_ctx_store <- new.env(parent = emptyenv())  # id -> ctx (ringlager)
.cfm_ctx_counter <- 0L

# Interne nøkler -> funksjonen som trekker dem
.cfm_rec_fns <- c(
  age         = "sample_age",
  bg          = ".cond_immigrant_background",
  sentr       = ".cond_sentralitet",
  edu         = ".cond_education",
  occ         = ".cond_occupation",
  nus         = ".cond_nus_field",
  par         = ".cond_parents",
  parcap      = ".parents_capital",
  sib         = ".cond_siblings",
  gp          = ".cond_grandparents",
  inc         = ".cond_income",
  ms          = ".cond_marital_status",
  household   = ".cond_household",
  ori         = ".cond_orientation",
  rel         = ".cond_religion",
  party       = ".cond_party",
  housing     = ".cond_housing",
  wealth      = ".cond_wealth",
  children    = ".cond_n_children",
  deprivation = ".cond_material_deprivation",
  health      = ".cond_health",
  isolation   = ".cond_social_isolation",
  media       = ".cond_media",
  sleep       = ".cond_sleep",
  diet        = ".cond_diet",
  alcohol     = ".cond_alcohol",
  hobbies     = ".cond_hobbies",
  bourdieu    = ".cond_bourdieu"
)

.cfm_install_recorders <- function() {
  for (key in names(.cfm_rec_fns)) {
    fname <- .cfm_rec_fns[[key]]
    if (!exists(fname, envir = globalenv())) next
    orig <- get(fname, envir = globalenv())
    if (identical(fname, ".cond_income")) {
      # Spesial: kjeden muterer edu$code og occ ETTER .cond_education/.cond_occupation
      # (bakgrunns-skift). .cond_income mottar de FERDIGE verdiene som argumenter —
      # fang dem her, så pinning bruker post-skift-verdier.
      rec <- local({
        o <- orig
        function(age, edu_code, occupation, ...) {
          v <- o(age, edu_code, occupation, ...)
          assign("inc", v, envir = .cfm_cap)
          assign("edu_code_final", edu_code, envir = .cfm_cap)
          assign("occ_final", occupation, envir = .cfm_cap)
          v
        }
      })
    } else {
      rec <- local({
        k <- key; o <- orig
        function(...) { v <- o(...); assign(k, v, envir = .cfm_cap); v }
      })
    }
    assign(fname, rec, envir = globalenv())
  }
}

# Hva som må trekkes på nytt når én dimensjon om-trekkes (= målet selv +
# alle som betinger på det, direkte eller transitivt). Alt annet pinnes.
# Spesialnøkler: "name" (navnetrekk), "mun" (kommune, pinnes via tabell-
# triks), "neet" (utfall, pinnes via sannsynlighet 0/1).
.cfm_redraw_sets <- list(
  name           = c("name"),
  age            = c("age","edu","occ","nus","par","parcap","sib","gp","neet",
                     "inc","ms","household","ori","rel","party","housing",
                     "wealth","children","deprivation","health","isolation",
                     "media","sleep","diet","alcohol","hobbies","bourdieu"),
  municipality   = c("mun","sentr","housing","wealth","party","media",
                     "hobbies","deprivation","bourdieu"),
  education      = c("edu","nus","occ","neet","inc","party","housing","wealth",
                     "deprivation","health","media","diet","alcohol","hobbies",
                     "bourdieu"),
  occupation     = c("occ","nus","inc","neet","party","housing","wealth",
                     "deprivation","bourdieu"),
  income         = c("inc","party","housing","wealth","deprivation","bourdieu"),
  marital_status = c("ms","household","children","isolation","deprivation",
                     "bourdieu"),
  household      = c("household","children","isolation","bourdieu"),
  religion       = c("rel","party","bourdieu"),
  party          = c("party","bourdieu")
)

.cfm_all_pin_keys <- function() c(names(.cfm_rec_fns), "name", "mun", "neet")

# --- Felles serialisering (brukt av både draw og reroll) ---
.cfm_serialize <- function(life, ctx_id, lang = "no") {
  # Generer biografi-narrasjon FØR vi stripper klassen (narrate_life leser
  # de nestede feltene mor/far/søsken på det fulle counterfactme-objektet).
  narrative <- tryCatch(
    as.character(narrate_life(life, style = "biography")),
    error = function(e) NA_character_)

  attr(life, "class") <- NULL
  .load_data()
  # edu_code (slå opp fra education-label)
  if (!is.null(life$education)) {
    et <- .cfm_env$education
    lcol <- if (identical(lang, "no")) "level_no" else "level"
    ix <- which(et[[lcol]] == life$education)
    if (length(ix) > 0) life$edu_code <- et$code[ix[1]]
  }
  # marital_code
  if (!is.null(life$marital_status)) {
    mt <- .cfm_env$marital
    lcol <- if (identical(lang, "no")) "label_no" else "label"
    ix <- which(mt[[lcol]] == life$marital_status)
    if (length(ix) > 0) life$marital_code <- mt$code[ix[1]]
  }
  # mun_pop — NB: slå opp i fulltabellen selv om den er midlertidig krympet
  if (!is.null(life$municipality)) {
    mn <- if (!is.null(.cfm_env$municipalities_full))
            .cfm_env$municipalities_full else .cfm_env$municipalities
    ix <- which(mn$name == life$municipality)
    if (length(ix) > 0) life$mun_pop <- mn$population[ix[1]]
  }
  life$drawn_at <- as.numeric(Sys.time()) * 1000
  life$"_ctx_id" <- ctx_id
  life$narrative <- narrative
  jsonlite::toJSON(life, auto_unbox = TRUE, null = "null", na = "null",
                   force = TRUE, dataframe = "rows")
}

# --- Trekk + fang kontekst ---
.cfm_run_draw <- function(min_age, max_age, gender, lang) {
  rm(list = ls(envir = .cfm_cap), envir = .cfm_cap)
  life <- counterfact_me(min_age = as.integer(min_age),
                         max_age = as.integer(max_age),
                         gender = gender, lang = lang)
  ctx <- as.list(.cfm_cap)

  # --- Post-mutasjons-korreksjoner ---
  # Kjeden muterer flere trekk ETTER at trekk-funksjonen returnerte
  # (bakgrunns-skift på utdanning/yrke, inntektsfaktor, NEET-override,
  # eier-konsistens på husholdning, barnetall-konsistens). Pin-verdiene må
  # være de FERDIGE verdiene, ellers "lekker" en om-trekning inn i pinnede
  # dimensjoner. Hent dem fra argument-fangsten og det ferdige livet.
  if (!is.null(ctx$occ_final)) ctx$occ <- ctx$occ_final
  if (!is.null(ctx$edu) && !is.null(ctx$edu_code_final)) {
    ctx$edu$code <- ctx$edu_code_final
    if (!is.null(life$education)) ctx$edu$label <- life$education
  }
  if (!is.null(ctx$inc)) {
    if (!is.null(life$income_nok))     ctx$inc$nok     <- life$income_nok
    if (!is.null(life$income_bracket)) ctx$inc$bracket <- life$income_bracket
  }
  if (!is.null(life$household)) ctx$household <- life$household
  if (!is.null(ctx$children) && !is.null(life$n_children))
    ctx$children$count <- life$n_children
  ctx$occ_final <- NULL; ctx$edu_code_final <- NULL

  ctx$gender       <- if (!is.null(life$gender)) life$gender else gender
  ctx$name         <- life$name
  ctx$neet         <- isTRUE(life$neet)
  ctx$municipality <- life$municipality
  ctx$min_age <- min_age; ctx$max_age <- max_age; ctx$lang <- lang

  .cfm_ctx_counter <<- .cfm_ctx_counter + 1L
  id <- .cfm_ctx_counter
  assign(as.character(id), ctx, envir = .cfm_ctx_store)
  keys <- suppressWarnings(as.integer(ls(envir = .cfm_ctx_store)))
  keys <- keys[!is.na(keys)]
  if (length(keys) > 40) {
    drop <- sort(keys)[seq_len(length(keys) - 40L)]
    rm(list = as.character(drop), envir = .cfm_ctx_store)
  }
  list(life = life, ctx_id = id)
}

# --- Kontekst-oppslag ---
.cfm_get_ctx <- function(ctx_id) {
  ctx <- get0(as.character(ctx_id), envir = .cfm_ctx_store, ifnotfound = NULL)
  if (is.null(ctx)) stop("ukjent ctx_id (", ctx_id, ") \u2014 trekk et nytt liv f\u00f8rst")
  ctx
}

# --- JS-bro: trekk et liv ---
cfm_draw_json <- function(min_age = 0, max_age = 99, gender = NULL,
                          lang = "no") {
  g <- if (is.null(gender) || identical(gender, "any")) NULL else toupper(gender)
  out <- .cfm_run_draw(min_age, max_age, g, lang)
  .cfm_serialize(out$life, out$ctx_id, lang)
}

# --- Felles pin-maskineri ---
# Pinner et sett interne nøkler til verdiene de hadde i ctx: trekk-funksjonen
# byttes ut med en som returnerer den lagrede verdien og registrerer den i
# .cfm_cap, slik at den nye konteksten arver pinnede verdier.
#
# Dette ble trukket ut av cfm_reroll_json fordi det er NØYAKTIG samme operasjon
# som et låst trekk trenger. Forskjellen ligger bare i hvilke nøkler som pinnes:
#   reroll(dim)  -> pin alt UNNTATT dim og dens nedstrøms-kjegle
#   låst trekk   -> pin brukerens låser, trekk ALT annet friskt
# Før dette hadde nettleseren en egen JS-implementasjon for det andre tilfellet
# (sampler.js, speilet R-pakken v0.9.1) mens R-en satt på maskineriet hele tida.
#
# Returnerer teardown som en funksjon; kalleren MÅ registrere den med on.exit.
.cfm_apply_pins <- function(ctx, pins) {
  restore <- list()
  mun_pinned <- FALSE

  pin_fn <- function(k, value) {
    force(k); force(value)
    function(...) { assign(k, value, envir = .cfm_cap); value }
  }
  pin_one <- function(fname, fn) {
    if (!exists(fname, envir = globalenv())) return(invisible(NULL))
    if (is.null(restore[[fname]])) restore[[fname]] <<- get(fname, envir = globalenv())
    assign(fname, fn, envir = globalenv())
  }

  for (k in pins) {
    if (k == "mun") {
      if (!is.null(ctx$municipality)) {
        .load_data()
        full <- .cfm_env$municipalities
        row <- full[full$name == ctx$municipality, , drop = FALSE]
        if (nrow(row) >= 1) {
          .cfm_env$municipalities_full <- full
          .cfm_env$municipalities <- row[1, , drop = FALSE]
          mun_pinned <- TRUE
        }
      }
    } else if (k == "name") {
      if (!is.null(ctx$name))
        pin_one(".draw_name_by_background", pin_fn("name_pin", ctx$name))
    } else if (k == "neet") {
      pin_one(".cond_neet_prob",
              pin_fn("neet_pin", if (isTRUE(ctx$neet)) 1 else 0))
    } else if (!is.null(ctx[[k]])) {
      pin_one(.cfm_rec_fns[[k]], pin_fn(k, ctx[[k]]))
      # Følge-pinner: bakgrunns-modulatorer som ellers ville re-modulert
      # en allerede modulert (pinnet) verdi.
      if (k == "edu")
        pin_one(".background_edu_shift", function(code, ...) code)
      if (k == "occ") {
        pin_one(".background_occupation_shift", function(styrk, ...) styrk)
        pin_one(".background_overqualified", function(edu_code, styrk, ...) styrk)
      }
      if (k == "inc")
        pin_one(".background_income_factor", function(...) 1.0)
    }
  }

  function() {
    for (fname in names(restore)) assign(fname, restore[[fname]], envir = globalenv())
    if (mun_pinned && !is.null(.cfm_env$municipalities_full)) {
      .cfm_env$municipalities <- .cfm_env$municipalities_full
      .cfm_env$municipalities_full <- NULL
    }
  }
}

# --- JS-bro: om-trekk én dimensjon betinget på resten ---
# keep: ekstra pin-nøkler (brukerens harde låser) som IKKE skal om-trekkes
#       selv om de ligger nedstrøms av målet.
cfm_reroll_json <- function(dim, ctx_id, keep = character(0), lang = "no") {
  ctx <- .cfm_get_ctx(ctx_id)
  redraw <- .cfm_redraw_sets[[dim]]
  if (is.null(redraw)) stop("ukjent reroll-dimensjon: ", dim)
  redraw <- setdiff(redraw, keep)

  teardown <- .cfm_apply_pins(ctx, setdiff(.cfm_all_pin_keys(), redraw))
  on.exit(teardown(), add = TRUE)

  out <- .cfm_run_draw(if (is.null(ctx$min_age)) 0L else ctx$min_age,
                       if (is.null(ctx$max_age)) 99L else ctx$max_age,
                       ctx$gender, lang)
  .cfm_serialize(out$life, out$ctx_id, lang)
}

# --- JS-bro: nytt trekk med brukerens låser holdt fast ---
# pin: interne nøkler (fra R_PIN_FOR_LOCK i app.jsx) som skal beholdes fra
#      ctx_id. ALT annet trekkes friskt, betinget på de pinnede verdiene.
#
# Merk at dette ikke er counterfact_me_constrained(): den gjør rejection
# sampling på ferdige labels og ville aldri truffet en pinnet kroneinntekt.
# Pinning griper inn i selve den betingede kjeden, som er det låsing betyr.
#
# Aldersvinduet og kjønn fra UI-filtrene sendes med som før; pinnene
# overstyrer dem der de overlapper.
cfm_draw_locked_json <- function(ctx_id, pin = character(0),
                                 min_age = 0, max_age = 99,
                                 gender = NULL, lang = "no") {
  pin <- intersect(pin, .cfm_all_pin_keys())
  if (length(pin) == 0) return(cfm_draw_json(min_age, max_age, gender, lang))

  ctx <- .cfm_get_ctx(ctx_id)

  teardown <- .cfm_apply_pins(ctx, pin)
  on.exit(teardown(), add = TRUE)

  # Alder trenger ingen egen behandling: er den pinnet, er sample_age byttet
  # ut, og aldersvinduet i argumentene blir uvirksomt av seg selv.
  #
  # Kjønn er ikke en pin-nøkkel (det er et argument til counterfact_me), så
  # når navnet er låst må kjønnet følge med — ellers kan "Kari" bli mann.
  g <- if (is.null(gender) || identical(gender, "any")) NULL else toupper(gender)
  if ("name" %in% pin && !is.null(ctx$gender)) g <- ctx$gender

  out <- .cfm_run_draw(min_age, max_age, g, lang)
  .cfm_serialize(out$life, out$ctx_id, lang)
}

# Installer recorderne (én gang, etter at alle pakkefiler + patch er sourcet)
.cfm_install_recorders()

invisible(NULL)
