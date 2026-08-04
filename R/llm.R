# ============================================================
# Optional local-LLM narration via Ollama.
# No API key, no cloud — talks to a local Ollama server (default
# http://localhost:11434). Requires the 'jsonlite' package and the
# 'curl' command-line tool (ships with macOS). Everything stays on
# the user's machine.
#
# Two pieces:
#   life_factsheet(x)      — deterministic, faithful fact sheet (the
#                            structured input the model rewrites). Also
#                            the artefact the future hosted backend uses.
#   narrate_life_llm(x)    — sends the fact sheet to Ollama and returns prose.
#   ollama_available(host) — TRUE if a server answers at host.
# ============================================================

#' Build a faithful fact sheet from a counterfactual life
#'
#' Produces a deterministic, plain-text summary of the life — one
#' "Felt: verdi" line per populated dimension. This is the structured,
#' no-hallucination input that \code{\link{narrate_life_llm}} (or a hosted
#' backend) rewrites into prose. Unlike \code{narrate_life}, it never
#' invents connective tissue; it only reports.
#'
#' @param x A \code{counterfactme} object.
#' @return A length-1 character string (newline-separated).
#' @export
life_factsheet <- function(x) {
  L <- list()
  add <- function(k, v) {
    if (is.null(v) || (length(v) == 1 && (is.na(v) || !nzchar(as.character(v))))) return(invisible())
    L[[length(L) + 1L]] <<- sprintf("%s: %s", k, as.character(v))
  }

  add("Navn", x$name)
  add("Alder", x$age)
  add("Født", .birthyear(x))
  add("Kjønn", if (identical(x$gender, "M")) "mann" else if (identical(x$gender, "F")) "kvinne" else NA)
  add("Bosted", x$municipality %||% x$county)
  add("Fylke", x$county)
  if (!is.null(x$background) && !is.na(x$background) && x$background != "majority") {
    add("Bakgrunn", switch(x$background,
        first_gen = "innvandrer (første generasjon)",
        second_gen = "norskfødt med innvandrerforeldre", x$background))
    add("Opprinnelsesland", x$country_background %||% x$country_label)
    add("År i Norge", x$years_in_norway)
  }

  if (!is.null(x$mother)) {
    add("Mor", paste(c(x$mother$name, x$mother$occupation), collapse = ", "))
  }
  if (!is.null(x$father)) {
    add("Far", paste(c(x$father$name, x$father$occupation), collapse = ", "))
  }
  ns <- .nsiblings(x)
  if (!is.na(ns)) add("Søsken", if (ns == 0) "ingen (enebarn)" else as.character(ns))
  if (!is.null(x$parents_relationship) && !is.na(x$parents_relationship))
    add("Foreldrenes samliv", x$parents_relationship)
  mob <- .mobility(x)
  if (!is.null(mob)) {
    add("Utdanningsmobilitet", switch(mob$dir,
        sterk_opp = "kraftig oppadgående vs. foreldrene",
        opp = "oppadgående vs. foreldrene",
        lik = "samme nivå som foreldrene",
        ned = "nedadgående vs. foreldrene",
        sterk_ned = "kraftig nedadgående vs. foreldrene"))
  }

  add("Utdanning", x$education)
  add("Fagfelt", x$field_of_study)
  add("Yrke", x$occupation)
  if (!isTRUE(x$neet) && !is.null(x$income_nok) && !is.na(x$income_nok) && x$income_nok >= 50000)
    add("Årsinntekt (kr)", formatC(round(x$income_nok / 10000) * 10000, format = "d", big.mark = " "))
  if (isTRUE(x$neet)) add("Arbeid", "ikke i jobb eller utdanning akkurat nå")
  if (!is.null(x$ukepenger) && !is.na(x$ukepenger)) add("Ukepenger", x$ukepenger)

  if (!is.null(x$partner)) {
    add("Partner", paste(c(x$partner$name, x$partner$occupation), collapse = ", "))
    add("Forhold til partner", x$partner$relation)
  }
  add("Sivilstatus", x$marital_status)
  if (!is.null(x$n_children) && !is.na(x$n_children) && x$n_children > 0) add("Barn", x$n_children)
  add("Husholdning", x$household)

  if (!is.null(x$housing_tenure) && !is.na(x$housing_tenure)) {
    ht <- if (grepl("leier|rent", tolower(x$housing_tenure))) "leier bolig (er ikke eier)"
          else if (grepl("eier|own", tolower(x$housing_tenure))) "eier egen bolig"
          else x$housing_tenure
    add("Boligforhold", ht)
  }
  add("Boligtype", x$housing_type)
  if (!is.null(x$housing_value_nok) && !is.na(x$housing_value_nok) && x$housing_value_nok > 0)
    add("Boligverdi (mill)", sprintf("%.1f", x$housing_value_nok / 1e6))
  if (isTRUE(x$has_hytte)) add("Hytte", "ja")
  if (!is.null(x$net_wealth_nok) && !is.na(x$net_wealth_nok))
    add("Nettoformue", .qualitative_wealth(x$net_wealth_nok))

  add("Klassebakgrunn", x$bourdieu_klasse)
  add("Religion", x$religion)
  add("Parti", x$party)
  if (!is.null(x$hobbies) && length(x$hobbies) > 0) {
    hl <- vapply(x$hobbies, function(h) if (is.list(h)) as.character(h$label %||% "") else as.character(h), character(1))
    hl <- hl[nzchar(hl)]
    if (length(hl)) add("Hobbyer", paste(hl, collapse = ", "))
  }
  add("Avis", x$media_paper)
  add("Kosthold", x$diet)
  add("Selvopplevd helse", x$self_rated_health)
  if (isTRUE(x$has_chronic)) add("Kronisk sykdom", x$chronic_type)
  if (isTRUE(x$has_disability)) add("Funksjonsnedsettelse", x$disability)
  add("Nære venner", x$close_friends)
  if (!is.null(x$has_confidant) && !is.na(x$has_confidant))
    add("Fortrolig venn", if (isTRUE(x$has_confidant)) "ja" else "nei")
  if (!is.null(x$loneliness) && !is.na(x$loneliness)) add("Ensomhet", x$loneliness)

  paste(unlist(L), collapse = "\n")
}

#' Check whether a local Ollama server is reachable
#'
#' @param host Base URL of the Ollama server. Default
#'   \code{"http://localhost:11434"}.
#' @return \code{TRUE} if the server responds, otherwise \code{FALSE}.
#' @export
ollama_available <- function(host = "http://localhost:11434") {
  if (nzchar(Sys.which("curl")) == FALSE) return(FALSE)
  out <- tryCatch(
    suppressWarnings(system2("curl", c("-sS", "-m", "3", shQuote(paste0(host, "/api/tags"))),
                             stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0))
  any(grepl("models", out))
}

#' Narrate a counterfactual life with a local LLM (Ollama)
#'
#' Sends \code{\link{life_factsheet}(x)} to a local Ollama server and
#' returns the model's prose. Nothing leaves the machine; no API key.
#' Use this to A/B test LLM narration against the template
#' \code{\link{narrate_life}}.
#'
#' @param x A \code{counterfactme} object.
#' @param model Ollama model tag, e.g. \code{"llama3.1"}, \code{"mistral"},
#'   \code{"gemma2"}. Run \code{ollama list} in a terminal to see installed models.
#' @param host Base URL. Default \code{"http://localhost:11434"}.
#' @param style One of \code{"biografi"} (default), \code{"kaaseri"} (looser,
#'   wry), or \code{"noktern"} (factual).
#' @param temperature Sampling temperature. Default 0.7.
#' @param system Optional system prompt override (advanced).
#' @param seed Optional integer for reproducible generation.
#' @param num_predict Maximum number of tokens to generate. Default 800.
#' @param think Passed to Ollama's \code{think} option for reasoning
#'   models. \code{NULL} (default) leaves it unset.
#' @return A \code{counterfactme_narrative} object.
#' @export
narrate_life_llm <- function(x, model = "llama3.1",
                             host = "http://localhost:11434",
                             style = c("biografi", "kaaseri", "noktern"),
                             temperature = 0.5, system = NULL, seed = NULL,
                             num_predict = 800, think = NULL) {
  style <- match.arg(style)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("Pakken 'jsonlite' trengs for narrate_life_llm(). Installer: install.packages('jsonlite')")
  if (nzchar(Sys.which("curl")) == FALSE)
    stop("Fant ikke 'curl' på systemet (trengs for å snakke med Ollama).")

  sheet <- life_factsheet(x)

  sys_default <- paste(
    "Du er en presis og varm biograf som skriver på norsk.",
    "Du får et faktaark om en fiktiv, kontrafaktisk norsk person.",
    "Skriv en kort, levende biografi (4-6 korte avsnitt).",
    "Strenge regler:",
    "1) Bruk BARE fakta fra arket. Ikke dikt opp navn, tall, steder eller hendelser som ikke står der.",
    "2) Ikke ramse opp felt. Bind sammen til naturlig prosa med årsak og tid.",
    "3) Varier setningsåpninger; unngå at hvert avsnitt starter med navnet eller han/hun.",
    "4) Unngå klisjeer og overdrivelser. Hvis et felt mangler, la det være.",
    "5) Ikke tolk eller overdriv. Ved tvil, gjengi nøkternt. En ung person uten jobb har ikke 'tilbrakt livet utenfor arbeidslivet'.",
    "6) Bruk korrekt norsk med æ, ø og å.")
  sys_style <- switch(style,
    biografi = "Tone: nøktern, observerende, lett varm.",
    kaaseri  = "Tone: løsere og lett ironisk, som et kåseri — men fortsatt tro mot fakta.",
    noktern  = "Tone: knapp og saklig, nesten som en oppslagsbok.")
  sys_msg <- system %||% paste(sys_default, sys_style)

  prompt <- paste0("Faktaark:\n", sheet, "\n\nSkriv biografien nå.")

  body <- list(model = model, prompt = prompt, system = sys_msg, stream = FALSE,
               options = list(temperature = temperature,
                              num_predict = as.integer(num_predict)))
  if (!is.null(seed)) body$options$seed <- as.integer(seed)
  # Reasoning models (qwen3, deepseek-r1, ...) emit a long hidden chain of
  # thought that is slow, hot, and can eat the whole token budget. Turn it
  # off by default for those; users can force it on with think = TRUE.
  auto_off <- grepl("qwen3|deepseek-r1|:r1|thinking|reason", model, ignore.case = TRUE)
  do_think <- if (is.null(think)) !auto_off else isTRUE(think)
  if (!do_think) body$think <- FALSE
  json <- jsonlite::toJSON(body, auto_unbox = TRUE)

  tmp <- tempfile(fileext = ".json"); on.exit(unlink(tmp))
  writeLines(json, tmp, useBytes = TRUE)

  res <- tryCatch(
    system2("curl", c("-sS", "-m", "300", "-X", "POST",
                      shQuote(paste0(host, "/api/generate")),
                      "-H", shQuote("Content-Type: application/json"),
                      "--data-binary", shQuote(paste0("@", tmp))),
            stdout = TRUE, stderr = TRUE),
    error = function(e) stop("curl-kallet feilet: ", conditionMessage(e)))
  txt <- paste(res, collapse = "\n")

  parsed <- tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$response)) {
    stop("Uventet svar fra Ollama. Kjører serveren på ", host,
         ", og er modellen '", model, "' lastet ned (ollama pull ", model, ")?\n",
         "Rådata: ", substr(txt, 1, 400))
  }

  out <- trimws(parsed$response)
  class(out) <- c("counterfactme_narrative", "character")
  out
}
