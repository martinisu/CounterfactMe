# cfm-bootstrap.R — kjøres i webR etter at R-filene og CSV-ene er montert
# på /cfm/R/ og /cfm/extdata/.
#
# Vi kan ikke kjøre `library(CounterfactMe)` fordi pakken ikke er installert
# som binærpakke i webR. I stedet sourcer vi R-filene direkte i global env.
#
# Datalasting: pakkens egen .load_data() (zzz.R) slår opp datamappa med
# system.file("extdata", package = "CounterfactMe"), som returnerer "" når
# pakken ikke er installert. Vi shimmer derfor system.file() i stedet for å
# duplisere .load_data() her. Da laster nye CSV-er i framtidige pakkeversjoner
# automatisk, uten at denne fila må vedlikeholdes i takt.

# --- 1. Shim system.file() FØR pakkefilene sources ---
.cfm_system_file_orig <- base::system.file
system.file <- function(..., package = "base", lib.loc = NULL,
                        mustWork = FALSE) {
  if (identical(package, "CounterfactMe")) {
    parts <- c(...)
    if (length(parts) == 0) return("/cfm")
    return(file.path("/cfm", paste(parts, collapse = "/")))
  }
  .cfm_system_file_orig(..., package = package, lib.loc = lib.loc,
                        mustWork = mustWork)
}

# --- 2. Source pakke-filer i avhengighetsrekkefølge ---
source("/cfm/R/zzz.R")            # .cfm_env + .load_data (bruker system.file)
source("/cfm/R/samplers.R")       # available_dimensions + uavhengige samplers
source("/cfm/R/conditional.R")    # .cond_* funksjoner (kjernen)
source("/cfm/R/impossibility.R")  # find_impossibilities + rejection-laget
source("/cfm/R/counterfact_me.R")
source("/cfm/R/constrained.R")    # counterfact_me_constrained + parallel_lives
source("/cfm/R/narrate.R")        # narrate_life() — biografi-narrasjon
source("/cfm/R/print.R")          # print-metoder + `%||%`
source("/cfm/R/sources.R")        # data_sources() — provenans (0.9.36)
source("/cfm/R/audit.R")          # audit_plausibility() (0.9.27)
source("/cfm/R/verify.R")         # verify_consistency()

# NB: R/llm.R sources bevisst IKKE. Den krever en lokal Ollama-server
# (narrate_life_llm / ollama_available) som ikke finnes i nettleseren, og
# ingenting i appen kaller den. Template-narrasjonen i narrate.R er den vi bruker.
#
# NB: patch-cond-wealth.R er også koblet ut. Den var en lokal hotfix mot en
# eldre .cond_wealth; oppstrøms 0.9.35 fikset samme klasse feil ved å holde
# formue som double. Patchen bruker as.integer() og ville reintrodusert
# heltallsoverflyt på de største formuene.

# Last data nå så første trekning er rask.
.load_data()

# --- 3. JS-bro: jsonlite for serialisering ---
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  tryCatch(webr::install("jsonlite"), error = function(e) NULL)
}

# --- 4. Trekk + betinget reroll: definerer cfm_draw_json() og
#        cfm_reroll_json(), og installerer recorder-wrappere rundt
#        .cond_*-funksjonene. MÅ sources sist. ---
source("/cfm/reroll.R")

invisible(NULL)
