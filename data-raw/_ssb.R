# data-raw/_ssb.R
#
# Helper for reproducible fetching from SSB's PxWebApi.
# Used by other scripts in data-raw/ to (re)generate CSVs in inst/extdata/.
#
# Not shipped with the package — this is developer-only code.
#
# Dependencies (install locally): httr2, jsonlite
#
# API docs:
#   https://data.ssb.no/api/v0/doc/apiKonsument
#
# Base endpoint:
#   https://data.ssb.no/api/v0/no/table/<tableId>
#
# GET  -> metadata (variables, values, labels)
# POST -> data query (JSON-stat2 by default)

SSB_BASE <- "https://data.ssb.no/api/v0/no/table"

#' Fetch PxWebApi metadata for a table
#' @param table_id Character. SSB table id, e.g. "10467".
#' @return Parsed list with $title and $variables.
ssb_metadata <- function(table_id) {
  req <- httr2::request(file.path(SSB_BASE, table_id))
  resp <- httr2::req_perform(httr2::req_retry(req, max_tries = 3))
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

#' Build a PxWebApi query body
#'
#' @param selections Named list. Name = variable code, value = character vector
#'   of value codes. Use `"*"` to select all (expanded to full list via metadata).
#' @param contents_code Which ContentsCode to return (first is used if NULL).
#' @param meta Optional metadata object (saves a round-trip if provided).
#' @return A list ready to be serialized to JSON.
ssb_build_query <- function(selections, contents_code = NULL, meta = NULL,
                            table_id = NULL) {
  if (is.null(meta)) {
    stopifnot(!is.null(table_id))
    meta <- ssb_metadata(table_id)
  }
  var_codes <- vapply(meta$variables, `[[`, character(1), "code")

  # Expand "*" to full value list per variable
  selections <- lapply(names(selections), function(v) {
    vals <- selections[[v]]
    if (identical(vals, "*")) {
      idx <- match(v, var_codes)
      vals <- vapply(meta$variables[[idx]]$values, identity, character(1))
    }
    list(code = v,
         selection = list(filter = "item", values = as.list(vals)))
  })

  # Default ContentsCode if user didn't specify it
  if (!"ContentsCode" %in% vapply(selections, `[[`, character(1), "code")) {
    idx <- match("ContentsCode", var_codes)
    if (!is.na(idx)) {
      cc_vals <- vapply(meta$variables[[idx]]$values, identity, character(1))
      cc <- contents_code %||% cc_vals[1]
      selections <- c(selections, list(list(
        code = "ContentsCode",
        selection = list(filter = "item", values = list(cc))
      )))
    }
  }

  list(query = selections,
       response = list(format = "json-stat2"))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' POST a query to SSB and return a tidy data.frame
#'
#' Parses json-stat2 response into long format (one row per cell). Retries
#' on transient errors. Respects the 40-requests-per-minute rate limit by
#' sleeping between retries.
#'
#' @param table_id Character. SSB table id.
#' @param query List built by [ssb_build_query()].
#' @return A data.frame with one column per dimension plus `value`.
ssb_fetch <- function(table_id, query) {
  req <- httr2::request(file.path(SSB_BASE, table_id)) |>
    httr2::req_headers(`Content-Type` = "application/json",
                       Accept = "application/json") |>
    httr2::req_body_json(query, auto_unbox = TRUE) |>
    httr2::req_retry(max_tries = 4,
                     is_transient = function(resp) {
                       httr2::resp_status(resp) %in% c(408, 425, 429, 500, 502, 503, 504)
                     },
                     backoff = function(i) 2 ^ i)
  resp <- httr2::req_perform(req)
  js <- jsonlite::fromJSON(httr2::resp_body_string(resp),
                           simplifyVector = FALSE)
  .jsonstat2_to_df(js)
}

#' Convert a json-stat2 dataset to a long data.frame
#' @keywords internal
.jsonstat2_to_df <- function(js) {
  dim_ids <- js$id
  sizes   <- unlist(js$size)
  names(sizes) <- dim_ids

  # Category labels for each dimension, in index order
  cats <- lapply(dim_ids, function(d) {
    cat <- js$dimension[[d]]$category
    idx <- cat$index
    if (is.list(idx)) {
      # {"code": position}
      ord <- order(unlist(idx))
      codes <- names(idx)[ord]
    } else if (is.null(idx)) {
      codes <- names(cat$label)
    } else {
      codes <- idx
    }
    labels <- vapply(codes, function(k) cat$label[[k]] %||% k, character(1))
    list(code = codes, label = unname(labels))
  })
  names(cats) <- dim_ids

  # Cartesian expansion in json-stat2 row-major order (last dim fastest)
  grid <- do.call(expand.grid, c(
    rev(lapply(cats, `[[`, "code")),
    list(stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  ))
  grid <- grid[, rev(seq_along(cats)), drop = FALSE]
  names(grid) <- dim_ids

  values <- unlist(js$value, use.names = FALSE)
  # json-stat2 can return NA as NULL; pad if needed
  if (length(values) < nrow(grid)) {
    values <- c(values, rep(NA_real_, nrow(grid) - length(values)))
  }
  grid$value <- values

  # Attach human-readable labels alongside codes
  for (d in dim_ids) {
    lbl <- setNames(cats[[d]]$label, cats[[d]]$code)
    grid[[paste0(d, "_label")]] <- unname(lbl[grid[[d]]])
  }
  grid
}

#' Convenience: fetch "all values for all variables" from a table
#' @param table_id Character. SSB table id.
#' @param contents_code Optional ContentsCode (defaults to first listed).
ssb_fetch_all <- function(table_id, contents_code = NULL) {
  meta <- ssb_metadata(table_id)
  sel <- setNames(
    lapply(meta$variables, function(v) "*"),
    vapply(meta$variables, `[[`, character(1), "code")
  )
  q <- ssb_build_query(sel, contents_code = contents_code, meta = meta)
  ssb_fetch(table_id, q)
}
