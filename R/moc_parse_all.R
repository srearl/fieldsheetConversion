#' @title Parse All Extracted MOCNESS Field Sheet Files
#'
#' @description Scans an input directory for tow JSON/TXT files produced by the
#'   companion extraction package, parses each valid file, and returns combined
#'   header and net tables.
#'
#' @param input_dir Path to directory containing files named like
#'   `tow_<number>.json` or `tow_<number>.txt`.
#'
#' @return A list with elements `headers` (tibble) and `nets` (tibble)
#'   containing parsed, combined results plus a numeric `tow_id` derived from
#'   the filename.
#'
#' @importFrom purrr map keep
#' @importFrom dplyr mutate relocate left_join bind_rows select
#' @importFrom stringr str_extract
#'
#' @seealso [moc_parse_single] for parsing one record; [moc_write_outputs] to
#'   write results.
moc_parse_all <- function(input_dir) {

  files <- list.files(
    path        = input_dir,
    pattern     = "^tow_\\d+\\.(json|txt)$",
    full.names  = TRUE,
    ignore.case = TRUE
  )

  if (!length(files)) stop("No tow_*.json/txt files in ", input_dir)

  recs <- purrr::map(
    .x = files,
    .f = moc_read_file
  )

  ok_recs <- purrr::keep(recs, ~ .x$ok)
  parsed  <- purrr::map(ok_recs, moc_parse_single)
  out     <- moc_bind_results(parsed)

  out$nets <- out$nets |>
    dplyr::mutate(
      tow_id = as.integer(
        stringr::str_extract(tow_file, "\\d+")
      )
    ) |>
    dplyr::relocate(tow_id, .before = tow_file)

  out$headers <- out$headers |>
    dplyr::mutate(
      tow_id = as.integer(
        stringr::str_extract(tow_file, "\\d+")
      )
    ) |>
    dplyr::relocate(tow_id, .before = tow_file)

  return(out)

}


#' @title Bind Parsed MOCNESS Results
#'
#' @description Helper that takes a list of parsed single-record results and
#'   row-binds headers and nets, adding selected header context to each net.
#'
#' @param parsed_list A list where each element is the return value of
#'   `moc_parse_single` (a list with components `header` and `nets`).
#'
#' @return A list with `headers` and `nets` tibbles.
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows left_join select
moc_bind_results <- function(parsed_list) {

  headers <- purrr::map(parsed_list, "header") |>
    dplyr::bind_rows()

  nets <- purrr::map(parsed_list, "nets") |>
    dplyr::bind_rows()

  nets <- nets |>
    dplyr::left_join(
      headers |>
        dplyr::select(
          tow_file,
          cruise,
          tow_label,
          date,
          date_raw
        ),
      by = "tow_file"
    )

  list(
    headers = headers,
    nets    = nets
  )

}