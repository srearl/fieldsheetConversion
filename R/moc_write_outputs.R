#' @title Write Parsed MOCNESS Tables to Disk
#'
#' @description Writes the combined headers and nets tables produced by
#'   `moc_parse_all()` to CSV files in the specified output directory.
#'
#' @param res A list with elements `headers` and `nets` (tibbles) as returned by
#'   `moc_parse_all()`.
#' @param out_dir Directory path where output CSV files will be written (must
#'   exist / be writable).
#'
#' @return (Invisibly) a list with `headers` and `nets` tibbles written.
#'
#' @importFrom dplyr distinct
#' @importFrom readr write_csv
#'
moc_write_outputs <- function(res, out_dir) {

  headers_out <- res$headers |>
    dplyr::distinct()

  nets_out <- res$nets

  readr::write_csv(headers_out, file.path(out_dir, "mocness_headers.csv"))
  readr::write_csv(nets_out, file.path(out_dir, "mocness_nets.csv"))

  invisible(
    list(
      headers = headers_out,
      nets    = nets_out
    )
  )

}
