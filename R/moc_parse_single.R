#' @title Parse a Single MOCNESS Field Sheet Record
#'
#' @description Parses one extracted JSON (or text convertible to JSON) field
#'   sheet record and returns normalized header and net information.
#'
#' @param rec A list produced by `moc_read_file()` containing elements `ok`,
#'   `data`, and `path`.
#'
#' @return A list with elements `header` and `nets` (tibbles) or NULL if parsing
#'   failed.
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#'
moc_parse_single <- function(rec) {

  if (!rec$ok) return(NULL)

  x <- rec$data

  header <- x$header %||% list()

  h_tbl <- moc_normalize_header(header) |>
    dplyr::mutate(tow_file = basename(rec$path))

  nets_tbl <- moc_normalize_nets(x) |>
    dplyr::mutate(tow_file = basename(rec$path))

  if (!nrow(nets_tbl)) {

    nets_tbl <- tibble::tibble(
      net          = NA_integer_,
      net_time_raw = NA_character_,
      depth_m      = NA_real_,
      angle_deg    = NA_real_,
      flow_counts  = NA_real_,
      volume_raw   = NA_character_,
      volume_calc  = NA_real_,
      mwo_raw      = NA_character_,
      comments     = NA_character_,
      tow_file     = basename(rec$path)
    )

  }

  return(
    list(
      header = h_tbl,
      nets   = nets_tbl
    )
  )

}


#' @title Null-Coalescing Helper
#'
#' @description Returns the first argument if it is non-null and length > 0,
#'   otherwise the second.
#'
#' @param a First value.
#' @param b Fallback value.
#'
#' @name null-coalesce
#' @aliases %||%
#'
#' @return Either `a` or `b`.
#'
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}


#' @title Strip Markdown Code Fence From JSON Text
#'
#' @description Removes any leading/trailing content outside the outermost curly
#'   braces so the text can be parsed as JSON even if surrounded by Markdown
#'   fences.
#'
#' @param txt Character string containing JSON possibly inside fences.
#'
#' @return A trimmed JSON string.
#'
moc_strip_code_fence <- function(txt) {

  # Keep only substring from first { to last } to allow jsonlite parsing even
  # when surrounded by markdown fences (```json ... ``` or similar)

  start <- regexpr("\\{", txt)[1]
  end   <- if (start > 0) {
    tail(x = gregexpr(pattern = "\\}", text = txt)[[1]], 1)
  } else {
    -1
  }

  if (start > 0 && end > start) {
    substr(txt, start, end)
  } else txt

}


#' @title Parse JSON String Safely
#'
#' @description Attempts to parse a JSON string after removing code fences;
#'   returns NULL on error.
#'
#' @param txt Character JSON string.
#'
#' @return Parsed R object (list) or NULL.
#'
#' @importFrom jsonlite fromJSON
#'
moc_from_json <- function(txt) {

  cleaned <- moc_strip_code_fence(txt)

  tryCatch(
    jsonlite::fromJSON(
      txt            = cleaned,
      simplifyVector = TRUE
    ),
    error = function(e) NULL
  )

}

#' @title Read Field Sheet File
#'
#' @description Reads a file and parses JSON content into a structured list.
#'
#' @param path Path to a tow JSON/TXT file.
#'
#' @return List with elements `path`, `raw`, `data`, and logical `ok`.
#'
#' @importFrom readr read_file
#'
moc_read_file <- function(path) {

  raw <- readr::read_file(path)
  obj <- moc_from_json(raw)

  return(
    list(
      path = path,
      raw  = raw,
      data = obj,
      ok   = !is.null(obj)
    )
  )

}

#' @title Split Time Range
#'
#' @description Parses a time range expressed either as a single string (e.g.
#'   "12:00 - 12:30") or a list with start/end components into a length-2
#'   character vector.
#'
#' @param x Character, list, or vector representing a time range.
#'
#' @return Character vector of length 2: start time and end time (may contain
#'   NA).
#'
#' @importFrom stringr str_match
#'
moc_split_range <- function(x) {

  if (is.null(x) || length(x) == 0) return(c(NA_character_, NA_character_))

  if (is.list(x)) { # list with start/end
    st <- x$start %||% x[[1]] %||% NA_character_
    en <- x$end   %||% x[[2]] %||% NA_character_
    return(c(as.character(st), as.character(en)))
  }

  s <- as.character(x[1])
  m <- stringr::str_match(s, "\\s*([0-2]?\\d:?\\d{2})\\s*(?:to|-|–)\\s*([0-2]?\\d:?\\d{2})")

  if (!is.na(m[1, 1])) return(c(m[1, 2], m[1, 3]))
  c(NA_character_, NA_character_)

}

#' @title Parse Latitude or Longitude String
#'
#' @description Converts a variety of latitude/longitude textual formats
#'   (degrees & minutes, decimal, hemisphere letters) into decimal degrees.
#'
#' @param x Character vector (first element used) representing a latitude or
#'   longitude.
#'
#' @return Numeric decimal degrees or NA.
#'
#' @importFrom stringr str_replace_all str_detect str_extract_all
#'
moc_parse_latlon <- function(x) {

  if (is.null(x) || (length(x) == 0)) return(NA_real_)
  if (all(is.na(x))) return(NA_real_)
  x <- x[1]

  if (is.na(x)) return(NA_real_)
  if (!isTRUE(nzchar(x))) return(NA_real_)
  x <- trimws(x)

  # Replace unicode degree, quotes
  x <- stringr::str_replace_all(x, "[°º]", " ")
  x <- stringr::str_replace_all(x, "[′'’]", " ")
  x <- stringr::str_replace_all(x, "[\"″]", " ")

  hemi <- 1
  if (stringr::str_detect(x, "[Ss]")) hemi <- -1
  if (stringr::str_detect(x, "[Ww]")) hemi <- -1

  # remove hemisphere letters
  x <- stringr::str_replace_all(x, "[NnSsEeWw]", "")
  nums <- stringr::str_extract_all(x, "-?[0-9]+\\.?[0-9]*")[[1]]

  if (length(nums) == 0) return(NA_real_)
  if (length(nums) == 1) {
    return(as.numeric(nums[1]) * hemi)
  }

  deg <- as.numeric(nums[1])
  min <- as.numeric(nums[2])
  dec <- deg + min/60
  dec * hemi

}

#' @title Parse Date Field
#'
#' @description Normalizes date strings (allowing separators '-' or '/') to Date
#'   objects.
#'
#' @param x Character date string.
#'
#' @return Date or NA.
#'
#' @importFrom stringr str_replace_all
#' @importFrom lubridate ymd
#'
moc_parse_date <- function(x) {

  if (is.null(x) || is.na(x) || !nzchar(x)) return(NA)
  x2 <- stringr::str_replace_all(x, "[/]", "-")
  suppressWarnings(lubridate::ymd(x2))

}

#' @title First Non-Empty List Element
#'
#' @description Returns the first element among provided keys in a list that is
#'   non-null and has length > 0.
#'
#' @param lst A named list.
#' @param keys Character vector of candidate names in priority order.
#'
#' @return The first matching element or NULL.
#'
#' @importFrom purrr detect
#'
moc_first <- function(lst, keys){

  purrr::detect(
    .x = keys,
    .f = ~ !is.null(lst[[.x]]) && length(lst[[.x]]) > 0
  ) |>
    (\(k) if (!is.null(k)) lst[[k]] else NULL)()

}


#' @title First Non-Empty Scalar List Element
#'
#' @description Like `moc_first` but coerces the first element found to a
#'   single-length character vector; returns NA_character_ if none found.
#'
#' @param lst A named list.
#' @param keys Character vector of candidate names in priority order.
#'
#' @return Character scalar or NA_character_.
#'
#' @importFrom purrr map detect
#'
moc_first_scalar <- function(lst, keys) {

  purrr::map(
    .x = keys,
    .f = ~ lst[[.x]]
  ) |>
    purrr::detect(~ !is.null(.x) && length(.x) > 0 && !all(is.na(.x))) |>
    (\(x) if (is.null(x)) NA_character_ else as.character(x[1]))()

}


#' @title Normalize Header Section
#'
#' @description Transforms the header portion of a parsed record into a
#'   standardized tibble with consistent column names and derived decimal degree
#'   coordinates.
#'
#' @param header List containing header fields from source JSON; may be NULL.
#'
#' @return A tibble with one row describing the tow header.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#'
moc_normalize_header <- function(header) {

  if (is.null(header)) {
    header <- list()
  }

  local_range <- moc_split_range(
    moc_first(
      header,
      c(
        "local_time",
        "local_time_range",
        "local"
      )
    )
  )

  if (all(is.na(local_range))) {
    ls <- moc_first(header, c("local_time_start"))
    le <- moc_first(header, c("local_time_end"))
    local_range <- c(
      ls %||% NA_character_,
      le %||% NA_character_
    )
  }

  gmt_range <- moc_split_range(
    moc_first(
      header,
      c(
        "gmt_time",
        "gmt"
      )
    )
  )

  if (all(is.na(gmt_range))) {
    gs <- moc_first(header, c("gmt_time_start"))
    ge <- moc_first(header, c("gmt_time_end"))
    gmt_range <- c(
      gs %||% NA_character_,
      ge %||% NA_character_
    )
  }

  tibble::tibble(
    cruise    = as.character(moc_first_scalar(header, c("cruise", "cruise_id"))),
    location  = as.character(moc_first_scalar(header, c("location"))),
    tow_label = as.character(moc_first_scalar(header, c("tow", "tow_number"))),
    date_raw  = as.character(moc_first_scalar(header, c("date"))),
    date      = moc_parse_date(
      as.character(
        moc_first_scalar(
          header,
          c("date")
        )
      )
    ),
    wind_speed       = as.character(moc_first_scalar(header, c("wind_speed"))),
    wind_direction   = as.character(moc_first_scalar(header, c("direction"))),
    sea_state        = as.character(moc_first_scalar(header, c("sea_state"))),
    local_start      = local_range[1],
    local_end        = local_range[2],
    gmt_start        = gmt_range[1],
    gmt_end          = gmt_range[2],
    start_lat_raw    = as.character(moc_first_scalar(header, c("start_lat"))),
    start_long_raw   = as.character(moc_first_scalar(header, c("start_long"))),
    end_lat_raw      = as.character(moc_first_scalar(header, c("end_lat"))),
    end_long_raw     = as.character(moc_first_scalar(header, c("end_long"))),
    net_size_raw     = as.character(moc_first_scalar(header, c("net_size"))),
    net_mesh_raw     = as.character(moc_first_scalar(header, c("net_mesh"))),
    net_condition    = as.character(moc_first_scalar(header, c("net_condition"))),
    general_comments = as.character(
      moc_first_scalar(
        header,
        c("general_comments", "comments")
      )
    )
  ) |>
    dplyr::mutate(
      start_lat_dd  = moc_parse_latlon(start_lat_raw),
      start_long_dd = moc_parse_latlon(start_long_raw),
      end_lat_dd    = moc_parse_latlon(end_lat_raw),
      end_long_dd   = moc_parse_latlon(end_long_raw)
    )

}


#' @title Normalize Nets Section
#'
#' @description Converts the nets portion of the parsed record into a tidy
#'   tibble, harmonizing varied field names and extracting numeric values.
#'
#' @param x Parsed record list (full record) containing one of several possible
#'   nets arrays.
#'
#' @return Tibble with one row per net (possibly empty if no nets present).
#'
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select arrange
#' @importFrom stringr str_extract str_extract_all
#' @importFrom purrr map_dbl
#'
moc_normalize_nets <- function(x) {

  if (is.null(x)) return(tibble())

  # nets arrays have many possible names
  nets <- x$nets %||% x$net_tows %||% x$net_tow_information %||% x$net_tow_info

  if (is.null(nets)) return(tibble())

  nets_df        <- suppressWarnings(tibble::as_tibble(nets))
  names(nets_df) <- tolower(names(nets_df))

  # unify time column
  if ("time_open" %in% names(nets_df) && !"time" %in% names(nets_df)) {
    nets_df$time <- nets_df$time_open
  }

  # unify volume
  if ("volume_filtered" %in% names(nets_df) && !"volume" %in% names(nets_df)) {
    nets_df$volume <- nets_df$volume_filtered
  }

  # unify flow
  if ("flow_counts" %in% names(nets_df) && !"flow" %in% names(nets_df)) {
    nets_df$flow <- nets_df$flow_counts
  }

  # unify mwo
  if ("mwo_net" %in% names(nets_df) && !"mwo" %in% names(nets_df)) {
    nets_df$mwo <- nets_df$mwo_net
  }

  if ("mwo_counts" %in% names(nets_df) && !"mwo" %in% names(nets_df)) {
    nets_df$mwo <- nets_df$mwo_counts
  }

  nets_df |>
    dplyr::mutate(
      net = as.integer(
        stringr::str_extract(
          as.character(net_number %||% net %||% row_number()),
          "\\d+"
        )
      ),
      net_time_raw = as.character(time %||% NA_character_),
      depth_raw = as.character(depth %||% NA_character_),
      depth_m = suppressWarnings(
        as.numeric(
          stringr::str_extract(
            depth_raw,
            "-?[0-9]+\\.?[0-9]*"
          )
        )
      ),
      angle_deg = suppressWarnings(
        as.numeric(
          stringr::str_extract(
            as.character(angle %||% NA_character_),
            "-?[0-9]+\\.?[0-9]*"
          )
        )
      ),
      flow_raw = as.character(flow %||% NA_character_),
      flow_counts = suppressWarnings(
        as.numeric(
          stringr::str_extract(
            flow_raw,
            "-?[0-9]+\\.?[0-9]*"
          )
        )
      ),
      volume_raw = as.character(volume %||% NA_character_),
      volume_calc = purrr::map_dbl(
        .x = volume_raw,
        .f = ~ {
          if (is.na(.) || !nzchar(.)) {
            return(NA_real_)
          }
          nums <- stringr::str_extract_all(., "[0-9]+\\.?[0-9]*")[[1]]
          if (!length(nums)) {
            return(NA_real_)
          }
          sum(as.numeric(nums))
        }
      ),
      mwo_raw = as.character(
        if ("mwo" %in% names(.data)) {
          mwo
        } else if ("mwo_raw" %in% names(.data)) {
          mwo_raw
        } else {
          NA_character_
        }
      ),
      comments = as.character(comments %||% NA_character_)
    ) |>
    dplyr::select(
      net,
      net_time_raw,
      depth_m,
      angle_deg,
      flow_counts,
      volume_raw,
      volume_calc,
      mwo_raw,
      comments
    ) |>
    dplyr::arrange(net)

}