#' Get USGS Rating Curve Data
#'
#' Reads current rating table for an active USGS streamgages. More information
#' can be found at https://api.waterdata.usgs.gov/docs/stac/.
#'
#' @param monitoring_location_id A unique identifier representing a single
#' monitoring location. Monitoring location IDs are created by combining the
#' agency code of the agency responsible for the monitoring location (e.g. USGS)
#' with the ID number of the monitoring location (e.g. 02238500), separated by
#' a hyphen (e.g. USGS-02238500).
#' @param file_type Rating file time. Could be any of "exsa", "corr", or "base".
#' If `file_type` is "base" then the columns are
#' INDEP, typically the gage height, in feet; DEP, typically the streamflow,
#' in cubic feet per second; and STOR, where "*" indicates that the pair are
#' a fixed point of the rating curve. If `file_type` is "exsa" then an
#' additional column, SHIFT, is included that indicates the current shift in
#' the rating for that value of INDEP. If `file_type` is "corr" then the
#' columns are INDEP, typically the gage height, in feet; CORR, the correction
#' for that value; and CORRINDEP, the corrected value for CORR.
#' @param file_path Path to save the rating curve rdb files. The
#' default is `tempdir()`, which will wipe out the files.
#' @param datetime Only return items that have a temporal property that
#' intersects this value. Either a date-time or an interval, open or closed.
#' See Details below.
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
#' vector structured: c(xmin,ymin,xmax,ymax).
#' Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param \dots Not used.
#' @param limit Limits the number of results that are included in each page of
#' the response (capped at the default 10,000).
#' @param download_and_parse Logical to define whether or not to download, parse,
#' and return a list of data frames with rating curve data (`TRUE`), or to return
#' just a list of available rating curve files (`FALSE`). Default is `TRUE`.
#' @export
#' @inherit read_waterdata_continuous details
#'
#' @return List of named lists, one per requested rating file. Each element
#' contains:
#' \describe{
#'   \item{ratings}{Data frame of the rating curve.}
#'   \item{metadata}{Data frame of header/value pairs parsed from the comment
#'     attribute of the ratings file.}
#' }
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' monitoring_location_id <- c("USGS-01104475", "USGS-01104460")
#' ratings_exsa <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = "exsa")
#'
#' head(ratings_exsa[["USGS-01104475.exsa.rdb"]]$ratings)
#' ratings_exsa[["USGS-01104475.exsa.rdb"]]$metadata
#'
#' ratings_corr <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = "corr")
#'
#' head(ratings_corr[["USGS-01104460.corr.rdb"]]$ratings)
#' ratings_corr[["USGS-01104460.corr.rdb"]]$metadata
#'
#' rating_2 <- read_waterdata_ratings(
#'       monitoring_location_id = monitoring_location_id,
#'       file_type = c("corr", "exsa"))
#' names(rating_2)
#'
#' bbox <- c(-95.00, 40.0, -92.0, 42)
#'
#' bbox_query <- read_waterdata_ratings(bbox = bbox,
#'                                      download_and_parse = FALSE)
#' length(bbox_query)
#' recent_query <- read_waterdata_ratings(bbox = bbox,
#'                                        datetime = c(Sys.Date()-7, NA),
#'                                        download_and_parse = FALSE)
#' length(recent_query)
#'}
read_waterdata_ratings <- function(
  monitoring_location_id = NA_character_,
  file_type = c("exsa", "base", "corr"),
  file_path = tempdir(),
  bbox = NA,
  datetime = NA_character_,
  ...,
  limit = 10000,
  download_and_parse = TRUE
) {
  match.arg(
    arg = file_type,
    choices = c("exsa", "base", "corr"),
    several.ok = TRUE
  )
  rlang::check_dots_empty()

  request <- httr2::request("https://api.waterdata.usgs.gov/stac/v0/") |>
    httr2::req_url_path_append("search")

  filter <- NA_character_

  if (!all(is.na(monitoring_location_id))) {
    if (length(monitoring_location_id) > 1) {
      monitoring_location_id <- paste0(
        monitoring_location_id,
        collapse = "', '"
      )
    }

    filter <- sprintf(
      "monitoring_location_id IN ('%s')",
      monitoring_location_id
    )
  }

  if (length(file_type) == 1) {
    filter <- sprintf("%s AND file_type = '%s'", filter, file_type)
  }

  if (!is.na(filter)) {
    if (substr(filter, 1, 3) == "AND") {
      filter <- substr(filter, 4, nchar(filter))
    }

    request <- request |>
      httr2::req_url_query(filter = filter)
  }

  if (!all(is.na(datetime))) {
    if (any(grepl("P", datetime))) {
      stop(
        "Periods are not supported in datetime argument in the rating curve service."
      )
    }
    datetime <- format_api_dates(datetime, date = FALSE)

    request <- request |>
      httr2::req_url_query(datetime = datetime)
  }

  if (all(!is.na(bbox))) {
    request <- httr2::req_url_query(
      request,
      bbox = as.numeric(bbox),
      .multi = "comma"
    )
  }

  request <- request |>
    httr2::req_url_query(limit = limit) |>
    basic_request()

  message("Requesting:\n", request$url)
  
  resp <- httr2::req_perform(request)
  log_rate_limit(resp)

  features <- httr2::resp_body_json(resp)[["features"]]

  if (download_and_parse) {
    return_list <- list()
    for (feature in features) {
      id <- feature$id
      df <- download_convert(feature, file_path, file_type)
      if (!is.null(df)) {
        return_list[[id]] <- df
      }
    }

    return(return_list)
  } else {
    return(features)
  }
}

# Download and convert a rating curve feature to a tidy list
download_convert <- function(feature, file_path, file_type) {
  links <- feature$links
  id <- feature$id
  url <- feature$assets$data$href

  req <- httr2::request(url) |>
    basic_request()

  if (any(sapply(file_type, function(x) grepl(x, url)))) {
    full_file_path <- file.path(file_path, id)
    message("Requesting: \n", url)
    resp <- httr2::req_perform(req, path = full_file_path)
    rating <- importRDB1(full_file_path)
    return(list(
      ratings = rating,
      metadata = parse_ratings_metadata(rating)
    ))
  }

  return(NULL)
}

# Parse rating comment metadata into a data frame
parse_ratings_metadata <- function(ratings_df) {
  raw_comments <- comment(ratings_df)

  if (is.null(raw_comments)) {
    return(data.frame(
      header = character(),
      value = character()
    ))
  }

  com <- data.frame(
    line = raw_comments
  )

  # remove whitespace
  com$line <- sub("^#\\s*//?", "", com$line)
  com$line <- sub("^//", "", com$line)
  com$line <- gsub("\\s+", " ", trimws(com$line))

  # concatenate warning info
  warn_rows <- grepl("^WARNING", com$line)
  warn_text <- sub("^WARNING\\s*", "", com$line[warn_rows])
  warn_text <- gsub("\\s+", " ", trimws(warn_text))
  warn_text <- warn_text[warn_text != ""]

  warning_block <- if (length(warn_text) > 0) {
    paste(warn_text, collapse = " ")
  } else {
    ""
  }

  non_warn <- com[!warn_rows, , drop = FALSE]

  # group the key-value pairs in the data
  kv_pat <- '[A-Za-z0-9_]+\\s*=\\s*"[^"]*"|[A-Za-z0-9_]+\\s*=\\s*[^\\s]+'
  kv_match <- regmatches(
    non_warn$line,
    gregexpr(kv_pat, non_warn$line, perl = TRUE)
  )
  has_kv <- sapply(kv_match, length) > 0
  kv_flat <- unlist(kv_match, use.names = FALSE)

  pairs_df <- data.frame(
    header = sub("^([A-Za-z0-9_]+).*", "\\1", kv_flat, perl = TRUE),
    value = gsub(
      '^"|"$',
      "",
      gsub(
        "\\s+",
        " ",
        trimws(
          sub("^[A-Za-z0-9_]+\\s*=\\s*", "", kv_flat, perl = TRUE)
        )
      )
    )
  )

  # handle data with colons
  colon_pat <- "^[A-Z][A-Z _-]+:"
  colon_rows <- grepl(colon_pat, non_warn$line)
  colon_lines <- non_warn$line[colon_rows]

  date_df <- data.frame(
    header = gsub(
      "\\s+",
      " ",
      trimws(
        sub("^([A-Z][A-Z _-]+):.*$", "\\1", colon_lines, perl = TRUE)
      )
    ),
    value = gsub(
      "\\s+",
      " ",
      trimws(
        sub("^[A-Z][A-Z _-]+:\\s*", "", colon_lines, perl = TRUE)
      )
    )
  )

  # parse any lines that don't fit other patterns
  other_lines <- non_warn$line[!has_kv & !colon_rows]

  other_df <- data.frame(
    header = gsub(
      "\\s+",
      " ",
      trimws(
        sub("^([A-Z][A-Z _-]+).*$", "\\1", other_lines, perl = TRUE)
      )
    ),
    value = gsub(
      "\\s+",
      " ",
      trimws(
        sub("^[A-Z][A-Z _-]+\\s*", "", other_lines, perl = TRUE)
      )
    )
  )

  # combine into one dataframe to be added to list object
  final <- rbind(
    data.frame(
      header = "WARNING",
      value = warning_block
    ),
    pairs_df,
    date_df,
    other_df
  )

  final$value <- gsub("[\\x00-\\x1F\\x7F]+", " ", final$value, perl = TRUE)
  final$value <- gsub("\\s+", " ", trimws(final$value))
  final$header <- gsub("\\s+", " ", trimws(final$header))

  final <- final[!(final$header == "" & final$value == ""), , drop = FALSE]
  final <- unique(final)
  rownames(final) <- NULL

  final
}
