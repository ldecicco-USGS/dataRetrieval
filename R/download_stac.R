#' Download STAC files
#'
#' Download STAC files based on filter parameters.
#'
#' @param features Output from search_stac
#' @param file_directory Folder to store parquet files.
#' @param output_returned Output of this function is either a vector of file
#' paths that point to each individual file ("targets"), or the subfolder of
#' partitioned parquet files ("partition_folder")
#' @param showWarnings Logical, whether to show a warning if the
#' file_directory already has subdirectory of the requested collection.
#' @param overwrite Logical, whether to overwrite local files
#' @export
#' @details
#' Downloaded files are structured as partitioned parquet files.
#' The partitioning is defined by the feature id, which in most cases is
#' monitoring_location_id. Each collection is stored in a
#' subfolder of the file_directory, and within those subfolders are partitioned
#' parquet files.
#'
#' @return vector of file paths to downloaded data
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' monitoring_location_id <- c("USGS-01435000", "USGS-05406360")
#' file_out_dir <- tempdir()
#'
#' features_2_sites <- search_stac(
#'     monitoring_location_id = monitoring_location_id,
#'     collections = c("daily", "continuous"))
#'
#' files_out <- download_stac(
#'     features = features_2_sites,
#'     file_directory = file_out_dir)
#'
#' files_out
#' log_stac_limit()
#'
#' file_out_dane <- tempdir()
#' dane_county_features <- search_stac(
#'     county_name = "Dane County",
#'     state_name = "Wisconsin",
#'     site_type = "Stream",
#'     collections = "daily",
#'     end_datetime = c("2026-10-01", NA)
#' )
#' log_stac_limit()
#'
#' files_out_county <- download_stac(
#'   features = dane_county_features,
#'   file_directory = file_out_dane)
#'
#' log_stac_limit()
#'
#' }
download_stac <- function(
  features,
  file_directory = tempdir(),
  output_returned = c("partition_folder"),
  showWarnings = TRUE,
  overwrite = TRUE
) {
  match.arg(output_returned, c("partition_folder", "targets"))
  multiple_files <- length(features) > 1
  collections <- unlist(unique(sapply(features, function(x) x["collection"])))
  multiple_assests <- length(collections) > 1

  if (showWarnings) {
    unique_folders <- file.path(file_directory, collections)
    for (i in unique_folders) {
      if (length(list.dirs(i)) > 0) {
        warning("Files already existed within: ", i)
      }
    }
  }

  paths_out <- c()

  for (f in features) {
    assets <- f[["assets"]]

    urls <- sapply(assets, function(x) x[["href"]])

    id <- f[["id"]]

    collection <- f[["collection"]]

    p1 <- file.path(
      file_directory,
      collection
    )

    dir.create(p1, showWarnings = FALSE)

    partition_path <- file.path(
      p1,
      paste0("monitoring_location_id=", id)
    )

    dir.create(partition_path, showWarnings = FALSE)

    for (i in seq_along(urls)) {
      path_to_save <- file.path(
        partition_path,
        paste0(names(urls[i]), ".parquet")
      )

      if (output_returned == "targets") {
        paths_out <- c(paths_out, path_to_save)
      }

      if (!overwrite) {
        if (file.exists(path_to_save)) {
          #move on if we don't want to overwrite and the file already exists
          next()
        }
      }

      message("Downloading ", id, ": ", collection)

      resp <- httr2::request(urls[i]) |>
        basic_request() |>
        httr2::req_perform(path = path_to_save)
    }

    if (output_returned == "partition_folder") {
      paths_out <- unique(c(paths_out, p1))
    }
  }

  return(paths_out)
}
