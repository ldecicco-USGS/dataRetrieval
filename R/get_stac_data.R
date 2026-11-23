get_stac_data <- function(
  args,
  data_type,
  collection,
  site_limits,
  memory_MB_limit
) {
  split_args <- split_stac_args(
    args = args,
    data_type = data_type,
    collection = collection
  )

  search_features <- do.call(add_non_stac_search, split_args[["non_stac_args"]])

  features <- do.call(search_stac, search_features)

  if (length(features) > site_limits) {
    stop("Unique monitoring_location_id exceed site_limits")
  }

  con_1 <- initial_connection()
  on.exit(DBI::dbDisconnect(con_1))

  df_out <- open_stac(
    features = features,
    con = con_1
  )

  split_args[["stac_args"]][["df_out"]] <- df_out

  df_out <- do.call(filter_stac, split_args[["stac_args"]])

  if (length(features) > ifelse(data_type == "continuous", 10, 1000)) {
    check_memory(df_out, memory_MB_limit)
  }

  df_out_loaded <- df_out |> dplyr::collect()
  df_out_loaded$qualifier <- as.character(df_out_loaded$qualifier)

  return(df_out_loaded)
}

split_stac_args <- function(args, data_type, collection) {
  non_stac_args <- list(
    monitoring_location_id = args[["monitoring_location_id"]],
    county_name = args[["county_name"]],
    state_name = args[["state_name"]],
    site_type = args[["site_type"]],
    bbox = args[["bbox"]],
    hydrologic_unit_code = args[["hydrologic_unit_code"]],
    site_type_code = args[["site_type_code"]],
    time_series_id = args[["time_series_id"]],
    parameter_code = args[["parameter_code"]],
    statistic_id = args[["statistic_id"]],
    start_time = args[["start_time"]],
    end_time = args[["end_time"]],
    approval_status = args[["approval_status"]],
    data_type = data_type,
    collection = collection,
    site_limits = args[["site_limits"]]
  )

  stac_args <- list(
    parameter_code = args[["parameter_code"]],
    time_series_id = args[["time_series_id"]],
    start_time = args[["start_time"]],
    end_time = args[["end_time"]],
    approval_status = args[["approval_status"]],
    statistic_id = args[["statistic_id"]]
  )

  return(list(non_stac_args = non_stac_args, stac_args = stac_args))
}
