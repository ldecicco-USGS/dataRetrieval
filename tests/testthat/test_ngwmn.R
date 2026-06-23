context("NGWMN functions")

test_that("NGWMN retrievals working", {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  
  cql <- '{
 "op": "between",
   "args": [
      { "property": "water_level_above_navd88_ft" },
      [ "100.00", "200.00" ]
   ]
}'
  
  wl_data <- read_ngwmn(service = "waterLevelObs",
                        monitoring_location_id = c("USGS-272838082142201",
                                                   "USGS-404159100494601",
                                                   "USGS-401216080362703"),
                        CQL = cql)
  
  # no CQL should work too:
  wl_data_full <- read_ngwmn(service = "waterLevelObs",
                        monitoring_location_id = c("USGS-272838082142201",
                                                   "USGS-404159100494601",
                                                   "USGS-401216080362703"))
  
  cql3 <- '{
  "op": "and",
    "args": [
     {
       "op": "between",
       "args": [
         { "property": "water_level_above_navd88_ft" },
         [ "100.00", "200.00" ]
       ]
     },
     {
       "op": "in",
       "args": [
         { "property": "monitoring_location_id" },
         [ "USGS-272838082142201", "USGS-404159100494601", "USGS-401216080362703" ]
       ]
     }
    ]
  }'
  
  
  wl_data_alt <- read_ngwmn(service = "waterLevelObs",
                            CQL = cql3)
  
  expect_equal(nrow(wl_data_alt), nrow(wl_data))
  expect_gt(nrow(wl_data_full), nrow(wl_data))

  sites <- c("ISWS-P428197",
             "AKDNR-535143966816631",
             "AKDNR-535134236016630")
  ngwml_lith_sites <- read_ngwmn_lithology(monitoring_location_id = sites)
  expect_all_true(c("ISWS", "AKDNR") %in% unique(ngwml_lith_sites$agency_code))
  
  org_type <- read_ngwmn_sites(agency_code = "MN_DNR",
                               county_name = "Washington County")
  expect_true("MN_DNR" == unique(org_type$agency_code))
  
  site <- "USGS-272838082142201"
  
  ngwml_wl_time2 <- read_ngwmn_water_level(monitoring_location_id = site,
                                           datetime = c("2022-01-01", "2024-01-01"))
  expect_true(min(as.Date(ngwml_wl_time2$sample_time)) >= as.Date("2022-01-01"))
  expect_true(max(as.Date(ngwml_wl_time2$sample_time)) <= as.Date("2024-01-01"))

  sites <- c("USGS-272838082142201", "USGS-404159100494601",
             "USGS-401216080362703", "MBMG-702934")
  ngwml_well_sites <- read_ngwmn_well_construction(monitoring_location_id = sites)
  
  expect_all_true(unique(ngwml_well_sites$monitoring_location_id) %in% sites)
})
