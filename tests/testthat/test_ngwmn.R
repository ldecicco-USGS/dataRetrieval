context("General NGWMN functions")

test_that("General NGWMN retrievals working", {
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



})