#' add a water year column
#'
#' Add a column to the dataRetrieval data frame with the water year. WQP
#' queries will return a water year column for the start and end dates
#' of the data.
#'
#' @param rawData the daily- or unit-values datset retrieved from NWISweb. Must
#' have at least one of the following columns to add the new water year columns:
#' `dateTime`, `Date`, `ActivityStartDate`, or `ActivityEndDate`. The date column(s)
#' can be character, POSIXct, Date. They cannot be numeric.
#'
#' @return data.frame with an additional integer column with "WY" appended to the
#' date column name. For WQP, there will be 2 columns: `ActivityStartDateWY` and
#' `ActivityEndDateWY`.
#' @export
#'
#' @examples
#' \donttest{
#' nwisData <- readNWISdv("04085427", "00060", "2022-01-01", "2022-06-30")
#' nwisData <- addWaterYear(nwisData)
#'
#' wqpData <- readWQPqw("USGS-01594440", "01075", "", "")
#' wqpData <- addWaterYear(wqpData)
#' }
addWaterYear <- function(rawData) {
  allowedDateColNames <- c("dateTime", "Date", "ActivityStartDate", 
                           "ActivityEndDate", "Activity_StartDate", "Activity_EndDate")
  allowedWYColNames <- c("waterYear", "waterYear", "ActivityStartWaterYear", 
                         "ActivityEndWaterYear", "Activity_StartDateWaterYear",
                         "Activity_EndDateWaterYear")
  names(allowedWYColNames) <- allowedDateColNames
  # only allow WY to be added if there is an appropriate date column
  if (all(!allowedDateColNames %in% names(rawData))) {
    stop("specified date column does not exist in supplied data frame")
  }

  # set the name of the date column(s) to use for calculating the WY &
  # if the WY column already exists, do not add another (rm that date col
  # from the list that will be looped over)
  dateColNames <- names(rawData)[names(rawData) %in% allowedDateColNames]
  dateColNames <- dateColNames[!allowedWYColNames[dateColNames] %in% names(rawData)]

  for (dateCol in dateColNames) {
    dateColWY <- allowedWYColNames[dateCol]

    # calculate WY & add as new column
    rawData[[dateColWY]] <- calcWaterYear(rawData[[dateCol]])

    # move waterYear so that it is always comes right after dateTime
    dateCol_i <- which(names(rawData) == dateCol)
    dateColWY_i <- which(names(rawData) == dateColWY)
    everything_else <- which(!(names(rawData) %in% c(dateCol, dateColWY)))
    everything_else <- everything_else[!everything_else %in% c(1:dateCol_i, dateColWY_i)]

    rawData <- rawData[, c(1:dateCol_i, dateColWY_i, everything_else)]
  }

  return(rawData)
}

#' Extract WY from a date
#'
#' Determine the correct water year based on a calendar date.
#'
#' @param dateVec vector of dates as character ("YYYY-DD-MM"),
#' Date, or POSIXct. Numeric does not work.
#'
#' @details This function calculates a water year based on the USGS
#' definition that a water year starts on October 1 of the year before,
#' and ends on September 30. For example, water year 2015 started on
#' 2014-10-01 and ended on 2015-09-30. 
#'
#' @return numeric vector indicating the water year
#' @export
#' @examples
#' x <- seq(as.Date("2010-01-01"), as.Date("2010-12-31"), by = "month")
#' calcWaterYear(x)
#'
#' y <- c("2010-01-01", "1994-02", "1980", "2009-11-01", NA)
#' calcWaterYear(y)
calcWaterYear <- function(dateVec) {
  if (is.numeric(dateVec)) {
    message("dateVec is numeric, with insufficient information to determine water year.")
    return(rep(NA, length(dateVec)))
  }

  dateTimeVec <- tryCatch(
    {
      as.POSIXlt(dateVec)
    },
    error = function(e) {
      date_vec <- tryCatch(
        {
          as.Date(dateVec)
        },
        error = function(e) {
          return(rep(NA, length(dateVec)))
        }
      )


      if (any(is.na(date_vec))) {
        dateVec <- as.character(dateVec)
        dateVec[grep("^(\\d{4}-\\d{2}$)", dateVec)] <- paste0(dateVec[grep("^(\\d{4}-\\d{2}$)", dateVec)], "-01")
        dateVec <- as.Date(dateVec)
      }
      dateTimeVec <- as.POSIXlt(dateVec)
      return(dateTimeVec)
    }
  )

  # POSIXlt years start at 100, POSIXlt months start at 0
  calYear <- dateTimeVec$year + 1900
  calMon <- dateTimeVec$mon + 1

  # when the date is NA, it should not try to add 1
  whichPastOct <- calMon >= 10
  whichPastOct[is.na(whichPastOct)] <- FALSE

  # add one to the year if it is in October or after
  waterYear <- calYear
  waterYear[whichPastOct] <- calYear[whichPastOct] + 1

  return(waterYear)
}
