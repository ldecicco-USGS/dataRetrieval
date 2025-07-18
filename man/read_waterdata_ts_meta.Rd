% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_waterdata_ts_meta.R
\name{read_waterdata_ts_meta}
\alias{read_waterdata_ts_meta}
\title{Get USGS Time Series Metadata}
\usage{
read_waterdata_ts_meta(
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
  parameter_name = NA_character_,
  properties = NA_character_,
  statistic_id = NA_character_,
  last_modified = NA_character_,
  begin = NA_character_,
  end = NA_character_,
  unit_of_measure = NA_character_,
  computation_period_identifier = NA_character_,
  computation_identifier = NA_character_,
  thresholds = NA,
  sublocation_identifier = NA_character_,
  primary = NA_character_,
  time_series_id = NA_character_,
  web_description = NA_character_,
  skipGeometry = NA,
  limit = NA,
  max_results = NA,
  bbox = NA,
  convertType = FALSE
)
}
\arguments{
\item{monitoring_location_id}{A unique identifier representing a single monitoring location. This corresponds to the \code{id} field in the \code{monitoring-locations} endpoint. Monitoring location IDs are created by combining the agency code of the agency responsible for the monitoring location (e.g. USGS) with the ID number of the monitoring location (e.g. 02238500), separated by a hyphen (e.g. USGS-02238500).}

\item{parameter_code}{Parameter codes are 5-digit codes used to identify the constituent measured and the units of measure. A complete list of parameter codes and associated groupings can be found at \url{https://help.waterdata.usgs.gov/codes-and-parameters/parameters}.}

\item{parameter_name}{A human-understandable name corresponding to \code{parameter_code}.}

\item{properties}{A vector of requested columns to be returned from the query.
Available options are:
geometry, id, unit_of_measure, parameter_name, parameter_code, statistic_id, last_modified, begin, end, computation_period_identifier, computation_identifier, thresholds, sublocation_identifier, primary, monitoring_location_id, web_description, parameter_description}

\item{statistic_id}{A code corresponding to the statistic an observation represents. Example codes include 00001 (max), 00002 (min), and 00003 (mean). A complete list of codes and their descriptions can be found at \url{https://help.waterdata.usgs.gov/code/stat_cd_nm_query?stat_nm_cd=\%25&fmt=html}.}

\item{last_modified}{The last time a record was refreshed in our database. This may happen due to regular operational processes and does not necessarily indicate anything about the measurement has changed.
You can query this field using date-times or intervals, adhering to RFC 3339, or using ISO 8601 duration objects. Intervals may be bounded or half-bounded (double-dots at start or end).
Examples:
\itemize{
\item A date-time: "2018-02-12T23:20:50Z"
\item A bounded interval: "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"
\item Half-bounded intervals: "2018-02-12T00:00:00Z/.." or "../2018-03-18T12:31:12Z"
\item Duration objects: "P1M" for data from the past month or "PT36H" for the last 36 hours
}

Only features that have a \code{last_modified} that intersects the value of datetime are selected.}

\item{begin}{The datetime of the earliest observation in the time series. Together with \code{end}, this field represents the period of record of a time series. Note that some time series may have large gaps in their collection record. This field is currently in the local time of the monitoring location. We intend to update this \strong{in version v0} to use UTC with a time zone.
You can query this field using date-times or intervals, adhering to RFC 3339, or using ISO 8601 duration objects. Intervals may be bounded or half-bounded (double-dots at start or end).
Examples:
\itemize{
\item A date-time: "2018-02-12T23:20:50Z"
\item A bounded interval: "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"
\item Half-bounded intervals: "2018-02-12T00:00:00Z/.." or "../2018-03-18T12:31:12Z"
\item Duration objects: "P1M" for data from the past month or "PT36H" for the last 36 hours
}

Only features that have a \code{begin} that intersects the value of datetime are selected.}

\item{end}{The datetime of the most recent observation in the time series. Data returned by this endpoint updates at most once per day, and potentially less frequently than that, and as such there may be more recent observations within a time series than the time series \code{end} value reflects. Together with \code{begin}, this field represents the period of record of a time series. It is additionally used to determine whether a time series is "active". We intend to update this \strong{in version v0} to use UTC with a time zone.
You can query this field using date-times or intervals, adhering to RFC 3339, or using ISO 8601 duration objects. Intervals may be bounded or half-bounded (double-dots at start or end).
Examples:
\itemize{
\item A date-time: "2018-02-12T23:20:50Z"
\item A bounded interval: "2018-02-12T00:00:00Z/2018-03-18T12:31:12Z"
\item Half-bounded intervals: "2018-02-12T00:00:00Z/.." or "../2018-03-18T12:31:12Z"
\item Duration objects: "P1M" for data from the past month or "PT36H" for the last 36 hours
}

Only features that have a \code{end} that intersects the value of datetime are selected.}

\item{unit_of_measure}{A human-readable description of the units of measurement associated with an observation.}

\item{computation_period_identifier}{Indicates the period of data used for any statistical computations.}

\item{computation_identifier}{Indicates whether the data from this time series represent a specific statistical computation.}

\item{thresholds}{Thresholds represent known numeric limits for a time series, for example the historic maximum value for a parameter or a level below which a sensor is non-operative. These thresholds are sometimes used to automatically determine if an observation is erroneous due to sensor error, and therefore shouldn't be included in the time series.}

\item{sublocation_identifier}{An optional human-readable identifier used to specify where measurements are recorded at a monitoring location.}

\item{primary}{A flag identifying if the time series is a "primary" time series. "Primary" time series (which have this flag) are standard observations which undergo \href{https://www.usgs.gov/survey-manual/5028-fundamental-science-practices-review-and-approval-scientific-data-release}{Bureau review and approval processes}. Non-primary time series, which will have missing values for "primary", are provisional datasets made available to meet the need for timely best science and to assist with daily operations which need real-time information. Non-primary time series data are only retained by this system for 120 days. See the \href{https://waterdata.usgs.gov/provisional-data-statement/}{USGS Provisional Data Statement} for more information.}

\item{time_series_id}{A unique identifier representing a single time series. This corresponds to the \code{id} field in the \code{time-series-metadata} endpoint.}

\item{web_description}{A description of what this time series represents, as used by WDFN and other USGS data dissemination products.}

\item{skipGeometry}{This option can be used to skip response geometries for
each feature. The returning object will be a data frame with no spatial
information.}

\item{limit}{The optional limit parameter is used to control the subset of the
selected features that should be returned in each page. The maximum allowable
limit is 10000. It may be beneficial to set this number lower if your internet
connection is spotty. The default (\code{NA}) will set the limit to the maximum
allowable limit for the service.}

\item{max_results}{The optional maximum number of rows to return. This value
must be less than the requested limit.}

\item{bbox}{Only features that have a geometry that intersects the bounding
box are selected.The bounding box is provided as four or six numbers, depending
on whether the coordinate reference system includes a vertical axis (height or
depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
vector structured: c(xmin,ymin,xmax,ymax). Another way to think of it is c(Western-most longitude,
Southern-most latitude, Eastern-most longitude, Northern-most longitude).}

\item{convertType}{logical, defaults to \code{TRUE}. If \code{TRUE}, the function
will convert the data to dates and qualifier to string vector.}
}
\description{
Daily data and continuous measurements are grouped into time series, which represent a collection of observations of a single parameter, potentially aggregated using a standard statistic, at a single monitoring location. This endpoint provides metadata about those time series, including their operational thresholds, units of measurement, and when the earliest and most recent observations in a time series occurred.
}
\examples{
\dontshow{if (is_dataRetrieval_user()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

\donttest{
site <- "USGS-02238500"
meta_1 <- read_waterdata_ts_meta(monitoring_location_id = site)

meta_multi <- read_waterdata_ts_meta(monitoring_location_id =  c("USGS-01491000", 
                                                            "USGS-01645000"),
                            parameter_code = c("00060", "00010"),
                            properties = c("monitoring_location_id",
                                           "parameter_code",
                                           "begin",
                                           "end",
                                           "time_series_id"),
                            skipGeometry = TRUE)
}
\dontshow{\}) # examplesIf}
}
