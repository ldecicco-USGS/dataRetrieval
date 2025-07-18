% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readNWISunit.R
\name{readNWISstat}
\alias{readNWISstat}
\title{Site statistics retrieval from USGS (NWIS)}
\usage{
readNWISstat(
  siteNumbers,
  parameterCd,
  startDate = "",
  endDate = "",
  convertType = TRUE,
  statReportType = "daily",
  statType = "mean"
)
}
\arguments{
\item{siteNumbers}{character USGS site number (or multiple sites).  This is usually an 8 digit number.}

\item{parameterCd}{character USGS parameter code.  This is usually a 5 digit number.}

\item{startDate}{character starting date for data retrieval in the form YYYY, YYYY-MM, or YYYY-MM-DD. Dates cannot
be more specific than the statReportType, i.e. startDate for monthly statReportTypes cannot include days, and annual
statReportTypes cannot include days or months.  Months and days are optional for the daily statReportType.
Default is "" which indicates retrieval for the earliest possible record.  For daily data, this indicates the
start of the period the statistics will be computed over.}

\item{endDate}{character ending date for data retrieval in the form YYYY, YYYY-MM, or YYYY-MM-DD. Default is ""
which indicates retrieval for the latest possible record.  For daily data, this
indicates the end of the period
the statistics will be computed over.  The same restrictions as startDate apply.}

\item{convertType}{logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to
numerics based on a standard algorithm. Years, months, and days (if appliccable) are also returned as numerics
in separate columns.  If convertType is false, everything is returned as a character.}

\item{statReportType}{character time division for statistics: daily, monthly, or annual.  Default is daily.
Note that daily provides statistics for each calendar day over the specified range
of water years, i.e. no more than 366
data points will be returned for each site/parameter.  Use readNWISdata or readNWISdv for daily averages.
Also note that 'annual' returns statistics for the calendar year.  Use readNWISdata
for water years. Monthly and yearly
provide statistics for each month and year within the range indivually.}

\item{statType}{character type(s) of statistics to output for daily values.
Default is mean, which is the only
option for monthly and yearly report types. See the statistics service documentation
at \url{https://waterservices.usgs.gov/docs/statistics/} for a full list of codes.}
}
\value{
A data frame with the following columns:
\tabular{lll}{
Name \tab Type \tab Description \cr
agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
site_no \tab character \tab The USGS site number \cr
parameter_cd \tab character \tab The USGS parameter code \cr

Other columns will be present depending on statReportType and statType
}
}
\description{
Retrieves site statistics from the USGS Statistics Web Service beta.
See \url{https://waterservices.usgs.gov/docs/statistics/} for more information.
}
\examples{
\dontshow{if (is_dataRetrieval_user()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
x1 <- readNWISstat(
  siteNumbers = c("02319394"),
  parameterCd = c("00060"),
  statReportType = "annual"
)

# all the annual mean discharge data for two sites
x2 <- readNWISstat(
  siteNumbers = c("02319394", "02171500"),
  parameterCd = c("00010", "00060"),
  statReportType = "annual"
)

# Request p25, p75, and mean values for temperature and discharge for the 2000s
# Note that p25 and p75 were not available for temperature, and return NAs
x <- readNWISstat(
  siteNumbers = c("02171500"),
  parameterCd = c("00010", "00060"),
  statReportType = "daily",
  statType = c("mean", "median"),
  startDate = "2000", endDate = "2010"
)
}
\dontshow{\}) # examplesIf}
}
\seealso{
\code{\link[=constructNWISURL]{constructNWISURL()}}, \code{\link[=importRDB1]{importRDB1()}}
}
