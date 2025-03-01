% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/constructNWISURL.R
\name{constructNWISURL}
\alias{constructNWISURL}
\title{Construct NWIS url for data retrieval}
\usage{
constructNWISURL(
  siteNumbers,
  parameterCd = "00060",
  startDate = "",
  endDate = "",
  service,
  statCd = "00003",
  format = "xml",
  expanded = TRUE,
  ratingType = "base",
  statReportType = "daily",
  statType = "mean"
)
}
\arguments{
\item{siteNumbers}{string or vector of strings USGS site number.  This is usually an 8 digit number}

\item{parameterCd}{string or vector of USGS parameter code.  This is usually an 5 digit number.}

\item{startDate}{character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
retrieval for the earliest possible record.}

\item{endDate}{character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
retrieval for the latest possible record.}

\item{service}{string USGS service to call. Possible values are "dv" (daily values),
"uv" (unit/instantaneous values),
"gwlevels" (groundwater),and "rating" (rating curve),
"peak", "meas" (discrete streamflow measurements),
"stat" (statistics web service BETA).}

\item{statCd}{string or vector USGS statistic code only used for daily value service.
This is usually 5 digits.  Daily mean (00003) is the default.}

\item{format}{string, can be "tsv" or "xml", and is only applicable for daily
and unit value requests.  "tsv" returns results faster, but there is a possibility
that an incomplete file is returned without warning. XML is slower,
but will offer a warning if the file was incomplete (for example, if there was a
momentary problem with the internet connection). It is possible to safely use the "tsv" option,
but the user must carefully check the results to see if the data returns matches
what is expected. The default is therefore "xml".}

\item{expanded}{logical defaults to \code{TRUE}. If \code{TRUE}, retrieves additional
information, only applicable for qw data.}

\item{ratingType}{can be "base", "corr", or "exsa". Only applies to rating curve data.}

\item{statReportType}{character Only used for statistics service requests.  Time
division for statistics: daily, monthly, or annual.  Default is daily.
Note that daily provides statistics for each calendar day over the specified
range of water years, i.e. no more than 366 data points will be returned for
each site/parameter.  Use \code{readNWISdata} or \code{readNWISdv} for daily averages.
Also note that "annual" returns statistics for the calendar year.  Use
\code{readNWISdata} for water years. Monthly and yearly
provide statistics for each month and year within the range individually.}

\item{statType}{character Only used for statistics service requests. Type(s)
of statistics to output for daily values.  Default is mean, which is the only
option for monthly and yearly report types. See the statistics service documentation
at \url{https://waterservices.usgs.gov/docs/statistics/} for a
full list of codes.}
}
\value{
url string
}
\description{
Using USGS water web services to construct urls.
}
\examples{
site_id <- "01594440"
startDate <- "1985-01-01"
endDate <- ""
pCode <- c("00060", "00010")
url_daily <- constructNWISURL(site_id, pCode,
  startDate, endDate, "dv",
  statCd = c("00003", "00001")
)
url_unit <- constructNWISURL(site_id, pCode, "2012-06-28", "2012-06-30", "iv")

url_daily_tsv <- constructNWISURL(site_id, pCode, startDate, endDate, "dv",
  statCd = c("00003", "00001"), format = "tsv"
)
url_rating <- constructNWISURL(site_id, service = "rating", ratingType = "base")
url_peak <- constructNWISURL(site_id, service = "peak")
url_meas <- constructNWISURL(site_id, service = "meas")
url_gwl <- constructNWISURL(site_id, service = "gwlevels",
                            startDate = "2024-05-01", endDate = "2024-05-30")
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}
