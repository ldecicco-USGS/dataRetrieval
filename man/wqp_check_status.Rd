% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readWQPdata.R
\name{wqp_check_status}
\alias{wqp_check_status}
\title{Get WQP service metadata}
\usage{
wqp_check_status(wqp_request_id)
}
\arguments{
\item{wqp_request_id}{A character returned from the header
of a WQP request.}
}
\value{
a list generated from the WQP describing what data
was returned.
}
\description{
The information from this request is only available for a
limited time after the original query from the WQP. In the
readWQPdata and readWQPqw functions, the results from this
function will be attached as an attribute to the data.
}
\examples{
\dontshow{if (is_dataRetrieval_user()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
rawPcode <- readWQPqw("USGS-01594440", "01075", 
                      ignore_attributes = TRUE, legacy = FALSE)
headerInfo <- attr(rawPcode, "headerInfo")
wqp_request_id <- headerInfo$`wqp-request-id`
count_info <- wqp_check_status(wqp_request_id)
count_info[["dataProviders"]]
}
\dontshow{\}) # examplesIf}
}
