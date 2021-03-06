% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oceanTime_FindFile.R
\name{oceanTime_FindFile}
\alias{oceanTime_FindFile}
\title{Find the file in which an input ocean_time is bracketed}
\usage{
oceanTime_FindFile(
  ocean_time,
  dfr_files = NULL,
  path = "",
  pattern = "*.nc",
  ref = as.POSIXct("1900-01-01 00:00:00", tz = "GMT"),
  verbose = FALSE
)
}
\arguments{
\item{ocean_time}{- time (in seconds relative to the calendar reference or as POSIXct date)}

\item{dfr_files}{- dataframe with ocean times by file name (output from \code{\link{oceanTime_GetTimes}})}

\item{path}{- file path to folder with model output files}

\item{pattern}{- pattern for model output filenames (e.g. "avg_*.nc")}

\item{ref}{- calendar reference (default = "1900-01-01 00:00:00")}

\item{verbose}{- flag to print extra information}
}
\value{
One-row dataframe with columns:
\itemize{
  \item{ot,input ocean time (s)}
  \item{date,associated date}
  \item{fn,Name of model output file in which the input ocean_time is bracketed.}
  \item{ts,lefthand bracketing time slice in file}
  \item{bracket, bracketing times}
}
}
\description{
Function to find the file in which an input ocean_time is bracketed.
}
\details{
None.
}
