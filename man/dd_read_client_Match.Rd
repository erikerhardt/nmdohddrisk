% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dd_read_client_Match.R
\name{dd_read_client_Match}
\alias{dd_read_client_Match}
\title{Read Client Matching ID files}
\usage{
dd_read_client_Match(
  fn_list = NULL,
  path_data = "../Data_in/Client_Matching",
  path_results_dat = NULL,
  sw_plot_missing = c(TRUE, FALSE)[1],
  sw_codebook = c(TRUE, FALSE)[1]
)
}
\arguments{
\item{fn_list}{file to read or \code{NULL} for most recent by filename}

\item{path_data}{path to data}

\item{path_results_dat}{path to write .RData file}

\item{sw_plot_missing}{T/F plot missing values}

\item{sw_codebook}{T/F generage codebook (only runs if \code{sw_plot_missing} is also \code{TRUE}}
}
\value{
dat_client_Match
}
\description{
Read Client Matching ID files
}
\examples{
\dontrun{
dd_read_client_Match(
    fn_list           = NULL
  , path_data         = "../Data_in/Client_Matching"
  , path_results_dat  = path_results_dat
  , sw_plot_missing   = c(TRUE, FALSE)[1]
  , sw_codebook       = c(TRUE, FALSE)[1]
  )
}
}
