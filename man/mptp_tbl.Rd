% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mptp_tbl.R
\name{mptp_tbl}
\alias{mptp_tbl}
\title{A Command-Line Interface for mPTP - multi-rate Poisson Tree Processes}
\usage{
mptp_tbl(
  infile,
  exe = NULL,
  outfolder = NULL,
  method = c("multi", "single"),
  minbrlen = 1e-04,
  webserver = NULL
)
}
\arguments{
\item{infile}{Path to tree file in Newick format. Should be dichotomous and rooted.}

\item{exe}{Path to an mPTP executable.}

\item{outfolder}{Path to output folder. Default to NULL. If not specified, a temporary location is used.}

\item{method}{Which algorithm for Maximum Likelihood point-estimate to be used. Available options are:
\itemize{
\item single Single-rate PTP model. It assumes that every species evolved with the same rate.
\item multi Multi-rate mPTP model. It assumes that all species have different evolutionary rates.
}}

\item{minbrlen}{Numeric. Branch lengths smaller or equal to the value provided
are ignored from computations. Default to 0.0001. Use \code{\link[delimtools]{min_brlen}}
for fine tuning.}

\item{webserver}{A .txt file containing mPTP results obtained from a webserver. Default to NULL.}
}
\value{
an object of class \code{\link[tibble]{tbl_df}}
}
\description{
\code{mptp_tbl()} returns species partition hypothesis estimated by mPTP software
(\url{https://github.com/Pas-Kapli/mptp}).
}
\details{
\code{mptp_tbl()} relies on \code{\link[base]{system}} to invoke mPTP software through
a command-line interface. Hence, you must have the software available as an executable file on 
your system in order to use this function properly. \code{mptp_tbl()}
saves all output files in \code{outfolder} and imports the results generated to \code{Environment}.
If an \code{outfolder} is not provided by the user, then a temporary location is used.
Alternatively, \code{mptp_tbl()} can parse a file obtained from webserver such as 
https://mptp.h-its.org/.
}
\author{
Paschalia Kapli, Sarah Lutteropp, Jiajie Zhang, Kassian Kobert, Pavlos Pavlides, Alexandros Stamatakis, Tomáš Flouri.
}
