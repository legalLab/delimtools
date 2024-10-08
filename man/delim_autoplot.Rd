% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delim_autoplot.R
\name{delim_autoplot}
\alias{delim_autoplot}
\title{Plot Phylogenetic Trees With Species Delimitation Partitions}
\usage{
delim_autoplot(
  delim,
  tr,
  consensus = TRUE,
  n_match = NULL,
  delim_order = NULL,
  tbl_labs = NULL,
  col_vec = NULL,
  hexpand = 0.1,
  widths = c(0.5, 0.2)
)
}
\arguments{
\item{delim}{Output from \code{\link[delimtools]{delim_join}}.}

\item{tr}{A \code{\link[tidytree]{treedata-class}} object. Both phylogram and
ultrametric trees are supported.}

\item{consensus}{Logical. Should the majority-vote consensus to be estimated?}

\item{n_match}{An Integer. If \code{consensus = TRUE}, threshold for majority-vote
calculations. See \code{\link[delimtools]{delim_consensus}} for details.}

\item{delim_order}{A character vector of species delimitation names ordered by user.
Default to NULL.}

\item{tbl_labs}{A \code{\link[tibble]{tbl_df}} of customized labels for tree plotting. The
first column must match tip labels of the \code{tr} object, while the second column
should have customized labels.}

\item{col_vec}{A color vector for species delimitation partitions. See 
\code{\link[delimtools]{delim_brewer}} for customized color palette options.}

\item{hexpand}{Numeric. Expand xlim of tree by a ratio of x axis range. Useful if
tiplabels become truncated when plotting. Default to \code{0.1}.}

\item{widths}{A numeric vector containing the relative widths of the tree and
species delimitation bars. See \code{\link[patchwork]{wrap_plots}} for details.
Defaults to \code{c(0.5, 0.2)}.}
}
\description{
\code{delim_autoplot()} returns a phylogenetic tree plotted using
\code{\link[ggtree]{ggtree}} alongside with a customized tile plot using
\code{\link[ggplot2]{geom_tile}} and \code{\link[patchwork]{wrap_plots}}.
}
\details{
\code{delim_autoplot()} is a wrapper for tree plotting with associated data implemented
using \code{\link[ggtree]{ggtree}} and \code{\link[patchwork]{wrap_plots}}. If
\code{consensus = TRUE}, a consensus bar will be plotted next to the species delimitation plot,
summarizing partitions across samples. If no consensus is reached, an "X" will be plotted instead.
}
\author{
Pedro S. Bittencourt, Rupert A. Collins.
}
