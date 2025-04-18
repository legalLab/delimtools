% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/locmin_tbl.R
\name{locmin_tbl}
\alias{locmin_tbl}
\title{Turns Local Minima Results into a Tibble}
\source{
Brown S.D.J., Collins R.A., Boyer S., Lefort M.-C., Malumbres-Olarte J., Vink C.J.,
Cruickshank, R.H. 2012. Spider: An R package for the analysis of species identity
and evolution, with particular reference to DNA barcoding.
Molecular Ecology Resources, 12: 562-565.
}
\usage{
locmin_tbl(distobj, threshold = 0.01, haps = NULL, delimname = "locmin")
}
\arguments{
\item{distobj}{A distance object (usually from \link[ape:dist.dna]{dist.dna}).}

\item{threshold}{Distance cutoff for clustering. Default of 0.01. See
\link[spider:localMinima]{localMinima} for details.}

\item{haps}{Optional. A vector of haplotypes to keep into the \link[tibble:tbl_df-class]{tbl_df}.}

\item{delimname}{Character. String to rename the delimitation method in the table. Default to 'locmin'.}
}
\value{
An object of class \link[tibble:tbl_df-class]{tbl_df}.
}
\description{
\code{locmin_tbl()} processes output from \link[spider:tclust]{tclust} into an object of
class \link[tibble:tbl_df-class]{tbl_df}.
}
\details{
\code{spider} package uses \link[spider:localMinima]{localMinima} to
determine possible thresholds for any distance matrix and \link[spider:tclust]{tclust}
to cluster samples within a given \code{threshold} into species partitions.
\code{locmin_tbl()} turns these inputs into a tibble which matches
the output from \link{gmyc_tbl} and \link{bgmyc_tbl}.
}
\examples{
# create a distance matrix
mat <- ape::dist.dna(geophagus, model = "raw", pairwise.deletion = TRUE)

# run Local Minima
locmin_res <- spider::localMinima(mat)

# create a tibble
locmin_df <- locmin_tbl(mat, 
                        threshold = locmin_res$localMinima[1], 
                        haps = ape::as.phylo(geophagus_beast)$tip.label)

# check
locmin_df 

}
\author{
Samuel Brown.
}
