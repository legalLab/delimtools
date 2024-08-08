#' Customize Delimitation Colors
#'
#' @description
#' \code{delim_brewer()} returns a set of colors created by interpolating
#' color palettes from \code{\link[RColorBrewer]{RColorBrewer}}.
#'
#' @param delim Output from \code{\link[delimtools]{delim_join}}.
#' @param name A palette name. See \code{\link[RColorBrewer]{RColorBrewer}} for options.
#' @param n Number of different colors to interpolate. Minimum 3 and maximum depending on palette.
#'
#' @details
#' \code{delim_brewer()} interpolates over a color palette and returns a vector of random colors
#' whose length is equal to the sum of unique species delimitation partitions in \code{delim}.
#' For reproducibility, make sure to use in combination with \code{\link[base]{set.seed}}.
#' One should also try different seeds to get best color combinations for plotting.
#'
#' @author
#' Rupert A. Collins, Pedro S. Bittencourt
#'
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
delim_brewer <- function(delim, name, n){

  getPalette <- colorRampPalette(RColorBrewer::brewer.pal(n, name))

  cols <- sample(getPalette(n=length(as.character(unique(unlist(delim[,-1]))))))

  cols2 <- rbind(cols, rev(cols))[seq_along(cols)]

  return(cols2)
}
