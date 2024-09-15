#' Customize Delimitation Colors
#'
#' @description
#' \code{delim_brewer()} returns a set of colors created by interpolating
#' color palettes from \code{\link[RColorBrewer]{RColorBrewer}}.
#'
#' @param delim Output from \code{\link[delimtools]{delim_join}}.
#' @param package Package which contains color palettes. Available options are "RColorBrewer" and "viridisLite".
#' @param palette A palette name. See \code{\link[RColorBrewer]{brewer.pal}} for RColorBrewer 
#' or \code{\link[viridisLite]{viridis}} for viridisLite options.
#' @param n Number of different colors to interpolate. Minimum 3 and maximum depending on palette.
#' @param seed Integer. Number to initialize random number generator. 
#'
#' @details
#' \code{delim_brewer()} interpolates over a color palette and returns a vector of random colors
#' whose length is equal to the sum of unique species delimitation partitions in \code{delim}.
#' For reproducibility, make sure to provide a \code{seed}. If not provided, \code{\link[base]{Sys.time}} 
#' will be used as seed instead. One should also try different seeds to get best color combinations for plotting.
#'
#' @author
#' Rupert A. Collins, Pedro S. Bittencourt
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom withr with_seed
#'
#' @export
delim_brewer <- function(delim, package, palette, n, seed){
  
  if(is.null(package)){
    
    package <- "RColorBrewer"
    
    cli::cli_warn(c("x"= "Argument {.arg package} not found. Using {.pkg RColorBrewer} package.",
                    "i"= "Available packages are {.pkg RColorBrewer} and {.pkg viridisLite}"))
  }
  
  if(is.null(palette)){
    
    palette <- "Set1"
    
    cli::cli_warn(c("x"= "Argument {.arg palette} not found. Using {.pkg RColorBrewer} {.strong Set1} palette.",
                    "i"= "Check {.pkg RColorBrewer} and {.pkg viridisLite} packages for more palette options."))
  }
  
  if(is.null(seed)){
    
    seed <- Sys.time()
    
    cli::cli_warn(c("x"= "Argument {.arg seed} not found. Using {.fn Sys.time} as seed.",
                    "i"= "For reproducibility, you may want to set a custom {.arg seed} instead. {.arg seed} is printed below:",
                    "{seed}"))
  }
  
  if(package == "RColorBrewer"){
    
    getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n, palette))
    
  } else if(package == "viridisLite"){
    
    getPalette <- grDevices::colorRampPalette(scales::pal_viridis(option = palette)(n))
    
  }
  
  # number of unique spps
  n.spp <- delim |>
    tidyr::pivot_longer(cols=-labels, names_to = "method", values_to = "spp") |>
    dplyr::summarise(n= dplyr::n_distinct(spp, na.rm = TRUE)) |>
    dplyr::pull(1)
  
  cols <- withr::with_seed(seed= seed, code= sample(getPalette(n= n.spp)))
  
  cols2 <- rbind(cols, rev(cols))[seq_along(cols)]
  
  return(cols2)
}
