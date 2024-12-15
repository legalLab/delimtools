#' Remove Sequences of a DNAbin list object
#'
#' @description
#' \code{drop_sequences()} removes sequences of a FASTA file by its names.
#'
#' @param dna a \code{\link[ape]{DNAbin}} object.
#' @param identifier a character vector containing sequence names.
#' @param drop Logical. If \code{TRUE}, sequence names in \code{identifier} will
#' be dropped from \code{fasta}. If \code{FALSE}, sequence names absent in \code{identifier}
#' will be dropped instead.
#'
#' @details
#' \code{drop_sequences()} relies on exact match between sequence names within
#' a fasta file and \code{identifier} argument.
#'
#' @return
#' an object of class \code{\link[ape]{"DNAbin"}}.
#'
#' @import purrr
#' @import dplyr
#' @importFrom ape as.list.DNAbin
#' @importFrom cli cli_abort cli_warn
#' @importFrom methods is
#'
#' @author
#' Pedro S. Bittencourt
#' 
#' @examples
#' 
#' # Import data from ape package.
#' data(woodmouse, package = "ape")
#' 
#' # Create a vector of sequence names to drop or keep.
#' identifier <- dimnames(woodmouse)[[1]][1:3]
#' 
#' # Remove sequences listed in identifier
#' drop_sequences(woodmouse, identifier, drop = TRUE)
#' 
#' # Remove sequences not listed in identifier
#' drop_sequences(woodmouse, identifier, drop = FALSE)
#' 
#' @export
drop_sequences <- function(dna, identifier, drop = TRUE){
  if(!methods::is(dna, "DNAbin")){
    
    cli::cli_abort(c("Input data must have class {.cls DNAbin}.",
                     "x" = "You've supplied an input of class {.cls {class(dna)}}.",
                     "i" = "Try importing your input file using {.pkg ape} {.fn read.FASTA}."))
  }
  
  if(!is.list(dna)) {
    
    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} {.arg dna} is a {typeof(dna)} object. Coercing to a {.cls DNAbin} list object.")
    
    dna <- ape::as.list.DNAbin(dna)
    
  }
  
  if(drop == TRUE){
    
    dna <- dna %>% purrr::discard(names(.) %in% identifier)
    
  } else {
    
    dna <- dna %>% purrr::keep(names(.) %in% identifier)
  }
  
  return(dna)
  
}
