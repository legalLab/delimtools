#' Remove Sequences of a FASTA File
#'
#' @description
#' \code{drop_sequences()} removes sequences of a FASTA file by its names.
#'
#' @param fasta a \code{\link[ape]{DNAbin}} object.
#' @param drop a character vector specifying the sequences to remove.
#'
#' @details
#' \code{drop_sequences()} relies on exact match between sequence names within
#' a fasta file and \code{drop} argument.
#'
#' @return
#' an object of class \code{\link[ape]{"DNAbin"}}.
#'
#' @import purrr
#' @import dplyr
#' @importFrom cli cli_abort
#'
#' @author
#' Pedro S. Bittencourt
#'
#' @export
drop_sequences <- function(fasta, drop){
  if(is(fasta, "DNAbin")){

    fasta <- fasta %>% purrr::discard(names(.) %in% drop)

    return(fasta)

  } else {

    cli::cli_abort(c("Input data must have class {.cls DNAbin}.",
                     "i" = "You've supplied an input of class {.cls {class(fasta)}}."))
  }
}
