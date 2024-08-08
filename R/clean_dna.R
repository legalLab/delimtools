#' Removes Gaps, Ambiguities and Missing Data from DNA Sequences
#'
#' @description
#' \code{clean_dna()} removes all character not a valid ACTG base from a \code{\link[ape]{DNAbin}}
#' object.
#'
#' @param dna an object of class \code{\link[ape]{DNAbin}}.
#' @param verbose logical. Returns a warning if any sequence contains non ACTG bases.
#'
#' @details
#' \code{clean_dna()} detects and removes any non ACTG bases from alignment. This includes:
#' "N", "-", "?", "R", "Y", etc. If \code{verbose = TRUE}, returns a warning if these characters
#' are inside the sequences, i.e, are not alignment padding chars at the ends.
#'
#' @return
#' an object of class \code{\link[ape]{DNAbin}}.
#'
#' @author
#' Rupert A. Collins
#'
#' @importFrom ape as.DNAbin
#' @importFrom cli cli_warn
#' @importFrom stringr str_detect str_flatten_comma
#'
#' @export
clean_dna <- function(dna, verbose= TRUE){#
  # convert to a list
  dat <- as.list(dna)
  # convert to character
  datc <- as.character(dat)
  # make a warning
  # collapse text into vector
  cdat <- lapply(datc, function(x) paste(x, collapse=""))
  # find all internal missing data
  res <- lapply(cdat, function(x) stringr::str_detect(string=x, pattern="[actg][^actg]+[actg]"))
  # get names
  errs <- names(which(res!=FALSE))
  # if else warning
  if(length(errs >= 1) & verbose == TRUE){

    cli::cli_warn(c("{cli::col_yellow({cli::symbol$warning})} You have missing data {.val ('N','-' '?')}
    or ambiguity inside your sequence, i.e. not padding the ends, and this may have unintended consequences later, as they have now been removed!",
    "i"= "The names of the samples are bellow.",
    stringr::str_flatten_comma(errs)))
  }

  # get positions of all non bases
  inds <- lapply(datc, function(x) grep("a|c|t|g", x, ignore.case=TRUE))
  # match positions and remove
  dlimp <- mapply(function(x,y) x[y], x=datc, y=inds, SIMPLIFY=FALSE)
  # convert to dnabin
  dbin <- ape::as.DNAbin(dlimp)
  return(as.list(dbin))
}#
