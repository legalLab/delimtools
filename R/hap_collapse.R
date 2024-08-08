#' Removes Duplicated Sequences from Alignment
#'
#' @description
#' \code{hap_collapse()} collapses haplotypes from a \code{\link[ape]{DNAbin}} object,
#' keeping unique haplotypes only.
#'
#' @param data an object of class \code{\link[ape]{DNAbin}}.
#' @param collapseSubstrings logical. Whether to collapse or not collapse shorter but identical sequences.
#' @param clean logical. Whether to remove or not remove non ACTG bases from alignment.
#'
#' @details
#' \code{hap_collapse()} collapses a \code{\link[ape]{DNAbin}} object, keeping unique
#' haplotypes only. If \code{collapseSubstrings = TRUE}, the function will consider
#' shorter but identical sequences as the same haplotype and collapse them, returning the
#' longest sequence. If \code{collapseSubstrings = FALSE}, the function will consider
#' shorter but identical sequences as different haplotypes and will keep them.
#' If \code{clean = TRUE}, the function will call \code{\link[delimtools]{clean_dna}}
#' to remove any non ACTG bases from alignment prior to collapsing haplotypes. If
#' \code{clean = FALSE}, the function will treat data as it is, and will not remove any bases.
#'
#' @return
#' an object of class \code{\link[ape]{DNAbin}}.
#'
#' @author
#' Rupert A. Collins
#'
#' @importFrom stringr str_detect
#'
#' @export
hap_collapse <- function(data, collapseSubstrings = TRUE, clean = TRUE){
  if(clean==TRUE){
    data <- clean_dna(data)
  }
  else{
    data <- as.list(data)
  }
  if(collapseSubstrings==TRUE){
    # sort by length
    data.ord <- data[order(mapply(length, data, SIMPLIFY=TRUE, USE.NAMES=FALSE), decreasing=TRUE)]
    # make a copy to index later
    data.ord.copy <- data.ord
    # collapse into a strings
    data.ord <- mapply(FUN=function(x) paste(x,collapse=""), as.character(data.ord), SIMPLIFY=TRUE, USE.NAMES=FALSE)
    # get the indices of the first match to each seq (the longest)
    ind <- unique(mapply(FUN=function(x) which(stringr::str_detect(string=data.ord, pattern=x) == TRUE)[1], data.ord, SIMPLIFY=TRUE, USE.NAMES=FALSE))

    return(data.ord.copy[ind])
  }
  else{
    data.copy <- data
    data.char <- mapply(FUN=function(x) paste(x,collapse=""), as.character(data), SIMPLIFY=TRUE, USE.NAMES=FALSE)
    dups <- duplicated(data)
    data.keep <- data.copy[!dups]

    return(data.keep)
  }
}
