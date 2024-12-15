#' Removes Duplicated Sequences from Alignment
#'
#' @description
#' \code{hap_collapse()} collapses haplotypes from a \code{\link[ape]{DNAbin}} object,
#' keeping unique haplotypes only.
#'
#' @param dna an object of class \code{\link[ape]{DNAbin}}.
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
#' @examples
#' 
#' # create a vector of DNA sequences
#' seqs <- c(seq1= "ATGCTAGTTCGC",
#'           seq2= "ATGCTAGTTCGC",
#'           seq3= "ATGCTAGTTCGC",
#'           seq4= "ATGCTAGTTCG-",
#'           seq5= "ATGCTAGTT---",
#'           seq6= "ATGCTAGTTCGN",
#'           seq7= "ATGCTA-TTCGC",
#'           seq8= "ATGCTA-TTCG-",
#'           seq9= "ATGCTANTTCGN",
#'           seq10="ATGCTAGTT")
#'           
#' # convert to DNAbin
#' dummy <- tolower(seqs) |>
#'  strsplit(split= "") |>
#'  ape::as.DNAbin.list()
#'  
#' # run hap_collapse with different settings
#' 
#' # 2 DNA sequences (seq1/seq7)
# hap_collapse(dummy, clean = TRUE, collapseSubstrings = TRUE) %>% names()

# 5 DNA sequences (seq1, seq4, seq5, seq7, seq8)
# hap_collapse(dummy, clean = TRUE, collapseSubstrings = FALSE) %>% names()

# 7 DNA sequences (seq1, seq4, seq5, seq6, seq7, seq8, seq9)
# hap_collapse(dummy, clean = FALSE, collapseSubstrings = TRUE) %>% names()

# 8 DNA sequences (seq1, seq4, seq5, seq6, seq7, seq8, seq9, seq10)
# hap_collapse(dummy, clean = FALSE, collapseSubstrings = FALSE) %>% names()           
#' 
#' @importFrom stringr str_detect
#' @importFrom delimtools clean_dna drop_sequences
#'
#' @export
hap_collapse <- function(dna, collapseSubstrings = TRUE, clean = TRUE){
  if(clean==TRUE){
    data <- delimtools::clean_dna(dna)
  }
  else{
    data <- as.list(dna)
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
    # get names of unique haplotypes
    names_haps <- names(data.ord.copy)[ind]
    # drop sequences from fasta
    dna_haps <- delimtools::drop_sequences(dna, names_haps, drop = FALSE)
    
  }
  else{
    # make a copy to index later
    data.copy <- data
    # collapse into a strings
    data.char <- mapply(FUN=function(x) paste(x,collapse=""), as.character(data), SIMPLIFY=TRUE, USE.NAMES=FALSE)
    # get duplicates
    dups <- duplicated(data)
    # get names of duplicated haplotypes
    names_dups <- names(data.copy)[dups]
    # drop sequences from fasta
    dna_haps <- delimtools::drop_sequences(dna, names_dups, drop = TRUE)
    
  }
  return(dna_haps)
}
