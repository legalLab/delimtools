boot_dna <- function(fasta, block= 3){
  
  if(!methods::is(fasta, "DNAbin")){
    
    cli::cli_abort(c("Input data must have class {.cls DNAbin}.",
                     "x" = "You've supplied an input of class {.cls {class(fasta)}}.",
                     "i" = "Try importing your input file using {.pkg ape} {.fn read.FASTA}."))
  }
  
  if(!is.matrix(fasta)) {
    
    mat <- ape::as.matrix.DNAbin(fasta)
    
  }
  
  # from ape::boot.phylo()
  if(block > 1){
    
    a <- seq(from= 1, to= ncol(mat)-1, by= block)
    
    b <- seq(from= block, to= ncol(mat), by= block)
    
    y <- mapply(":", a, b, SIMPLIFY = FALSE)
    
    iboot <- sample(y, replace = TRUE) |> unlist()
    
  } else {
    
    iboot <- sample.int(ncol(mat), replace= TRUE)
    
  }
  
  matb <- mat[, iboot]
  
  return(matb)
}