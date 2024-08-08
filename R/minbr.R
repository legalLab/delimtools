# wrapper function to estimate minimum branch length
minbr <- function(tree, file){
  string_minbr <- paste0("mptp", " --tree_file ",tree, " --minbr_auto ",file, " --output_file stout")
  res <- system(command=string_minbr, intern = TRUE)
  writeLines(res)
  res <- stringr::str_extract(res[[8]], "\\d+\\.*\\d*") |> as.numeric()
  return(res)
}#
