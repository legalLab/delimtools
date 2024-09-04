#' Report Unique Species Partitions
#' 
#' @description
#' \code{report_delim()} reports the number of unique species partitions in \code{delim}.
#' 
#' @param delim Output from any \code{*_tbl()} (e.g. \code{\link[delimtools]{gmyc_tbl}}),
#' \code{\link[delimtools]{delim_join}} or \code{\link[delimtools]{delim_consensus}}.
#' @param summary Logical. If TRUE, returns a message and a summary of \code{delim}. If FALSE, 
#' only the message is printed on Console.
#' 
#' @details 
#' For each column in \code{delim}, \code{report_delim()} will calculate the 
#' number of unique partitions. If \code{delim} is an output from \code{*_tbl()},
#' \code{report_delim()} will get unique species partitions using \code{\link[vctrs]{vec_unique_count}}.
#' If \code{delim} is an output from \code{\link[delimtools]{delim_join}} 
#' or \code{\link[delimtools]{delim_consensus}}, values are summarized by using 
#' \code{\link[dplyr]{n_distinct}} with \code{na.rm = TRUE}. This is to prevent any columns with
#' NA values to be interpreted as species partitions.
#' 
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#' 
#' @import dplyr
#' 
#' @importFrom cli cli_inform
#' @importFrom knitr kable
#' @importFrom tidyr pivot_longer
#' @importFrom purrr pluck
#' @importFrom vctrs vec_unique_count
#' 
#' @author
#' Rupert A. Collins, Pedro S. Bittencourt
#' 
#' @examples
#' # Create a species partition
#' delim_df <- tibble::tibble(labels= stringr::str_c("seq", 1:5), 
#' delim_A= c(rep(1,3), rep(2,2)), 
#' delim_B= c(rep(1,1), rep(2,2), rep(3,2)),
#' delim_C= c(rep(1,1), rep(2,2), rep(3,2)))
#' 
#' # View
#' delim_df
#' 
#' # Report species partitions across delims 
#' delim_join(delim_df) %>%
#' report_delim()
#' 
#' @export
report_delim <- function(delim, summary= TRUE){
  
  n_cols <- colnames(delim[,-1])
  
  if(length(n_cols) == 1) {
    
    rep <- vctrs::vec_unique_count(purrr::pluck(delim, 2))
    
    
    if(summary == TRUE){
      
      cli::cli_inform("The {.arg delim} below has a total of {.strong {rep}} 
                      unique species partitions.", wrap= TRUE)
      
      return(delim)
      
    } else {
      
      cli::cli_inform("{.arg delim} has a total of {.strong {rep}} 
                      unique species partitions.", wrap= TRUE)
      
      invisible(delim)
    }
    
  } else if(length(n_cols) > 1){
    
    rep <- delim %>%
      tidyr::pivot_longer(cols=-labels,
                          names_to = "method",
                          values_to = "spp")
    
    all_unique <- rep %>% dplyr::summarise(n= dplyr::n_distinct(spp, na.rm = TRUE))
    
    cli::cli_inform(c("i" = "{.arg delim} has a total of
                      {.strong {purrr::pluck(all_unique,1)}} unique species partitions"))
    
    if(summary==TRUE) {
      
      group_unique <- rep %>% dplyr::summarise(partitions= dplyr::n_distinct(spp, na.rm = TRUE), .by = "method")
      
      cli::cli_inform(c("i" = "Check below the number of species partitions per method:"))
      
      print(knitr::kable(group_unique))
      
      invisible(delim)
      
    } 
  }
}
