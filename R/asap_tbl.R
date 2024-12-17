#' A Command-Line Interface for ASAP - Assemble Species by Automatic Partitioning
#'
#' @description
#' \code{asap_tbl()} returns species partition hypothesis estimated by ASAP software
#' (\url{https://bioinfo.mnhn.fr/abi/public/asap/}).
#'
#' @param infile Path to fasta file.
#' @param exe Path to an ASAP executable.
#' @param haps Optional. A vector of haplotypes to keep into the \code{\link[tibble]{tbl_df}}.
#' @param model An integer specifying evolutionary model to be used. Available options are:
#' \itemize{
#'   \item 0: Kimura-2P
#'   \item 1: Jukes-Cantor (default)
#'   \item 2: Tamura-Nei
#'   \item 3: simple distance (p-distance)
#' }
#' @param outfolder Path to output folder. Default to NULL. If not specified, a temporary location is used.
#' @param webserver A .csv file containing ASAP results obtained from a webserver. Default to NULL.
#' @param delimname Character. String to rename the delimitation method in the table. Default to 'asap'.
#'
#' @details
#' \code{asap_tbl()} relies on \code{\link[base]{system}} to invoke ASAP software through
#' a command-line interface. Hence, you must have the software available as an executable file on 
#' your system in order to use this function properly.
#' \code{asap_tbl()} saves all output files in \code{outfolder} and imports the first partition
#' file generated to \code{Environment}.
#' Alternatively, \code{asap_tbl()} can parse a .csv file obtained from webserver such as 
#' https://bioinfo.mnhn.fr/abi/public/asap/asapweb.html.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}
#'
#' @author
#' Nicolas Puillandre, Sophie Brouillet, Guillaume Achaz.
#'
#' @examples
#' asap_df <- asap_tbl(infile = "data/pacus/pacus.trimmed.fasta", model= 3, outfolder = "pacus.asap")
#'
#' @importFrom cli cli_warn col_yellow cli_abort
#' @importFrom readr read_csv
#' @importFrom glue glue
#' @importFrom here here
#'
#' @export
asap_tbl <- function(infile, exe = NULL, haps = NULL, model = 3, outfolder = NULL, webserver = NULL, delimname = "asap") {

  if(!is.null(webserver) && !file.exists(webserver)) {

    cli::cli_abort("Error: Please provide a valid path to an ASAP results file.")

  }

  if(!is.null(webserver) && file.exists(webserver)) {

    delim <- readr::read_csv(webserver, col_names = c("labels", delimname), col_types = "c")

    return(delim)

  }

  if(!file.exists(exe)){

    cli::cli_abort("Error: Please provide a valid path to the ASAP executable file.")
  
  }

  if(missing(model)){

    model <- 1

    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Evolutionary model not specified. Using p-distance as default model.")
  }

  if(is.null(outfolder)){

    outfolder <- tempdir()

  }

  if(!dir.exists(outfolder)){

    cli::cli_abort("Error: Please provide a valid results directory.")

  }

  if(is.null(haps)){
    string_asap <- paste0(exe, " -d ", model, " -a", " -o ", outfolder, " ", infile)

    res <- system(command=string_asap, intern = TRUE)

    delim <- readr::read_csv(paste0(outfolder, "/", basename(infile),".Partition_1.csv"), col_names = c("labels", delimname), col_types = "c")
  } else if(!is.null(haps)){

    string_asap <- paste0(exe, " -d ", model, " -a", " -o ", outfolder, " ", infile)

    res <- system(command=string_asap, intern = TRUE)

    delim <- readr::read_csv(paste0(outfolder, "/", basename(infile),".Partition_1.csv"), col_names = c("labels", delimname), col_types = "c") %>%
      dplyr::filter(labels %in% haps)
  }

  # clean up rogue files

  rogue1 <- here::here(glue::glue("{basename(infile)}.res.cvs"))
  rogue2 <- here::here(glue::glue("{basename(infile)}_distmat.txt"))

  if(file.exists(rogue1)) {file.copy(rogue1,glue::glue("{tempdir()}/{basename(rogue1)}"))}
  if(file.exists(rogue2)) {file.copy(rogue2,glue::glue("{tempdir()}/{basename(rogue2)}"))}

  if(file.exists(rogue1)) {file.remove(rogue1)}
  if(file.exists(rogue2)) {file.remove(rogue2)}

  cli::cli_alert_info("ASAP files are located in directory '{outfolder}'.")

  return(delim)
}
