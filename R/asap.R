#' A Command-Line Interface for ASAP - Assemble Species by Automatic Partitioning
#'
#' @description
#' \code{asap()} returns species partition hypothesis estimated by ASAP software
#' (\url{https://bioinfo.mnhn.fr/abi/public/asap/}).
#'
#' @param infile Path to fasta file.
#' @param haps Optional. A vector of haplotypes to keep into the \code{\link[tibble]{tbl_df}}.
#' @param model An integer specifying evolutionary model to be used. Available options are:
#' \itemize{
#'   \item 0: Kimura-2P
#'   \item 1: Jukes-Cantor (default)
#'   \item 2: Tamura-Nei
#'   \item 3: simple distance (p-distance)
#' }
#' @param outfolder Path to output folder.
#'
#' @details
#' \code{asap()} relies on \code{\link[base]{system}} to invoke ASAP software through
#' a command-line interface. Hence, you must have the software installed on your
#' operating system root in order to use this function properly.
#' \code{asap()} saves all output files in \code{outfolder} and imports the first partition
#' file generated to \code{Environment}.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}
#'
#' @author
#' Nicolas Puillandre, Sophie Brouillet, Guillaume Achaz.
#'
#' @examples
#' asap_df <- asap(infile = "data/pacus/pacus.trimmed.fasta", model= 3, outfolder = "pacus.asap")
#'
#' @importFrom cli cli_warn col_yellow
#' @importFrom readr read_csv
#'
#' @export
asap <- function(infile, haps= NULL, model= 1, outfolder){

  if(missing(model)){

    model <- 1

    cli::cli_warn("{cli::col_yellow({cli::symbol$warning})} Evolutionary model not specified. Using JC69 as default model.")
  }

  if(is.null(haps)){
    string_asap <- paste0("asap", " -d ", model, " -a", " -o ", outfolder, " ", infile)

    res <- system(command=string_asap, intern = TRUE)

    delim <- readr::read_csv(paste0(outfolder, "/", basename(infile),".Partition_1.csv"), col_names = c("labels", "asap"))
  } else if(!is.null(haps)){

    string_asap <- paste0("asap", " -d ", model, " -a", " -o ", outfolder, " ", infile)

    res <- system(command=string_asap, intern = TRUE)

    delim <- readr::read_csv(paste0(outfolder, "/", basename(infile),".Partition_1.csv"), col_names = c("labels", "asap")) %>%
      dplyr::filter(labels %in% haps)
  }

  return(delim)
}
