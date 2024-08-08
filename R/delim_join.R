#' Join Multiple Species Delimitation Methods Outputs
#'
#' @description
#' \code{delim_join()} returns a \code{\link[tibble]{tbl_df}} of species delimitation
#' outputs whose partitions are consistent across different methods.
#'
#' @param delim A \code{\link[base]{list}} or \code{\link[base]{data.frame}} of multiple
#' species delimitation methods outputs.
#'
#' @details
#' \code{delim_join()} is a helper function to join multiple lists or columns of species
#' delimitation outputs into a single \code{\link[tibble]{tbl_df}} while keeping consistent
#' identifications across multiple methods. Species delimitation outputs are in general a
#' list or data frame of sample labels and its species partitions (Species 1, Species 2, etc.). These
#' partition names may be or not the same across two or more methods. \code{delim_join()} standardizes
#' partition names across two or more species delimitation outputs while keeping its underlying structure intact.
#'
#' @return
#' an object of class \code{\link[tibble]{tbl_df}}.
#'
#' @import purrr
#' @import dplyr
#' @import stringr
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unite
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @export
delim_join <- function(delim){

  if(is(delim, "list")){

    # Reduce to a tibble and turn into a list
    dlist <- delim %>%
      purrr::reduce(., dplyr::full_join, by= "labels") %>%
      tidyr::pivot_longer(cols = -labels,
                          names_to = "method",
                          values_to = "delims") %>%
      tidyr::unite("delims", method:delims, sep = "") %>%
      dplyr::group_by(delims) %>%
      dplyr::reframe(labels= as.vector(labels, mode= "character")) %>%
      split(.$delims, .$labels) %>%
      purrr::map(., dplyr::select, -delims) %>%
      purrr::map(., unlist, use.names=FALSE)

    # loop 1
    ff <- list()
    for(i in seq_along(dlist)){#
      ff[[i]] <- which(dlist%in%dlist[i])#
    }#
    sff <- lapply(ff, sort)
    dff <- sff[!duplicated(sff)]

    # loop 2
    new.labs <- paste0("sp",rep(1:length(dff)))
    dd <- vector(mode="character", length=length(dlist))
    for(i in 1:length(dff)){#
      dd[dff[[i]]] <- new.labs[i]#
    }#

    # names
    names(dlist) <- paste(gsub("[0-9]+", "", names(dlist)), dd, sep="-")

    # join
    delim_df <- tibble::tibble(gr=rep(names(dlist), sapply(dlist, length)),
                               labels=unlist(dlist)) %>%
      dplyr::mutate(method= stringr::str_remove(gr, "-sp[0-9]+")) %>%
      tidyr::pivot_wider(id_cols= labels, names_from = method, values_from = gr) %>%
      dplyr::mutate(across(.cols= c(-1),
                           .fns= ~stringr::str_split_fixed(., "-", n=2)[,2])) %>%
      dplyr::relocate(colnames(delim)) %>%
      dplyr::arrange(match(labels, delim$labels))

  } else if(is(delim, "data.frame")){

    # pivot data to long format and turn into a list
    dlist <- delim %>%
      tidyr::pivot_longer(cols = -labels,
                          names_to = "method",
                          values_to = "delims") %>%
      tidyr::unite("delims", method:delims, sep = "") %>%
      dplyr::group_by(delims) %>%
      dplyr::reframe(labels= as.vector(labels, mode= "character")) %>%
      split(.$delims, .$labels) %>%
      purrr::map(., dplyr::select, -delims) %>%
      purrr::map(., unlist, use.names=FALSE)

    # loop 1
    ff <- list()
    for(i in seq_along(dlist)){#
      ff[[i]] <- which(dlist%in%dlist[i])#
    }#
    sff <- lapply(ff, sort)
    dff <- sff[!duplicated(sff)]

    # loop 2
    new.labs <- paste0("sp",rep(1:length(dff)))
    dd <- vector(mode="character", length=length(dlist))
    for(i in 1:length(dff)){#
      dd[dff[[i]]] <- new.labs[i]#
    }#

    # names
    names(dlist) <- paste(gsub("[0-9]+", "", names(dlist)), dd, sep="-")

    # join
    delim_df <- tibble::tibble(gr=rep(names(dlist), sapply(dlist, length)),
                               labels=unlist(dlist)) %>%
      dplyr::mutate(method= stringr::str_remove(gr, "-sp[0-9]+")) %>%
      tidyr::pivot_wider(id_cols= labels, names_from = method, values_from = gr) %>%
      dplyr::mutate(across(.cols= c(-1),
                           .fns= ~stringr::str_split_fixed(., "-", n=2)[,2])) %>%
      dplyr::relocate(colnames(delim)) %>%
      dplyr::arrange(match(labels, delim$labels))


  }
  return(delim_df)
}
