#' Report Unique Species Partitions
#'
#' @description
#' `report_delim()` reports the number of unique species partitions in `delim`.
#'
#' @param delim Output from any `*_tbl()` (e.g. [gmyc_tbl]), [delim_join] or [delim_consensus].
#' @param verbose Logical. If TRUE, returns a message and a tabulated summary of `delim`.
#'
#' @details
#' For each column in `delim`, `report_delim()` will calculate the
#' number of unique partitions and print them to `Console`, using
#' [n_distinct][dplyr::n_distinct] with `na.rm = TRUE` whether `delim` is a single-method
#' output from `*_tbl()` or a multi-method output from [delim_join] or [delim_consensus].
#' This is to prevent any columns with NA values from being interpreted as species partitions.
#'
#' @return
#' an object of class [tbl_df][tibble::tbl_df]].
#'
#' @author
#' Rupert A. Collins, Pedro S. Bittencourt
#'
#' @examples
#'
#' # report geophagus delimitations
#' report_delim(geophagus_delims)
#'
#' @export
report_delim <- function(delim, verbose = TRUE) {
  n_cols <- colnames(delim[, -1])

  if (length(n_cols) == 0) {
    cli::cli_abort(c("`delim` must have at least one partition column besides {.field labels}.",
      "x" = "No columns remain after removing the first (labels) column."
    ))
  }

  if (length(n_cols) == 1) {
    rep <- dplyr::n_distinct(purrr::pluck(delim, 2), na.rm = TRUE)

    if (verbose == TRUE) {
      cli::cli_inform(c("i" = "Delim {.arg {n_cols}} has a total of {.strong {rep}} unique species partitions:"))

      delim |>
        dplyr::group_by(dplyr::pick(2)) |>
        dplyr::tally(sort = TRUE) |>
        dplyr::rename_with(~"partition", .cols = 1) |>
        knitr::kable(align = "lr") |>
        print()
    }
  }

  if (length(n_cols) > 1) {
    rep <- delim |>
      tidyr::pivot_longer(
        cols = -labels,
        names_to = "method",
        values_to = "spp"
      )

    all.unique <- rep |> dplyr::summarise(n = dplyr::n_distinct(.data$spp, na.rm = TRUE))

    if (verbose == TRUE) {
      cli::cli_inform(c("i" = "Joined delimitations have a total of {.strong {purrr::pluck(all.unique,1)}} unique species partitions."))

      group.unique <- rep |> dplyr::summarise(partitions = dplyr::n_distinct(.data$spp, na.rm = TRUE), .by = "method")

      cli::cli_inform(c("i" = "Check below the number of species partitions per method:"))

      group.unique |>
        dplyr::arrange(dplyr::desc(.data$partitions)) |>
        knitr::kable(align = "lr") |>
        print()
    }
  }

  return(delim)
}
