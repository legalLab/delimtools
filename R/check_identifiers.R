#' Checks for Differences Between Identifiers in Metadata and DNA Sequence Files
#'
#' @description
#' `check_identifiers()` checks for differences between identifiers in metadata
#' and DNA sequence files.
#'
#' @param dna a [DNAbin][ape::DNAbin] object.
#' @param identifier column in `data` which contains sequence identifiers.
#' @param data an object of class [tbl_df][tibble::tbl_df] containing sequence metadata.
#'
#' @details
#' `check_identifiers()` is a helper function to check for inconsistencies
#' between identifiers in metadata and DNA sequence files. It performs three
#' checks, in order:
#' 1. every identifier in `dna` must be present in `data`; any missing
#'    identifiers are reported and abort the function.
#' 2. `dna` must not contain duplicated identifiers; any duplicates are
#'    reported and abort the function.
#' 3. `data` must not contain duplicated identifiers. Duplicated identifiers
#'    that also occur in `dna` are reported and abort the function, since they
#'    make matching sequences to metadata ambiguous. Duplicated identifiers
#'    that do not occur in `dna` are reported as a warning only, since they
#'    do not affect matching.
#'
#' @return
#' Invisibly returns `TRUE` if all checks pass.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins.
#'
#' @examples
#' check_identifiers(geophagus, "gbAccession", geophagus_info)
#'
#' @export
check_identifiers <- function(dna, identifier, data) {
  id_meta <- dplyr::pull(data, {{ identifier }})
  id_seq <- names(dna)

  # 1) every identifier in dna (FASTA)_data must be present in data (metadata)
  missing_ids <- id_seq[!id_seq %in% id_meta] |> unique()

  if (length(missing_ids) > 0) {
    cli::cli_abort(c(
      "Identifiers missing from metadata.",
      "x" = "The following identifiers occur in sequence data {.arg {deparse(substitute(dna))}} but are absent from metadata {.arg {deparse(substitute(data))}}.",
      "i" = "Missing identifiers:",
      stringr::str_flatten_comma(missing_ids)
    ))
  }

  # 2) no duplicated identifiers in dna (FASTA)
  dup_seq <- id_seq[vctrs::vec_duplicate_detect(id_seq)] |> unique()

  if (length(dup_seq) > 0) {
    cli::cli_abort(c(
      "Duplicate identifiers found in sequence data {.arg {deparse(substitute(dna))}}.",
      "x" = "You've supplied a {.arg {deparse(substitute(dna))}} with duplicated identifiers.",
      "i" = "Duplicated identifiers:",
      stringr::str_flatten_comma(dup_seq)
    ))
  }

  # 3) no duplicated identifiers in metadata
  dup_meta <- id_meta[vctrs::vec_duplicate_detect(id_meta)] |> unique()

  if (length(dup_meta) > 0) {
    dup_seq_in_meta <- dup_meta[dup_meta %in% id_seq]
    dup_seq_not_in_meta <- dup_meta[!dup_meta %in% id_seq]

    if (length(dup_seq_not_in_meta) > 0) {
      cli::cli_warn(c(
        "Duplicate identifiers found in metadata {.arg {deparse(substitute(data))}}.",
        "!" = "The following duplicated identifiers in {.arg {deparse(substitute(data))}} do not occur in {.arg {deparse(substitute(dna))}} and do not affect matching, but you may want to clean them up.",
        "i" = "Duplicated identifiers:",
        stringr::str_flatten_comma(dup_seq_not_in_meta)
      ))
    }

    if (length(dup_seq_in_meta) > 0) {
      cli::cli_abort(c(
        "Duplicate identifiers found in metadata {.arg {deparse(substitute(data))}}.",
        "x" = "The following duplicated identifiers in {.arg {deparse(substitute(data))}} also occur in {.arg {deparse(substitute(dna))}}, which makes matching sequences to metadata ambiguous.",
        "i" = "Duplicated identifiers:",
        stringr::str_flatten_comma(dup_seq_in_meta)
      ))
    }
  }

  cli::cli_alert_success("Identifiers passed all checks.")
  invisible(TRUE)
}
