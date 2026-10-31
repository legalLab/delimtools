# spart.R
# Minimal reader for the SPART matricial text format (as opposed to SPART-XML),
# used to let abgd_tbl() and asap_tbl() accept a results file exported in SPART
# format, in addition to each tool's own native webserver/CLI output format.
# Grammar follows SpartParserMatricial in
# https://github.com/iTaxoTools/SpartParser (src/itaxotools/spart_parser/main.py).
# Only the fields abgd_tbl()/asap_tbl() need are parsed: N_spartitions and the
# Individual_assignment block. Concordances, locations, individual/subset/
# spartition scores and SPART-XML are out of scope.

#' @keywords internal
.is_spart_file <- function(path) {
  lines <- readLines(path, n = 5L, warn = FALSE)
  nonblank <- trimws(lines)
  nonblank <- nonblank[nzchar(nonblank)]
  if (length(nonblank) == 0) return(FALSE)
  grepl("^begin\\s+spart\\s*;", nonblank[1], ignore.case = TRUE)
}

#' @keywords internal
.read_spart <- function(path, delimname = "delim") {
  lines <- readLines(path, warn = FALSE)

  get_field <- function(key) {
    pat <- paste0("^\\s*", key, "\\s*=\\s*(.*)$")
    hit <- grep(pat, lines, ignore.case = TRUE, value = TRUE)
    if (length(hit) == 0) return(NULL)
    sub(pat, "\\1", hit[1], ignore.case = TRUE)
  }

  # --- N_spartitions = <n> : <label1> [,<score>] / <label2> / ... / <labelN>;
  spart_field <- get_field("N_spartitions")
  if (is.null(spart_field))
    stop("SPART file is missing 'N_spartitions'.")
  spart_field <- sub(";\\s*$", "", trimws(spart_field))
  spart_parts <- strsplit(spart_field, ":", fixed = TRUE)[[1]]
  n_spart <- as.integer(trimws(spart_parts[1]))
  spart_labels <- trimws(strsplit(spart_parts[2], "/", fixed = TRUE)[[1]])
  # drop an optional ",<spartition score>" suffix on each label
  spart_labels <- trimws(vapply(
    strsplit(spart_labels, ",", fixed = TRUE), `[`, character(1L), 1L
  ))

  # --- Individual_assignment block: one "label : v1 / v2 / ... / vN" row per
  # individual, the block ends either at a bare ';' line or at a data row
  # whose last character is ';'.
  start <- grep("^\\s*individual_assignment\\s*=\\s*$", lines, ignore.case = TRUE)
  if (length(start) == 0)
    stop("SPART file is missing an 'Individual_assignment' block.")

  labels      <- character(0)
  assignments <- list()
  i <- start[1] + 1L
  while (i <= length(lines)) {
    raw <- trimws(lines[i])
    if (raw == ";") break
    if (raw == "") { i <- i + 1L; next }

    last <- substr(raw, nchar(raw), nchar(raw)) == ";"
    if (last) raw <- substr(raw, 1L, nchar(raw) - 1L)

    colon <- regexpr(":", raw, fixed = TRUE)[1]
    if (colon < 0)
      stop("Malformed 'Individual_assignment' row (no ':'): ", raw)
    label <- trimws(substr(raw, 1L, colon - 1L))
    vals <- trimws(strsplit(substr(raw, colon + 1L, nchar(raw)), "/", fixed = TRUE)[[1]])

    labels <- c(labels, label)
    assignments[[label]] <- vals

    if (last) break
    i <- i + 1L
  }

  if (length(labels) == 0)
    stop("SPART file's 'Individual_assignment' block is empty.")

  mat <- do.call(rbind, assignments)
  colnames(mat) <- if (n_spart == 1L) delimname else paste0(delimname, "_", seq_len(n_spart))

  # Every real-world ABGD/ASAP/GMYC/PTP-exported SPART file uses plain integer
  # subset numbers; SPART itself allows arbitrary subset labels and "?" for
  # missing, so only coerce a column to integer when every value round-trips.
  cols <- lapply(seq_len(ncol(mat)), function(j) {
    x <- mat[, j]
    x[x == "?"] <- NA
    nums <- suppressWarnings(as.integer(x))
    if (all(is.na(nums) == is.na(x))) nums else x
  })
  names(cols) <- colnames(mat)

  tibble::tibble(labels = labels, !!!cols)
}
