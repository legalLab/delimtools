#' Plot Phylogenetic Trees With Species Delimitation Partitions
#'
#' @description
#' `delim_autoplot2()` returns a phylogenetic tree plotted using `ggtree` alongside
#' with a customized tile plot using [geom_tile][ggplot2::geom_tile] combined by
#' [wrap_plots][patchwork::wrap_plots].
#'
#' @inheritParams delim_autoplot
#' @param species column name in `tbl_labs` which contains species names for each tip of the tree.
#'
#' @details
#' `delim_autoplot2()` is a wrapper for tree plotting with associated data implemented
#' using `ggtree`, `ggplot2`, and `patchwork`. If `consensus = TRUE`, a consensus bar will be plotted next to the species delimitation plot,
#' summarizing partitions across samples. If no consensus is reached, an "X" will be plotted instead.
#' This function is a modified version of [delim_autoplot] which plots
#' species partitions using a black and grey color scheme.
#' 
#' @return
#' A `patchwork` object.
#'
#' @author
#' Pedro S. Bittencourt, Rupert A. Collins
#' 
#' @contributor
#' Tomas Hrbek
#' 
#' @examples
#' # create labels
#' labs <- geophagus_info |> dplyr::select(gbAccession, scientificName)
#'
#' # view partitions using an ultrametric tree
#' p <- delim_autoplot2(geophagus_delims,
#'   geophagus_beast,
#'   tbl_labs = labs,
#'   species = "scientificName"
#' )
#' p
#' # or
#' p <- delim_autoplot2(geophagus_delims,
#'   "geophagus_beast.nex",
#'   tbl_labs = labs,
#'   species = "scientificName"
#' )
#' p
#'
#' # view partitions using a phylogram
#' p1 <- delim_autoplot2(geophagus_delims,
#'   geophagus_raxml,
#'   tbl_labs = labs,
#'   species = "scientificName"
#' )
#' p1
#' # or
#' p1 <- delim_autoplot2(geophagus_delims,
#'   "geophagus_raxml.nwk",
#'   tbl_labs = labs,
#'   species = "scientificName"
#' )
#' p1
#'
#' @export
delim_autoplot2 <- function(delim, infile, consensus = TRUE, n_match = NULL,
                            delim_order = NULL, tbl_labs, species,
                            hexpand = 0.1, widths = c(0.5, 0.2)) {

  # checks
  if (methods::is(infile, "treedata")) {
    tr <- infile
  } else if (file.exists(infile)) {
    lines <- readLines(infile, warn = FALSE)
    lines <- trimws(lines)
    lines <- lines[lines != ""]
    lines <- lines[!grepl("^\\[.*\\]$", lines)]  # drop pure NEXUS comments
    if (length(lines) == 0) {
      cli::cli_abort("Phylogenetic tree file is empty or contains only comments.")
    }
    if (grepl("^#NEXUS", toupper(lines[1])) && grepl("^END;$", toupper(lines[length(lines)]))) {
      tr <- treeio::read.beast(infile)
    } else if (grepl("^\\(", lines[1]) && grepl(";$", lines[length(lines)])) {
      # tree must have bootstrap support
      tr <- treeio::read.newick(infile, node.label="support")
    } else {
      cli::cli_abort("Infile is improperly formatted Newick or Nexus tree file.")
    }
  } else {
    cli::cli_abort("Please provide a phylogenetic tree object or a path to a Newick/Nexus tree file that can be read by `ape`.")
  }
  
  # check if `patchwork` is installed
  rlang::check_installed("patchwork", reason = "to run `delim_autoplot` properly.")
  
  if (is.null(tbl_labs)) {
    cli::cli_alert_info("Argument {.arg tbl_labs} not provided. Please provide one or use {.fn delim_autoplot} instead.")
    return(invisible(NULL))
  }

  if (is.null(species)) {
    cli::cli_alert_info("Argument {.arg species} not provided. Please provide one or use {.fn delim_autoplot} instead.")
    return(invisible(NULL))
  }

  if (ape::is.ultrametric(tr@phylo)) {
    p <- ggtree::ggtree(tr, ladderize = TRUE, color = "grey50", size = 1) %<+% tbl_labs

    pp <- p +
      ggtree::hexpand(ratio = hexpand) +
      ggtree::geom_tiplab(ggplot2::aes(label = get(colnames(tbl_labs)[2])), size = 3.5) +
      ggtree::geom_point2(ggplot2::aes(subset = !is.na(.data$posterior) & .data$posterior >= 0.95))
  } else {
    p <- ggtree::ggtree(tr, ladderize = TRUE, color = "grey50", size = 1) %<+% tbl_labs

    pp <- p +
      ggtree::hexpand(ratio = hexpand) +
      ggtree::geom_tiplab(ggplot2::aes(label = get(colnames(tbl_labs)[2])), size = 3.5, align = TRUE) +
      ggtree::geom_point2(ggplot2::aes(subset = !.data$isTip & .data$support >= 75))
  }

  if (is.null(delim_order)) {
    delim_order <- colnames(delim)[-1]

    cli::cli_warn("Argument {.arg delim_order} not provided. Using default order from {.arg delim}.")
  }

  # load rle function
  rle_method <- function(x) {
    # Compute the run-length encoding
    rle_result <- rle(x)

    # Create a logical vector initialized to FALSE
    separate_runs_logical <- logical(length(x))

    # Identify unique values
    unique_values <- unique(x)

    # Determine if each value appears in separate runs
    runs_count <- sapply(unique_values, function(val) {
      length(which(rle_result$values == val))
    })

    # Create a logical vector indicating if a value appears in separate runs
    is_separate_run <- sapply(x, function(val) {
      runs_count[val] > 1
    })
    # Print the logical vector
    return(unname(is_separate_run))
  }

  if (consensus == TRUE) {
    # reorder and turn into long format
    delim_long <- delim |>
      dplyr::mutate(species = dplyr::pull(tbl_labs, {{ species }})[match(.data$labels, dplyr::pull(tbl_labs, 1))]) |>
      dplyr::arrange(match(.data$labels, ggtree::get_taxa_name(pp))) |>
      delimtools::delim_consensus(n_match = n_match) |>
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) |>
      tidyr::pivot_longer(
        cols = c(-labels, -species),
        names_to = "method",
        values_to = "spp",
        cols_vary = "fastest"
      ) |>
      dplyr::mutate(
        method = as.factor(.data$method) |> 
          forcats::fct_inorder(),
        spp = as.factor(.data$spp) |> 
          forcats::fct_inorder(),
        label = labels,
        labels = as.factor(.data$labels) |> 
          forcats::fct_inorder() |> 
          forcats::fct_rev()
      ) |>
      dplyr::group_by(.data$spp) |>
      dplyr::mutate(interaction = interaction(.data$method, .data$species, .data$spp)) |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$method) |>
      dplyr::arrange(.data$labels, .by_group = TRUE) |>
      dplyr::mutate(is_monophyletic = !rle_method(as.character(.data$spp))) |>
      dplyr::ungroup()

    # delim plot
    delim_tile_bw <- ggplot2::ggplot(delim_long, ggplot2::aes(x = .data$method, y = .data$labels, fill = .data$is_monophyletic, color = .data$is_monophyletic, group = .data$interaction)) +
      ggplot2::geom_line(lineend = "round", linejoin = "round", linewidth = 5, show.legend = FALSE) +
      ggplot2::geom_point(data = ~ subset(., !is.na(spp)), shape = 21, size = 4.2, show.legend = FALSE) +
      ggplot2::geom_point(data = ~ subset(., is.na(spp)), shape = 4, color = "black", show.legend = FALSE) +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.5, 0.5))) +
      ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add = c(0.5, 0.5))) +
      ggplot2::scale_fill_manual(values = c("grey80", "black"), na.value = "transparent") +
      ggplot2::scale_colour_manual(values = c("grey80", "black"), na.value = "transparent") +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(color = "black", size = 10),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    # rescale tree plot
    pp2 <- pp +
      ggplot2::coord_cartesian(xlim = ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile_bw), expand = FALSE) +
      ggtree::geom_treescale(color = "grey50", linesize = 1, fontsize = 3, y = ggfun::yrange(delim_tile_bw)[1])
  } else {
    delim_long <- delim |>
      dplyr::mutate(species = dplyr::pull(tbl_labs, {{ species }})[match(labels, dplyr::pull(tbl_labs, 1))]) |>
      dplyr::arrange(match(.data$labels, ggtree::get_taxa_name(pp))) |>
      dplyr::relocate(tidyselect::all_of(delim_order), .after = labels) |>
      tidyr::pivot_longer(
        cols = c(-labels, -species),
        names_to = "method",
        values_to = "spp",
        cols_vary = "fastest"
      ) |>
      dplyr::mutate(
        method = as.factor(.data$method) |> 
          forcats::fct_inorder(),
        spp = as.factor(.data$spp) |> 
          forcats::fct_inorder(),
        label = labels,
        labels = as.factor(.data$labels) |> 
          forcats::fct_inorder() |> 
          forcats::fct_rev()
      ) |>
      dplyr::group_by(.data$spp) |>
      dplyr::mutate(interaction = interaction(.data$method, .data$spp)) |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$method) |>
      dplyr::arrange(.data$labels, .by_group = TRUE) |>
      dplyr::mutate(is_monophyletic = !rle_method(as.character(.data$spp))) |>
      dplyr::ungroup()

    # delim plot
    delim_tile_bw <- ggplot2::ggplot(delim_long, ggplot2::aes(x = .data$method, y = .data$labels, fill = .data$is_monophyletic, color = .data$is_monophyletic, group = .data$interaction)) +
      ggplot2::geom_line(lineend = "round", linejoin = "round", linewidth = 5, show.legend = FALSE) +
      ggplot2::geom_point(shape = 21, size = 4.2, show.legend = FALSE) +
      ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = c(0.5, 0.5))) +
      ggplot2::scale_x_discrete(position = "top", expand = ggplot2::expansion(add = c(0.5, 0.5))) +
      ggplot2::scale_fill_manual(values = c("grey80", "black"), na.value = "transparent") +
      ggplot2::scale_colour_manual(values = c("grey80", "black"), na.value = "transparent") +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(color = "black", size = 10),
        axis.ticks.x = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank()
      )

    # rescale tree plot
    pp2 <- pp +
      ggplot2::coord_cartesian(xlim = ggfun::xrange(pp), ylim = ggfun::yrange(delim_tile_bw), expand = FALSE) +
      ggtree::geom_treescale(color = "grey50", linesize = 1, fontsize = 3, y = ggfun::yrange(delim_tile_bw)[1])
  }

  x <- patchwork::wrap_plots(pp2, delim_tile_bw, widths = widths)

  return(x)
}
