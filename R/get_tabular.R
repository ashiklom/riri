#' Retrieve climate data from a source in tabular form
#'
#' @param source_string An expert mode string or character vector
#' @param sites_df A data frame of site information. Must contain columns 
#' `site_name`, `latitude`, and `longitude`
#' @param group_size Size of groups for segmenting data. Default = 20.
#' @return `tibble` containing data to be returned
#' @export
get_tabular <- function(source_string, sites_df, group_size = 20, verbose = TRUE, ...) {
  src <- paste(source_string, collapse = " ")
  nsites <- nrow(sites_df)
  if (nsites > group_size) {
    if (verbose) {
      message(
        "Number of sites (", nsites, ") ",
        "greater than group size (", group_size, "). ",
        "Segmenting input dataframe."
      )
    } 
    get_tabular_pb <- function(sites_sub, pb) {
      get_tabular(
        source_string = source_string,
        sites_df = sites_sub,
        group_size = group_size,
        verbose = verbose,
        ...
      )
    }
    sites_segmented <- segment_df(sites_df, group_size)
    pb <- progress::progress_bar$new(total = nrow(sites_segmented))
    pb$tick(0)
    met_data <- sites_segmented %>%
      dplyr::mutate(
        .riri_data = purrr::map(.segmented_data, get_tabular_pb, pb = pb)
      )
    result <- met_data %>%
      dplyr::pull(.riri_data) %>%
      purrr::reduce(dplyr::full_join, by = "T")
    return(result)
  }
  site_expert <- site2expert(sites_df)
  nsite <- nrow(sites_df)
  all_expert <- expert2string(paste(src, site_expert))
  src_url <- expert2url(all_expert, "")
  file_url <- paste(src_url, ncoltable(nsite), sep = "/")
  col_spec <- readr::cols(T = "c", .default = "d")
  if (verbose) message("Downloading data...")
  dat <- readr::read_tsv(file_url, col_types = col_spec)
  if (verbose) message("Done!")
  colnames(dat)[-1] <- sites_df$site_name
  dat
}

ncoltable <- function(nsite) {
  ntab <- nsite + 1
  ntabopt <- ntab + 1
  tabopt_n <- sprintf("tabopt.N=%d", ntabopt)
  tabopt_cols <- sprintf("tabopt.%d=text", seq_len(ntab))
  tabopt_end <- c(
    sprintf("tabopt.%d=blankNaN", ntabopt),
    "NaNmarker=",
    "tabtype=R.tsv",
    "eol=LF+(unix)",
    "filename=datafile.tsv"
  )
  tabopt_list <- c(tabopt_n, tabopt_cols, tabopt_end)
  tabopt_string <- paste(tabopt_list, collapse = "&")
  paste0(ntab, "+ncoltable.html?", tabopt_string)
}

site2expert <- function(sites_df, xvar = ".T") {
  stopifnot(
    is.data.frame(sites_df),
    "latitude" %in% colnames(sites_df),
    "longitude" %in% colnames(sites_df)
  )
  coord_strings <- purrr::map2_chr(sites_df$latitude, sites_df$longitude, latlon2string)
  firstline <- paste0("a: ", xvar)
  out1 <- paste(c(firstline, coord_strings), collapse = " \n:a: ")
  paste(out1, ":a", sep = "\n")
}

latlon2string <- function(latitude, longitude) {
  sprintf("X (%f) VALUE Y (%f) VALUE", longitude, latitude)
}

segment_df <- function(.data, group_size = 5) {
  .data %>%
    dplyr::mutate(.segment_group = 1 + (row_number() - 1) %/% !!group_size) %>%
    dplyr::group_by(.segment_group) %>%
    tidyr::nest(.key = ".segmented_data")
}
