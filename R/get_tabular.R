#' Retrieve climate data from a source in tabular form
#'
#' @param source_string An expert mode string or character vector
#' @param sites_df A data frame of site information. Must contain columns 
#' `site_name`, `latitude`, and `longitude`
#' @return `tibble` containing data to be returned
#' @export
get_tabular <- function(source_string, sites_df, ...) {
  src <- paste(source_string, collapse = " ")
  site_expert <- site2expert(sites_df)
  nsite <- nrow(sites_df)
  all_expert <- expert2string(paste(src, site_expert))
  src_url <- expert2url(all_expert, "")
  file_url <- paste(src_url, ncoltable(nsite), sep = "/")
  message("Downloading data...")
  dat <- readr::read_tsv(file_url)
  message("Done!")
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
