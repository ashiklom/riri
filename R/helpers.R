#' Base URL for IRIDL
#'
#' @export
iridl_base_url <- "http://iridl.ldeo.columbia.edu"

#' Concatenate list of filters to a single string
#'
#' @param filter_list Vector/List of character filters
#' @seealso [filter_point()]
#' @export
filter_list2string <- function(filter_list) {
  paste_filter <- function(x) {
    paste(names(filter_list)[x], filter_list[x], sep = "/")
  }
  filter_vec <- sapply(seq_along(filter_list), paste_filter)
  paste(filter_vec, collapse = "/")
}

#' Generate IRIDL URL for a gven return type
#'
#' @inheritParams retrieve_data
#' @param returntype Type of return
#' @export
generate_url <- function(url_string, returntype = "ncdf4") {
    return_type <- returntype(returntype)
    url_full_string <- c(url_string, return_type)
    paste(url_full_string, collapse = "/")
}
