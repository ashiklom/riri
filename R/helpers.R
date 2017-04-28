#' @importFrom magrittr %>%

#' @export
base_url <- function() {
    'http://iridl.ldeo.columbia.edu'
}

#' @export
filter_list2string <- function(filter_list) {
    filter_string <- paste(sapply(seq_along(filter_list), function(x) paste(names(filter_list)[x], filter_list[x], sep = '/')), collapse = '/')
    return(filter_string)
}

#' @export
generate_url <- function(url_string) {
    full_url <- paste(url_string, collapse = '/')
    return(full_url)
}
