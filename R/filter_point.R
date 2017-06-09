#' @export
make_filter_point <- function(lat, lon) {
    filter_list <- sprintf('%f/VALUES', c(lat, lon))
    names(filter_list) <- c("Y", "X")
    filter_string <- filter_list2string(filter_list)
    return(filter_string)
}

#' @export
filter_point <- function(url_string, lat, lon) {
    filter_string <- make_filter_point(lat, lon)
    output <- c(url_string, filter_string)
    return(output)
}
