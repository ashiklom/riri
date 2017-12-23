#' Convert latitude-longitude pair to IRIDL filter point
#'
#' @param lat Latitude, in decimal degrees
#' @param lon Longitude, in decimal degrees
#' @export
filter_point <- function(url_string, lat, lon) {
    filter_string <- make_filter_point(lat, lon)
    c(url_string, filter_string)
}

#' @describeIn filter_point Create string from lat/lon pair
make_filter_point <- function(lat, lon) {
    filter_list <- sprintf("%f/VALUES", c(lat, lon))
    names(filter_list) <- c("Y", "X")
    filter_list2string(filter_list)
}
