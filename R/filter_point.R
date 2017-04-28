#' @export
make_filter_point <- function(lat, lon) {
    lat_hemi <- ifelse(lat > 0, 'N' ,'S')
    lon_hemi <- ifelse(lon > 0, 'E', 'W')
    filter_list <- sprintf('(%f%s) VALUES', abs(c(lat, lon)), c(lat_hemi, lon_hemi))
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
