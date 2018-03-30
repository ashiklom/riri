value_string <- function(...) {
  l <- rlang::list2(...)
  char_vec <- purrr::map2_chr(
    names(l),
    l,
    sprintf,
    fmt = "%s (%f) VALUE"
  )
  paste(char_vec, collapse = " ")
}

latlon2string <- function(latitude, longitude) {
  value_string(X = longitude, Y = latitude)
}
