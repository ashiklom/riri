#' Get metadata for a source string
#'
#' @param source_string Source string
#' @return tibble of information about source
#' @export
get_source_info <- function(source_string) {
  url <- expert2url(source_string)
  website <- rvest::read_html(url)

  div_info <- rvest::html_nodes(website, ".infodivinfo")
  div_info_childs <- rvest::html_children(div_info)
  ind_vars <- div_info_childs[2]

  ind_var_labels <- rvest::html_nodes(ind_vars, "dt") %>% rvest::html_text()
  lab_rxp <- "(.*?)[[:space:]]*\\((.*)\\)"
  lab_matches <- stringr::str_match(ind_var_labels, lab_rxp)[, -1]

  ind_var_info <- rvest::html_nodes(ind_vars, "dd") %>% rvest::html_text()
  info_rxp <- paste0(
    "^[[:space:]]*grid:[[:space:]]+/",    # Opening
    "([[:alnum:]]+)[[:space:]]+",         # Grid variable
    "\\((.*?)\\)[[:space:]]+",            # Units
    "(.*?)[[:space:]]+",                  # Grid type (e.g. ordered, periodic)
    "\\((.*?)\\)[[:space:]]+to[[:space:]]+",
    "\\((.*?)\\)[[:space:]]+by[[:space:]]+",
    "(.*?)[[:space:]]+",
    "N=[[:space:]]+(.*?)[[:space:]]:grid"
  )
  info_matches <- stringr::str_match(ind_var_info, info_rxp)[, -1]
  var_info <- cbind(lab_matches, info_matches) %>%
    `colnames<-`(c("title", "name", "grid_id", "grid_unit", "grid_type",
                "grid_from", "grid_to", "grid_by", "grid_n")) %>%
    dplyr::as_tibble()
  var_info
}
