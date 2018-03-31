#' Get worldclim data for a bunch of variables for a given site
#'
#' @param latitude Latitude coordinate
#' @param longitude Longitude coordinate
#' @param variables Character vector of variables to extract. See [worldclim_vars].
#' @return `tibble` containing all variables
#' @export
get_worldclim_site <- function(latitude, longitude, variables = worldclim_vars$label) {
  stopifnot(all(variables %in% worldclim_vars$label))
  l <- purrr::map(variables, get_worldclim_sitevar, latitude = latitude, longitude = longitude)
  lrows <- purrr::map_int(l, nrow)
  monthly <- l[lrows == 12] %>%
    purrr::reduce(dplyr::full_join, by = "month")
  l[lrows != 12] %>%
    dplyr::bind_cols() %>%
    dplyr::mutate(
      monthly_data = list(monthly),
      latitude = latitude,
      longitude = longitude
    ) %>%
    dplyr::select(latitude, longitude, dplyr::everything())
}

#' Get worldclim data for a single site
#'
#' @inheritParams get_worldclim_site
#' @param variable Character string indicating which variable to download
#' @export
get_worldclim_sitevar <- function(latitude, longitude, variable) {
  stopifnot(
    variable %in% worldclim_vars$label
  )
  worldclim_src <- paste0("SOURCES .WORLDCLIM .", variable)
  wc_coords <- worldclim_coords(latitude, longitude)
  coord_string <- value_string(!!!wc_coords)
  expert <- paste(worldclim_src, coord_string)
  src_url <- expert2url(expert, "data.tsv")
  file_url <- paste(src_url, ncoltable(1), sep = "/")
  out <- suppressMessages(readr::read_tsv(src_url, col_names = FALSE)) %>%
    `colnames<-`(variable)
  if (nrow(out) == 12) out$month <- 1:12
  out
}

# Variant that uses NetCDF
# Not actually that much faster -- main bottleneck is variable access
get_worldclim_sitevar2 <- function(latitude, longitude, variable) {
  stopifnot(
    variable %in% worldclim_vars$label
  )
  worldclim_src <- paste0("SOURCES .WORLDCLIM .", variable)
  wc_coords <- worldclim_coords(latitude, longitude)
  coord_string <- value_string(!!!wc_coords)
  expert <- paste(worldclim_src, coord_string)
  src_url <- expert2url(expert, "dods")
  nc <- ncdf4::nc_open(src_url)
  out <- ncdf4::ncvar_get(nc, variable)
  tibble::tibble(!!variable := out)
}

#' Convert lat lon coordinates to two-part Worldclim grid
#'
#' @param lat Latitude coordinate
#' @param lon Longitude coordinate
#' @return Named list
#' @export
worldclim_coords <- function(lat, lon) {
  stopifnot(
    length(lat) == length(lon),
    is.numeric(lat),
    is.numeric(lon)
  )
  x1 <- (lon %/% 30) * 30 + 15
  x2 <- lon - x1
  y1 <- (lat %/% 30) * 30 + 15
  y2 <- lat - y1
  list(
    X = x1,
    X2 = x2,
    Y = y1,
    Y2 = y2
  )
}

#' Worldclim variables
#'
#' @export
worldclim_vars <- tibble::tribble(
  ~name,                                       ~label,
  "Altitude",                                  "alt",
  "Temperature Amplitude",                     "amplitude",
  "Annual Mean Temperature",                   "AMT",
  "Annual Precipitation",                      "AP",
  "Isothermality (MDR/TAR)",                   "ITY",
  "Mean Diurnal Range",                        "MDR",
  "Min Temperature of Coldest Month",          "MTCM",
  "Mean Temperature of Coldest Quarter",       "MTCQ",
  "Mean Temperature of Driest Quarter",        "MTDQ",
  "Mean Temperature of Warmest Quarter",       "MTWaQ",
  "Mean Temperature of Wettest Quarter",       "MTWeQ",
  "Max Temperature of Warmest Month",          "MTWM",
  "Precipitation of oldest Quarter",           "PCQ",
  "Precipitation of Driest Month",             "PDM",
  "Precipitation of Driest Quarter",           "PDQ",
  "Precipitation",                             "prec",
  "Precipitation Seasonality (coef. of var.)", "PS_cv",
  "Precipitation of Warmest Quarter",          "PWaQ",
  "Precipitation of Wettest Quarter",          "PWeQ",
  "Precipitation of Wettest Month",            "PWM",
  "Temperature Annual Range (MTWM-MTCM)",      "TAR",
  "Maximum temperature",                       "tmax",
  "Mean temperature",                          "tmean",
  "Minimum temperature",                       "tmin",
  "Temperature Seasonality (variance)",        "TS_std"
)
