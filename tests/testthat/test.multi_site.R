library(riri)
library(testthat)
library(magrittr)

site_df <- tibble::tribble(
  ~site_name, ~latitude, ~longitude,
  "Harvard Forest", 42.5, -72.2,
  "UNDERC", 46.2, -89.5,
  "Honolulu", 21.3, -157.8
) %>%
  dplyr::bind_rows(., ., ., ., ., ., ., ., ., ., ., .) %>%
  dplyr::mutate(site_name = paste0("site_", row_number()))

temp_src <- c("SOURCES .NOAA .NCEP .CPC .GHCN_CAMS .gridded .deg0p5 .temp")
temp_data <- get_tabular(temp_src, site_df)
temp_data$date <- lubridate::parse_date_time(temp_data$T, "my")

precip_src <- c("SOURCES .NOAA .NCEP .CPC .Merged_Analysis .monthly .latest .ver2 .prcp_est")
precip_data <- get_tabular(precip_src, site_df)
precip_data$date <- lubridate::parse_date_time(precip_data$T, "my")
