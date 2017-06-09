library(tidyverse)
library(riri)
library(ncdf4)

worldclim_coord_filter <- function(lat, lon) {
    x1 <- (lon %/% 30) * 30 + 15
    x2 <- lon - x1
    
    y1 <- (lat %/% 30) * 30 + 15
    y2 <- lat - y1

    vals <- c(X = lon, X2 = x2, Y = lat, Y2 = y2)

    strings <- sprintf('%f/VALUES', vals)
    names(strings) <- names(vals)

    return(strings)
}

filter_coord_worldclim <- function(url_string, lat, lon) {
    filter_list <- worldclim_coord_filter(lat, lon)
    filter_string <- filter_list2string(filter_list)
    output <- c(url_string, filter_string)
    return(output)
}

worldclim_amt <- function(lat, lon) {
    full_url <- base_url() %>% 
        c('SOURCES', '.WORLDCLIM', '.AMT') %>% 
        filter_coord_worldclim(lat, lon) %>% 
        generate_url('value')
    temp <- as.numeric(XML::readHTMLTable(full_url, as.data.frame = FALSE)[[1]][[1]][2])
    return(temp)
}

sites <- tribble(
    ~site, ~lat, ~lon,
    'reston', 38.9675, -77.3606,
    'barrow', 71.2906, -156.7886,
    'key_west', 24.5551, -81.7800
    ) %>% 
    mutate(AMT = map2_dbl(lat, lon, worldclim_amt))

print(sites)
