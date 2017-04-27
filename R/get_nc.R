ncep_temp_point <- function(lat, lon, fname = tempfile()) {
    lat_hemi <- ifelse(lat > 0, 'N' ,'S')
    lon_hemi <- ifelse(lon > 0, 'E', 'W')
    filter_list <- sprintf('(%f%s) VALUES', c(lat, lon), c(lat_hemi, lon_hemi))
    names(filter_list) <- c("Y", "X")

    base_url <- 'http://iridl.ldeo.columbia.edu'
    var_list <- c('SOURCES', '.NOAA', '.NCEP', '.CPC', '.GHCN_CAMS', '.gridded', '.deg0p5', '.temp')
    filter_string <- paste(sapply(seq_along(filter_list), function(x) paste(names(filter_list)[x], filter_list[x], sep = '/')), collapse = '/')
    suffix <- 'data.nc'
    full_url <- paste(c(base_url, var_list, filter_string, suffix), collapse = '/')
    download.file(full_url, fname)

    ncfile <- ncdf4::nc_open(fname)
    out_df <- data.frame(time = ncdf4::ncvar_get(ncfile, 'T'), 
                         temperature = ncdf4::ncvar_get(ncfile, 'temp'))
    return(out_df)
}

# An example
a <- ncep_temp_point(43.633, -89.758)
b <- ncep_temp_point(71.276, -156.641)
ab <- merge(a, b, by = 'time')

library(tidyverse)
ab %>% 
    as_data_frame %>% 
    gather(variable, value, -time) %>% 
    ggplot() + 
    aes(x = time, y = value, color = variable) + 
    geom_line()
