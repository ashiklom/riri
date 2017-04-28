library(tidyverse)
devtools::load_all('.')

ncep_temp1 <- base_url() %>% 
    ncep_var('air_temperature') %>% 
    filter_point(lat = 43.633, lon = -89.758) %>% 
    generate_url() %>% 
    retrieve_data() %>% 
    read_nc2list(dims = 'T') %>% 
    as_data_frame()

nc <- ncdf4::nc_open('example.nc')
ncdf4::ncatt_get(nc, 'temp', 'units')[['value']]

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
