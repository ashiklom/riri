library(riri)
library(magrittr)
library(testthat)
library(methods)
#devtools::load_all('.')

context('Test NCEP point retrievals')

ncep_point <- function(lat, lon, varname) {
    base_url() %>% 
        ncep_var(varname) %>% 
        filter_point(lat = lat, lon = lon) %>% 
        generate_url('ncdf4') %>% 
        retrieve_data() %>% 
        read_nc2list(dims = 'T') %>% 
        process_date()
}

test_ncep <- function(lat, lon, varname, var_colname, var_minval) {
    test_message <- sprintf('NCEP retrieval for variable "%s"', varname)
    test_that(test_message, {
                  dat <- ncep_point(lat, lon, varname)
                  expect_is(dat, 'list')
                  expect_equal(length(dat), 2)
                  expect_equal(names(dat), c(var_colname, 'date'))
                  expect_is(dat[[var_colname]], 'numeric')
                  expect_is(dat[['date']], 'Date')
                  expect_true(all(dat[[var_colname]] >= var_minval))
    })
}

# Site for testing
lat <- 45
lon <- 85

test_ncep(lat, lon, 'air_temperature', 'temp', 200)
test_ncep(lat, lon, 'precipitation', 'rain', 0)
test_ncep(lat, lon, 'soil_moisture', 'w', 10)
