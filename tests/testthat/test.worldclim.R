context("WorldClim extractions work")

sites <- tibble::tribble(
  ~site, ~lat, ~lon,
  "reston", 38.9675, -77.3606,
  "barrow", 71.2906, -156.7886,
  "key_west", 24.5551, -81.7800
)

test_that(
  "Coordinates are converted correctly",
  {
    lat <- sites$lat[1]
    lon <- sites$lon[1]
    wccoords <- worldclim_coords(lat, lon)
    expect_equal(wccoords$X, -75)
    expect_equal(wccoords$X2, -2.3606)
    expect_equal(wccoords$Y, 45)
    expect_equal(wccoords$Y2, -6.0325)
  }
)

test_that(
  "Single site, single variable extraction works",
  {
    latitude <- sites$lat[1]
    longitude <- sites$lon[1]
    variable <- "AMT"
    amt <- get_worldclim_sitevar(latitude, longitude, variable)
    expect_true(is.data.frame(amt))
    expect_equal(colnames(amt), "AMT")
    expect_true(is.numeric(amt$AMT))
    expect_true(amt$AMT > 0)
  }
)

test_that(
  "Single site, all variable extraction works",
  {
    latitude <- sites$lat[1]
    longitude <- sites$lon[1]
    wcvars <- get_worldclim_site(latitude, longitude)
  }
)
