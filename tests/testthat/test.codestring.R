library(riri)
library(testthat)

string <- "
SOURCES .NOAA .NCEP .CPC .GHCN_CAMS .gridded .deg0p5 .temp
a:
  .T
:a:
  X (30) VALUE
  Y (30) VALUE
:a:
  X (45) VALUE
  Y (45) VALUE
:a
"

nsite <- 2

dl_url <- paste(iridl_base_url, expert2string(string), ncoltable(nsite), sep = "/")
dat <- readr::read_tsv(dl_url)
