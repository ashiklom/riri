ncoltable <- function(nsite) {
  ntab <- nsite + 1
  ntabopt <- ntab + 1
  tabopt_n <- sprintf("tabopt.N=%d", ntabopt)
  tabopt_cols <- sprintf("tabopt.%d=text", seq_len(ntab))
  tabopt_end <- c(
    sprintf("tabopt.%d=blankNaN", ntabopt),
    "NaNmarker=",
    "tabtype=R.tsv",
    "eol=LF+(unix)",
    "filename=datafile.tsv"
  )
  tabopt_list <- c(tabopt_n, tabopt_cols, tabopt_end)
  tabopt_string <- paste(tabopt_list, collapse = "&")
  paste0(ntab, "+ncoltable.html?", tabopt_string)
}
