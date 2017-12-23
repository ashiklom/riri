#' Dispatch return type
#'
#' @export
returntype <- function(type, ...) {
    type <- tolower(type)
    if (type %in% c("ncdf4", "ncdf", "netcdf")) {
        return("data.nc")
    } else if (type %in% c("table", "tsv")) {
        return(returntype_table(...))
    } else if (type == "value") {
        return(returntype_value(...))
    } else {
        stop("Unkonwn return type \"", type, "\".")
    }
}

#' Return a two-column table
#'
#' @export
returntype_table <- function(table_opts = NULL) {
    default_opts <- list(tabopt.N = 3, tabopt.1 = "ISO8601", tabopt.2 = "numbers", tabopt.3 = "blankNaN",
                         NaNmarker = "", tabtype = "R.tsv", eol = "LF+(unix)", filename = "datafile.tsv")
    if (!is.null(table_opts)) {
        use_opts <- modifyList(default_opts, table_opts)
    } else {
        use_opts <- default_opts
    }
    table_opts_char <- sprintf("%s=%s", names(use_opts), unlist(use_opts))
    table_opts_string <- paste0(table_opts_char, collapse = "&")
    table_suffix <- "T+exch/2+ncoltable.html"
    paste(table_suffix, table_opts_string, sep = "?")
}

#' Return a single vector value
#'
#' @export
returntype_value <- function(table_opts = NULL) {
    "ngridtable+table-+skipanyNaN+1+-table+.html"
}

