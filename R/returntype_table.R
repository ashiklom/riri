#' @export
returntype <- function(type, ...) {
    if (type == 'ncdf4') {
        return('data.nc')
    } else if (type == 'table') {
        return(returntype_table(...))
    } else {
        stop('Unkonwn return type "', type, '".')
    }
}

#' @export
returntype_table <- function(table_opts = NULL) {
    default_opts <- list(tabopt.N = 3, tabopt.1 = 'ISO8601', tabopt.2 = 'numbers', tabopt.3 = 'blankNaN',
                         NaNmarker = '', tabtype = 'R.tsv', eol = 'LF+(unix)', filename = 'datafile.tsv')
    if (!is.null(table_opts)) {
        use_opts <- modifyList(default_opts, table_opts)
    } else {
        use_opts <- default_opts
    }
    table_opts_char <- sprintf('%s=%s', names(use_opts), unlist(use_opts))
    table_opts_string <- paste0(table_opts_char, collapse = '&')
    table_suffix <- 'T+exch/2+ncoltable.html'
    out <- paste(table_suffix, table_opts_string, sep = '?')
    return(out)
}

