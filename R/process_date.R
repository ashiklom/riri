#' @export 
process_date <- function(nclist, remove_original = TRUE) {
    if (!'T' %in% names(nclist)) {
        warning('No time variable found in list. Returning original list.')
        return(nclist)
    }
    t_unit <- attr(nclist[['T']], 'units')
    months_since <- grepl('months since', t_unit, ignore.case = TRUE)
    if (months_since) {
        base_date <- lubridate::ymd(t_unit)
        if (is.na(base_date)) {
            warning('Unable to parse date from unit "', t_unit, '". Returning original list.')
            return(nclist)
        }
        nclist[['date']] <- lubridate::`%m+%`(base_date, months(floor(nclist[['T']]))) + 
            lubridate::days(30 * c(nclist[['T']] %% 1))
        message('Added date column. First few dates are: ', 
                paste(head(nclist[['date']]), collapse = ', '))
        if (remove_original) {
            nclist['T'] <- NULL
        }
    } else {
        warning('Unknown time unit "', t_unit, '". Returning original list.')
        return(nclist)
    }
    return(nclist)
}
