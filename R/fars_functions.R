#' fars_read
#'
#' This is a simple function to read a file.
#'
#' @param file A character string giving the text the function will print

#' @return Data object from the file specified by filename.
#' If file does not exist, return a message "file does not exists".
#'
#' @examples
#' fars_read(filename)
#'
#' @importFrom read read_csv
#'
#' @references http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)
#'
#' @export
fars_read <- function(filename) {
    if(!file.exists(filename))
        stop("file '", filename, "' does not exist")
    data <- suppressMessages({
        readr::read_csv(filename, progress = FALSE)
    })
    dplyr::tbl_df(data)
}

#' make_filename
#'
#' This is to create a file name as "accident_%d.csv.bz2" combined with a specific year.
#'
#' @param years Specify years to read data
#'
#' @return Print created file name.
#'
#' @examples make_filename(2015)
#'
#' @references http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    file<-sprintf("accident_%d.csv.bz2", year)
    system.file("extdata", file, package="far")
}
#' fars_read_years
#'
#' Reading files for a single or multiple years.
#'
#' @param years Spcify years to read specific files.
#'
#' @return If years are not in valid range, "invalid year" will be returned.
#'
#' @examples
#' fars_read_years(2012)
#' fars_read_years(2012:2015)
#'
#' @importFrom dplyr mutate select
#'
#' @references http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)
#'
#' @export
fars_read_years <- function(years) {
    lapply(years, function(year) {
        file <- make_filename(year)
        tryCatch({
            dat <- fars_read(file)
            dplyr::mutate(dat, year = year) %>%
                dplyr::select(MONTH, year)
        }, error = function(e) {
            warning("invalid year: ", year)
            return(NULL)
        })
    })
}

#' fars_summarize_years
#'
#' Summarizes files by years and month.
#'
#' @param  years years to summarize.
#'
#' @return Summary firls for specified years aggregated by months.
#'
#' @examples
#' fars_summarize_years(2014)
#' fars_summarize_years(2014:2015)
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr spread
#'
#' @references http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)
#'
#' @export

fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' fars_map_state
#'
#' Extract a data for a specific year and state.
#'
#' @param year year to specify
#' @param state.num index of a state
#'
#' @return map plot of the number of accident. If there is no accidents for the year in the stat, return
#' "no accidents to plot".
#'
#' @examples fars_map_state(state.num, 2015)
#'
#' @importFrom maps graphics
#'
#' @importFrom maps map
#' @importFrom graphics point
#'
#' @references http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)
#'
#' @export
#'
fars_map_state <- function(state.num, year) {
    filename <- make_filename(year)
    data <- fars_read(filename)
    state.num <- as.integer(state.num)

    if(!(state.num %in% unique(data$STATE)))
        stop("invalid STATE number: ", state.num)
    data.sub <- dplyr::filter(data, STATE == state.num)
    if(nrow(data.sub) == 0L) {
        message("no accidents to plot")
        return(invisible(NULL))
    }
    is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
    is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
    with(data.sub, {
        maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                  xlim = range(LONGITUD, na.rm = TRUE))
        graphics::points(LONGITUD, LATITUDE, pch = 46)
    })
}
