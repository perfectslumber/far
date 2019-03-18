#' @title Read a .csv file into a tibble
#'
#' @description This is a function to read a .csv file with the given file name into a tibble.
#' Conditions that may result in an error: \code{filename} is not a valid path.
#'
#' @param filename A character string representing a path to a file.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @return This function returns a tibble read from the requested file.
#'
#' @examples fars_read("accident_2013.csv.bz2")
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

#' @title Print a file name based on year input
#'
#' @description Conditions that may result in an error:
#' \code{year} is not a valid number of a year;
#' Data of requested year does not exist.
#'
#' @param year An integer or element coercible to integer representing a year.
#'
#' @return This function prints a character string of file name based on input year.
#' It can be used for reading the file.
#'
#' @examples make_filename(2014)
#'
#' @export
make_filename <- function(year) {
    year <- as.integer(year)
    sprintf("accident_%d.csv.bz2", year)
}

#' @title Batch read data "MONTH" info in given year(s)
#'
#' @description This function creates a list of tibbles based on input of years.
#' It will first read in the data corresponding to the year(s),
#' then extract the year and month information for each requested year.
#' Conditions that may result in an error:
#' \code{years} is not a valid vector containing number of year(s);
#' Data of requested year does not exist.
#'
#' @param years a vector of integer representing number of year(s)
#'
#' @importFrom dplyr mutate select
#'
#' @return This function returns a list of tibbles.
#' Each tibble contain two columns with month and year information extrated from data of requested years.
#'
#' @examples fars_read_years(c(2013, 2014))
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

#' @title Summarize number of observations in each month in given year(s)
#'
#' @description This function counts the number of observations in each month based on year input.
#' Conditions that may result in an error:
#' \code{years} is not a valid vector containing number of year(s);
#' Data of requested year does not exist.
#'
#' @param years a vector of integer representing number of year(s)
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @return This function returns a list of tibbles, each tibble contains two columns.
#' The first column is each month, the second column is observation number in that month.
#'
#' @examples fars_summarize_years(c(2013, 2014))
#'
#' @export
fars_summarize_years <- function(years) {
    dat_list <- fars_read_years(years)
    dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' @title Plot the location of each incident in a given state and year
#'
#' @description This function takes input of a state number and a year, and
#' plot the incidents of the given year in the map of the given state.
#' Conditions that may result an error:
#' \code{state.num} is not a valid number of states;
#' \code{year} is not a valid  number of year;
#' Data of requested year does not exist;
#'
#' @param state.num An integer for the code number of a state
#' @param year An integer or element coercible to integer representing a year.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @return This function draws a map of the requested state, and plot on it the
#' incidents occured in that state in the requested year. Each point represents an incident
#'
#' @examples fars_map_state(1, 2014)
#'
#' @export
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
