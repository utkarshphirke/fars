#' Read a FARS dataset
#'
#' This function reads the specified file and returns a tibble containing the FARS data
#'
#' @param filename The filename of the file to read
#'
#' @return This function returns a tibble containing the FARS data for the given filename
#' @details Function throws an error os file with the given filename does not exist
#'
#'
#' @importFrom readr read_csv
#' @importFrom  dplyr tbl_df
#' @examples
#' \dontrun{
#' fars_read('accident_2013.csv.bz2')
#' }
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}



#' Create FARS filename for a year
#'
#' @param year The year for the FARS data
#'
#' @return Filename of the FARS dataset for the corresponding year
#' @details
#'
#' @examples
#' make_filename(2013)
make_filename <- function(year) {
        year <- as.integer(year)
        fp <- sprintf("accident_%d.csv.bz2", year)
        system.file("extdata", fp, package = "fars")
}

#' Read multiple FARS dataset
#'
#' Reads the data for the given years. If the dataset corresponding to a year is not found,
#' the function creates a warning and returns null
#'
#' @param years The years for which FARS data has to be read
#'
#' @return list of datasets for each year having the MONTH and year column. The count of MONTH represents the fatalities in that month.
#' @details Error if file for the given year not found in the FARS dataset
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' fars_read_years(c(2013, 2014, 2015))
#' }
fars_read_years <- function(years) {
        library(magrittr)
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = YEAR) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year, '\n')
                        return(NULL)
                })
        })
}

#' Fatalities by month
#'
#' Summary of fatalities by year and month
#'
#' @param years The years for which fatalities count is calculated
#'
#' @return The count of fatalities by year and month
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(c(2013, 2014))
#' }
#'
fars_summarize_years <- function(years) {
  library(magrittr)
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n, sep = '_')
}

#' Map of fatalities by location
#'
#' Plot the location of fatalities on a state map
#'
#' @param state.num The state number
#' @param year The year to use the FARS data
#'
#' @return a map object
#'
#' @details The function checks for a valid state.num. Then it filters the accidents for the state. If there aren no
#' accidents, then a message is provided.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' }
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
