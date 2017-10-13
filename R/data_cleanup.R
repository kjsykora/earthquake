
#' Cleanup Earthquake Data
#'
#' This functions cleans up data from the NOAA significant earthquake database.
#' The cleanup includes building a single date field
#' and removing BCE dates since POSIXlt dates do not work well with pre 1 CE dates.
#'
#' @param raw_data The earthquake dataframe that needs to be cleaned.
#'
#' @param YEAR The name of the column that holds the year of the earthquake as a numeric/integer.
#'
#' @param MONTH The name of the column that holds the month of the earthquake as a numeric/integer.
#'
#' @param DAY The name of the column that holds the day of the month of earthquake as a numeric/integer.
#'
#' @param LONGITUDE The longitude of the earthquake - numeric.
#'
#' @param LATITUDE The latitude of the earthquake - numeric.
#'
#' @param DEATH The latitude of the earthquake - numeric or character.
#'
#' @param MAGNITUDE The latitude of the earthquake - numeric or character.
#'
#' @import dplyr
#'
#' @import magrittr
#'
#' @importFrom stringr str_pad
#'
#' @importFrom lubridate ymd
#'
#' @return A cleaned up dataframe with the new date column.
#'
#' @examples
#' \donttest{
#' read_delim(file = 'signif.txt', delim = '\t') %>% earthquake::eq_clean_data()
#' }
#'
#' @export


eq_clean_data <- function(raw_data,
                          YEAR = 'YEAR',
                          MONTH = 'MONTH',
                          DAY = 'DAY',
                          LONGITUDE = 'LONGITUDE',
                          LATITUDE = 'LATITUDE',
                          MAGNITUDE = "EQ_PRIMARY",
                          DEATH = "DEATHS"){
  clean_data <- raw_data %>% dplyr::filter_( ~ YEAR > 0)
  if(! base::identical( base::dim(clean_data), base::dim(raw_data))) {
    base::warning("BCE dates are not supported. We are dropping them from the analysis dataset.")
  }
  clean_data[[MONTH]][base::is.na(clean_data[[MONTH]])] <- 1
  clean_data[[DAY]][base::is.na(clean_data[[DAY]])] <- 1
  clean_data[["DATE"]] <- lubridate::ymd(paste(stringr::str_pad(clean_data[[YEAR]], width = 4, side = 'left', pad = '0'),
                                               stringr::str_pad(clean_data[[MONTH]], width = 2, side = 'left', pad = '0'),
                                               stringr::str_pad(clean_data[[DAY]], width = 2, side = 'left', pad = '0'), sep = '-'))

  clean_data[[LONGITUDE]] %<>% base::as.numeric()
  clean_data[[LATITUDE]] %<>% base::as.numeric()

  clean_data[[MAGNITUDE]] %<>% base::as.numeric()
  clean_data[[MAGNITUDE]][base::is.na(clean_data[[MAGNITUDE]])] <- 0

  clean_data[[DEATH]] %<>% base::as.numeric()
  clean_data[[DEATH]][base::is.na(clean_data[[DEATH]])] <- 0

  return(eq_location_clean(clean_data))
}

#' Location name cleanup
#'
#' This function drops the country-name and colon from the location name.
#' Furthermore, it converts the text to title case. WOW, it does soooo much!!!
#'
#' @param my_data data.frame that holds your NOAA earthquake data. Can be raw or cleaned up by earthquake::eq_clean_data
#'
#' @param LOCATION_NAME A character with the name of the column that holds the raw NOAA location name.
#'
#' @examples
#' \donttest{
#' read_delim(file = 'signif.txt', delim = '\t') %>% earthquake::eq_location_clean()
#' }
#'
#' @export

eq_location_clean <- function(my_data, LOCATION_NAME = 'LOCATION_NAME'){

  country.removed <- base::sub('.+:', '', my_data[[LOCATION_NAME]], perl=TRUE) %>% base::trimws(which='both')

  my_data[[LOCATION_NAME]] <- base::gsub('([^a-z][a-z]|^[a-z])', '\\U\\1' ,country.removed %>% base::tolower(), perl=TRUE, fixed = FALSE )

  return(my_data)

}



