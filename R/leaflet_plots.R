

#' Create leaflet plot with annotations
#'
#' This function reads in a dataframe and annotation column and outputs a leaflet plot.
#'
#' @param data A dataframe that contains the annotation column and numeric columns named LATITUDE and LONGITUDE
#'
#' @param annot_col The name of the column that holds the annotations.
#'
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @importFrom lazyeval interp
#'
#' @return A leaflet plot with annotations.
#'
#' @examples
#' \donttest{
#' data(earthquake_raw)
#' earthquake_raw %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")}
#'
#' @export



eq_map <- function(data, annot_col){

  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = data , popup = lazyeval::interp(~ paste(var), var = as.name(annot_col)))


}


#' Create fancy annotations for the leaflet plots
#'
#' This function reads in a dataframe with LOCATION_NAME, EQ_PRIMARY, and DEATHS as columns.
#' It then creates and html tagged annotation for use in the leaflet plots.
#'
#' @param data A dataframe that contains the columns: LOCATION_NAME, EQ_PRIMARY, and DEATHS
#'
#' @return A character column with html-tagged annotations
#'
#' @examples
#' \donttest{
#' data(earthquake_raw)
#' earthquake_raw %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")}
#'
#' @export



eq_create_label <- function(data){
  if(base::is.null(data$LOCATION_NAME)) stop("eq_create_label requires a column 'LOCATION_NAME'")
  if(base::is.null(data$EQ_PRIMARY)) stop("eq_create_label requires a column 'EQ_PRIMARY'")
  if(base::is.null(data$DEATHS)) stop("eq_create_label requires a column 'DEATHS'")
  base::paste("<br> <b>Location:</b>", data$LOCATION_NAME,
        "</br> <br> <b>Magnitude:</b>", data$EQ_PRIMARY,
        "</br> <br> <b>Total deaths:</b>", data$DEATHS, "</br>")
}

