#' Retrieve data from API "Température des cours d'eau"
#'
#' @description
#'
#' Available endpoints are:
#'
#' - `get_temperature_stations` retrieves temperature stations
#' - `get_temperature_chronique` retrieves temperature time series
#'
#' See the API documentation of each endpoint for available filter parameters:
#' \url{https://hubeau.eaufrance.fr/page/api-temperature-continu}
#'
#' @inheritParams doApiQuery
#' @inherit convert_list_to_tibble return return
#'
#' @export
#' @rdname get_temperature
#' @examples
#' \dontrun{
#' # Retrieve the temperature stations in the department of Loiret
#' get_temperature_stations(code_departement = "45")
#'
#' # Retrieve the temperature from 2012-01-01 to 2012-01-05 at site 04051125
#'
#' get_temperature_chronique(
#'   code_station = "04051125",
#'   date_debut_mesure = "2012-01-01",
#'   date_fin_mesure="2012-01-05",
#'   fields = paste("code_station,date_mesure_temp,heure_mesure_temp,resultat,symbole_unite")
#' )
#'
#' }
#'
get_temperature_stations  <- function(...)
  {
  l <- doApiQuery(api = "temperature",
                  endpoint = "station",
                  ...)

    convert_list_to_tibble(l)
}

#' @rdname get_temperature
#' @export
get_temperature_chronique <- function(...) {
  l <- doApiQuery(api = "temperature",
                  endpoint = "chronique",
                  ...)
  convert_list_to_tibble(l)
}
