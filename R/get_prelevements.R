#' Retrieve data from API "Prélèvements en eau"
#'
#' @description
#' Available endpoints are:
#'
#' - `get_prelevements_points_prelevement` retrieves withdrawal points
#' - `get_prelevements_ouvrages` retrieves withdrawal devices
#' - `get_prelevements_chroniques` retrieves time series of withdrawals
#'
#' See the API documentation for available filter parameters: \url{https://hubeau.eaufrance.fr/page/api-prelevements-eau}
#'
#' @inheritParams doApiQuery
#' @inherit convert_list_to_tibble return return
#'
#' @rdname get_prelevements
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve the withdrawal points located in Romilly-sur-Seine
#' get_prelevements_points_prelevement(code_commune_insee = "10323")
#'
#' # Retrieve the withdrawal devices located in Romilly-sur-Seine
#' get_prelevements_ouvrages(code_commune_insee = "10323")
#'
#' # Retrieve the withdrawal time series of the devices located in Romilly-sur-Seine
#' get_prelevements_chroniques(code_commune_insee = "10323")
#' }
get_prelevements_points_prelevement <- function(...) {
  l <- doApiQuery(api = "prelevements",
                  endpoint = "points_prelevement",
                  ...)

  convert_list_to_tibble(l)
}

#' @rdname get_prelevements
#' @export
get_prelevements_ouvrages <- function(...) {
  l <- doApiQuery(api = "prelevements",
                  endpoint = "ouvrages",
                  ...)
  convert_list_to_tibble(l)
}

#' @rdname get_prelevements
#' @export
get_prelevements_chroniques <- function(...) {
  l <- doApiQuery(api = "prelevements",
                  endpoint = "chroniques",
                  ...)
  convert_list_to_tibble(l)
}
