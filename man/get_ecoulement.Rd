% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_ecoulement.R
\name{get_ecoulement_stations}
\alias{get_ecoulement_stations}
\alias{get_ecoulement_observations}
\alias{get_ecoulement_campagnes}
\title{Retrieve data from API "Ecoulement des cours d'eau"}
\usage{
get_ecoulement_stations(...)

get_ecoulement_observations(...)

get_ecoulement_campagnes(...)
}
\arguments{
\item{...}{parameters of the queries and their values in the format
\verb{Param1_Name = "Param1 value", Param2_Name = "Param2 value"}, use the
function \link{list_params} for a list of the available filter parameters
for a given API endpoint and see the API documentation for their description}
}
\value{
A \link[tibble:tibble]{tibble::tibble} with one row by record and one column by field.
}
\description{
The data originate from the "ONDE" river low waters monitoring network.
Available endpoints are:
\itemize{
\item \code{get_ecoulement_stations} retrieves site data and locations
\item \code{get_ecoulement_observations} retrieves flow information
\item \code{get_ecoulement_campagnes} retrieves annual surveys
}

See the API documentation for available filter parameters: \url{https://hubeau.eaufrance.fr/page/api-ecoulement}
}
\examples{
\dontrun{
# Retrieve 2022 observation campaigns in the Jura French department
get_ecoulement_campagnes(
  code_departement = "39",
  date_campagne_min = "2022-01-01",
  date_campagne_max = "2022-12-31"
)

# Retrieve river stations
stations_39 <- get_ecoulement_stations(
  code_departement = "39",
  fields = "code_station,libelle_cours_eau,libelle_commune"
)
stations_39

# Get the query parameters for the requested API/endpoint
list_params(api = "ecoulement",
                     endpoint = "observations")

# Retrieve the river flow data in the Jura departement in 2022 with
# a selection of the fields
onde_39 <- get_ecoulement_observations(
  code_departement = "39",
       date_observation_min = "2022-01-01",
       date_observation_max = "2022-12-31",
       fields = "code_station,libelle_station,date_observation,libelle_ecoulement"
)
onde_39
}
}
