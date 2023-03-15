## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.asp = 0.618,
  out.width = "70%",
  fig.align = "center"
)
load(system.file("vignettes/example_ecoulement_api.RData", package = "hubeau"))

## -----------------------------------------------------------------------------
my_dept <- "35"

## ----setup--------------------------------------------------------------------
library(hubeau)

library(dplyr)
library(sf)
library(mapview)
library(ggplot2)

## -----------------------------------------------------------------------------
list_apis()

## -----------------------------------------------------------------------------
list_endpoints(api = "ecoulement")

## -----------------------------------------------------------------------------
list_params(api = "ecoulement",
            endpoint = "observations")

## -----------------------------------------------------------------------------
param_stations <- paste(
    list_params(api = "ecoulement", endpoint = "stations"),
    collapse = ","
)

## ---- eval=FALSE--------------------------------------------------------------
#  stations <- get_ecoulement_stations(
#    code_departement = my_dept,
#    fields = param_stations
#  )

## -----------------------------------------------------------------------------
stations_geo <- stations %>% 
  select(code_station,
         libelle_station,
         longitude,
         latitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

mapview::mapview(
  stations_geo,
  popup = leafpop::popupTable(
    stations_geo,
    zcol = c("code_station", "libelle_station"),
    feature.id = FALSE,
    row.numbers = FALSE
  ),
  label = "libelle_station", 
  legend = FALSE
)

## ---- eval=FALSE--------------------------------------------------------------
#  surveys <- get_ecoulement_campagnes(
#    code_departement = my_dept, # department id
#    date_campagne_min = "2012-01-01" # start date
#    )

## -----------------------------------------------------------------------------
surveys <- surveys %>%
  mutate(code_campagne = as.factor(code_campagne), 
         year = lubridate::year(date_campagne),
         month = lubridate::month(date_campagne)) %>%
    select(code_campagne,
           year, 
           month,
           libelle_type_campagne)

## -----------------------------------------------------------------------------
surveys %>%
  head() %>% 
  knitr::kable()

## -----------------------------------------------------------------------------
param_obs <- paste(
    list_params(api = "ecoulement", endpoint = "observations"),
    collapse = ","
)

## ---- eval=FALSE--------------------------------------------------------------
#  observations <-
#    get_ecoulement_observations(
#      code_departement = my_dept,
#      date_observation_min = "2012-01-01",
#      fields = param_obs
#    )

## -----------------------------------------------------------------------------
observations <- observations %>%
  filter(!is.na(code_ecoulement)) %>%
  mutate(code_campagne = as.factor(code_campagne))

## -----------------------------------------------------------------------------
obs_and_surv <- observations %>% 
  left_join(surveys, by = join_by(code_campagne)) %>% 
  select(code_station, libelle_station, year, month, code_ecoulement)

## -----------------------------------------------------------------------------
obs_and_surv <- obs_and_surv %>% 
  arrange(code_ecoulement) %>%
  group_by(code_station, libelle_station, year, month) %>%
  summarise(code_ecoulement = last(code_ecoulement), .groups = 'drop')

## -----------------------------------------------------------------------------
flow_labels <- c(
  "1"  = "Visible flow",
  "1a" = "Decent visible flow",
  "1f" = "Weak visible flow",
  "2"  = "No visible flow",
  "3"  = "Dry"
)
obs_and_surv$flow_label <- flow_labels[obs_and_surv$code_ecoulement]

## -----------------------------------------------------------------------------
gg_stream_flow <-
  function(sel_station, data) {
    
    # selected data
    sel_data <- data %>%
      filter(code_station == sel_station) %>%
      mutate(flow_label = factor(flow_label, levels = flow_labels))

    # station name for plot title
    station_lab <- unique(sel_data$libelle_station)

    # year range
    year_range <-
      min(sel_data$year, na.rm = T):max(sel_data$year, na.rm = T)
    
    # plot
    sel_data %>%
      ggplot(aes(x = month,
                 y = year,
                 color = flow_label)) +
      geom_point(shape = 15, size = 7) +
      scale_color_manual(name = "Flow status",
                         values = c("blue", 
                                    "deepskyblue", 
                                    "lightblue", 
                                    "orange", 
                                    "red"), 
                         labels = flow_labels, 
                         drop = FALSE) +
      scale_x_continuous(breaks = 1:12,
                         labels = 1:12,
                         limits = c(1, 12)) +
      scale_y_continuous(breaks = year_range,
                         labels = year_range) +
      labs(x = "Months",
           y = "Years",
           title = sprintf("%s (%s)", station_lab, sel_station)) +
      theme(
        axis.line = element_line(color = 'black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.5,
                                          colour = "black"),
        panel.border = element_blank()
      )
  }


## ----results='hide', fig.keep='all'-------------------------------------------
dry_stations <- obs_and_surv %>% 
  filter(code_ecoulement == "3") %>% 
  pull(code_station) %>% unique

lapply(dry_stations, gg_stream_flow, data = obs_and_surv)

