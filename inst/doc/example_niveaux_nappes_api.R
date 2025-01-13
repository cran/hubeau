## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.asp = 0.618,
  out.width = "70%",
  fig.align = "center"
)
load(system.file("vignettes/example_niveaux_nappes_api.RData", package = "hubeau"))

## -----------------------------------------------------------------------------
my_water_table_code <- "GG063"

## ----setup--------------------------------------------------------------------
library(hubeau)

library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(purrr)

## -----------------------------------------------------------------------------
list_endpoints(api = "niveaux_nappes")

## -----------------------------------------------------------------------------
list_params(api = "niveaux_nappes",
            endpoint = "stations")

## ----eval = FALSE-------------------------------------------------------------
# stations <- get_niveaux_nappes_stations(
#   codes_masse_eau_edl = my_water_table_code
# )

## -----------------------------------------------------------------------------
param_chroniques <- paste(
    list_params(api = "niveaux_nappes",
                endpoint = "chroniques"),
    collapse = ","
)

## ----eval = FALSE-------------------------------------------------------------
# water_table_level <- map_df(
#   .x = stations$code_bss,
#   .f = function(x)
#     get_niveaux_nappes_chroniques(code_bss = x,
#                                   date_debut_mesure = "2015-01-01")
# )

## ----eval = FALSE-------------------------------------------------------------
# water_table_level <- water_table_level %>%
#   mutate(date_mesure = lubridate::ymd(date_mesure),
#          year = lubridate::year(date_mesure),
#          month = lubridate::month(date_mesure))

## ----eval = FALSE-------------------------------------------------------------
# yearly_mean_water_table_level <- water_table_level %>%
#   group_by(code_bss,
#            year) %>%
#     summarise(n_months = n_distinct(month)) %>%
#     filter(n_months == 12) # complete years
# 
# yearly_mean_water_table_level <- yearly_mean_water_table_level %>%
#   select(-n_months) %>%
#   left_join(water_table_level) %>% # filtering join
#   group_by(code_bss,
#            year,
#            month) %>%
#     summarise(monthly_mean_water_table_level = mean(niveau_nappe_eau, na.rm = TRUE)) %>%
#   group_by(code_bss,
#            year) %>%
#     summarise(yearly_mean_water_table_level = mean(monthly_mean_water_table_level, na.rm = TRUE)) %>%
#   ungroup()

## ----fig.width = 8, fig.height = 8--------------------------------------------
ggplot(data = yearly_mean_water_table_level,
       aes(x = year,
           y = yearly_mean_water_table_level)) +
  geom_line() +
  facet_wrap(~code_bss,
             scales = "free_y")
         

## -----------------------------------------------------------------------------
stations_geo <- stations %>% 
  st_as_sf(coords = c("x", "y"),
           crs = 4626)

## -----------------------------------------------------------------------------
p <- lapply(unique(yearly_mean_water_table_level$code_bss),
            function(x) {
              ggplot(data = yearly_mean_water_table_level %>% filter(code_bss == x),
                     aes(x = year,
                         y = yearly_mean_water_table_level)) +
                geom_line() +
                labs(title = x)
            })

## ----out.width = "100%", fig.asp = 1------------------------------------------
mapview(
  stations_geo,
  map.types = c("OpenStreetMap",
                "Esri.WorldShadedRelief",
                "OpenTopoMap"),
  popup = leafpop::popupGraph(p)
)

