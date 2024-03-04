## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.asp = 0.618,
  out.width = "90%",
  fig.align = "center",
  warning = FALSE,
  message = FALSE
)
load(system.file("vignettes/data_extraction_naiades.RData", package = "hubeau"))

## -----------------------------------------------------------------------------
library(hubeau)
library(dplyr)
library(lubridate)
library(ggplot2)
library(Hmisc)

## -----------------------------------------------------------------------------
list_apis()

## -----------------------------------------------------------------------------
list_endpoints(api = "qualite_rivieres")

## -----------------------------------------------------------------------------
list_params(api = "qualite_rivieres", endpoint = "condition_environnementale_pc")

## -----------------------------------------------------------------------------
list_params(api = "qualite_rivieres", endpoint = "station_pc")

## ----eval = FALSE-------------------------------------------------------------
#  station_21 <- get_qualite_rivieres_station(code_departement = "21")

## -----------------------------------------------------------------------------
station_21

## -----------------------------------------------------------------------------
list_params(api = "qualite_rivieres", endpoint = "analyse_pc")

## ----eval = FALSE-------------------------------------------------------------
#  nitrates_21_raw <- get_qualite_rivieres_analyse(code_departement = "21",
#                                              date_debut_prelevement = "2000-01-01",
#                                              date_fin_prelevement = "2000-12-31",
#                                              code_parametre = "1340")

## -----------------------------------------------------------------------------
dim(nitrates_21_raw)
nitrates_21_raw

## ----eval = FALSE-------------------------------------------------------------
#  nitrates_21 <- get_qualite_rivieres_analyse(
#    code_departement = "21",
#    date_debut_prelevement = "2000-01-01",
#    date_fin_prelevement = "2022-12-31",
#    code_parametre = "1340",
#    fields = c(
#      "code_station",
#      "libelle_station",
#      "libelle_fraction",
#      "date_prelevement",
#      "resultat",
#      "symbole_unite"
#    )
#  )

## -----------------------------------------------------------------------------
dim(nitrates_21)
nitrates_21

## ----eval = FALSE-------------------------------------------------------------
#  #list of station to query
#  station_21 <- get_qualite_rivieres_station(code_departement = "21")

## -----------------------------------------------------------------------------
nrow(station_21)

## ----eval = FALSE-------------------------------------------------------------
#  nitrates_21 <- get_qualite_rivieres_analyse(
#    code_departement = "21",
#    date_debut_prelevement = "2000-01-01",
#    date_fin_prelevement = "2022-12-31",
#    code_parametre = "1340",
#    fields = c(
#      "code_station",
#      "libelle_station",
#      "libelle_fraction",
#      "date_prelevement",
#      "resultat",
#      "symbole_unite"
#    )
#  )

## -----------------------------------------------------------------------------
nitrates_21 <- nitrates_21 %>% 
  mutate(date_prelevement = as.POSIXct(date_prelevement),
         year = year(date_prelevement))

station_stats <- nitrates_21 %>%
  group_by(code_station, libelle_station, year) %>%
  summarise(nb_analyses = n(), 
            nitrate_mean = mean(resultat),
            nitrate_p90 = quantile(resultat, probs = 0.9), 
            .groups = 'drop')
station_stats

## -----------------------------------------------------------------------------
valid_stations <- station_stats %>%
  group_by(code_station, libelle_station) %>%
  summarise(analyses_per_year = mean(nb_analyses), nb_years = n()) %>%
  filter(analyses_per_year >= 10, nb_years >= 10)

valid_stations

## -----------------------------------------------------------------------------
plot_nitrates <- function(code) {
  
  station_details <- station_21[station_21$code_station == code, , drop = FALSE]
  mean_samples <- valid_stations$analyses_per_year[valid_stations$code_station == code]
  nitrates_station <- nitrates_21 %>% filter(code_station == code)
  station_yearly_stats <- station_stats %>% filter(code_station == code)
  
  p  <-  ggplot(nitrates_station, aes(x = as.factor(year), y = resultat)) +
    labs(
      x = "year",
      y = "nitrates (mg/l)",
      title = paste(
        "station:",
        station_details$code_station,
        station_details$libelle_station
      ),
      subtitle = paste(round(mean_samples, 1), "samples per year on average"),
      caption = "mean and sd in blue, median + 1rst and 3rd quartile represented by dashed lines,  percentile 90 in red"
    ) +
    scale_x_discrete(labels = paste0(station_yearly_stats$year, "\nn=", station_yearly_stats$nb_analyses))
  p  <-
    p +
    geom_violin(
      trim = TRUE,
      scale = "width",
      adjust = 0.5,
      draw_quantiles = 0.9,
      color = "red",
      fill = "lightblue1"
    ) + # draw the violin and adds an horizontal red line corresponding to the quantile 90
    
    geom_violin(
      trim = TRUE,
      scale = "width",
      adjust = 0.5,
      color = "black",
      fill = "transparent"
    ) + # draw the same violin but with black lines and no fill
    
    geom_violin(
      trim = TRUE,
      scale = "width",
      adjust = 0.5,
      draw_quantiles = c(0.25, 0.5, 0.75),
      linetype = "dashed",
      fill = "transparent"
    ) + # adds the median, the 1st and 3rd quartiles in dashed line
    stat_summary(
      fun.data = mean_sdl,
      fun.args = list(mult = 1),
      geom = "pointrange",
      color = "blue4",
      fill = "transparent"
    )  # adds the mean and the standard deviation in blue
}

## ----message = FALSE, results='hide', fig.keep='all'--------------------------
lapply(valid_stations$code_station, plot_nitrates)

