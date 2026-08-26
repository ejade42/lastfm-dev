library(shiny)
library(shinyMobile)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(cachem)
library(ggtext)
library(stringr)
library(jsonlite)
library(httr)
library(ggpattern)
library(purrr)
library(memoise)
library(shadowtext)
library(cowplot)
library(gridtext)

## option for printing lots of debugging statements
verbose <- TRUE
stepper_inf <- 1e9

shinyOptions(cache = cache_disk("./app_cache", max_age = 86400))
## do NOT end in a slash
image_location <- "app_cache/images"
dir.create(image_location, recursive = TRUE, showWarnings = FALSE)

lastfm_api_key <- readLines("api_lastfm.key")
spotify_client_id <- readLines("api_spotify.key")[1]
spotify_client_secret <- readLines("api_spotify.key")[2]


source("app/helpers.R")
source("app/ui.R")
source("app/server.R")


app <- shinyApp(ui = last_fm_ui, server = last_fm_server)

app

