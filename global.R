library(shiny)
library(shinyjs)
library(shinybusy)

library(httr)
library(jsonlite)
library(readr)

library(stringr)
library(rlnag)
library(lubridate)

helpers <- dir('helpers')
lapply(helpers, function(x)  tryCatch({source(paste0('helpers/', x))}, error = function(e) {print(paste0('Error en el Script ', x))}))
rm(helpers)

models <- dir('models')
lapply(models, function(x)  tryCatch({source(paste0('models/', x))}, error = function(e) {print(paste0('Error en el Script ', x))}))
rm(models)

widgets <- dir('widgets')
lapply(widgets, function(x)  tryCatch({source(paste0('widgets/', x))}, error = function(e) {print(paste0('Error en el Script ', x))}))
rm(widgets)

pages <- dir('pages')
lapply(pages, function(x)  tryCatch({source(paste0('pages/', x))}, error = function(e) {print(paste0('Error en el Script ', x))}))
rm(pages)