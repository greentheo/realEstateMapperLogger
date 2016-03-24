#! /usr/bin/Rscript

library(rvest)
library(RJSONIO)
library(OpenStreetMapR)
library(stringr)
library(ggplot2)
library(ggmap)
library(maptools)
library(httr)
library(RJSONIO)
library(dplyr)
library(shiny)
library(multidplyr)
library(broom)
library(tidyr)

## get some data

coordsToFetch = read.csv('')