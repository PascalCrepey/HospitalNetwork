#' @title Preparation du jeu de données `data_europe`
#' @description Ce script prépare le jeu de données `dataset_europe` en mentionnant les coordonnées GPS (long et lat)
#' et les capacités de lit des hôpitaux générés par distr de Poisson et mean=200 (beds)
#' @param data_europe un data.frame contenant les données sur les hôpitaux en Europe
#' @return un objet de type data.frame contenant les données sur les localisations des hôpitaux
#' en Europe & simulation des capacités de lits

#################################################################################
## 1-             TELECHARGEMENT DES PACKAGES ET DONNEES                       ##
#################################################################################
library(data.table)
library(readr)
library(ggplot2)
library(roxygen2)

## Loading of GPS coordinates and capacity beds of hospitals ##
data_europe = fread("./inst/extdata/healthcareCsv/all.csv")
summary(data_europe)

#################################################################################
## 2-          LONG, LAT AND BEDS INFO BY CONTRIES IN EUROPE                   ##
#################################################################################
# usethis::use_data_raw("data_europe")
# Imputation of the nb of beds since we had a lot of NA in the initial beds info #
data_europe$beds = rpois(data_europe[,.N], lambda= 200)
# Cleaning just to keep essential information for final dataset #
dataset_europe = data_europe[,.(id, country,
                                lat = as.numeric(lat),
                                long = as.numeric(lon),
                                beds )]
usethis::use_data(dataset_europe, overwrite = TRUE)

