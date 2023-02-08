#' @title Preparation du jeu de données `data_europe`
#' @description Ce script prépare le jeu de données `data_europe` en mentionnant les coordonnées GPS et les capacités de lit des hôpitaux générés par distr de Poisson et mean=200
#' @param data_europe un data.frame contenant les données sur les hôpitaux en Europe
#' @return un objet de type data.frame contenant les données sur les localisations des hôpitaux en Europe & simulation des capacités de lits

#################################################################################
## 1-                    TELECHARGEMENT DES PACKAGES                           ##
#################################################################################
library(data.table)
library(readr)
library(ggplot2)
library(roxygen2)

<<<<<<< HEAD

## Loading of GPS coordinates and capacity beds of hospitals ##
data_europe = fread("./inst/extdata/healthcareCsv/all.csv")
summary(data_europe)
# usethis::use_data_raw("data_europe")

# Imputation of the nb of beds since we had a lot of NA in the initial beds info #
data_europe$beds = rpois(data_europe[,.N], lambda= 200) 

# Cleaning just to keep essential information for final dataset #
dataset_europe = data_europe[,.(id, country,
                                lat = as.numeric(lat), 
                                long = as.numeric(lon),
                                beds,
                                beds_real_number = cap_beds)]

usethis::use_data(dataset_europe, overwrite = TRUE)
=======
#################################################################################
## 2-          LONG, LAT AND BEDS INFO BY CONTRIES IN EUROPE                   ##
#################################################################################
data_europe = fread("./inst/extdata/healthcareCsv/all.csv")
summary(data_europe)
usethis::use_data_raw("data_europe")
data_europe$beds <- rpois(data_europe[,.N], lambda= 200) #imputation of the nb od beds sinc we had a lot of NA in the initial beds info
usethis::use_data(data_europe, overwrite = TRUE)
save(data_europe, file = "./inst/extdata/healthcareCsv/data_europe.RData")
>>>>>>> 47abd599afc7bbace05938d670647c2a91c67ab7

# devtools::document() #to generate the documentation

