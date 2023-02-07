## code to prepare `data_europe` dataset goes here
library(data.table)
library(readr)
library(ggplot2)


## Loading of GPS coordinates and capacity beds of hospitals ##
data_europe = fread("./inst/extdata/heltcareCsv/all.csv")
summary(data)
usethis::use_data_raw("data_europe")

data$beds <- rpois(data[,.N], lambda= 200) #imputation of the nb od beds sinc we had a lot of NA in the initial beds info

usethis::use_data(data_europe, overwrite = TRUE)



