## code to prepare `data_europe` dataset goes here
library(data.table)
library(readr)
library(ggplot2)


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



