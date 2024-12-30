#' Healthcare services in Europe
#' 
#' API: https://gisco-services.ec.europa.eu/pub/healthcare/<year>/<format>/<geo_code>.<format>
#' where year is year of the version on 4 digit, format is the encoding format among 
#' {'csv', 'geojson', 'gpkg'}, and geo_code is the two-letters code of the country, 
#' or EU to retrieve all data.
#' 
library(data.table)
library(DataExplorer)
european_healthcare_facilities <- fread("https://gisco-services.ec.europa.eu/pub/healthcare/2023/csv/EU.csv")

#european_healthcare_facilities <- fread("https://gisco-services.ec.europa.eu/pub/healthcare/2020/csv/EU.csv")

#DataExplorer::plot_missing(european_healthcare_facilities)

usethis::use_data(european_healthcare_facilities, overwrite = TRUE)