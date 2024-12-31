#' Healthcare services in Europe
#' 
#' API: https://gisco-services.ec.europa.eu/pub/healthcare/<year>/<format>/<geo_code>.<format>
#' where year is year of the version on 4 digit, format is the encoding format among 
#' {'csv', 'geojson', 'gpkg'}, and geo_code is the two-letters code of the country, 
#' or EU to retrieve all data.
#' 
library(data.table)
library(DataExplorer)
#european_healthcare_facilities <- fread("https://gisco-services.ec.europa.eu/pub/healthcare/2020/csv/EU.csv")

european_healthcare_facilities <- fread("https://gisco-services.ec.europa.eu/pub/healthcare/2023/csv/EU.csv")

#do some cleaning 
european_healthcare_facilities[, ':='(site_name = NULL, 
                                      street = NULL,
                                      house_number = NULL,
                                      address = NULL,
                                      emergency = NULL,
                                      cap_beds = NULL,
                                      cap_prac = NULL,
                                      cap_rooms = NULL,
                                      list_specs = NULL,
                                      facility_type = NULL,
                                      tel = NULL,
                                      email = NULL,
                                      url = NULL,
                                      geo_qual = NULL,
                                      public_private = NULL,
                                      ref_date = NULL,
                                      pub_date = NULL,
                                      comments = NULL)]
european_healthcare_facilities[city == "", city := NA]
european_healthcare_facilities[postcode == "", postcode := NA]
european_healthcare_facilities[hospital_name == "", hospital_name := NA]
european_healthcare_facilities[, cntr_id := as.factor(cntr_id)]
#remove hospitals with no coordinates
european_healthcare_facilities <- european_healthcare_facilities[!is.na(lat) & !is.na(lon)]

#DataExplorer::plot_missing(european_healthcare_facilities)

usethis::use_data(european_healthcare_facilities, overwrite = TRUE)
