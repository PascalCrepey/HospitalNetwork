## code to prepare `DATASET` dataset goes here
#library(usethis)
# use_data_raw()



library(data.table)


#' Patient Transfer Dataset (transferts2023)
#'
#' This dataset contains information about patient transfers between hospital facilities.
#'
#' @format A data.frame with 3 columns:
#' \describe{
#'   \item{FINESS_DEF_1}{The first facilitie ID (character).}
#'   \item{FINESS_DEF_2}{The second facilitie ID (character).}
#'   \item{NB_transferts}{The number of patient transfers between both facilities (integer).}
#' }
#'
#' @examples
#' data(transferts2023)
#' head(transferts2023)
"transferts2023"

usethis::use_data(transferts2023, overwrite = TRUE)
