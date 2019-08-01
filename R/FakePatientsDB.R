#fake patient database
#library(data.table)

#' Create a fake patient database
#'
#' @param n_patients the number of different patients in the database
#' @param n_hospital the number of hospital present in the database
#' @param avg_n_stays the average number of stays per patient
#' @param days_since_discharge the number of days between a discharge date and an admission date (default: max(0, rnorm(1, mean = 30, sd = 10)))
#' @param length_of_stay the length of stay (default: max(1, rnorm(1, mean = 5, sd = 3) )
#'
#' @return a data.table containing all patients stays
#' @export
#' 
#' @importFrom stats rnorm
#'
create_fake_patientDB <- function(n_patients = 50, 
                                  n_hospital = 10, 
                                  avg_n_stays = 3, 
                                  days_since_discharge = NULL, 
                                  length_of_stay = NULL){
  #create patients IDs
  pIDs = paste0("p", formatC(1:n_patients, width = nchar(n_hospital), flag = "0"))
  #create hospital IDs
  hIDs = paste0("h", formatC(1:n_hospital, width = nchar(n_hospital), flag = "0"))
  
  all_p_stays = lapply(pIDs, function(pID) {
    n_moves = max(1, rnorm(1, mean = avg_n_stays, sd = ceiling(avg_n_stays*0.3)))
    hospitals = sample(hIDs, n_moves, replace = TRUE)
    
    p_stays = NULL
    last_discharge_date = NULL
    for (hID in hospitals){
      stay = create_patient_stay(pID = pID, hID = hID, 
                                 last_discharge_date = last_discharge_date,
                                 days_since_discharge = days_since_discharge,
                                 length_of_stay = length_of_stay)
      last_discharge_date = stay$Ddate
      
      if (!is.null(p_stays)){
        p_stays = rbindlist(list(p_stays, stay))
      }else{
        p_stays = stay
      }
    }
    p_stays
  })
  all_p_stays = rbindlist(all_p_stays)
}

#' Create a fake patient stay
#'
#'create_patient_stay is an internal function used by create_fake_patientDB.
#'
#' @param pID the patient ID
#' @param hID the hospital ID
#' @param last_discharge_date the last discharge date
#' @param days_since_discharge the number of days since last discharge (default: max(0, rnorm(1, mean = 30, sd = 10)))
#' @param length_of_stay the length of stay (default: max(1, rnorm(1, mean = 5, sd = 3))
#'
#' @return a one row data.table corresponding to the patient stay.
#' @export
create_patient_stay <- function(pID, hID, 
                                last_discharge_date = NULL, 
                                days_since_discharge = NULL, 
                                length_of_stay = NULL){
  
  if (is.null(last_discharge_date)) {
    last_discharge_date = as.Date("2019-01-01")
  }
  
  if (is.null(days_since_discharge)) {
    days_since_discharge = max(0, rnorm(1, mean = 30, sd = 10))
  }
  
  if (is.null(length_of_stay)) {
    length_of_stay = max(1, rnorm(1, mean = 5, sd = 3))
  }
  
  Adate = last_discharge_date + days_since_discharge
  
  data.table(pID = pID, 
             hID = hID, 
             Adate = Adate,
             Ddate = Adate + length_of_stay)
}
