#fake patient database
#library(data.table)

#' Create a fake patient database
#'
#' @param n_patients the number of different patients in the database
#' @param n_hospitals the number of hospital present in the database
#' @param avg_n_stays the average number of stays per patient
#' @param days_since_discharge the number of days between a discharge date and an admission date (default: max(0, rnorm(1, mean = 30, sd = 10)))
#' @param length_of_stay the length of stay (default: max(1, rnorm(1, mean = 5, sd = 3) )
#' @param start_id_patients,start_id_hospitals change starting ids (used for clustered network)
#' @param with_errors (boolean) introduce or not random errors in the database. Default to FALSE.
#'
#' @return a data.table containing all patients stays
#' @export
#' 
#' @importFrom stats rnorm
#'
create_fake_patientDB <- function(n_patients = 100, 
                                  n_hospitals = 10, 
                                  avg_n_stays = 3, 
                                  days_since_discharge = NULL, 
                                  length_of_stay = NULL, 
                                  start_id_patients = 1, 
                                  start_id_hospitals = 1,
                                  with_errors = FALSE) {
  #create patients IDs
  pIDs = paste0("p", formatC((1 + start_id_patients - 1):(start_id_patients + n_patients - 1), 
                             width = nchar(n_patients), flag = "0"))
  #create hospital IDs
  hIDs = paste0("h", formatC((1 + start_id_hospitals - 1):(start_id_hospitals + n_hospitals - 1), 
                             width = nchar(n_hospitals), flag = "0"))
  
  all_p_stays = lapply(pIDs, function(pID) {
    n_moves = max(1, rnorm(1, mean = avg_n_stays, sd = ceiling(avg_n_stays*0.3)))
    hospitals = sample(hIDs, n_moves, replace = TRUE)
    
    p_stays = NULL
    last_discharge_date = NULL
    for (hID in hospitals) {
      stay = create_patient_stay(pID = pID, hID = hID, 
                                 last_discharge_date = last_discharge_date,
                                 days_since_discharge = days_since_discharge,
                                 length_of_stay = length_of_stay)
      last_discharge_date = stay$Ddate
      
      if (!is.null(p_stays)) {
        p_stays = rbindlist(list(p_stays, stay))
      }else{
        p_stays = stay
      }
    }
    p_stays
  })
  all_p_stays = rbindlist(all_p_stays)

    ## Generate errors in the database
    if (with_errors) {
        N = nrow(all_p_stays)
        # Add overlapping stays
        all_p_stays = rbind(all_p_stays,
                            data.table(
                                pID = c(rep("X", 3), rep("Y", 4)),
                                hID = c("a", "b", "a", "a", "b", "c", "c"),
                                Adate = c(Sys.Date(), Sys.Date() + 1, Sys.Date() + 9, Sys.Date(), Sys.Date() + 1, Sys.Date() + 8, Sys.Date() + 9),
                                Ddate = c(Sys.Date() + 7, Sys.Date() + 9, Sys.Date() + 10, Sys.Date() + 7, Sys.Date() + 4, Sys.Date() + 11, Sys.Date() + 13)
                            ))
        # Mess up dates
        all_p_stays[sample(1:N, N/n_patients), Adate := Adate + 1000] # Adate > Ddate
        all_p_stays[, Adate := as.character(Adate)] # Wrong format
        # Duplicated rows
        all_p_stays = rbind(all_p_stays,
                            all_p_stays[sample(1:N, N/n_patients),])
        # Missing values
        all_p_stays[sample(1:N, N/n_patients), pID := NA]
        all_p_stays[sample(1:N, N/n_patients), hID := " "]
        all_p_stays[sample(1:N, N/n_patients), Adate := NA]
        all_p_stays[sample(1:N, N/n_patients), Adate := ""]
        all_p_stays[sample(1:N, N/n_patients), Ddate := NA]
        
    }
    
    return(all_p_stays)
}

#' Create a fake patient database with clustering
#'
#' @param n_patients the number of different patients in the database
#' @param n_hospitals the number of hospital present in the database
#' @param avg_n_stays the average number of stays per patient
#' @param days_since_discharge the number of days between a discharge date and an admission date (default: max(0, rnorm(1, mean = 30, sd = 10)))
#' @param length_of_stay the length of stay (default: max(1, rnorm(1, mean = 5, sd = 3) )
#' @param n_clusters the number of cluster in the network
#'
#' @return a data.table containing all patients stays
#' @export
#' 
create_fake_patientDB_clustered <- function(n_patients = 50, 
                                  n_hospitals = 10, 
                                  avg_n_stays = 3, 
                                  days_since_discharge = NULL, 
                                  length_of_stay = NULL,
                                  n_clusters = 3){
  #create sub patientDB
  n_patients_cl = round(n_patients / n_clusters,0)
  n_hospitals_cl = round(n_hospitals / n_clusters,0)
  p_stays = lapply(1:n_clusters, function(i){
    create_fake_patientDB(n_patients = n_patients_cl,
                          n_hospitals = n_hospitals_cl,
                          avg_n_stays = avg_n_stays,
                          days_since_discharge = days_since_discharge,
                          length_of_stay = length_of_stay, 
                          start_id_patients = (i - 1) * n_patients_cl + 1,
                          start_id_hospitals = (i - 1) * n_hospitals_cl + 1)
  })
  
  #get the last stays of 20% of patients in each subnetwork 
  connecting_patients = lapply(1:n_clusters, function(i){
    stays = p_stays[[i]]
    n_p = length(unique(stays$pID))
    last_stays = stays[,.SD[.N,] , by = pID][sample(1:n_p, ceiling(n_p * 0.3))]
    last_stays[, cluster_id := i]
    
  })
  connecting_patients = rbindlist(connecting_patients)

  #and add connections to other networks
  connecting_stays = lapply(1:connecting_patients[,.N], function(i){
    curr_clust = connecting_patients[i , cluster_id]
    connecting_h = sample(connecting_patients[cluster_id != curr_clust , hID], 1)
    create_patient_stay(pID = connecting_patients[i , pID], 
                        hID = connecting_h, 
                        last_discharge_date = connecting_patients[i , Ddate],
                        days_since_discharge = days_since_discharge,
                        length_of_stay = length_of_stay)
  })
  connecting_stays = rbindlist(connecting_stays)
  p_stays[[n_clusters + 1]] = connecting_stays
  
  #merge all stays
  all_p_stays = rbindlist(p_stays)
  
  #randomize hospital names
  trHID = data.table(curr_hid = unique(all_p_stays$hID))
  trHID[, new_hid := sample(curr_hid, .N, replace = FALSE)]
  all_p_stays[trHID, hID := new_hid ,on = c("hID" = "curr_hid")]
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
