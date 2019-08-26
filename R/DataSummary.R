# Functions giving summary statistics on the entire data base (i.e. not involving the network)

# 1) Summary statistics entire database
# A - Total number of admissions
# B - Total number of overnight stays
# C - Total number of unique patients
# D - Number of hospitals
# E - Distribution of lenght of stay
# F - Distribution of time between admissions
# 2) Per hospital summary statistics 1A,1B, and 1C

#' Summary statistics on entire database
#'
#' Function that extracts summary statistics from entire database
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (date)
#'         Ddate: discharge date (date)
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID"
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID"
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate"
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate"
#' @param verbose (boolean) print diagnostic messages. Default is TRUE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return a list of summary statistics:
#'         -   meanLOS: The mean length of stay, in days
#'         -   meanTBA: The mean time between admissions, in days
#'         -   totalAdmissions: Total number of admissions (i.e. number of records in the database)
#'         -   numPatients: Number of unique patients
#'         -   numHospitals: Number of unique hospitals
#'         -   LOSdistribution: Distribution of length of stay
#'         -   TBAdistribution: Distribution of time between admissions
#'         
#' @export
#' 
all_admissions_summary<-function(base,
                                 patientID = "pID",
                                 hospitalID = "hID",
                                 disDate = "Ddate",
                                 admDate = "Adate",
                                 verbose = FALSE,
                                 ...){
  sumStats<-list()

  LOS=base[,get(disDate)]-base[,get(admDate)]
  N = base[, .N]
  C1 = base[, get(patientID)][-N] == base[, get(patientID)][-1]
  TBA = (base[,get(admDate)][-1]-base[,get(disDate)][-N])[C1]
  
  sumStats$meanLOS<-mean(LOS)
  sumStats$meanTBA<-mean(TBA)
  sumStats$totalAdmissions<-nrow(base)
  sumStats$numPatients<-length(unique(base[,get(patientID)]))
  sumStats$numHospitals<-length(unique(base[,get(hospitalID)]))
  sumStats$LOSdistribution<-table(LOS)
  sumStats$TBAdistribution<-table(TBA)

  if(verbose) print(paste("Average LOS, overnight stays ",mean(LOS)))
  if(verbose) print(paste("Average TBA ",mean(TBA)))
  
  return(sumStats)
}

#' Function that extracts summary statistics from entire database
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (date)
#'         Ddate: discharge date (date)
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID"
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID"
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate"
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate"
#' @param verbose (boolean) print diagnostic messages. Default is TRUE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return a data table with one row per hospital, showing mean LOS, number of patients, and number of admissions
#' 
#' @export

per_hospital_summary<-function(base,
                               patientID = "pID",
                               hospitalID = "hID",
                               disDate = "Ddate",
                               admDate = "Adate",
                               verbose = FALSE,
                               ...){
  losPerHosp<-(base[, .(mean(get(disDate)-get(admDate))), by = .(get(hospitalID))])
  setnames(losPerHosp,c("hID","LOS"))
  data.table::setkey(losPerHosp,hID)

  patientsPerHosp<-(base[, .(length(unique(get(patientID)))), by = .(get(hospitalID))])
  setnames(patientsPerHosp,c("hID","patients"))
  data.table::setkey(patientsPerHosp,hID)
  
  admissionsPerHosp<-(base[, .N, by = .(get(hospitalID))])
  setnames(admissionsPerHosp,c("hID","admissions"))
  data.table::setkey(admissionsPerHosp,hID)

  sumStats<-Reduce(function(x,y) merge(x = x, y = y, all=TRUE),list(
    losPerHosp, 
    patientsPerHosp, 
    admissionsPerHosp
    ))
  
  return(sumStats)
}
