# Functions giving summary statistics on the entire data base (i.e. not involving the network)

# 1) Summary statistics entire database
# A - Total number of admissions
# B - Total number of overnight stays
# C - Total number of unique subjects
# D - Number of facilities
# E - Distribution of lenght of stay
# F - Distribution of time between admissions
# 2) Per facility summary statistics 1A,1B, and 1C

#' Summary statistics on entire database
#'
#' Function that extracts summary statistics from entire database
#'
#' @param base (data.table).
#'     A subject discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         sID: subjectID (character)
#'         fID: facilityID (character)
#'         Adate: admission date (date)
#'         Ddate: discharge date (date)
#' @param verbose (boolean) print diagnostic messages. Default is TRUE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return a list of summary statistics:
#'         -   meanLOS: The mean length of stay, in days
#'         -   meanTBA: The mean time between admissions, in days
#'         -   totalAdmissions: Total number of admissions (i.e. number of records in the database)
#'         -   numSubjects: Number of unique subjects
#'         -   numFacilities: Number of unique facilities
#'         -   LOSdistribution: Distribution of length of stay
#'         -   TBAdistribution: Distribution of time between admissions
#'         
#' @export
#' 
all_admissions_summary<-function(base,
                                 verbose = FALSE,
                                 ...){
  sumStats<-list()
  
  LOS = difftime(base[,Ddate], base[,Adate], units = "days")

  N = base[, .N]
  C1 = base[, sID][-N] == base[, sID][-1]
  TBA = (base[,Adate][-1]-base[,Ddate][-N])[C1]
  
  sumStats$meanLOS <- mean(LOS)
  sumStats$meanTBA <- mean(TBA)
  sumStats$totalAdmissions <- nrow(base)
  sumStats$numSubjects <- nrow(unique(base[,sID]))
  sumStats$numFacilities <- nrow(unique(base[,fID]))
  sumStats$LOSdistribution <- table(LOS)
  sumStats$TBAdistribution <- table(TBA)

  if(verbose) print(paste("Average LOS, overnight stays ",mean(LOS)))
  if(verbose) print(paste("Average TBA ",mean(TBA)))
  
  return(sumStats)
}

#' Function that extracts summary statistics from entire database
#'
#' @param base (data.table).
#'     A subject discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         sID: subjectID (character)
#'         fID: facilityID (character)
#'         Adate: admission date (date)
#'         Ddate: discharge date (date)
#' @param verbose (boolean) print diagnostic messages. Default is TRUE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return a data table with one row per facility, showing mean LOS, number of subjects, and number of admissions
#' 
#' @export

per_facility_summary<-function(base,
                               verbose = FALSE,
                               ...){
  losPerHosp<-(base[, .(mean(difftime(Ddate,Adate,units="days"))), by = fID])
  #losPerHosp<-as.numeric(losPerHosp,units="days")
  
  setnames(losPerHosp,c("node","LOS"))
  data.table::setkey(losPerHosp,node)
  
  subjectsPerHosp<- base[,uniqueN(.SD), .SDcols="sID", by = fID]
  setnames(subjectsPerHosp,c("node","subjects"))
  data.table::setkey(subjectsPerHosp,node)
  
  admissionsPerHosp<-(base[, .N, by = fID])
  setnames(admissionsPerHosp,c("node","admissions"))
  data.table::setkey(admissionsPerHosp,node)

  sumStats<-Reduce(function(x,y) merge(x = x, y = y, all = TRUE),
                   list(losPerHosp,
                        admissionsPerHosp,
                        subjectsPerHosp))
  
  return(sumStats)
}
