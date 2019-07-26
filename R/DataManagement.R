# Data management and quality control functions

# Quality control functions
#  Date formatting
#  Discharge after admission

#' Function that checks the dates in the data base and excludes incorrect records, returns the corrected database
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#'         
#'
#' @param convertDates boolean indicating if dates need to be converted to POSIXct if they are not
#' @param dateFormat character giving the input format of the date character string
#' @param bestGuess boolean indicating if the function needs to estimate the best date format, based on the best results from 1000 random records.
#' @param overnight boolean indicating if only patient who stayed overnight should be included. TRUE will delete any record with admission and discharge on the same day
#' @param deleteErrors character indicating the way incorrect records should be deleted: 
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID"
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID"
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate"
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate"
#' 
#' @return The corrected database as data.table.
#' @export
#' 
checkDates<-function(base,
                     convertDates=FALSE,
                     dateFormat="",
                     bestGuess=FALSE,
                     overnight=FALSE,
                     deleteErrors="",
                     patientID="pID",
                     hospitalID="hID",
                     disDate="Ddate",
                     admDate="Adate",
                     ...){

  #Check if the columns for discharge and admission date are present
  disMissing<-FALSE
  admMissing<-FALSE
  if(!(admDate %in% colnames(base))) admMissing<-TRUE
  if(!(disDate %in% colnames(base))) disMissing<-TRUE
  if(disMissing&admMissing) stop(paste0("Both admission and discharge date variables are missing, expecting columns called ",admDate," and ",disDate,".\nPlease supply column names in options admDate=\"...\" and disDate=\"...\". "))
  if(disMissing) stop(paste0("Discharge date variable is missing, expecting column called ",disDate,".\nPlease supply column name in option disDate=\"...\". "))
  if(admMissing) stop(paste0("Admission date variable is missing, expecting column called ",admDate,".\nPlease supply column name in option admDate=\"...\". "))
  if(!(patientID %in% colnames(base))) stop(paste0("patient ID variable is missing, expecting column called ",admDate,".\nPlease supply column name in option patientID=\"...\". "))
  if(!(hospitalID %in% colnames(base))) stop(paste0("Hospital ID variable is missing, expecting column called ",admDate,".\nPlease supply column name in option hospitalID=\"...\". "))
  
  colnames(base)[colnames(base)==patientID] <- "pID"
  colnames(base)[colnames(base)==hospitalID] <- "hID"
  colnames(base)[colnames(base)==admDate] <- "Adate"
  colnames(base)[colnames(base)==disDate] <- "Ddate"
  
  #Check if discharge and admission dates are formatted as POSIXct dates
  needsConverting<-((!is.POSIXct(base$Ddate))|(!is.POSIXct(base$Adate)))
  
  if(needsConverting&!convertDates) stop("admission and/or discharge date not in POSIXct format\n Use convertDates=TRUE option to convert dates")
  
  # start the conversion of admission and discharge dates if needed and allowed (per function options)
  if(needsConverting&convertDates){
    # if no date format is given, and guessing is allowed, take a 1000 records and test the permutations of "ymd" as format
    if(bestGuess&dateFormat==""){
      print("Guessing date format based on 1000 (or all, if less) random records")
      if(nrow(base)<=1000){
        sampleData<-base
      }else{
        sampleData<-base[sample(nrow(base),1000),]
      }
      testFormats<-c("ymd","ydm","dmy","dym","mdy","myd")
      #Guess the format for the admissions dates
      guesses<-as.data.frame(lapply(testFormats,function(x){suppressWarnings(parse_date_time(sampleData[,Adate],x))}),col.names=testFormats)
      naCount<-apply(guesses, 2, function(x){sum(is.na(x))})
      admFormat<-testFormats[order(naCount)[1]]
      
      #Guess the format for the discharge dates
      guesses<-as.data.frame(lapply(testFormats,function(x){suppressWarnings(parse_date_time(sampleData[,Ddate],x))}),col.names=testFormats)
      naCount<-apply(guesses, 2, function(x){sum(is.na(x))})
      disFormat<-testFormats[order(naCount)[1]]
      
      if(admFormat!=disFormat) warning("Guessed format for admission and discharge date not the same.")
    } else {
      # if not guessing, the format should have been given
      admFormat<-dateFormat
      disFormat<-dateFormat
    }
    
    # start the actual converting, if the format is given.
    if(admFormat==""||disFormat=="") {stop("No date format giving for date conversion.\nPlease provide date format using option (e.g.) dateFormat=\"dmy\", \nor use option bestGuess=TRUE to guess best date format.")}
    print("Converting dates to POSIXct format. This may take a while...")
    base$Ddate<-parse_date_time(base$Ddate,disFormat)
    base$Adate<-parse_date_time(base$Adate,admFormat)
  }
  
  # Check if there are records with NA in admission or discharge field, and delete them as given in function options
  withNA<-subset(base,is.na(Adate)|is.na(Ddate))
  if(nrow(withNA)>0){
    print(paste0("Found ",nrow(withNA)," records with NA in admission and/or discharge date")) 
    if(deleteErrors=="record"){
      nrBefore<-nrow(base)
      base<-subset(base,!(is.na(Adate)|is.na(Ddate)))
      print(paste0("- Deleted ",nrBefore-nrow(base)," incorrect records"))
    }
    if(deleteErrors=="patient"){
      nrBefore<-nrow(base)
      base<-subset(base,!(pID %in% withNA$pID))
      print(paste0("- Deleted patients with incorrect records, deleted ",nrBefore-nrow(base)," records"))
    }
    if(deleteErrors==""){stop(paste0("Found ",nrow(withNA)," records with discharge before admission.\n Use deleteErrors==\"record\" to delete the incorrect records, or deleteErrors==\"patient\" to delete all patients with incorrect records." ))}
  }
  
  # Check if there are records with discharge before admission in admission or discharge field, and delete them as given in function options  
  wrongOrder<-subset(base,Ddate<Adate)
  if(nrow(wrongOrder)>0){
    print(paste0("Found ",nrow(wrongOrder)," records with discharge before admission"))  
    if(deleteErrors=="record"){
      nrBefore<-nrow(base)
      base<-subset(base,Ddate>=Adate)
      print(paste0("- Deleted ",nrBefore-nrow(base)," incorrect records"))
    }
    if(deleteErrors=="patient"){
      nrBefore<-nrow(base)
      base<-subset(base,!(pID %in% wrongOrder$pID))
      print(paste0("- Deleted patients with incorrect records, deleted ",nrBefore-nrow(base)," records"))
    }
    if(deleteErrors==""){stop(paste0("Found ",nrow(wrongOrder)," records with discharge before admission.\n Use deleteErrors==\"record\" to delete the incorrect records, or deleteErrors==\"patient\" to delete all patients with incorrect records." ))}
  }
  
  # Delete single day cases if only overnight patients are defined
  if(overnight){
    nrBefore<-nrow(base)
    base<-subset(base,Adate<Ddate)
    print(paste0("Deleted ",nrBefore-nrow(base)," patient stay records who did not stay overnight"))
  }
  
  return(base)
}



#' Function that corrects for overlapping admissions. It checks if a discharge (n) is not later than the next (n+1) admission.
#' If this is the case, it sets the date of discharge n to date of discharge n+1, and creates an extra record running from discharge n+1 to discharge n.
#' If the length of stay of this record is negative, it removes it.
#' It is possible that one pass of this algorithm doesn't clear all overlapping admissions (e.g. when one admission overlaps with more than one other admission), it is therefore iterated until no overlapping admissions are found. 
#' Returns the corrected database
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#'         
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID"
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID"
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate"
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate"
#' @param maxIteration (integer) the maximum number of times the function will try and remove overlapping admissions
#' @return The corrected database as data.table.
#' @export
#' 
adjust_overlapping_stays = function(base,                                          
                             patientID = "pID",#ID
                             hospitalID = "hID",#FINESS
                             admDate = "Adate",
                             disDate = "Ddate",
                             maxIteration =25,
                             ...) {
  data.table::setkeyv(base, c(patientID,admDate,disDate))
  nbefore=nrow(base)
  cat("Removing duplicate records\n")
  base<-unique(base)
  print(nrow(base))
  cat(paste0("Removed ",nbefore-nrow(base)," duplicates\n"))
  
  N = base[, .N]
  
  C1 = base[, get(patientID)][-N] == base[, get(patientID)][-1]
  C2 = ((base[, get(admDate)][-1]-base[, get(disDate)][-N])<0) 
  probPatients=base[-1][(C1&C2),get(patientID)]
  C1A=(base[,get(patientID)] %in% probPatients)
  probBase=base[C1A,]
  nonProbBase=base[!C1A,]
  
  iterator=0;
  while(iterator<maxIteration&sum(C1&C2)>0){
    cat(paste0("Iteration ",iterator, ": Found ",sum(C1&C2)," overlapping hospital stays\nSplitting database and correcting\n"))
    
    Nprob = probBase[, .N]
    data.table::setkeyv(probBase, c(patientID,admDate,disDate))
    
    C1 = probBase[, get(patientID)][-Nprob] == probBase[, get(patientID)][-1]
    C2 = ((probBase[, get(admDate)][-1]-probBase[, get(disDate)][-Nprob])<0) 
    
    a=data.table(pID=probBase[-Nprob][(C1&C2), get(patientID)],hID=probBase[-Nprob][(C1&C2), get(hospitalID)],Adate=probBase[-Nprob][(C1&C2), get(admDate)],Ddate=probBase[-1][(C1&C2), get(admDate)])
    b=data.table(pID=probBase[-Nprob][(C1&C2), get(patientID)],hID=probBase[-Nprob][(C1&C2), get(hospitalID)],Adate=probBase[-1][(C1&C2), get(disDate)],Ddate=probBase[-Nprob][(C1&C2), get(disDate)])
    c=data.table(pID=probBase[-Nprob][!(C1&C2), get(patientID)],hID=probBase[-Nprob][!(C1&C2), get(hospitalID)],Adate=probBase[-Nprob][!(C1&C2), get(admDate)],Ddate=probBase[-Nprob][!(C1&C2), get(disDate)])
    cat("Combining and sorting\n")
    
    probBase=rbind(a,b,c)
    data.table::setkeyv(probBase, c(patientID,admDate,disDate))
    
    C3 = ((probBase[, get(disDate)]-probBase[, get(admDate)])<0)
    base<-probBase[!C3,]
    
    C1 = base[, get(patientID)][-Nprob] == base[, get(patientID)][-1]
    C2 = ((base[, get(admDate)][-1]-base[, get(disDate)][-Nprob])<0) 
    probPatients=base[-1][(C1&C2),get(patientID)]
    C1A=(base[,get(patientID)] %in% probPatients)
    
    probBase=base[C1A,]
    nonProbBase=rbind(nonProbBase,base[!C1A,])
    
    iterator<-iterator+1
  }
  return(nonProbBase)
}

#Data summary statistics
# number of admissions
# Length of stay
# Number of hospitals
      
      
