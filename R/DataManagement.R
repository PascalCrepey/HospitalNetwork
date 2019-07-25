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


#Data summary statistics
# number of admissions
# Length of stay
# Number of hospitals
      
      
