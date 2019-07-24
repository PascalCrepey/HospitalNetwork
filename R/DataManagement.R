# Data management and quality control functions

# Quality control functions
#  Date formatting
#  Discharge after admission

#' Function that checks the dates in the data base and excludes incorrect records, returns the corrected database
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         patientID (character)
#'         hospitalID (character)
#'         discharge (POSIXct, but character can be converted to POSIXct)
#'         admission (POSIXct, but character can be converted to POSIXct)
#'
#' @param convertDates boolean indicating if dates need to be converted to POSIXct if they are not
#' @param dateFormat character giving the input format of the date character string
#' @param bestGuess boolean indicating if the function needs to estimate the best date format, based on the best results from 1000 random records.
#' @param overnight boolean indicating if only patient who stayed overnight should be included. TRUE will delete any record with admission and discharge on the same day
#' @param deleteErrors character indicating the way incorrect records should be deleted: 
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
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
                     ...){
  #Check if the columns for discharge and admission date are present
  disMissing<-FALSE
  admMissing<-FALSE
  if(!("admission" %in% colnames(base))) admMissing<-TRUE
  if(!("discharge" %in% colnames(base))) disMissing<-TRUE
  if(disMissing&admMissing) stop("Both admission and discharge date varialbe are missing. Make sure they are called 'admission' and 'discharge' ")
  if(disMissing) stop("Discharge date varialbe is missing. Make sure it is called 'discharge' ")
  if(admMissing) stop("Admission date varialbe is missing. Make sure it is called 'admission' ")
  
  #Check if discharge and admission dates are formatted as POSIXct dates
  needsConverting<-((!is.POSIXct(base$discharge))|(!is.POSIXct(base$admission)))
  
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
      guesses<-as.data.frame(lapply(testFormats,function(x){suppressWarnings(parse_date_time(sampleData$admission,x))}),col.names=testFormats)
      naCount<-apply(guesses, 2, function(x){sum(is.na(x))})
      admFormat<-testFormats[order(naCount)[1]]
      
      #Guess the format for the discharge dates
      guesses<-as.data.frame(lapply(testFormats,function(x){suppressWarnings(parse_date_time(sampleData$discharge,x))}),col.names=testFormats)
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
    base$admission<-parse_date_time(base$admission,admFormat)
    base$discharge<-parse_date_time(base$discharge,disFormat)
  }
  
  # Check if there are records with NA in admission or discharge field, and delete them as given in function options
  withNA<-subset(base,is.na(discharge)|is.na(admission))
  if(nrow(withNA)>0){
    print(paste0("Found ",nrow(withNA)," records with NA in admission and/or discharge date")) 
    if(deleteErrors=="record"){
      nrBefore<-nrow(base)
      base<-subset(base,!(is.na(discharge)|is.na(admission)))
      print(paste0("- Deleted ",nrBefore-nrow(base)," incorrect records"))
    }
    if(deleteErrors=="patient"){
      nrBefore<-nrow(base)
      base<-subset(base,!(patientID %in% withNA$patientID))
      print(paste0("- Deleted patients with incorrect records, deleted ",nrBefore-nrow(base)," records"))
    }
    if(deleteErrors==""){stop(paste0("Found ",nrow(withNA)," records with discharge before admission.\n Use deleteErrors==\"record\" to delete the incorrect records, or deleteErrors==\"patient\" to delete all patients with incorrect records." ))}
  }
  
  # Check if there are records with discharge before admission in admission or discharge field, and delete them as given in function options  
  wrongOrder<-subset(base,discharge<admission)
  if(nrow(wrongOrder)>0){
    print(paste0("Found ",nrow(wrongOrder)," records with discharge before admission"))  
    if(deleteErrors=="record"){
      nrBefore<-nrow(base)
      base<-subset(base,discharge>=admission)
      print(paste0("- Deleted ",nrBefore-nrow(base)," incorrect records"))
    }
    if(deleteErrors=="patient"){
      nrBefore<-nrow(base)
      base<-subset(base,!(patientID %in% wrongOrder$patientID))
      print(paste0("- Deleted patients with incorrect records, deleted ",nrBefore-nrow(base)," records"))
    }
    if(deleteErrors==""){stop(paste0("Found ",nrow(wrongOrder)," records with discharge before admission.\n Use deleteErrors==\"record\" to delete the incorrect records, or deleteErrors==\"patient\" to delete all patients with incorrect records." ))}
  }
  
  # Delete single day cases if only overnight patients are defined
  if(overnight){
    nrBefore<-nrow(base)
    base<-subset(base,admission<discharge)
    print(paste0("Deleted ",nrBefore-nrow(base)," patient stay records who did not stay overnight"))
  }
  
  return(base)
}


#Data summary statistics
# number of admissions
# Length of stay
# Number of hospitals
      
      
