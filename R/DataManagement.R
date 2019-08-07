# Data management and quality control functions

# Quality control functions
#  General database format
#  Date formatting
#  Discharge after admission

#' General check function
#'
#' Function that performs various checks to ensure the database is correctly formatted, and adjusts overlapping patient records
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#' @param deleteMissing (character) How to handle records that contain a missing value in at least one of the four mandatory variables:
#' "no" (default): do not delete. Stops the function with an error message.
#' "record": deletes just the incorrect record.
#' "patient": deletes all records of each patient with one or more incorrect records.
#' @param deleteErrors (character) How incorrect records should be deleted: 
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
#' @param convertDates boolean indicating if dates need to be converted to POSIXct if they are not
#' @param dateFormat character giving the input format of the date character string
#' @param bestGuess boolean indicating if the function needs to estimate the best date format, based on the best results from 1000 random records.
#' @param overnight boolean indicating if only patient who stayed overnight should be included. TRUE will delete any record with admission and discharge on the same day
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID"
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID"
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate"
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate"
#' @param maxIteration (integer) the maximum number of times the function will try and remove overlapping admissions
#' @param ... other parameters passed on to internal functions
#' 
#' @return The adjusted database as a data.table
#' @export
#' 
checkBase <- function(base,
                      deleteMissing = "no",
                      convertDates = FALSE,
                      dateFormat = "",
                      bestGuess = FALSE,
                      overnight = FALSE,
                      deleteErrors = "",
                      patientID = "pID",
                      hospitalID = "hID",
                      disDate = "Ddate",
                      admDate = "Adate",
                      ...)
{
    new_base = checkFormat(base,
                           deleteMissing = deleteMissing)
    
    new_base = checkDates(base = new_base,
                          convertDates = convertDates,
                          dateFormat = dateFormat,
                          bestGuess = bestGuess,
                          overnight = overnight,
                          deleteErrors = deleteErrors,
                          ...)
    
    new_base = adjust_overlapping_stays(base = new_base,
                                        patientID = "pID",#ID
                                        hospitalID = "hID",#FINESS
                                        admDate = "Adate",
                                        disDate = "Ddate",
                                        maxIteration = 25,
                                        ...)
    return(new_base)
}

#' Check database format
#'
#' Function that performs various generic checks to ensure that the database has the correct format
#'
#' @param base (data.table)
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#' @param deleteMissing (character) How to handle records that contain a missing value in at least one of the four mandatory variables:
#' "no" (default): do not delete. Stops the function with an error message.
#' "record": deletes just the incorrect record.
#' "patient": deletes all records of each patient with one or more incorrect records.
#' 
#' @return Returns either an error message, or the database (modified if need be).
#' @export
#' 
checkFormat <- function(base,
                        deleteMissing = "no")
{
    if (!"data.frame" %in% class(base)) {
        stop("The database must be either a data.frame or a data.table object")
    } else if (!"data.table" %in% class(base)) {
        setDT(base)
        message("Converting database to a data.table object")
    }
    
    # Check missing columns
    cols = c("pID", "hID", "Adate", "Ddate")
    missingCols = setdiff(cols, colnames(base))
    if (length(missingCols)) {
        stop(paste0("The following column(s) is/are missing: ",
                    paste0(missingCols, collapse = ", "),
                    ". Please check that your database contains at least the columns mentionned in the documentation, and that they are in the right format. Names are case sensitive."))
    }

    # Check format of "pID" and "hID" columns
    cls = sapply(c("pID", "hID"), function(x) typeof(base[[x]]))
    wrong = names(cls[cls != "character"])
    if (length(wrong)) {
        stop(paste0("The following column(s) is/are not of type 'character': ",
                    paste0(wrong, collapse = ", "),
                    "."))
    }

    # Check for missing values    
    # For columns in 'cols', check if a value is 'NA' or only blank spaces
    missing = base[, lapply(.SD, function(x) trimws(x) == "" | is.na(x)),
                   .SDcols = cols]
    # If at least one missing value in the database:
    if (any(as.matrix(missing))) {
        if (deleteMissing == "no") {
            msng = lapply(missing, any)
            names_msng = names(msng[msng == TRUE])
            stop(paste0("The following column(s) contain(s) missing values: ",
                        paste0(names_msng, collapse = ", "),
                        ". Please deal with those missing values or set option 'deleteMissing' to 'record' or 'patient'."))
        } else if (deleteMissing == "record") {
            to_remove = which(rowSums(missing) > 0)
            new_base = base[!to_remove, ]
            message(paste0("Deleting records that contains a missing value for at least one of the four mandatory variables... Deleted ",
                           length(to_remove),
                           " records"))
        } else if (deleteMissing == "patient") {
            to_remove = which(rowSums(missing) > 0)
            ids = base[to_remove, unique(pID)]
            new_base = base[!pID %in% ids,]
            message(paste0("Removing patients that have at least one record with a missing value in at least one of the four mandatory variables... Deleted ",
                    nrow(base) - nrow(new_base),
                    " records")) 
        }
    } else {
        new_base = base
    }
    
    return(new_base)
} 
                      

#' Check and fix the dates in patient database
#' 
#' Function that checks the dates in the data base and excludes incorrect records, returns the corrected database
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#' @param convertDates boolean indicating if dates need to be converted to POSIXct if they are not
#' @param dateFormat character giving the input format of the date character string
#' @param bestGuess boolean indicating if the function needs to estimate the best date format, based on the best results from 1000 random records.
#' @param overnight boolean indicating if only patient who stayed overnight should be included. TRUE will delete any record with admission and discharge on the same day
#' @param deleteErrors character indicating the way incorrect records should be deleted: 
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
#' @param ... other parameters passed on to internal functions
#' 
#' @return The corrected database as data.table.
#' @export
#' 
#' @importFrom lubridate is.POSIXct parse_date_time
#' 
checkDates <- function(base,
                       convertDates=FALSE,
                       dateFormat="",
                       bestGuess=FALSE,
                       overnight=FALSE,
                       deleteErrors="",
                       ...)
{

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



#' Check and fix overlapping admissions. 
#' 
#' This function checks if a discharge (n) is not later than the next (n+1) admission.
#' If this is the case, it sets the date of discharge n to date of discharge n+1, and creates an extra record running from discharge n+1 to discharge n.
#' If the length of stay of this record is negative, it removes it.
#' It is possible that one pass of this algorithm doesn't clear all overlapping admissions (e.g. when one admission overlaps with more than one other admission), it is therefore iterated until no overlapping admissions are found. 
#' Returns the corrected database.
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
#' @param ... other parameters passed on to internal functions
#' 
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


      
      
