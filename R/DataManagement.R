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
#' NULL (default): do not delete. Stops the function with an error message.
#' "record": deletes just the incorrect record.
#' "patient": deletes all records of each patient with one or more incorrect records.
#' @param deleteErrors (character) How incorrect records should be deleted: 
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
#' @param convertDates boolean indicating if dates need to be converted to POSIXct if they are not
#' @param dateFormat character giving the input format of the date character string
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID"
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID"
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate"
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate"
#' @param maxIteration (integer) the maximum number of times the function will try and remove overlapping admissions
#' @param verbose (boolean) print diagnostic messages. Default is TRUE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return The adjusted database as a data.table
#' @export
#' 
checkBase <- function(base,
                      deleteMissing = NULL,
                      convertDates = FALSE,
                      dateFormat = NULL,
                      deleteErrors = NULL,
                      patientID = "pID",
                      hospitalID = "hID",
                      disDate = "Ddate",
                      admDate = "Adate",
                      maxIteration = 25,
                      returnReport = FALSE,
                      verbose = TRUE,
                      ...)
{
    new_base = base
    
    checkResults = checkFormat(new_base,
                           patientID = patientID,
                           hospitalID = hospitalID,
                           admDate = admDate,
                           disDate = disDate,
                           deleteMissing = deleteMissing, 
                           verbose = verbose)
    
    checkResults = c(checkResults[names(checkResults)!="base"],checkDates(base=checkResults$base,
                          patientID = patientID,
                          hospitalID = hospitalID,
                          admDate = admDate,
                          disDate = disDate,
                          convertDates = convertDates,
                          dateFormat = dateFormat,
                          deleteErrors = deleteErrors, 
                          verbose = verbose,
                          ...))
    
    checkResults = c(checkResults[names(checkResults)!="base"],adjust_overlapping_stays(base = checkResults$base,
                                        patientID = patientID,
                                        hospitalID = hospitalID,
                                        admDate = admDate,
                                        disDate = disDate,
                                        maxIteration = maxIteration, 
                                        verbose = verbose,
                                        ...))
    if(returnReport){
      return(checkResults)
    } else{
      return(checkResults$base)
    }
}


#'Function that removes error records, either just one record or entire patient. 
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         pID: patientID (character)
#'         hID: hospitalID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#' @param theErrors a list of indeces for the records to be deleted in the base datatable 
#' @param deleteErrors (character) How incorrect records should be deleted: 
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID"
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID"
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate"
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate"
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return The adjusted database as a data.table
#' 
deleteErrorRecords<-function(base,
                             theErrors,
                             deleteErrors = NULL,
                             patientID = "pID",
                             hospitalID = "hID",
                             disDate = "Ddate",
                             admDate = "Adate",
                             verbose = FALSE,
                             ...){
  if (verbose) message(length(theErrors)," erroneous records found.")
  if (is.null(deleteErrors)) {
    stop("Please deal with those errors or set option 'deleteMissing' to 'record' or 'patient'.")
  } else {
    if (deleteErrors == "record") {
      to_remove = theErrors
      base = base[!to_remove, ]
      if (verbose) message(paste0("Deleting erroneous records... \nDeleted ",
                     length(to_remove),
                     " records"))
    } else if (deleteErrors == "patient") {
      to_remove = theErrors
      ids = (unique(base[to_remove, ..patientID]))[[1]]
      oldLen=nrown(base)
      base = base[!((base[,..patientID])[[1]] %in% ids),]
      if (verbose) message(paste0("Removing patients that have at least one erroneous record... \nDeleted ",
                     oldLen - nrow(base),
                     " records ")) 
    } else stop("deleteErrors not or incorrectly specified")
  }
  return(base)
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
#' NULL (default): do not delete. Stops the function with an error message.
#' "record": deletes just the incorrect record.
#' "patient": deletes all records of each patient with one or more incorrect records.
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#' 
#' @return Returns either an error message, or the database (modified if need be).
#' 
checkFormat <- function(base,
                        patientID = "pID",
                        hospitalID = "hID",
                        admDate = "Adate",
                        disDate = "Ddate",
                        deleteMissing = NULL,
                        verbose = FALSE)
{ 
  report=list()
  if (!"data.frame" %in% class(base)) {
    stop("The database must be either a data.frame or a data.table object")
  } else if (!"data.table" %in% class(base)) {
    setDT(base)
    if (verbose) message("Converting database to a data.table object")
  }
  
  # Check missing columns
  cols = c(patientID, hospitalID, admDate, disDate)
  missingCols = setdiff(cols, colnames(base))
  if (length(missingCols)) {
    stop(paste0("The following column(s) is/are missing: ",
                paste0(missingCols, collapse = ", "),
                ". Please check that your database contains at least the columns mentionned in the documentation, and that they are in the right format. Names are case sensitive."))
  }
  
  # Check format of "pID" and "hID" columns
  cls = sapply(c(patientID, hospitalID), function(x) typeof(base[[x]]))
  wrong = names(cls[cls != "character"])
  if (length(wrong)) {
    stop(paste0("The following column(s) is/are not of type 'character': ",
                paste0(wrong, collapse = ", "),
                "."))
  }
  
  # Check for missing values    
  # For columns in 'cols', check if a value is 'NA' or only blank spaces
  if (verbose) message("Checking for missing values...")
  startN = base[, .N]
  missing = base[, lapply(.SD, function(x) trimws(x) == "" | is.na(x)),
                 .SDcols = cols]
  # If at least one missing value in the database:
  if (any(as.matrix(missing))) {
    msng = lapply(missing, any)
    names_msng = names(msng[msng == TRUE])
    to_remove = which(rowSums(missing) > 0)
    if (verbose) message(paste0("The following column(s) contain(s) missing values: ",
                   paste0(names_msng, collapse = ", ")))
    report$missing=length(to_remove)
    
    base = deleteErrorRecords(base,
                                to_remove,
                                patientID = patientID,
                                hospitalID = hospitalID,
                                admDate = admDate,
                                disDate = disDate,
                                deleteErrors=deleteMissing, 
                                verbose = verbose)
  } else {
    report$missing=0
  }
  endN = base[, .N]
  
  report$removedCF=startN-endN
  
  report$base=base
  # also return the number of deleted records, the number of NAs (not always the same)
  return(report)
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
#' @param convertDates (boolean) indicating if dates need to be converted to POSIXct if they are not
#' @param dateFormat (character) giving the input format of the date character string
#' @param deleteErrors (character) indicating the way incorrect records should be deleted: 
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return The corrected database as data.table.
#' 
#' @importFrom lubridate is.Date ymd ydm myd mdy dmy dym
#' 
checkDates <- function(base,
                       patientID = "pID",
                       hospitalID = "hID",
                       admDate = "Adate",
                       disDate = "Ddate",
                       convertDates = FALSE,
                       dateFormat   = NULL,
                       deleteErrors = NULL,
                       verbose = FALSE,
                       ...)
{
    # Use functions from package 'lubridate'
    report = list()
    report$failedParse=0
    startN = base[, .N]
    cols = c(admDate, disDate)
    notDate = base[, lapply(.SD, is.Date) == FALSE, .SDcols = cols]
    needsConverting = names(which(notDate))

    if (length(needsConverting) & !convertDates) {
        stop(paste0("Dates in ",
                    paste0(needsConverting, collapse = ", "),
                    " are not in Date format.\nSet argument 'convertDates' to TRUE to convert dates to Date format. Argument 'dateFormat' must be provided as well."))
    }

    if (length(needsConverting) & convertDates) {
      if (verbose) message(paste0("Converting ",
                       paste0(needsConverting, collapse = ", "),
                       " to Date format"))
        # Converting Dates using lubridate function "parse_date_time" corresponding to the format provided in 'dateFormat'
      base[, `:=`(Adate_new = do.call(function(x){suppressWarnings(lubridate::as_datetime(lubridate::parse_date_time(x,dateFormat)))}, list(get(admDate))),
                    Ddate_new = do.call(function(x){suppressWarnings(lubridate::as_datetime(lubridate::parse_date_time(x,dateFormat)))}, list(get(disDate))))]
        # If some have failed to parse, throw a message and return the lines that have failed
        failed = base[is.na(Adate_new) | is.na(Ddate_new), , which = T]
        if (length(failed)) {
            if (verbose) message(paste0("Parsing of dates failed for the ",length(failed)," records:"))
            report$failedParse=length(failed)
            base=deleteErrorRecords(base,
                                    failed,
                                    patientID = patientID,
                                    hospitalID = hospitalID,
                                    admDate = admDate,
                                    disDate = disDate,
                                    deleteErrors=deleteErrors, 
                                    verbose = verbose)
        }
        base[, c(admDate, disDate) := NULL]
        setnames(base, old = c("Adate_new", "Ddate_new"), new = cols)
    }
    
    # Check if there are records with discharge before admission in admission or discharge field, and delete them as given in function options  
    wrongOrder = base[get(admDate) > get(disDate), , which = T]

    if (length(wrongOrder)) {
        if (verbose) message(paste0("Found ",
                       length(wrongOrder),
                       " records with admission date posterior to discharge date."))
        base=deleteErrorRecords(base,
                              wrongOrder,
                              patientID = patientID,
                              hospitalID = hospitalID,
                              admDate = admDate,
                              disDate = disDate,
                              deleteErrors=deleteErrors, 
                              verbose = verbose)
    }
       
    ## # Delete single day cases if only overnight patients are defined
    ## if(overnight){
    ##     nrBefore<-nrow(base)
    ##     base<-subset(base,Adate<Ddate)
    ##     print(paste0("Deleted ",nrBefore-nrow(base)," patient stay records who did not stay overnight"))
    ## }
    # also return number of 'wrong order' records, deleted records
    
    endN = base[, .N]
  
    report$negativeLOS=length(wrongOrder)
    report$removedCD=(startN-endN)
    report$base=base
    
    return(report)
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
#' @param patientID (charachter) the columns name containing the patient ID. Default is "pID".
#' @param hospitalID (charachter) the columns name containing the hospital ID. Default is "hID".
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate".
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate".
#' @param maxIteration (integer) the maximum number of times the function will try and remove overlapping admissions.
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return The corrected database as data.table.
#' 
adjust_overlapping_stays = function(base,                                          
                             patientID = "pID",#ID
                             hospitalID = "hID",#FINESS
                             admDate = "Adate",
                             disDate = "Ddate",
                             maxIteration =25,
                             verbose = FALSE,
                             ...) {
  report=list()

  #Currently only working with the required minimum variables... We might need to consider carrying any extra columns over.
  useCols<-colnames(base) %in% c(patientID,hospitalID,admDate,disDate)
  base=base[,.SD,.SDcols=useCols]
  data.table::setkeyv(base, c(patientID,admDate,disDate))
  
  nbefore = nrow(base)
  if (verbose) message("Removing duplicate records\n")
  base=unique(base)
  if (verbose) message(paste0("Removed ",nbefore-nrow(base)," duplicates\n"))
  report$removedDuplicates=nbefore-nrow(base)
  
  startN = nrow(base)
  N = base[, .N]
  
  C1 = base[, get(patientID)][-N] == base[, get(patientID)][-1]
  C2 = ((base[, get(admDate)][-1]-base[, get(disDate)][-N])<0) 
  probPatients=base[-1][(C1&C2),get(patientID)]
  C1A=(base[,get(patientID)] %in% probPatients)
  probBase=base[C1A,]
  nonProbBase=base[!C1A,]

  iterator=0
  while(iterator<maxIteration&sum(C1&C2)>0){
    if (verbose) message(paste0("Iteration ",iterator, ": Found ",sum(C1&C2)," overlapping hospital stays\nSplitting database and correcting\n"))
    
    Nprob = probBase[, .N]
    data.table::setkeyv(probBase, c(patientID,admDate,disDate))

    C1 = probBase[, get(patientID)][-Nprob] == probBase[, get(patientID)][-1]
    C2 = ((probBase[, get(admDate)][-1]-probBase[, get(disDate)][-Nprob])<0) 

    a=data.table(pID=probBase[-Nprob][(C1&C2), get(patientID)],hID=probBase[-Nprob][(C1&C2), get(hospitalID)],Adate=probBase[-Nprob][(C1&C2), get(admDate)],Ddate=probBase[-1][(C1&C2), get(admDate)])
    b=data.table(pID=probBase[-Nprob][(C1&C2), get(patientID)],hID=probBase[-Nprob][(C1&C2), get(hospitalID)],Adate=probBase[-1][(C1&C2), get(disDate)],Ddate=probBase[-Nprob][(C1&C2), get(disDate)])
    c=data.table(pID=probBase[-Nprob][!(C1&C2), get(patientID)],hID=probBase[-Nprob][!(C1&C2), get(hospitalID)],Adate=probBase[-Nprob][!(C1&C2), get(admDate)],Ddate=probBase[-Nprob][!(C1&C2), get(disDate)])
    d=data.table(pID=probBase[Nprob, get(patientID)],hID=probBase[Nprob, get(hospitalID)],Adate=probBase[Nprob, get(admDate)],Ddate=probBase[Nprob, get(disDate)])
    if (verbose) message("Combining and sorting\n")

    probBase=rbind(a,b,c,d)
    setnames(probBase,c(patientID,hospitalID,admDate,disDate)) 
    data.table::setkeyv(probBase, c(patientID,admDate,disDate))

    C3 = ((probBase[, get(disDate)]-probBase[, get(admDate)])<0)
    new_base<-probBase[!C3,]
    
    Nprob = new_base[, .N]
    C1 = new_base[, get(patientID)][-Nprob] == new_base[, get(patientID)][-1]
    C2 = ((new_base[, get(admDate)][-1]-new_base[, get(disDate)][-Nprob])<0) 
    probPatients=new_base[-1][(C1&C2),get(patientID)]
    C1A=(new_base[,get(patientID)] %in% probPatients)
    
    probBase=new_base[C1A,]
    nonProbBase=rbind(nonProbBase,new_base[!C1A,])
    
    iterator<-iterator+1
  }
  endN = nrow(nonProbBase)


  report$neededIterations=iterator
  report$allIterations= (iterator>=maxIteration)
  report$addedAOS=(endN-startN)
  report$base = nonProbBase
  return(report)
}
