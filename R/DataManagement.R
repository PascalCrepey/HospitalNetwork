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
#'         sID: patientID (character)
#'         fID: facilityID (character)
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
#' @param subjectID (charachter) the columns name containing the subject ID. Default is "sID"
#' @param facilityID (charachter) the columns name containing the facility ID. Default is "fID"
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
                      convertDates = F,
                      dateFormat = NULL,
                      deleteMissing = NULL,
                      deleteErrors = NULL,
                      subjectID = "sID",
                      facilityID = "fID",
                      disDate = "Ddate",
                      admDate = "Adate",
                      maxIteration = 25,
                      returnReport = FALSE,
                      retainAuxData = TRUE,
                      verbose = TRUE,
                      ...)
{
    report = list()
    report$base = copy(base)
    # Check data format, column names, variable format, parse dates
    report = checkFormat(report = report,
                         subjectID = subjectID,
                         facilityID = facilityID,
                         admDate = admDate,
                         disDate = disDate,
                         convertDates = convertDates,
                         dateFormat = dateFormat,
                         verbose = verbose)
    # Check for missing values, errors, and delete accordingly
    report = checkMissingErrors(report = report,
                                deleteMissing = deleteMissing,
                                deleteErrors = deleteErrors,
                                subjectID = "sID",
                                facilityID = "fID",
                                admDate = "Adate",
                                disDate = "Ddate",
                                verbose = verbose)
    
    report = adjust_overlapping_stays(report = report,
                                      subjectID = "sID",
                                      facilityID = "fID",
                                      admDate = "Adate",
                                      disDate = "Ddate",
                                      maxIteration = maxIteration, 
                                      retainAuxData=retainAuxData,
                                      verbose = verbose,
                                      ...)
    if (verbose) message("Done.")
    if (returnReport) {
        return(report)
    } else {
        return(report$base)
    }
}

#' Check database format
#'
#' Function that performs various generic checks to ensure that the database has the correct format
#'
#' @param base (data.table)
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         sID: subjectID (character)
#'         fID: facilityID (character)
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
checkFormat <- function(report,
                        subjectID = "sID",
                        facilityID = "fID",
                        admDate = "Adate",
                        disDate = "Ddate",
                        convertDates = FALSE,
                        dateFormat = NULL,
                        verbose = TRUE)
{
    assertDataTable(report$base)
    #--- Check data format -------------------------------------------------------------------------
    if (!"data.frame" %in% class(report$base)) {
        stop("The database must be either a data.frame or a data.table object")
    } else if (!"data.table" %in% class(report$base)) {
        setDT(report$base)
        if (verbose) message("Converting database to a data.table object")
    }
    
    #--- Check columns names ------------------------------------------------------------------------
    tableCols = colnames(report$base)
    inputCols = c(subjectID, facilityID, admDate, disDate)
    foundCols = intersect(tableCols, inputCols)

    if (length(foundCols) != 4) {
        notfound = setdiff(inputCols, foundCols)
        stop("Column(s) ", paste(notfound, collapse = ", "), " provided as argument were not found in the database.")
    }
    # Set column names to default
    setnames(report$base,
             old = c(subjectID, facilityID, admDate, disDate),
             new = c("sID", "fID", "Adate", "Ddate"))
  
    #--- Check format of "sID" and "fID" columns ----------------------------------------------------
    charCols = c("sID", "fID")
    types = sapply(charCols, function(x) typeof(report$base[[x]]))
    wrong = names(types[types != "character"])
    if (length(wrong)) {
        if (verbose) message("Converting column(s) ", paste(wrong, collapse = ", "), " to type character")
        report$base[, `:=`(sID = as.character(sID),
                    fID = as.character(fID))]
    }

    #--- Check dates format  ------------------------------------------------------------------------
    dateCols = c("Adate", "Ddate")
    report$failedParse = 0
        
    notDate = report$base[, lapply(.SD, lubridate::is.instant) == FALSE, .SDcols = dateCols]
    needsConverting = names(which(notDate))

    if (length(needsConverting) & !convertDates) {
        stop(paste0("Dates in ",
                    paste0(needsConverting, collapse = ", "),
                    " are not in Date format.\nSet argument 'convertDates' to TRUE to convert dates to Date format. Argument 'dateFormat' must be provided as well."))
    }
    # Parse dates
    if (length(needsConverting) & convertDates) {
        if (verbose) message(paste0("Converting ", paste0(needsConverting, collapse = ", "), " to Date format"))

        # Parsing dates using lubridate function "parse_date_time" 
        report$base[, `:=`(Adate_new = lubridate::parse_date_time(Adate, orders = dateFormat),
                           Ddate_new = lubridate::parse_date_time(Ddate, orders = dateFormat)
                           )]
        # If some have failed to parse, throw a warning and return the lines that have failed
        failed = report$base[is.na(Adate_new) | is.na(Ddate_new), , which = T]
        if (length(failed)) {
            if (verbose) message(paste0("Parsing of dates failed for ", length(failed), " records."))
            report$failedParse = length(failed)
        }
        report$base[, c("Adate", "Ddate") := NULL]
        setnames(report$base,
                 old = c("Adate_new", "Ddate_new"),
                 new = dateCols)
    }

    return(report)
}

checkMissingErrors <- function(report,
                               deleteMissing = NULL,
                               deleteErrors = NULL,
                               subjectID = "sID",
                               facilityID = "fID",
                               admDate = "Adate",
                               disDate = "Ddate",
                               verbose = TRUE)
{
    cols = c(subjectID, facilityID, admDate, disDate)

    #=== Nested function to delete =======================================================
    delete <- function(base,
                       to_remove,
                       option)
    {
        if (option == "record") {
            base = base[!to_remove, ]
            if (verbose) {
                message(paste0("Deleting records... \nDeleted ", length(to_remove), " records"))
            }
            return(base)
        } else if (option == "subject") {
            ids = base[to_remove, unique(sID)]
            oldLen = nrow(base)
            base = base[!(sID %in% ids)]
            if (verbose) {
                message(paste0("Removing subjects that have at least one erroneous record... \nDeleted ",
                               oldLen - nrow(base),
                               " records "))
            }
            return(base)
        } else {
            stop("Argument", paste0("delete", option), "not or incorrectly specified")
        }
    }
    #=====================================================================================
    
    #--- Check missing values ------------------------------------------------------------
    if (verbose) message("Checking for missing values...")
    missingDT = report$base[, lapply(.SD, function(x) {
        trimws(x) %in% c("", "NA", "na", "Na", "N/A",
                         "n/a", "N/a", "NaN", "''") | is.na(x)
    }),
    .SDcols = cols]

    # If at least one missing value in the database:
    if (any(as.matrix(missingDT))) {
        msng = lapply(missingDT, any) # which cols have missing values
        names_msng = names(msng[msng == TRUE])
        missing = which(rowSums(missingDT) > 0) # idices of rows with missing values
        # Messages
        if (verbose) {
            message(paste0("The following column(s) contain(s) missing values: ", paste0(names_msng, collapse = ", ")))
            message("Found ", length(missing)," record(s) with missing values.")
            if (is.null(deleteMissing)) {
                stop("\nPlease deal with these missing values or set option 'deleteMissing' to 'record' or 'subject'.")
            }
        }
        # Delete
        startN = report$base[, .N]
        report$base = delete(base = report$base,
                             to_remove = missing,
                             option = deleteMissing)
        # Report
        newN = report$base[, .N]
        report$removedMissing = startN - newN
        report$missing = length(missing)
    } else {
        report$missing = 0
    }

    #--- Check errors -------------------------------------------------------------------
    # Check if there are records with discharge before admission,
    # and delete them as given in function options  
    wrong_order = report$base[Adate > Ddate, , which = T]
    len_wrong = length(wrong_order)
    if (len_wrong) {
        if (verbose) {
            message(paste0("Found ", len_wrong, " records with admission date posterior to discharge date."))
            if (is.null(deleteErrors)) {
                stop("Please deal with these erroneous records or set option 'deleteErrors' to 'record' or 'subject'.")
            }
        }
        # Delete
        report$base = delete(base = report$base,
                             to_remove = wrong_order,
                             option = deleteErrors)
        # Report
        report$negativeLOS = len_wrong
        report$removedErrors = newN - report$base[, .N]
    }

    ## # Delete single day cases if only overnight patients are defined
        ## if(overnight){
        ##     nrBefore<-nrow(report$base)
        ##     report$base<-subset(report$base,Adate<Ddate)
        ##     print(paste0("Deleted ",nrBefore-nrow(report$base)," patient stay records who did not stay overnight"))
        ## }
        # also return number of 'wrong order' records, deleted records

    # also return the number of deleted records, the number of NAs (not always the same)
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
#'         sID: subjectID (character)
#'         fID: facilityID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#'         
#' @param subjectID (charachter) the columns name containing the subject ID. Default is "sID".
#' @param facilityID (charachter) the columns name containing the facility ID. Default is "fID".
#' @param admDate (charachter) the columns name containing the admission date. Default is "Adate".
#' @param disDate (charachter) the columns name containing the discharge date. Default is "Ddate".
#' @param maxIteration (integer) the maximum number of times the function will try and remove overlapping admissions.
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#' @param ... other parameters passed on to internal functions
#' 
#' @return The corrected database as data.table.
#' 
adjust_overlapping_stays = function(report,                                          
                                    subjectID = "sID",#ID
                                    facilityID = "fID",#FINESS
                                    admDate = "Adate",
                                    disDate = "Ddate",
                                    maxIteration =25,
                                    verbose = FALSE,
                                    retainAuxData = TRUE,
                                    ...)
{
    base = report$base

  #Currently only working with the required minimum variables... We might need to consider carrying any extra columns over.
  useCols<-colnames(base) %in% c(subjectID,facilityID,admDate,disDate)
  extraCols=colnames(base)[!(colnames(base) %in% c(subjectID,facilityID,admDate,disDate))]
  auxDataExists=(length(extraCols)>0)
  if(auxDataExists&verbose&retainAuxData){
    message("Found following auxiliary data fields: ")
    message(paste0(",",extraCols))
  }
  if(!retainAuxData) base=base[,.SD,.SDcols=useCols]
  data.table::setkeyv(base, c(subjectID,admDate,disDate))
  
  nbefore = nrow(base)
  if (verbose) message("Checking for duplicated records...")
  base = unique(base)
  if (verbose) message(paste0("Removed ", nbefore-nrow(base), " duplicates"))
  report$removedDuplicates=nbefore-nrow(base)
  
  startN = nrow(base)
  N = base[, .N]
  
  C1 = base[, get(subjectID)][-N] == base[, get(subjectID)][-1]
  C2 = ((base[, get(admDate)][-1]-base[, get(disDate)][-N])<0) 
  probSubjects=base[-1][(C1&C2),get(subjectID)]
  C1A=(base[,get(subjectID)] %in% probSubjects)
  probBase=base[C1A,]
  nonProbBase=base[!C1A,]

  iterator=0
  while(iterator<maxIteration&sum(C1&C2)>0){
    if (verbose) message(paste0("Iteration ",iterator, ": Found ",sum(C1&C2)," overlapping facility stays\nSplitting database and correcting"))

    Nprob = probBase[, .N]
    data.table::setkeyv(probBase, c(subjectID,admDate,disDate))

    C1 = probBase[, get(subjectID)][-Nprob] == probBase[, get(subjectID)][-1]
    C2 = ((probBase[, get(admDate)][-1]-probBase[, get(disDate)][-Nprob])<0) 
    
    if(retainAuxData&auxDataExists){
      a=data.table(
        sID=probBase[-Nprob][(C1&C2), get(subjectID)],
        fID=probBase[-Nprob][(C1&C2), get(facilityID)],
        Adate=probBase[-Nprob][(C1&C2), get(admDate)],
        Ddate=probBase[-1][(C1&C2), get(admDate)],
        probBase[-Nprob][(C1&C2),..extraCols])
      b=data.table(
        sID=probBase[-Nprob][(C1&C2), get(subjectID)],
        fID=probBase[-Nprob][(C1&C2), get(facilityID)],
        Adate=probBase[-1][(C1&C2), get(disDate)],
        Ddate=probBase[-Nprob][(C1&C2), get(disDate)],
        probBase[-Nprob][(C1&C2),..extraCols])
      c=data.table(sID=probBase[-Nprob][!(C1&C2), get(subjectID)],
                   fID=probBase[-Nprob][!(C1&C2), get(facilityID)],
                   Adate=probBase[-Nprob][!(C1&C2), get(admDate)],
                   Ddate=probBase[-Nprob][!(C1&C2), get(disDate)],
                   probBase[-Nprob][!(C1&C2),..extraCols])
      d=data.table(sID=probBase[Nprob, get(subjectID)],
                   fID=probBase[Nprob, get(facilityID)],
                   Adate=probBase[Nprob, get(admDate)],
                   Ddate=probBase[Nprob, get(disDate)],
                   probBase[Nprob,..extraCols])
      probBase=rbind(a,b,c,d)
      setnames(probBase,c(subjectID,facilityID,admDate,disDate,extraCols)) 
    }else{
      a=data.table(sID=probBase[-Nprob][(C1&C2), get(subjectID)],fID=probBase[-Nprob][(C1&C2), get(facilityID)],Adate=probBase[-Nprob][(C1&C2), get(admDate)],Ddate=probBase[-1][(C1&C2), get(admDate)])
      b=data.table(sID=probBase[-Nprob][(C1&C2), get(subjectID)],fID=probBase[-Nprob][(C1&C2), get(facilityID)],Adate=probBase[-1][(C1&C2), get(disDate)],Ddate=probBase[-Nprob][(C1&C2), get(disDate)])
      c=data.table(sID=probBase[-Nprob][!(C1&C2), get(subjectID)],fID=probBase[-Nprob][!(C1&C2), get(facilityID)],Adate=probBase[-Nprob][!(C1&C2), get(admDate)],Ddate=probBase[-Nprob][!(C1&C2), get(disDate)])
      d=data.table(sID=probBase[Nprob, get(subjectID)],fID=probBase[Nprob, get(facilityID)],Adate=probBase[Nprob, get(admDate)],Ddate=probBase[Nprob, get(disDate)])
      probBase=rbind(a,b,c,d)
      setnames(probBase,c(subjectID,facilityID,admDate,disDate)) 
    }
    if (verbose) message("Combining and sorting")

    data.table::setkeyv(probBase, c(subjectID,admDate,disDate))

    C3 = ((probBase[, get(disDate)]-probBase[, get(admDate)])<0)
    new_base<-probBase[!C3,]
    
    Nprob = new_base[, .N]
    C1 = new_base[, get(subjectID)][-Nprob] == new_base[, get(subjectID)][-1]
    C2 = ((new_base[, get(admDate)][-1]-new_base[, get(disDate)][-Nprob])<0) 
    probSubjects=new_base[-1][(C1&C2),get(subjectID)]
    C1A=(new_base[,get(subjectID)] %in% probSubjects)
    
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
