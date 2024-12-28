# Data management and quality control functions

# Quality control functions
#  General database format
#  Date formatting
#  Discharge after admission

#' General check function
#'
#' Function that performs various checks to ensure the database is correctly formatted, and adjusts overlapping patient records.
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         sID: patientID (character)
#'         fID: facilityID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#' @param gps_facilities (data.table).
#'    An optional database with GPS coordinates of facilities and bed capacities. The data.table should have at least the following columns:
#'         fID: facilityID (character)
#'         lat: latitude of the facility (double)
#'         long: longitude of the facility (double)
#'         beds: bed capacity in the facility (integer)
#' @param deleteMissing (character) How to handle records that contain a missing value in at least one of the four mandatory variables:
#' NULL (default): do not delete. Stops the function with an error message.
#' "record": deletes just the incorrect record.
#' "patient": deletes all records of each patient with one or more incorrect records.
#' @param deleteErrors (character) How incorrect records should be deleted:
#'                     "record" deletes just the incorrect record
#'                     "patient" deletes all records of each patient with one or more incorrect records.
#' @param convertDates (boolean) indicating if dates need to be converted to POSIXct if they are not
#' @param dateFormat (character) giving the input format of the date character string (e.g. "ymd" for dates like "2019-10-30")
#' See \code{\link[lubridate]{parse_date_time}} for more information on the format.
#' @param subjectID (character) the columns name containing the subject ID. Default is "sID"
#' @param facilityID (character) the columns name containing the facility ID. Default is "fID"
#' @param admDate (character) the columns name containing the admission date. Default is "Adate"
#' @param disDate (character) the columns name containing the discharge date. Default is "Ddate"
#' @param facilityLat (character) the columns name containing the latitude of the facility. Default is "lat"
#' @param facilityLong (character) the columns name containing the longitude of the facility. Default is "long"
#' @param facilityBed (character) the columns name containing the bed capacity of the facility. Default is "beds"
#' @param maxIteration (integer) the maximum number of times the function will try and remove overlapping admissions
#' @param retainAuxData (boolean) allow retaining additional data provided in the database. Default is TRUE.
#' @param verbose (boolean) print diagnostic messages. Default is TRUE.
#' @param ... other parameters passed on to internal functions
#' 
#' @seealso \code{\link[lubridate]{parse_date_time}}
#'
#' @return The adjusted database as a data.table with a new class attribute "hospinet.base" and an attribute "report" containing information related to the quality of the database.
#' @examples
#' ## create a "fake and custom" data base
#' mydb = create_fake_subjectDB(n_subjects = 100, n_facilities = 100)
#' setnames(mydb, 1:4, c("myPatientId", "myHealthCareCenterID", "DateOfAdmission", "DateOfDischarge"))
#' mydb[,DateOfAdmission:= as.character(DateOfAdmission)]
#' mydb[,DateOfDischarge:= as.character(DateOfDischarge)]
#' 
#' head(mydb)
#' #   myPatientId myHealthCareCenterID DateOfAdmission DateOfDischarge
#' #1:        s001                 f078      2019-01-26      2019-02-01
#' #2:        s002                 f053      2019-01-18      2019-01-21
#' #3:        s002                 f049      2019-02-25      2019-03-05
#' #4:        s002                 f033      2019-04-17      2019-04-21
#' #5:        s003                 f045      2019-02-02      2019-02-04
#' #6:        s003                 f087      2019-03-12      2019-03-19
#' 
#' str(mydb)
#' #Classes ‘data.table’ and 'data.frame':	262 obs. of  4 variables:
#' # $ myPatientId         : chr  "s001" "s002" "s002" "s002" ...
#' # $ myHealthCareCenterID: chr  "f078" "f053" "f049" "f033" ...
#' # $ DateOfAdmission     : chr  "2019-01-26" "2019-01-18" "2019-02-25" "2019-04-17" ...
#' # $ DateOfDischarge     : chr  "2019-02-01" "2019-01-21" "2019-03-05" "2019-04-21" ...
#' #- attr(*, ".internal.selfref")=<externalptr> 
#' 
#' my_checked_db = checkBase(mydb, 
#'      subjectID = "myPatientId", 
#'      facilityID = "myHealthCareCenterID", 
#'      disDate = "DateOfDischarge",
#'      admDate = "DateOfAdmission", 
#'      convertDates = TRUE, 
#'      dateFormat = "ymd")
#'
#' #Converting Adate, Ddate to Date format
#' #Checking for missing values...
#' #Checking for duplicated records...
#' #Removed 0 duplicates
#' #Done.
#' 
#' head(my_checked_db)
#' #    sID  fID      Adate      Ddate
#' #1: s001 f078 2019-01-26 2019-02-01
#' #2: s002 f053 2019-01-18 2019-01-21
#' #3: s002 f049 2019-02-25 2019-03-05
#' #4: s002 f033 2019-04-17 2019-04-21
#' #5: s003 f045 2019-02-02 2019-02-04
#' #6: s003 f087 2019-03-12 2019-03-19
#' str(my_checked_db)
#' #Classes ‘hospinet.base’, ‘data.table’ and 'data.frame':	262 obs. of  4 variables:
#' #$ sID  : chr  "s001" "s002" "s002" "s002" ...
#' #$ fID  : chr  "f078" "f053" "f049" "f033" ...
#' #$ Adate: POSIXct, format: "2019-01-26" "2019-01-18" "2019-02-25" "2019-04-17" ...
#' #$ Ddate: POSIXct, format: "2019-02-01" "2019-01-21" "2019-03-05" "2019-04-21" ...
#' # ...
#' 
#' ## Show the quality report
#' attr(my_checked_db, "report")
#' @export
#'
checkBase <- function(base,
                      gps_facilities = NULL,
                      convertDates = FALSE,
                      dateFormat = NULL,
                      deleteMissing = NULL,
                      deleteErrors = NULL,
                      subjectID = "sID",
                      facilityID = "fID",
                      disDate = "Ddate",
                      admDate = "Adate",
                      facilityLat = "lat",
                      facilityLong = "long",
                      facilityBed = "beds",
                      maxIteration = 25,
                      retainAuxData = TRUE,
                      verbose = TRUE,
                      ...)
{
    report = list()
    report$base = copy(base)
    
    #--- Check columns names --

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
    
    # Check data format, column names, variable format, parse dates
    report = checkFormat(report = report,
                         convertDates = convertDates,
                         dateFormat = dateFormat,
                         verbose = verbose)
    # Check for missing values, errors, and delete accordingly
    report = checkMissingErrors(report = report,
                                deleteMissing = deleteMissing,
                                deleteErrors = deleteErrors,
                                verbose = verbose)
    
    report = adjust_overlapping_stays(report = report,
                                      maxIteration = maxIteration,
                                      retainAuxData=retainAuxData,
                                      verbose = verbose,
                                      ...)
    if (verbose) message("Done.")

    #--- Check columns names for gps data ------------------------------------------------------------------------
    if(!is.null(gps_facilities)){ # A datatable with gps coordinates is provided
      gps_tableCols = colnames(gps_facilities)
      gps_inputCols = c(facilityID, facilityLat, facilityLong, facilityBed)
      gps_foundCols = intersect(gps_tableCols, gps_inputCols)
      
      if (length(gps_foundCols) != 4) {
        notfound = setdiff(gps_inputCols, gps_foundCols)
        stop("Column(s) ", paste(notfound, collapse = ", "), " provided as argument were not found in the database.")
      }
      report$gps_base = copy(gps_facilities)
      # Set column names to default
      setnames(report$gps_base,
               old = c(facilityID, facilityLat, facilityLong, facilityBed),
               new = c("fID", "lat", "long", "beds"))
      
      # Check variable format
      report = checkFormat_gps(report = report,
                               verbose = verbose)
    } else { # No datatable is provided
      report$gps_base = data.table::data.table(fID = unique(report$base$fID) |> sort(),
                                         lat = NA_real_,
                                         long = NA_real_,
                                         beds = NA_integer_)
    }
    
    #add the class "hospinet.base" to the list of class so that we can easily
    #identify whether the base has been checked or not.
    if (!inherits(report$base, "hospinet.base")) class(report$base) <- c("hospinet.base", class(report$base))
    
    #Get the summary statistics of the dataset
    dataSummary = all_admissions_summary(report$base)
    
    #export the "quality report"
    attr(report$base, "report") <- list(
      failedParse = report$failedParse,
      removedMissing = report$removedMissing,
      missing = report$missing,
      negativeLOS = report$negativeLOS,
      removedNegativeLOS = report$removedNegativeLOS,
      removedDuplicates = report$removedDuplicates,
      neededIterations = report$neededIterations,
      allIterations = report$allIterations,
      addedAOS = report$addedAOS,
      originalSize = report$originalSize,
      finalSize = report$base[,.N],
      
      LOSmean = dataSummary$meanLOS,
      TBAmean = dataSummary$meanTBA,
      admissions = dataSummary$totalAdmissions,
      subjects = dataSummary$numSubjects,
      numFacilities = dataSummary$numFacilities, # Same as n_facilities, but with a different source, maybe useful as double-check
      LOSdistribution = dataSummary$LOSdistribution,
      TBAdistribution = dataSummary$TBAdistribution,
      
      # GPS data
      gps = report$gps_base
    )
    return(report$base)
}

#' Check database format
#'
#' Function that performs various generic checks to ensure that the database has the correct format
#'
#' @param report (list).
#'     A list containing the base and in which will be stored reporting variables.
#'     The base is a patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         sID: subjectID (character)
#'         fID: facilityID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#' @param convertDates (boolean) TRUE/FALSE: whether the dates should converted. Default is TRUE.
#' @param dateFormat (boolean) The format of date as a character string (e.g. \%y\%m\%d for 20190524, or \%d-\%m-\%y for 24-05-2019).
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#'
#' @return Returns either an error message, or the database (modified if need be).
#'
checkFormat <- function(report,
                        convertDates = FALSE,
                        dateFormat = NULL,
                        verbose = TRUE)
{
    #assertDataTable(report$base)
    #--- Check data format ---
    if (!"data.frame" %in% class(report$base)) {
        stop("The database must be either a data.frame or a data.table object")
    } else if (!"data.table" %in% class(report$base)) {
        setDT(report$base)
        if (verbose) message("Converting database to a data.table object")
    }
    #--- Register the original size of the dataset ---
    report$originalSize = report$base[,.N]

    #--- Check format of "sID" and "fID" columns ---
    charCols = c("sID", "fID")
    types = sapply(charCols, function(x) typeof(report$base[[x]]))
    wrong = names(types[types != "character"])
    if (length(wrong)) {
        if (verbose) message("Converting column(s) ", paste(wrong, collapse = ", "), " to type character")
        report$base[, `:=`(sID = as.character(sID),
                    fID = as.character(fID))]
    }

    #--- Check dates format  ---
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
        report$base[, `:=`(Adate_new = lubridate::parse_date_time(Adate, orders = dateFormat, truncated = 3),
                           Ddate_new = lubridate::parse_date_time(Ddate, orders = dateFormat, truncated = 3)
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
                               verbose = TRUE)
{
    cols = c("sID", "fID", "Adate", "Ddate")

    #=== Nested function to delete ===
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
            stop("Argument ", paste0("delete ", option), " not or incorrectly specified")
        }
    }
    
    #--- Check missing values ---
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
        }
        if (is.null(deleteMissing)) {
                stop("\nPlease deal with these missing values or set option 'deleteMissing' to 'record' or 'subject'.")
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
        report$removedMissing = 0
    }

    #--- Check errors ---
    # Check if there are records with discharge before admission,
    # and delete them as given in function options
    wrong_order = report$base[Adate > Ddate, , which = T]
    len_wrong = length(wrong_order)
    if (len_wrong) {
        if (verbose) {
            message(paste0("Found ", len_wrong, " records with admission date posterior to discharge date."))
        }
        if (is.null(deleteErrors)) {
            stop("Please deal with these erroneous records or set option 'deleteErrors' to 'record' or 'subject'.")
        }
        
        # Delete
        startN = report$base[, .N]
        report$base = delete(base = report$base,
                             to_remove = wrong_order,
                             option = deleteErrors)
        # Report
        report$negativeLOS = len_wrong
        report$removedNegativeLOS = startN - report$base[, .N]
    } else {
      report$negativeLOS=0
      report$removedNegativeLOS=0
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
#' @param report (list).
#'     A list containing the base and in which will be stored reporting variables.
#'     The base is a patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         sID: subjectID (character)
#'         fID: facilityID (character)
#'         Adate: admission date (POSIXct, but character can be converted to POSIXct)
#'         Ddate: discharge date (POSIXct, but character can be converted to POSIXct)
#'
#' @param maxIteration (integer) the maximum number of times the function will try and remove overlapping admissions.
#' @param retainAuxData (boolean) allow retaining additional data provided in the database. Default is TRUE.
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#' @param ... other parameters passed on to internal functions
#'
#' @return The corrected database as data.table.
#'
adjust_overlapping_stays = function(report,
                                    maxIteration =25,
                                    verbose = FALSE,
                                    retainAuxData = TRUE,
                                    ...)
{
  base = report$base

  useCols<-colnames(base) %in% c("sID","fID","Adate","Ddate")
  extraCols=colnames(base)[!(colnames(base) %in% c("sID","fID","Adate","Ddate"))]
  auxDataExists=(length(extraCols)>0)
  if(auxDataExists&verbose&retainAuxData){
    message("Found following auxiliary data fields: ")
    message(paste0(",",extraCols))
  }
  if(!retainAuxData) base=base[,.SD,.SDcols=useCols]
  data.table::setkeyv(base, c("sID", "fID", "Adate", "Ddate"))

  nbefore = nrow(base)
  if (verbose) message("Checking for duplicated records...")
  base = unique(base, by = key(base))
  if (verbose) message(paste0("Removed ", nbefore-nrow(base), " duplicates"))
  report$removedDuplicates=nbefore-nrow(base)

  data.table::setkeyv(base, c("sID", "Adate", "Ddate"))
  
  startN = nrow(base)
  N = base[, .N]

  C1 = base[, sID][-N] == base[, sID][-1]
  C2 = ((base[, Adate][-1]-base[, Ddate][-N])<0)
  probSubjects=base[-1][(C1&C2),sID]
  C1A=(base[,sID] %in% probSubjects)
  probBase=base[C1A,]
  nonProbBase=base[!C1A,]
  
  iterator=0
  while(iterator<maxIteration&sum(C1&C2)>0){
    if (verbose) message(paste0("Iteration ",iterator, ": Found ",sum(C1&C2)," overlapping facility stays\nSplitting database and correcting"))

    Nprob = probBase[, .N]
    data.table::setkeyv(probBase, c("sID","Adate","Ddate"))

    C1 = probBase[, sID][-Nprob] == probBase[, sID][-1]
    C2 = ((probBase[, Adate][-1]-probBase[, Ddate][-Nprob])<0)

    if(retainAuxData&auxDataExists){
      a=data.table(
        sID=probBase[-Nprob][(C1&C2), sID],
        fID=probBase[-Nprob][(C1&C2), fID],
        Adate=probBase[-Nprob][(C1&C2), Adate],
        Ddate=probBase[-1][(C1&C2), Adate],
        probBase[-Nprob][(C1&C2),..extraCols])
      b=data.table(
        sID=probBase[-Nprob][(C1&C2), sID],
        fID=probBase[-Nprob][(C1&C2), fID],
        Adate=probBase[-1][(C1&C2), Ddate],
        Ddate=probBase[-Nprob][(C1&C2), Ddate],
        probBase[-Nprob][(C1&C2),..extraCols])
      c=data.table(sID=probBase[-Nprob][!(C1&C2), sID],
                   fID=probBase[-Nprob][!(C1&C2), fID],
                   Adate=probBase[-Nprob][!(C1&C2), Adate],
                   Ddate=probBase[-Nprob][!(C1&C2), Ddate],
                   probBase[-Nprob][!(C1&C2),..extraCols])
      d=data.table(sID=probBase[Nprob, sID],
                   fID=probBase[Nprob, fID],
                   Adate=probBase[Nprob, Adate],
                   Ddate=probBase[Nprob, Ddate],
                   probBase[Nprob,..extraCols])
      b=b[(b[, Adate] < b[, Ddate]),]
      probBase=rbind(a,b,c,d)
      setnames(probBase,c("sID","fID","Adate","Ddate",extraCols)) #might not be needed here
    }else{
      a=data.table(sID=probBase[-Nprob][(C1&C2), sID],fID=probBase[-Nprob][(C1&C2), fID],Adate=probBase[-Nprob][(C1&C2), Adate],Ddate=probBase[-1][(C1&C2), Adate])
      b=data.table(sID=probBase[-Nprob][(C1&C2), sID],fID=probBase[-Nprob][(C1&C2), fID],Adate=probBase[-1][(C1&C2), Ddate],Ddate=probBase[-Nprob][(C1&C2), Ddate])
      c=data.table(sID=probBase[-Nprob][!(C1&C2), sID],fID=probBase[-Nprob][!(C1&C2), fID],Adate=probBase[-Nprob][!(C1&C2), Adate],Ddate=probBase[-Nprob][!(C1&C2), Ddate])
      d=data.table(sID=probBase[Nprob, sID],fID=probBase[Nprob, fID],Adate=probBase[Nprob, Adate],Ddate=probBase[Nprob, Ddate])
      b=b[(b[, Adate] < b[, Ddate]),]
      probBase=rbind(a,b,c,d)
      setnames(probBase,c("sID","fID","Adate","Ddate")) #might not be needed here
    }
    if (verbose) message("Combining and sorting")

    data.table::setkeyv(probBase, c("sID","Adate","Ddate"))

    C3 = ((probBase[, Ddate]-probBase[, Adate])<0)
    new_base<-probBase[!C3,]

    Nprob = new_base[, .N]
    C1 = new_base[, sID][-Nprob] == new_base[, sID][-1]
    C2 = ((new_base[, Adate][-1]-new_base[, Ddate][-Nprob])<0)
    probSubjects=new_base[-1][(C1&C2),sID]
    C1A=(new_base[,sID] %in% probSubjects)

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


adjust_overlaps <- function(report,
                            leading = "admission")
{
    base = report$base
    ## For each subject, get the difference between the Adate date of stay N+1 and the
    ## Ddate date of stay N (left). And the difference between the Ddate date of stay
    ## N+1 and Ddate date of stay N (right). This allows us to type the overlaps.
    ## Column I in 'over' is the index in 'base' of stay N+1
    over = base[, list("left" = .SD[, Adate][-1] - .SD[, Ddate][-.N],
                       "right" = .SD[, Ddate][-1] - .SD[, Ddate][-.N],
                       "I" = .I[-1]),
                by = sID]
    ## This is a partial overlap: Adate date of stay N+1 is prior to the Ddate date of
    ## stay N (left < 0), but the Ddate date of N+1 is posterior to the Ddate date of N
    ## (right > 0).
    over[left < 0 & right >= 0, type := 'p']
    ## This is a full overlap: Adate date of stay N+1 is prior to the Ddate date of stay
    ## N (left < 0), and the Ddate date of N+1 is prior to the Ddate date of stay N
    ## (right < 0).
    over[left < 0 & right < 0, type := 'f']
    ## Adjust overlaps by the right (admission leading)
    if (leading == "admission") {
        ## Ddate date of stay N (i.e. I-1) becomes the Adate date of stay N+1 (i.e. I)
        base[over[type == 'p', I-1], Ddate := base[over[type == 'p', I], Adate]]
    }
    ## Adjust by the left (discharge leading)
    if (leading == "discharge") {
        ## Adate date of stay N+1 (i.e. I) becomes the Ddate date of stay N (i.e. I-1)
        base[over[type == 'p', I], Adate := base[over[type == 'p', I-1], Ddate]]
    }
    ## Adjust for 'inclusions'
    ## Ddate date of stay N (i.e. I-1) becomes the Adate date of stay N+1 (i.e. I)
    additional_stays = data.table("sID" = over[type == 'f', sID],
                                  "fID" = base[over[type == 'f', I-1], fID],
                                  "Adate" = base[over[type == 'f', I], Ddate],
                                  "Ddate" = base[over[type == 'f', I-1], Ddate])
    base[over[type == 'f', I-1], Ddate := base[over[type == 'f', I], Adate]]
    base = rbind(base, additional_stays)
    setkey(base, sID, Adate)

    report$base = base
    report$addedAOS = nrow(additional_stays)
    return(report)
}



#' Check database format for GPS coordinates and bed capacities
#'
#' Function that performs various generic checks to ensure that the GPS database has the correct format
#'
#' @param report (list).
#'     A list containing the base and in which will be stored reporting variables.
#'     The database with GPS coordinates of facilities and bed capacities. The data.table should have at least the following columns:
#'         fID: facilityID (character)
#'         lat: latitude of the facility (double)
#'         long: longitude of the facility (double)
#'         beds: bed capacity in the facility (integer)
#' @param verbose (boolean) print diagnostic messages. Default is FALSE.
#'
#' @return Returns either an error message, or the database (modified if need be).
#'
checkFormat_gps <- function(report,
                            verbose = TRUE)
{
  #--- Check data format -------------------------------------------------------------------------
  if (!"data.frame" %in% class(report$gps_base)) {
    stop("The database must be either a data.frame or a data.table object")
  } else if (!"data.table" %in% class(report$gps_base)) {
    setDT(report$gps_base)
    if (verbose) message("Converting database to a data.table object")
  }
  #--- Register the original size of the dataset --------------------------------------------------
  report$originalSize = report$gps_base[,.N]
  
  #--- Check format of columns ----------------------------------------------------
  charCols = "fID"
  types = sapply(charCols, function(x) typeof(report$gps_base[[x]]))
  wrong = names(types[types != "character"])
  if (length(wrong)) {
    if (verbose) message("Converting column(s) ", paste(wrong, collapse = ", "), " to type character")
    report$gps_base[, `:=`(fID = as.character(fID))]
  }
  
  doubleCols = c("lat","long")
  types = sapply(doubleCols, function(x) typeof(report$gps_base[[x]]))
  wrong = names(types[types != "double"])
  if (length(wrong)) {
    if (verbose) message("Converting column(s) ", paste(wrong, collapse = ", "), " to type double")
    report$gps_base[, `:=`(lat = as.numeric(lat),
                           long = as.numeric(long))]
  }
  
  intCols = "beds"
  types = sapply(intCols, function(x) typeof(report$gps_base[[x]]))
  wrong = names(types[types != "integer"])
  if (length(wrong)) {
    if (verbose) message("Converting column(s) ", paste(wrong, collapse = ", "), " to type integer")
    report$gps_base[, `:=`(beds = as.integer(beds))]
  }
  
  return(report)
}