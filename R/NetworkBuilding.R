
#' Create an adjacency matrix from an edgelist
#'
#' @param edgelist a list of edges corresponding to hospital transfers
#' @param origin_name the name of the variable containing the origin hospital id
#' @param target_name the name of the variable containing the target hospital id
#' @param value.var the name of the variable containing the number of transfers
#' @param format.long specify if the number of moves needs to be computed (TRUE) or is already present (FALSE)
#' @export
#' 
#' @import data.table
#' 
matrix_from_edgelist <- function(edgelist,
                                 origin_name = "origin",
                                 target_name = "target",
                                 value.var = "N",
                                 format.long = F)
{
    if (!"data.table" %in% class(edgelist)) {
        setDT(edgelist)
    }
    if (format.long) {
        edgelist = edgelist[, .N, by = c(origin_name, target_name)] # group identical couples
    }
    if (!format.long) {
        setnames(edgelist, old = value.var, new = "N")
    }
    setnames(edgelist,
             old = c(origin_name, target_name),
             new = c("origin", "target"))

    ## Some hospitalID are origins but not target, and vice-versa.
    ## They must be added in the respective columns to have a NxN matrix
    ## Filling with missing origins and targets (to make the matrix square)
    from = unique(edgelist[, origin])
    to = unique(edgelist[, target])
    missing_origin = setdiff(to, from)
    missing_target = setdiff(from, to)

    if(length(missing_origin) + length(missing_target) > 0) {
        missing = data.table("origin" = c(missing_origin, missing_target),
                             "target" = c(missing_origin, missing_target),
                             'N' = 0)
        complete = data.table::rbindlist(list(edgelist, missing))
    } else {
      #if nothing is missing
        complete = edgelist
    }
    
    DT_trans = data.table::dcast.data.table(complete,
                                            origin ~ target,
                                            drop = F,
                                            fill = 0,
                                            value.var = 'N')
    DT_trans[, origin := NULL]
    matrix = as.matrix(DT_trans)
    rownames(matrix) = colnames(matrix)

    return(matrix)
}


#' Create an edge list from a patient database
#' 
#' This function creates the list of edges of the network from a patient discharge database. 
#' 
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'     
#'        * patientID (character)
#'        * hospitalID (character)
#'        * admDate (date)
#'        * disDate (date)
#'        
#' @param patientID (character)
#' @param hospitalID (character)
#' @param admDate (character)
#' @param disDate (character)
#'      Change the default names of the base columns.
#'      
#' @param noloops (boolean).
#'     Should transfers within the same nodes (loops) be kept or set to 0. Defaults to TRUE, removing loops (setting matrix diagonal to 0).
#' @param window_threshold (numeric)
#'     A threshold for the number of days between discharge and admission to be counted as a transfer. Set to 0 for same day transfer, default is 365 days.
#' @param nmoves_threshold (numeric)
#'     A threshold for the minimum number of patient transfer between two hospitals. Set to NULL to deactivate, default to NULL.
#' @param verbose TRUE to print computation steps
#' 
#' @return The edge list in the form of a data.table.
#'
#' @export
#' 
edgelist_from_base <- function(base,
                               window_threshold = 0,
                               count_option = "all",
                               noloops = TRUE,
                               nmoves_threshold = NULL, 
                               patientID = "pID",
                               hospitalID = "hID",
                               admDate = "Adate",
                               disDate = "Ddate",
                               verbose = FALSE
                               )
{
    #=== GET MOVEMENTS OF PATIENTS =====================================================
    ## This will be computed differently depending on what is our definition
    ## of a connection
    ## 1. if window_threshold = 0, this is by definition a direct transfer
    ## 2. if window_threshold > 0:
    ##        a. count a connection only for successive stays within the window
    ##        b. count a connection for all stays within the window

    if (window_threshold == 0) {
        count_option = "successive"
    }
    N = base[, .N]
    data.table::setkeyv(base, c(patientID, admDate))
    
    #--- Count only for successive stays -------------------------------------------------
    ## Condition 1: rows n and n+1 must have same patientID (C1)
    ## Condition 2: time between discharge of row n and admission of row n+1 needs to be
    ## less than or equal to window_threshold (C2)
    if (count_option == "successive") {

        C1 = base[, get(patientID)][-N] == base[, get(patientID)][-1]
        diff = difftime(time1 = base[, get(admDate)][-1],
                        time2 = base[, get(disDate)][-N],
                        units = "days")
        C2 = between(diff, 0, window_threshold)

        ## If all conditions are met, retrieve hospitalID number of row n (origin)
        if (verbose) cat("Compute origins...\n")
        origin = base[-N][C1 & C2, .("pID" = get(patientID),
                                     "origin" = get(hospitalID))]

        ## If all conditions are met, retrieve hospitalID number of row n+1 (target)
        if (verbose) cat("Compute targets...\n")
        target = base[-1][C1 & C2, .("target" = get(hospitalID))]

        ## Create DT with each row representing a movement from "origin" to "target"
        ## this is the edgelist, by individual connections
        if (verbose) cat("Compute frequencies...\n")
        el_indiv = data.table(cbind(origin, target)) 
        
    } else if (count_option == "all") {
        #--- Count for all stays ---------------------------------------------------------
        ## Compute time between admissions between EACH PAIR of facilities, for one
        ## individual
        ##    1. Sort by admission date
        ##    2. Take admission date from last record N, and substract ALL previous
        ##       discharge dates (not just the previous one), i.e discharge dates from
        ##       records 1 to N-1
        ##    3. Then decrement N by 1, and repeat.
        get_tba = function(x) {
            N = nrow(x)
            tba = lapply(1:(N-1), function(i) {
                vals = list()
                vals$diff = difftime(time1 = x$Adate[N-i+1],
                                     time2 = x$Ddate[(N-i):1],
                                     units = "days")
                vals$origin = x$hID[(N-i):1]
                vals$target = x$hID[N-i+1]
                return(vals)
            })
            tba = rbindlist(lapply(tba, as.data.table))
            return(tba)
        }

        ## Compute the times between admissions, by individual
        if (verbose) cat("Compute frequencies...\n")
        tba = base[, get_tba(.SD), by = pID]
    
        ## Filter according to the window threshold
        ## this creates the edgelist, by individual connections
        el_indiv = tba[between(diff, 0, window_threshold), .(pID, origin, target)]
    }
    
    #--- Aggregate edgelist by node ----------------------------------------------------
    if (verbose) cat("Compute edgelist...\n")
    data.table::setkey(el_indiv, origin, target)
    unique_links = unique(el_indiv[, .(origin, target)])
    el_aggr = el_indiv[unique_links, .N, by = .EACHI]

    #--- Deal with loops ---------------------------------------------------------------
    if (noloops) {
        if (verbose) cat("Removing loops...\n")
        el_indiv = subset(el_indiv, origin != target)
        el_aggr = subset(el_aggr, origin != target)
    }
    
    if (!is.null(nmoves_threshold)) {
        if (verbose) cat("Removing connections below nmoves_threshold...\n")
        el_aggr = subset(el_aggr, N >= nmoves_threshold)
    }

    return(list("el_aggr" = el_aggr,
                "el_indiv" = el_indiv))
}


#' Create HospiNet object from patient database
#' 
#' This function creates a HospiNet object from the database containing patients stays.
#'
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'     \itemize{
#'        \item patientID (character)
#'        \item hospitalID (character)
#'        \item admDate (date)
#'        \item disDate (date)
#'        }
#' @param patientID,hospitalID,admDate,disDate (character)
#'      Change the default names of the base columns.
#'      
#' @param noloops (boolean).
#'     Should transfers within the same nodes (loops) be kept or set to 0. Defaults to TRUE, removing loops (setting matrix diagonal to 0).
#' @param window_threshold (numeric)
#'     A threshold for the number of days between discharge and admission to be counted as a transfer. Set to 0 for same day transfer, default is 365 days.
#' @param nmoves_threshold (numeric)
#'     A threshold for the minimum number of patient transfer between two hospitals. Set to NULL to deactivate, default to NULL.
#' @param create_MetricsTable (boolean)
#'     Should the metrics table be created along with the network. Setting to FALSE will speed up the results. Default is TRUE.
#' @param verbose TRUE to print computation steps
#'     
#' @seealso \code{\link{HospiNet}}
#' 
#' @return The function returns a HospiNet object.
#' @export
#' @examples
#' mydb = create_fake_patientDB(n_patients = 100, n_hospital = 5)
#' mat = hospinet_from_patient_database(base = mydb)
#' mat
#' 
hospinet_from_patient_database <- function(base,
                                           window_threshold = 0,
                                           count_option = "all",
                                           noloops = TRUE,
                                           nmoves_threshold = NULL, 
                                           patientID = "pID",
                                           hospitalID = "hID",
                                           admDate = "Adate",
                                           disDate = "Ddate",
                                           create_MetricsTable = TRUE,
                                           verbose = FALSE)
{
    edgelists = edgelist_from_base(base = base,
                                   window_threshold = window_threshold,
                                   count_option = count_option,
                                   noloops = noloops,
                                   nmoves_threshold = nmoves_threshold, 
                                   patientID = patientID,
                                   hospitalID = hospitalID,
                                   admDate = admDate,
                                   disDate = disDate,
                                   verbose = verbose)
                               
  
  dataSummary = all_admissions_summary(base,
                                     patientID = patientID,
                                     hospitalID = hospitalID,
                                     admDate = admDate,
                                     disDate = disDate)
  
  hospitalSummary = per_hospital_summary(base,
                                         patientID = patientID,
                                         hospitalID = hospitalID,
                                         admDate = admDate,
                                         disDate = disDate)

  HospiNet$new(edgelists$el_aggr,
               window_threshold = window_threshold, 
               nmoves_threshold = nmoves_threshold, 
               noloops = noloops,
               hsummary = hospitalSummary,
               dsummary = dataSummary,
               create_MetricsTable=create_MetricsTable)
}
