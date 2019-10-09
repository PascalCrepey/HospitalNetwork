###########################################################
## MAIN FUNCTIONS THAT COMPUTE THE NETWORK FROM THE BASE ##
###########################################################

#' Compute the adjacency matrix of a network from its edgelist
#'
#' @param edgelist (data.table) A table containing the edges (or links) of the
#'     network, i.e. representing the movements of subjects between
#'     facilities. Either in long format with at least two columns (origin and
#'     target facilities of a link), each row corresponding to a single
#'     movement, or aggregated by unique pairs of origin/target, therefore with
#'     an additional variable for movements count (default). See details.
#' @param origin_name (character) Column of the origin facilities of the links.
#' @param target_name (character) Column of the target facilities of the links.
#' @param count (character) Column of the counts of movements by unique pair of
#'     facilities.
#' @param format_long (logical) Whether the edgelist is in long format, with
#'     each row corresponding to a single movement. If TRUE, the edgelist will
#'     be aggregated by unique pairs of facilities to compute the matrix.
#' @return A square numeric matrix, the ajacency matrix of the network.
#' @details The edgelist contains the information on the connections between
#'     nodes of the network, that is the movements of subjects between
#'     facilities. The edgelist can be in two different formats: long or
#'     aggregated. In long format, each row corresponds to a single movement
#'     between two facilities, therefore only two columns are needed, one
#'     containing the origin facilities of a movement, the other containing the
#'     target facilities. In aggregated format, the edgelist is aggregated by
#'     unique pairs of origin-target facilities. Thus, each row corresponds to a
#'     unique connection between two facilities, and the table contains an
#'     additional variable which is the count of the number of movements
#'     recorded for the pair. If the edgelist is provided in long format, it
#'     will be aggregated to compute the matrix.
#' @seealso \code{\link{edgelist_from_base}}, \code{\link{matrix_from_base}}
#' @export
#' @import data.table
#' 
matrix_from_edgelist <- function(edgelist,
                                 origin_name = "origin",
                                 target_name = "target",
                                 count,
                                 format_long = F)
{
    #--- Check arguments -----------------------------------------------------------
    checks = makeAssertCollection()
    cols = colnames(edgelist)
    assertDataFrame(edgelist, add = checks)
    assertTRUE(ncol(edgelist) >= 2, add = checks)
    assertCharacter(origin_name, len = 1, add = checks)
    assertChoice(origin_name, cols, add = checks)
    assertCharacter(target_name, len = 1, add = checks)
    assertChoice(target_name, cols, add = checks)
    assertCharacter(count, len = 1, null.ok = T, add = checks)
    assertChoice(count, cols, null.ok = T, add = checks)
    assertLogical(format_long, add = checks)
    reportAssertions(checks)
    if (!"data.table" %in% class(edgelist)) {
        setDT(edgelist)
    }
    if (format_long) {
        if (!is.null(count)) stop("The edgelist is said to be in long format, but a count variable was provived. Please verify the format of the edgelist, and either set 'count' to NULL (if long format) or 'format_long' to FALSE.")
        if (ncol(edgelist) > 2) warning("The edgelist is said to be in long format, but it contains more than two columns. Please make sure that this is intended, and that you do need to aggregate it, or it could lead to incorrect results.")
        edgelist = edgelist[, .N, by = c(origin_name, target_name)] # group identical couples
    }
    if (!format_long) {
        setnames(edgelist, old = count, new = "N")
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
                                            count = 'N')
    DT_trans[, origin := NULL]
    matrix = as.matrix(DT_trans)
    rownames(matrix) = colnames(matrix)

    return(matrix)
}


#' Create an edge list from a patient database
#' 
#' This function creates the list of edges of the network from a database of stays of subjects in facilities. 
#' 
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'     
#'        * patientID (character)
#'        * hospitalID (character)
#'        * admDate (date)
#'        * disDate (date)
#' @param window_threshold (integer) A number of days. If two stays of a subject at two facilities occured within this window, this constitutes a connection between the two facilities (given that potential other conditions are met).
#' @param count_option (character) How to count connections. Either "successive" or "all". See details.
#' @param condition (character) Condition(s) used to decide what constitutes a connection. Can be "dates", "flags", or "both". See details.
#' @param noloops (boolean).
#'     Should transfers within the same nodes (loops) be kept or set to 0. Defaults to TRUE, removing loops (setting matrix diagonal to 0).
#' @param nmoves_threshold (numeric)
#'     A threshold for the minimum number of patient transfer between two hospitals. Set to NULL to deactivate, default to NULL.
#' @param flag_vars (list) Additional variables that can help flag a transfer, besides the dates of admission and discharge. Must be a named list of two character vectors which are the names of the columns that can flag a transfer: the column that can flag a potential origin, and the column that can flag a potential target. The list must be named with "origin" and "transfer". Eg: list("origin" = "var1", "target" = "var2"). See details.
#' @param flag_values (list) A named list of two character vectors which contain the values of the variables in flag_var that are matched to flag a potential transfer. The list must be named with "origin" and "transfer". The character vectors might be of length greater than one. Eg: list("origin" = c("value1", "value2"), "target" = c("value2", "value2")). The values in 'origin' and 'target' are the values that flag a potential origin of a transfer, or a potential target, respectively. See details.
#' @param patientID (character)
#' @param hospitalID (character)
#' @param admDate (character)
#' @param disDate (character)
#'      Change the default names of the base columns.
#' @param verbose TRUE to print computation steps
#' 
#' @return A list of two data.tables, which are the edgelists. One in long format (el_long), and one aggregated by pair of nodes (el_aggr).
#'
#' @details TODO
#' @export
#' 
edgelist_from_base <- function(base,
                               window_threshold,
                               count_option,
                               condition,
                               noloops = TRUE,
                               nmoves_threshold = NULL,
                               flag_vars = NULL,
                               flag_values = NULL,
                               patientID = "pID",
                               hospitalID = "hID",
                               admDate = "Adate",
                               disDate = "Ddate",
                               verbose = FALSE
                               )
{
    #--- Check arguments ---------------------------------------------------------------
    checks = makeAssertCollection()
    assertDataFrame(base, add = checks)
    assertTRUE(ncol(base) >= 4, add = checks)
    assertPOSIXct(base[[admDate]], add = checks)
    assertPOSIXct(base[[disDate]], add = checks)
    assertCount(window_threshold, add = checks)
    assertChoice(count_option, c("all", "successive"), add = checks)
    assertLogical(noloops, add = checks)
    assertCount(nmoves_threshold, null.ok = T, add = checks)    
    assertChoice(condition, c("dates", "flags", "both"), add = checks)
    assertCharacter(patientID, len = 1, add = checks)
    assertCharacter(hospitalID, len = 1, add = checks)
    assertCharacter(admDate, len = 1, add = checks)
    assertCharacter(disDate, len = 1, add = checks)
    assertLogical(verbose, add = checks)
    if (any(!is.null(flag_vars), !is.null(flag_values))) {
        if (is.null(flag_vars)) stop("If flag_values is provided, flag_vars must be provided too.")
        if (is.null(flag_values)) stop("If flag_vars is provided, flag_values must be provided too.")
        assertList(flag_vars, len = 2, unique = T, names = "strict", null.ok = T, add = checks)
        assertSetEqual(names(flag_vars), c("origin", "target"), add = checks)
        assertSubset(unlist(flag_vars), colnames(base), add = checks)
        assertList(flag_values, len = 2, names = "strict", null.ok = T, add = checks)
        assertSetEqual(names(flag_values), c("origin", "target"), add = checks)
        assertSubset(flag_values$origin, base[[flag_vars$origin]], add = checks)
        if (count_option == "all") message("Count option is all, therefore ignoring flags")
        if (!any(flag_values$origin %in% base[[flag_vars$origin]])) {
            warning("None of the values provided for origin in flag_values was found in ", flag_vars$origin)
        }
        if (!any(flag_values$target %in% base[[flag_vars$target]])) {
            warning("None of the values provided for target in flag_values was found in ", flag_vars$target)
        }
    }
    reportAssertions(checks)
    if (is.null(flag_vars) & is.null(flag_values) & condition %in% c("flags", "both")) {
        stop("You want to compute connections using flag variables but you have not specified any. Please specify 'flag_vars' and 'flag_values', or set argument 'condition' to 'dates'")
    }
    # Messages
    if (condition == "flags") {
        message("Using only flag variables to compute connections. Therefore ignoring window threshold")
    }
    if (window_threshold == 0 & count_option == "all") {
        message("Window_threshold = 0 automatically sets count_option to 'successive'")
        count_option = "successive"
    }
    
    #=== GET MOVEMENTS OF PATIENTS =====================================================
    ## This will be computed differently depending on what is our definition
    ## of a connection
    ## 1. if window_threshold = 0, this is by definition a direct transfer
    ## 2. if window_threshold > 0:
    ##        a. count a connection only for successive stays within the window
    ##        b. count a connection for all stays within the window
    if (!"data.table" %in% class(base)) {
        setDT(base)
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

        if (!is.null(flag_vars)) {
            # Use additional variables to flag direct transfer
            C3a = base[, get(flag_vars$origin)][-N] %in% flag_values$origin
            C3b = base[, get(flag_vars$target)][-1] %in% flag_values$target
            C3 = C3a & C3b
        }

        ##--- Compute origins and targets based on conditions ----------------------------
        if (verbose) cat("Compute origins and targets...\n")
        ## Compute connections only using dates
        if (condition == "dates") {
            origin = base[-N][C1 & C2, .("pID" = get(patientID),
                                         "origin" = get(hospitalID))]
            target = base[-1][C1 & C2, .("target" = get(hospitalID))]
        } else if (condition == "flags") {
        ## Compute connections only using flags
            origin = base[-N][C1 & C3, .("pID" = get(patientID),
                                         "origin" = get(hospitalID))]
            target = base[-1][C1 & C3, .("target" = get(hospitalID))]
        } else if (condition == "both") {
        ## Compute connections using both dates and flags
            origin = base[-N][C1 & C2 & C3, .("pID" = get(patientID),
                                              "origin" = get(hospitalID))]
            target = base[-1][C1 & C2 & C3, .("target" = get(hospitalID))]
        } else { 
            stop("Argument 'condition' must be set to 'dates', 'flags', or 'both'")
        }
                    
        ## Create DT with each row representing a movement from "origin" to "target"
        ## this is the edgelist, by individual connections
        if (verbose) cat("Compute frequencies...\n")
        el_long = data.table(cbind(origin, target)) 
        
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
        el_long = tba[between(diff, 0, window_threshold), .(pID, origin, target)]
    }
    
    #--- Aggregate edgelist by node ----------------------------------------------------
    if (verbose) cat("Compute edgelist...\n")
    data.table::setkey(el_long, origin, target)
    el_aggr = el_long[, .N, by = c("origin", "target")]

    #--- Deal with loops ---------------------------------------------------------------
    if (noloops) {
        if (verbose) cat("Removing loops...\n")
        el_long = subset(el_long, origin != target)
        el_aggr = subset(el_aggr, origin != target)
    }
    
    if (!is.null(nmoves_threshold)) {
        if (verbose) cat("Removing connections below nmoves_threshold...\n")
        el_aggr = subset(el_aggr, N >= nmoves_threshold)
    }

    return(list("el_aggr" = el_aggr,
                "el_long" = el_long))
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
                                           window_threshold,
                                           count_option,
                                           condition,
                                           noloops = TRUE,
                                           nmoves_threshold = NULL,
                                           flag_vars = NULL,
                                           flag_values = NULL,
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
                                   condition = condition,
                                   noloops = noloops,
                                   nmoves_threshold = nmoves_threshold,
                                   flag_vars = flag_vars,
                                   flag_values = flag_values,
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
