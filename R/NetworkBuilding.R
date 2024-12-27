##
## MAIN FUNCTIONS THAT COMPUTE THE NETWORK FROM THE BASE ##
##
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
#' @return A square numeric matrix, the adjacency matrix of the network.
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
#' @examples
#' mydb <- create_fake_subjectDB(n_subjects = 100, n_facilities = 10)
#' myBase <- checkBase(mydb)
#' hospinet <- hospinet_from_subject_database(myBase)
#' matrix_from_edgelist(hospinet$edgelist, count = "N")
#' @export
#' @import data.table
#' @import checkmate
#'
matrix_from_edgelist <- function(edgelist,
                                 origin_name = "origin",
                                 target_name = "target",
                                 count,
                                 format_long = FALSE) {
    #--- Check arguments ---
    checks <- makeAssertCollection()
    cols <- colnames(edgelist)
    assertDataFrame(edgelist, add = checks)
    assertTRUE(ncol(edgelist) >= 2, add = checks)
    assertCharacter(origin_name, len = 1, add = checks)
    assertChoice(origin_name, cols, add = checks)
    assertCharacter(target_name, len = 1, add = checks)
    assertChoice(target_name, cols, add = checks)
    assertCharacter(count, len = 1, null.ok = TRUE, add = checks)
    assertChoice(count, cols, null.ok = TRUE, add = checks)
    assertLogical(format_long, add = checks)
    reportAssertions(checks)
    if (!"data.table" %in% class(edgelist)) {
        setDT(edgelist)
    }
    if (format_long) {
        if (!is.null(count)) stop("The edgelist is said to be in long format, but a count variable was provided. Please verify the format of the edgelist, and either set 'count' to NULL (if long format) or 'format_long' to FALSE.")
        if (ncol(edgelist) > 2) warning("The edgelist is said to be in long format, but it contains more than two columns. Please make sure that this is intended, and that you do need to aggregate it, or it could lead to incorrect results.")
        edgelist <- edgelist[, .N, by = c(origin_name, target_name)] # group identical couples
    }
    if (!format_long) {
        setnames(edgelist, old = count, new = "N")
    }
    setnames(edgelist,
        old = c(origin_name, target_name),
        new = c("origin", "target")
    )

    ## Some facilityID are origins but not target, and vice-versa.
    ## They must be added in the respective columns to have a NxN matrix
    ## Filling with missing origins and targets (to make the matrix square)
    from <- unique(edgelist[, origin])
    to <- unique(edgelist[, target])
    missing_origin <- setdiff(to, from)
    missing_target <- setdiff(from, to)

    if (length(missing_origin) + length(missing_target) > 0) {
        missing <- data.table(
            "origin" = c(missing_origin, missing_target),
            "target" = c(missing_origin, missing_target),
            "N" = 0
        )
        complete <- data.table::rbindlist(list(edgelist, missing))
    } else {
        # if nothing is missing
        complete <- edgelist
    }

    DT_trans <- data.table::dcast.data.table(complete,
        origin ~ target,
        drop = FALSE,
        fill = 0,
        count = "N", value.var = "N"
    )
    DT_trans[, origin := NULL]
    matrix <- as.matrix(DT_trans)
    rownames(matrix) <- colnames(matrix)

    return(matrix)
}


#' Compute the adjacency matrix of a network from a database of movements records.
#'
#' This function computes the adjacency matrix of a network of facilities across
#' which subjects can be transferred. The matrix is computed from a database that
#' contains the records of the subjects' stays in the facilities. This function
#' is a simple wrapper around the two functions
#' \code{\link{edgelist_from_base}}, which computes the edgelist of the network
#' from the database, and \code{\link{matrix_from_edgelist}}, which converts the
#' edgelist into the adjacency matrix.
#'
#' @inheritParams edgelist_from_base
#' @return A square matrix, the adjacency matrix of the network.
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
#' @seealso \code{\link{edgelist_from_base}}, \code{\link{matrix_from_edgelist}}
#' @examples
#' mydb <- create_fake_subjectDB(n_subjects = 100, n_facilities = 10)
#' myBase <- checkBase(mydb)
#' matrix_from_base(myBase)
#' @export
matrix_from_base <- function(base,
                             window_threshold = 365,
                             count_option = "successive",
                             prob_params = c(0.0036, 1 / 365, 0.128),
                             condition = "dates",
                             noloops = TRUE,
                             nmoves_threshold = NULL,
                             flag_vars = NULL,
                             flag_values = NULL,
                             verbose = FALSE) {
    # First, compute edgelist from base
    edgelists <- edgelist_from_base(
        base = base,
        window_threshold = window_threshold,
        prob_params = prob_params,
        count_option = count_option,
        condition = condition,
        noloops = noloops,
        nmoves_threshold = nmoves_threshold,
        flag_vars = flag_vars,
        flag_values = flag_values,
        verbose = verbose
    )
    # Second, compute matrix from edgelist
    matrix <- matrix_from_edgelist(edgelists$el_aggr,
        origin_name = "origin",
        target_name = "target",
        count = "N",
        format_long = F
    )

    return(matrix)
}


#' Compute the edgelist of a network from a database of movements records.
#'
#' This function computes the edgelist of a network of facilities across
#' which subjects can be transferred. The edgelist is computed from a database that
#' contains the records of the subjects' stays in the facilities.
#'
#' @param base (data.table) A database of records of stays of subjects in
#'     facilities. The table should have at least the following columns:
#'     \itemize{ \item\bold{subjectID} (character) unique subject identifier
#'     \item\bold{facilityID} (character) unique facility identifier
#'     \item\bold{admDate} (POSIXct) date of admission in the facility
#'     \item\bold{disDate} (POSIXct) date of discharge of the facility }
#' @param window_threshold (integer) A number of days. If two stays of a subject
#'     at two facilities occurred within this window, this constitutes a
#'     connection between the two facilities (given that potential other
#'     conditions are met).
#' @param count_option (character) How to count connections. Options are
#'     "successive", "probability" or "all". See details.
#' @param condition (character) Condition(s) used to decide what constitutes a
#'     connection. Can be "dates", "flags", or "both". See details.
#' @param prob_params (vector of numeric) Three numerical values to calculate
#'     the probability that a movement causes an introduction from hospital A
#'     to hospital B. See Donker T, Wallinga J, Grundmann H. (2010) <doi:10.1371/journal.pcbi.1000715> for more details.
#'     For use with count_option="probability".
#'     prob_params[1] is the rate of acquisition in hospital A (related to LOS
#'     in hospital A). Default: 0.0036
#'     prob_params[2] is the rate of loss of colonisation (related to time
#'     between admissions). Default: 1/365
#'     prob_params[4] is the rate of transmission to other patients in hospital
#'     B (related to LOS in hospital B). Default: 0.128
#' @param noloops (boolean). Should transfers within the same nodes (loops) be
#'     kept or set to 0. Defaults to TRUE, removing loops (setting matrix
#'     diagonal to 0).
#' @param nmoves_threshold (numeric) A threshold for the minimum number of
#'     subject transfer between two facilities. Set to NULL to deactivate,
#'     default to NULL.
#' @param flag_vars (list) Additional variables that can help flag a transfer,
#'     besides the dates of admission and discharge. Must be a named list of two
#'     character vectors which are the names of the columns that can flag a
#'     transfer: the column that can flag a potential origin, and the column
#'     that can flag a potential target. The list must be named with "origin"
#'     and "transfer". Eg: list("origin" = "var1", "target" = "var2"). See
#'     details.
#' @param flag_values (list) A named list of two character vectors which contain
#'     the values of the variables in flag_var that are matched to flag a
#'     potential transfer. The list must be named with "origin" and
#'     "transfer". The character vectors might be of length greater than
#'     one. Eg: list("origin" = c("value1", "value2"), "target" = c("value2",
#'     "value2")). The values in 'origin' and 'target' are the values that flag
#'     a potential origin of a transfer, or a potential target,
#'     respectively. See details.
#' @param keep_nodes (logical) Should nodes with no connections be kept in the
#'     edgelist? Defaults to FALSE.
#' @param verbose TRUE to print computation steps
#'
#' @return A list of two data.tables, which are the edgelists. One in long
#'     format (el_long), and one aggregated by pair of nodes (el_aggr).
#'
#' @details The edgelist contains the information on the connections between
#'     nodes of the network, that is the movements of subjects between
#'     facilities. The edgelist can be in two different formats: long or
#'     aggregated. In long format, each row corresponds to a single movement
#'     between two facilities, therefore only two columns are needed, one
#'     containing the origin facilities of a movement, the other containing the
#'     target facilities. In aggregated format, the edgelist is aggregated by
#'     unique pairs of origin-target facilities.
#' @examples
#' mydb <- create_fake_subjectDB(n_subjects = 100, n_facilities = 10)
#' myBase <- checkBase(mydb)
#' edgelist_from_base(myBase)
#' @seealso \code{\link{matrix_from_edgelist}}, \code{\link{matrix_from_base}}
#' @export
#'
edgelist_from_base <- function(base,
                               window_threshold = 365,
                               count_option = "successive",
                               prob_params = c(0.0036, 1 / 365, 0.128),
                               condition = "dates",
                               noloops = TRUE,
                               nmoves_threshold = NULL,
                               flag_vars = NULL,
                               flag_values = NULL,
                               keep_nodes = FALSE,
                               verbose = FALSE) {
    #--- Check arguments ---
    checks <- makeAssertCollection()
    assertDataFrame(base, add = checks)
    assertTRUE(ncol(base) >= 4, add = checks)
    assertNumber(window_threshold, lower = 0, add = checks)
    assertChoice(count_option, c("all", "successive", "probability"), add = checks)
    assertLogical(noloops, add = checks)
    assertCount(nmoves_threshold, null.ok = TRUE, add = checks)
    assertChoice(condition, c("dates", "flags", "both"), add = checks)
    assertLogical(verbose, add = checks)
    if (any(!is.null(flag_vars), !is.null(flag_values))) {
        if (is.null(flag_vars)) stop("If flag_values is provided, flag_vars must be provided too.")
        if (is.null(flag_values)) stop("If flag_vars is provided, flag_values must be provided too.")
        assertList(flag_vars, len = 2, unique = TRUE, names = "strict", null.ok = TRUE, add = checks)
        assertSetEqual(names(flag_vars), c("origin", "target"), add = checks)
        assertSubset(unlist(flag_vars), colnames(base), add = checks)
        assertList(flag_values, len = 2, names = "strict", null.ok = TRUE, add = checks)
        assertSetEqual(names(flag_values), c("origin", "target"), add = checks)
        assertSubset(flag_values$origin, base[[flag_vars$origin]], add = checks)
        if (count_option == "all") message("Count option is all, therefore ignoring flags")
        if (!any(flag_values$origin %in% base[[flag_vars$origin]])) {
            warning(
                "None of the values provided for origin in flag_values was found in ",
                flag_vars$origin
            )
        }
        if (!any(flag_values$target %in% base[[flag_vars$target]])) {
            warning(
                "None of the values provided for target in flag_values was found in ",
                flag_vars$target
            )
        }
    }
    ## Checking base
    ## NOTE: in the current workflow, this check might not be needed, as it's checked previously, however, double check doesn't hurt
    if (!inherits(base, "hospinet.base")) {
        stop("Cannot compute the network: the database must first be checked with the
             function 'checkBase()'. See the vignettes for more details on the
             workflow of the package.")
    }
    reportAssertions(checks)
    if (is.null(flag_vars) & is.null(flag_values) & condition %in% c("flags", "both")) {
        stop("You want to compute connections using flag variables but you have
             not specified any. Please specify 'flag_vars' and 'flag_values',
             or set argument 'condition' to 'dates'")
    }
    # Messages
    if (condition == "flags") {
        message("Using only flag variables to compute connections. Therefore ignoring window threshold")
    }
    if (window_threshold == 0 & count_option == "all") {
        message("Window_threshold = 0 automatically sets count_option to 'successive'")
        count_option <- "successive"
    }



    # === GET MOVEMENTS OF SUBJECTS ===
    ## This will be computed differently depending on what is our definition
    ## of a connection
    ## 1. if window_threshold = 0, this is by definition a direct transfer
    ## 2. if window_threshold > 0:
    ##        a. count a connection only for successive stays within the window
    ##        b. count a connection for all stays within the window
    if (!"data.table" %in% class(base)) {
        setDT(base)
    }
    N <- base[, .N]
    data.table::setkeyv(base, c("sID", "Adate"))

    #--- Count only for successive stays ---
    ## Condition 1: rows n and n+1 must have same subjectID (C1)
    ## Condition 2: time between discharge of row n and admission of row n+1 needs to be
    ## less than or equal to window_threshold (C2)
    if (count_option == "successive") {
        C1 <- base[, sID][-N] == base[, sID][-1]
        diff <- difftime(
            time1 = base[, Adate][-1],
            time2 = base[, Ddate][-N],
            units = "days"
        )
        C2 <- between(diff, 0, window_threshold, incbounds = TRUE)

        if (!is.null(flag_vars)) {
            # Use additional variables to flag direct transfer
            C3a <- base[, get(flag_vars$origin)][-N] %in% flag_values$origin
            C3b <- base[, get(flag_vars$target)][-1] %in% flag_values$target
            C3 <- C3a & C3b
        }

        ## --- Compute origins and targets based on conditions ---
        if (verbose) cat("Compute origins and targets...\n")
        ## Compute connections only using dates
        if (condition == "dates") {
            origin <- base[-N][C1 & C2, .(
                "sID" = sID,
                "origin" = fID
            )]
            target <- base[-1][C1 & C2, .("target" = fID)]
        } else if (condition == "flags") {
            ## Compute connections only using flags
            origin <- base[-N][C1 & C3, .(
                "sID" = sID,
                "origin" = fID
            )]
            target <- base[-1][C1 & C3, .("target" = fID)]
        } else if (condition == "both") {
            ## Compute connections using both dates and flags
            origin <- base[-N][C1 & C2 & C3, .(
                "sID" = sID,
                "origin" = fID
            )]
            target <- base[-1][C1 & C2 & C3, .("target" = fID)]
        } else {
            stop("Argument 'condition' must be set to 'dates', 'flags', or 'both'")
        }

        ## Create DT with each row representing a movement from "origin" to "target"
        ## this is the edgelist, by individual connections
        if (verbose) cat("Compute frequencies...\n")
        el_long <- data.table(cbind(origin, target))
        data.table::setkey(el_long, origin, target)
        el_aggr <- el_long[, .N, by = c("origin", "target")]
    } else if (count_option == "all") {
        #--- Count for all stays ---
        ## Compute time between admissions between EACH PAIR of facilities, for one
        ## individual
        ##    1. Sort by admission date
        ##    2. Take admission date from last record N, and substract ALL previous
        ##       discharge dates (not just the previous one), i.e discharge dates from
        ##       records 1 to N-1
        ##    3. Then decrement N by 1, and repeat.
        get_tba <- function(x) {
            N <- nrow(x)
            tba <- lapply(1:(N - 1), function(i) {
                vals <- list()
                vals$diff <- difftime(
                    time1 = x$Adate[N - i + 1],
                    time2 = x$Ddate[(N - i):1],
                    units = "days"
                )
                vals$origin <- x$fID[(N - i):1]
                vals$target <- x$fID[N - i + 1]
                return(vals)
            })
            tba <- rbindlist(lapply(tba, as.data.table))
            return(tba)
        }

        ## Compute the times between admissions, by individual
        if (verbose) cat("Compute frequencies...\n")
        tba <- base[, get_tba(.SD), by = sID]

        ## Filter according to the window threshold
        ## this creates the edgelist, by individual connections
        el_long <- tba[between(diff, 0, window_threshold, incbounds = TRUE), .(sID, origin, target)]
        data.table::setkey(el_long, origin, target)
        el_aggr <- el_long[, .N, by = c("origin", "target")]
    } else if (count_option == "probability") {
        numAdm <- max(as.data.frame(table(base$sID))$Freq)
        el_long <- Reduce(rbind, lapply(
            1:(numAdm - 1),
            function(it) {
                C1 <- base[, sID][-((1 + N - it):N)] == base[, sID][-(1:it)]
                LOS1 <- as.numeric(difftime(
                    time1 = base[, Ddate][-(1:it)],
                    time2 = base[, Adate][-(1:it)],
                    units = "days"
                )[C1])
                LOS2 <- as.numeric(difftime(
                    time1 = base[, Ddate][-((1 + N - it):N)],
                    time2 = base[, Adate][-((1 + N - it):N)],
                    units = "days"
                )[C1])
                timebtw <- as.numeric(difftime(
                    time1 = base[, Adate][-(1:it)],
                    time2 = base[, Ddate][-((1 + N - it):N)],
                    units = "days"
                )[C1])
                origin <- base[-((1 + N - it):N)][C1, .("sID" = sID, "origin" = fID)]
                target <- base[-(1:it)][C1, .("target" = fID)]
                probTr <- (1 - exp(-prob_params[1] * LOS1)) * exp(-prob_params[2] * timebtw) * (1 - exp(-prob_params[3] * LOS2))
                el_long_temp <- data.table(cbind(origin, target, timebtw, LOS1, LOS2, probTr))
                return((el_long_temp))
            }
        ))
        data.table::setkey(el_long, origin, target)

        orderOfMagn <- max(el_long$probTr)
        el_long <- subset(el_long, probTr > orderOfMagn / (10^10)) # To avoid crashes, if the movements probability is 10 orders of magnitude less than the maximum, remove it.
        el_aggr <- el_long[, sum(probTr), by = c("origin", "target")]
        colnames(el_aggr) <- c("origin", "target", "N")
        el_aggr <- subset(el_aggr, N > 0)
    }

    #--- Aggregate edgelist by node ---
    if (verbose) cat("Compute edgelist...\n")
    data.table::setkey(el_long, origin, target)
    # el_aggr = el_long[, .N, by = c("origin", "target")]

    #--- Deal with direct loops ---
    if (noloops) {
        if (verbose) cat("Removing loops...\n")
        el_long <- subset(el_long, origin != target)
        el_aggr <- subset(el_aggr, origin != target)
    }

    if (!is.null(nmoves_threshold)) {
        if (verbose) cat("Removing connections below nmoves_threshold...\n")
        el_aggr <- subset(el_aggr, N >= nmoves_threshold)
    }
    
    if (keep_nodes) {
        if (verbose) cat("Keeping nodes with no connections...\n")
        curr_nodes <- unique(c(as.character(el_aggr$origin), as.character(el_aggr$target)))
        exp_nodes <- unique(base$fID)
        nodes_to_add <- setdiff(exp_nodes, curr_nodes)
        if(length(nodes_to_add) > 0) {
            el_aggr <- rbind(el_aggr, 
                             data.table(origin = nodes_to_add, 
                                        target = nodes_to_add, N = 0))
        }
    }

    return(list(
        "el_aggr" = el_aggr,
        "el_long" = el_long
    ))
}


#' Create HospiNet object from subject database
#'
#' This function creates a HospiNet object from the database containing subjects stays.
#'
#' @param base (hospinet.base) A database of records of stays of subjects in
#'     facilities. This can be obtained using the function \code{\link{checkBase}}.
#' @param noloops (boolean).
#'     Should transfers within the same nodes (loops) be kept or set to 0. Defaults to TRUE, removing loops (setting matrix diagonal to 0).
#' @param window_threshold (numeric)
#'     A threshold for the number of days between discharge and admission to be counted as a transfer. Set to 0 for same day transfer, default is 365 days.
#' @param count_option (character) TODO. Default is "successive".
#' @param condition (character) TODO. Default is "dates".
#' @param nmoves_threshold (numeric)
#'     A threshold for the minimum number of subject transfer between two facilities. Set to NULL to deactivate, default to NULL.
#' @param prob_params (vector of numeric) Three numerical values to calculate
#'     the probability that a movement causes an introduction from hospital A
#'     to hospital B. See Donker T, Wallinga J, Grundmann H. (2010) <doi:10.1371/journal.pcbi.1000715> for more details.
#'     prob_params[1] is the rate of acquisition in hospital A (related to LOS
#'     in hospital A). Default: 0.0036
#'     prob_params[2] is the rate of loss of colonisation (related to time
#'     between admissions). Default: 1/365
#'     prob_params[4] is the rate of transmission to other patients in hospital
#'     B (related to LOS in hospital B). Default: 0.128
#' @param flag_vars (list) Additional variables that can help flag a transfer,
#'     besides the dates of admission and discharge. Must be a named list of two
#'     character vectors which are the names of the columns that can flag a
#'     transfer: the column that can flag a potential origin, and the column
#'     that can flag a potential target. The list must be named with "origin"
#'     and "transfer". Eg: list("origin" = "var1", "target" = "var2"). See
#'     details.
#' @param flag_values (list) A named list of two character vectors which contain
#'     the values of the variables in flag_var that are matched to flag a
#'     potential transfer. The list must be named with "origin" and
#'     "transfer". The character vectors might be of length greater than
#'     one. Eg: list("origin" = c("value1", "value2"), "target" = c("value2",
#'     "value2")). The values in 'origin' and 'target' are the values that flag
#'     a potential origin of a transfer, or a potential target,
#'     respectively. See details.
#' @param keep_nodes (logical) Should nodes with no connections be kept in the
#'     edgelist? Defaults to FALSE.
#' @param create_MetricsTable (boolean)
#'     Should the metrics table be created along with the network. Setting to FALSE will speed up the results. Default is TRUE.
#' @param ... Additional parameters to be sent to checkBase in case the database has not been checked yet.
#' @param verbose TRUE to print computation steps
#' @param shinySession (NULL) internal variable to deal with the progress bar
#'
#' @details This function will build a HospiNet object from a line-listed
#' subject database. The HospiNet object has all of the functions stored as
#' active bindings which can be accessed in the usual way. For more info, see
#' \code{\link{HospiNet}}.
#' Note that the subject database will need to be run through \code{\link{checkBase}}
#' before going into this function.
#'
#' @seealso \code{\link{HospiNet}}
#'
#' @return The function returns a HospiNet object.
#' @export
#' @examples
#' mydb <- create_fake_subjectDB(n_subjects = 100, n_facilities = 10)
#' myBase <- checkBase(mydb)
#' hospinet_from_subject_database(myBase)
hospinet_from_subject_database <- function(base,
                                           window_threshold = 365,
                                           count_option = "successive",
                                           condition = "dates",
                                           prob_params = c(0.0036, 1 / 365, 0.128),
                                           noloops = TRUE,
                                           nmoves_threshold = NULL,
                                           flag_vars = NULL,
                                           flag_values = NULL,
                                           keep_nodes = FALSE,
                                           create_MetricsTable = TRUE,
                                           verbose = FALSE,
                                           shinySession = NULL,
                                           ...) {
    if (!inherits(base, "hospinet.base")) {
        message("Input database was not checked yet, which is required for network reconstruction.
              Running 'checkBase()' with default parameters.
              If this doesn't work, please run checkBase() separatelty with custom parameters first.")
        base <- checkBase(base,
            verbose = verbose,
            ...
        )
    }

    ## Compute the edgelists (long and aggregated format)
    edgelists <- edgelist_from_base(
        base = base,
        window_threshold = window_threshold,
        prob_params = prob_params,
        count_option = count_option,
        condition = condition,
        noloops = noloops,
        nmoves_threshold = nmoves_threshold,
        flag_vars = flag_vars,
        flag_values = flag_values,
        keep_nodes = keep_nodes,
        verbose = verbose
    )
    if (!is.null(shinySession)) {
        incProgress(session = shinySession, amount = 0.5)
    }
    ## Abort if the edgelist is empty
    if (nrow(edgelists$el_aggr) == 0) {
        message("No connections satisfying the conditions were found in the database. Aborting.")
        return(NULL)
    }

    facilitySummary <- per_facility_summary(base)

    if (!is.null(shinySession)) {
        incProgress(session = shinySession, amount = 0.3)
    }
    hn <- HospiNet$new(
        edgelist = edgelists$el_aggr,
        edgelist_long = edgelists$el_long,
        window_threshold = window_threshold,
        prob_params = prob_params,
        nmoves_threshold = nmoves_threshold,
        noloops = noloops,
        fsummary = facilitySummary,
        create_MetricsTable = create_MetricsTable
    )

    if (!is.null(shinySession)) {
        incProgress(session = shinySession, amount = 0.2)
    }
    hn
}