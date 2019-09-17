
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
matrix_from_edgelist <-
    function(edgelist,
             origin_name = "origin",
             target_name = "target",
             value.var = "N",
             format.long = F){
    if (format.long) {
        edgelist = edgelist[, .N, by = c(origin_name, target_name)] # group identical couples
    }
    if (!format.long) {
        colnames(edgelist)[colnames(edgelist) == value.var] = "N"
    }
    colnames(edgelist)[colnames(edgelist) == origin_name] = "origin"
    colnames(edgelist)[colnames(edgelist) == target_name] = "target"

    ## Some hospitalID are origins but not target, and vice-versa.
    ## They must be added in the respective columns to have a NxN matrix
    ## Filling with missing origins and targets (to make the matrix square)
    from = unique(edgelist[, origin])
    to = unique(edgelist[, target])
    missing_origin = setdiff(to, from)
    missing_target = setdiff(from, to)

    if(length(missing_origin) + length(missing_target) > 0){
      missing = data.table("origin" = c(missing_origin, missing_target),
                           "target" = c(missing_origin, missing_target),
                           'N' = 0)
      complete = data.table::rbindlist(list(edgelist, missing))
    }else{
      #if nothing is missing
      complete = edgelist
    }
    
    DT_trans = data.table::dcast.data.table(complete, origin ~ target,
                                drop = F, fill = 0, value.var = 'N')
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
edgelist_from_patient_database = function(base,
                              patientID = "pID",
                              hospitalID = "hID",
                              admDate = "Adate",
                              disDate = "Ddate",
                              noloops = TRUE,
                              window_threshold = 365,
                              nmoves_threshold = NULL, 
                              verbose = FALSE
                              )
{
    data.table::setkeyv(base, c(patientID, admDate))
    N = base[, .N]

    #### GET MOVEMENTS OF PATIENTS
    ## Compare rows n and n+1
    ## First condition, rows n and n+1 must have same patientID (C1)
    ## Second condition, "mode_sortie" of row n must be "mutation" or
    ## "transfert" (C2) => not included yet
    ## Third condition, "mode_entree" of row n+1 must be "mutation" or
    ## "transfert" (C3) => not included yet
    ## Fourth condition, the time between discharge of row n and admission  
    ## of row n+1 needs to be shorter or equal to window_threshold (C4)

    C1 = base[, get(patientID)][-N] == base[, get(patientID)][-1]
    #C2 = base[, mode_sortie][-N] %in% c("mutation", "transfert") 
    #C3 = base[, mode_entree][-1] %in% c("mutation", "transfert")
    C4 = ((base[, get(admDate)][-1] - base[, get(disDate)][-N]) < (window_threshold*3600*24))
    
    ## If all conditions are met, retrieve hospitalID number of row n (origin)
    if (verbose) cat("Compute origins...\n")
    origin = base[-N][C1 & C4, get(hospitalID)]

    ## If all conditions are met, retrieve hospitalID number of row n+1 (target)
    if (verbose) cat("Compute targets...\n")
    target = base[-1][C1 & C4, get(hospitalID)]

    ## Create DT with each row representing a movement from "origin" to "target"
    if (verbose) cat("Compute frequencies...\n")
    DT_links = data.table(cbind(origin, target))
    data.table::setkey(DT_links, origin, target)

    ## Count the movements across each nodes
    if (verbose) cat("Compute unique links...\n")
    single_links = unique(DT_links)
    if (verbose) cat("Compute histogram of movements (edge list)\n")
    histogr = DT_links[single_links, .N, by = .EACHI] # this is the edge list

    if (noloops) {
        if (verbose) cat("Removing loops...\n")
        # histogr[origin == target, N := 0] # set matrix diagonal to 0 # TD: This delivers an empty list I run it.
        histogr <- subset(histogr,origin != target)
    }
    
    if (!is.null(nmoves_threshold)) {
      if (verbose) cat("Removing edges below nmoves_threshold...\n")
      histogr = histogr[N >= nmoves_threshold]
    }

    return(histogr)
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
                                           patientID = "pID",
                                           hospitalID = "hID",
                                           admDate = "Adate",
                                           disDate = "Ddate",
                                           noloops = TRUE,
                                           window_threshold = 365,
                                           nmoves_threshold = NULL, 
                                           create_MetricsTable=FALSE,
                                           verbose = FALSE){
  
  edgelist = edgelist_from_patient_database(base = base,
                                            patientID = patientID,
                                            hospitalID = hospitalID,
                                            admDate = admDate,
                                            disDate = disDate,
                                            noloops = noloops,
                                            window_threshold = window_threshold,
                                            nmoves_threshold = nmoves_threshold, 
                                            verbose = verbose)
  
  # HospiNet(matrix, edgelist, 
  #          window_threshold = window_threshold, 
  #          nmoves_threshold = nmoves_threshold, 
  #          noloops = noloops)
  dataSummary=all_admissions_summary(base,
                                     patientID = patientID,
                                     hospitalID = hospitalID,
                                     admDate = admDate,
                                     disDate = disDate)
  
  hospitalSummary = per_hospital_summary(base,
                                         patientID = patientID,
                                         hospitalID = hospitalID,
                                         admDate = admDate,
                                         disDate = disDate)
  
  HospiNet$new(edgelist,
               window_threshold = window_threshold, 
               nmoves_threshold = nmoves_threshold, 
               noloops = noloops,
               hsummary = hospitalSummary,
               dsummary = dataSummary,
               create_MetricsTable=create_MetricsTable
               
               )
}
