
#' Function that creates an adjacency matrix from an edgelist
#'
#' @param edgelist
#' @param origin the name of the variable containing the origin hospital id
#' @param target the name of the variable containing the target hospital id
#' @param value.var the name of the variable containing the number of transfers
#' @param format.long
#' @export
matrix_from_edgelist <-
    function(edgelist,
             origin = "FINESS_DEF_1",
             target = "FINESS_DEF_2",
             value.var = "NB_transferts",
             format.long = F)
{
    if (format.long) {
        edgelist = edgelist[, .N, by = c(origin, target)] # group identical couples
    }
    if (!format.long) {
        colnames(edgelist)[colnames(edgelist) == value.var] = "N"
    }
    colnames(edgelist)[colnames(edgelist) == origin] = "origin"
    colnames(edgelist)[colnames(edgelist) == target] = "target"

    ## Some hospitalID are origins but not target, and vice-versa.
    ## They must be added in the respective columns to have a NxN matrix
    ## Filling with missing origins and targets (to make the matrix square)
    from = unique(edgelist[, origin])
    to = unique(edgelist[, target])
    missing_origin = setdiff(to, from)
    missing_target = setdiff(from, to)
    missing = data.table("origin" = c(missing_origin, missing_target),
                         "target" = c(missing_origin, missing_target),
                         'N' = 0)
    complete = data.table::rbindlist(list(edgelist, missing))
    DT_trans = data.table::dcast.data.table(complete, origin ~ target,
                                drop = F, fill = 0, value.var = 'N')
    DT_trans[, origin := NULL]
        matrix = as.matrix(DT_trans)
    rownames(matrix) = colnames(matrix)
    return(matrix)
}



#' Create a transfer matrix from a patient database
#' 
#' This function creates the adjacency matrix of the network from a patient discharge database. 
#' Can also return an edge list instead of the adjacency matrix.
#' @param base (data.table).
#'     A patient discharge database, in the form of a data.table. The data.table should have at least the following columns:
#'         patientID (character)
#'         stayOrder (character) OR dateBeginStay (date) AND dateEndStay (date)
#'         hospitalID (character)
#'         mode_entree (character, one of "mutation", "transfert" or "domicile")
#'         mode_sortie (character, one of "mutation", "transfert", "domicile" or "deces")
#'
#' @param noloops (boolean).
#'     Should transfers within the same nodes (loops) be kept or set to 0. Defaults to TRUE, removing loops (setting matrix diagonal to 0).
#' @param threshold (numeric)
#'     A threshold for the number of days between discharge and admission to be counted as a transfer. Set to 0 for same day transfer, default is 365 days.
#'
#' @return The adjency matrix or the edge list in the form of a data.table.
#'
#' @export
#'
#' @examples
edgelist_from_patient_database = function(base,
                              patientID = "pID",#ID
                              hospitalID = "hID",#FINESS
                              admDate = "Adate",
                              disDate = "Ddate",
                              noloops = TRUE,
                              threshold = 365)
{
    data.table::setkeyv(base, c(patientID,admDate))
    N = base[, .N]

    #### GET MOVEMENTS OF PATIENTS
    ## Compare rows n and n+1
    ## First condition, rows n and n+1 must have same patientID (C1)
    ## Second condition, "mode_sortie" of row n must be "mutation" or
    ## "transfert" (C2) => not included yet
    ## Third condition, "mode_entree" of row n+1 must be "mutation" or
    ## "transfert" (C3) => not included yet
    ## Fourth condition, the time between discharge of row n and admission  
    ## of row n+1 needs to be shorter or equal to threshold (C4)
    
    C1 = base[, get(patientID)][-N] == base[, get(patientID)][-1]
    #C2 = base[, mode_sortie][-N] %in% c("mutation", "transfert") 
    #C3 = base[, mode_entree][-1] %in% c("mutation", "transfert")
    C4 = ((base[, get(admDate)][-1]-base[, get(disDate)][-N])<(threshold*3600*24))
    
    ## If all conditions are met, retrieve FINESS number of row n (origin)
    cat("Compute origins...\n")
    origin = base[-N][C1 & C4, get(hospitalID)]

    ## If all conditions are met, retrieve FINESS number of row n+1 (target)
    cat("Compute targets...\n")
    target = base[-1][C1 & C4, get(hospitalID)]

    ## Create DT with each row representing a movement from "orig" to "target"
    cat("Compute frequencies...\n")
    DT_links = data.table(cbind(origin, target))
    data.table::setkey(DT_links, origin, target)

    ## Count the movements across each nodes
    cat("Compute unique links...\n")
    single_links = unique(DT_links)
    cat("Compute histogram of movements (edge list)\n")
    histogr = DT_links[single_links, .N, by = .EACHI] # this is the edge list

    if (noloops) {
        cat("Removing loops...\n")
        # histogr[origin == target, N := 0] # set matrix diagonal to 0 # TD: This delivers an empty list I run it.
        histogr<-subset(histogr,origin != target)
    }

    return(histogr)
}




