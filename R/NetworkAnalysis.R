# file: functions_network_analysis.R


### FUNCTIONS ###

#' Compute network metrics
#' 
#' Function computing different network analysis metrics.
#' 
#' @param network the network to analyse. Must be an igraph, HospiNet or a square adjacency matrix (n*n).
#' @param mode either "directed" or "undirected" network measures
#' @param weighted TRUE if the network is weighted
#' @param transfers TRUE if metrics specific to subject transfers must be computed
#' @param metrics list of the metrics to compute
#' @param clusters choose between cluster algorithm: cluster_fast_greedy or cluster_infomap
#' @param hubs choose between getting hubs from "all_clusters" or "global"
#' @param options named list of options to be passed to the igraph functions
#' 
#' @import igraph
#' @import checkmate
#' 
get_metrics <-
    function(network,
             mode = "directed",
             weighted = TRUE,
             transfers = TRUE,
             metrics = c("degree",
                         "closeness",
                         "clusters",
                         "betweenness"),
             clusters = c("cluster_fast_greedy","cluster_infomap"),
            # hubs=NULL,
             hubs = "all_clusters",
             options = list(
                 degree = list(modes = c("in", "out", "total")),
                 closeness = list(modes = "total"),
                 betweenness = list() ,
                 cluster_fast_greedy = list(undirected = "collapse"),
                 cluster_infomap = list(undirected = "collapse"),
                 clusters= list(algos=c("cluster_fast_greedy","cluster_infomap"),undirected = "collapse")
                 )
             )
{
    ## ARGUMENTS CHECK
    coll = checkmate::makeAssertCollection()
    ## Check network argument
    if (sum(class(network) %in% c("igraph", "matrix", "HospiNet"))==0) {
        stop("Please provide the network in the form of either a square adjacency matrix, an igraph graph, or an HospiNet object.")
    }
    if ("matrix" %in% class(network)) {
        checkmate::assertMatrix(network, mode = "numeric", add = coll) # matrix must contain numerics
        checkmate::assertMatrix(network, nrows = ncol(network), add = coll) # matrix must be square
    }
    if ("HospiNet" %in% class(network)) {
      checkmate::assertMatrix(network$matrix, mode = "numeric", add = coll) # matrix must contain numerics
      checkmate::assertMatrix(network$matrix, nrows = ncol(network$matrix), add = coll) # matrix must be square
    }
    ## Check metrics argument
    checkmate::assertCharacter(metrics, unique = T)
    ## Check options argument
    checkmate::assertList(options, types = "list", add = coll)
    checkmate::assertNamed(options, type = "unique", add = coll)
    checkmate::reportAssertions(coll)
    ## END OF ARGUMENT CHECK
        
    if ("matrix" %in% class(network)) {
        graph = igraph::graph_from_adjacency_matrix(network,
                                            mode = mode,
                                            weighted = weighted)
    } else if ("HospiNet" %in% class(network)){
      graph = igraph::graph_from_adjacency_matrix(network$matrix,
                                          mode = mode,
                                          weighted = weighted)
    } else {
      graph = network
    }
    
    ## MAIN
    DT_list = list()
    ## transfers
    if (transfers) {
        subjects_sent<-as.data.table(igraph::strength(graph,mode = "out"), keep.rownames = T)
        colnames(subjects_sent)<-c("node","subjects_sent")
        setkey(subjects_sent, node)
        
        subjects_received<-as.data.table(igraph::strength(graph,mode = "in"), keep.rownames = T)
        colnames(subjects_received)<-c("node","subjects_received")
        setkey(subjects_received, node)
        DT_list$transfers = merge(subjects_received, subjects_sent)
    }
    ## metrics   
    DT_list[metrics] = lapply(metrics, function(metric) {
        options[[metric]]$graph = graph
        DT = do.call(paste0("get_", metric), options[[metric]])
        setkey(DT, node)
        return(DT)
    })

    ## hubs
    if (!is.null(hubs)) {
        if (hubs == "global") {
            DT_list["global"] =  get_hubs_global(graph)
        }
        if (hubs == "all_clusters") {
            hubs = clusters
        }
        DT_list[paste0("hubs",hubs)] = lapply(hubs, function(g) {
            ## instead of getting matrices by cluster, and getting graphs by group from the matrices:
            ## use the direct method, based on subgraph() to get graphs by cluster
            graph_byclust=get_graph_bycluster(graph=graph,
                                DT = DT_list[[g]],
                                clusters = g)
            
            ## get hub scores by group
            DT = get_hubs_bycluster(graphs = graph_byclust, name = g)
            return(DT)
        })
    }   
    
    DT_merged = Reduce(merge, DT_list)

    return(DT_merged)
}


#' Compute the degree of each nodes in the network
#'
#' @param graph an igraph object
#' @param modes the type of degree: "in", "out", "total"
#'
#' @return a data.table of nodes degree
#'
get_degree <-
    function(graph, modes = c("in", "out", "total"))
{
    ## CHECK ARGUMENTS
    coll = checkmate::makeAssertCollection()
    checkmate::assertClass(graph, classes = "igraph", add = coll)
    if(!class(modes) %in% c("list", "character")) {
        stop("Argument 'modes' must be either a character vector or a list of character elements")
    }
    if(class(modes) == "list") {
      checkmate::assertList(modes, types = "character", unique = T, add = coll)
    }
    if(class(modes) == "character") {
      checkmate::assertCharacter(modes, unique = T, add = coll)
    }
    ## END OF CHECK
    ## MAIN        
    DT_list = list()
    DT_list[modes] = lapply(modes, function(mode) {
        tmp = as.data.table(igraph::degree(graph, mode = mode), keep.rownames = T)
        colnames(tmp) = c("node", paste0("degree_", mode))
        setkey(tmp, node)
        return(tmp)
    })
    DT_merged = Reduce(merge, DT_list)
    ## END OF MAIN
    return(DT_merged)
}


#' Compute closeness
#' 
#' Compute one or several closeness measure for facility networks.
#'
#' @param graph an igraph object
#' @param modes option passed on to igraph::closeness : "out", "in", "all", "total"
#'
#' @return a data.table containing the closeness measure
#' 
#' @seealso \code{\link[igraph]{closeness}}

get_closeness <-
    function(graph, modes = "total")
{
    ## CHECK ARGUMENTS
    coll = checkmate::makeAssertCollection()
    checkmate::assertClass(graph, classes = "igraph", add = coll)
    if(!class(modes) %in% c("list", "character")) {
        stop("Argument 'modes' must be either a character vector or a list of character elements")
    }
    if(class(modes) == "list") {
        checkmate::assertList(modes, types = "character", unique = T, add = coll)
    }
    if(class(modes) == "character") {
        checkmate::assertCharacter(modes, unique = T, add = coll)
    }
    ## END OF CHECK
    ## MAIN        
    DT_list = list()
    DT_list[modes] = lapply(modes, function(mode) {
        tmp = as.data.table(igraph::closeness(graph, mode = mode), keep.rownames = T)
        colnames(tmp) = c("node", paste0("closeness_", mode))
        setkey(tmp, node)
        return(tmp)
    })
    DT_merged = Reduce(merge, DT_list)
    ## END OF MAIN
    return(DT_merged)
}

#' Compute the betweeness centrality
#'
#' @param graph an igraph object
#'
#' @return a data.table containing the centrality measure

get_betweenness <-
    function(graph)
{
    ## CHECK ARGUMENTS
    coll = checkmate::makeAssertCollection()
    checkmate::assertClass(graph, classes = "igraph", add = coll)
    ## END OF CHECK
    ## MAIN
    DT = as.data.table(igraph::betweenness(graph), keep.rownames = T)
    colnames(DT) = c("node", "betweenness")
    setkey(DT, node)
    ## END OF MAIN
    return(DT)
}

#' Compute the clusters
#'
#' @param graph an igraph object
#' @param algo the type of algorithm, single argument describing a cluster function from the igraph package
#' @param undirected either "mutual" or "arbitrary"
#' @param ... other arguments to be passed on to the algorithm
#'
#' @return a data.table
#' 
get_clusters <-
    function(graph,
             algos,
             undirected,
             ...)
{
    ## CHECK ARGUMENTS
    coll = checkmate::makeAssertCollection()
    checkmate::assertClass(graph, classes = "igraph", add = coll)
    if(!class(algos) %in% c("list", "character")) {
      stop("Argument 'algo' must be either a character vector or a list of character elements")
    }
    if(class(algos) == "list") {
      checkmate::assertList(algos, types = "character", unique = T, add = coll)
    }
    if(class(algos) == "character") {
      checkmate::assertCharacter(algos, unique = T, add = coll)
    }
    ## END OF CHECK
    ## MAIN
    if(length(undirected)) {
      graph = igraph::as.undirected(graph, mode = undirected)
    }
    DT_list = list()
    DT_list[algos] = lapply(algos, function(x) {
      ## running the cluster algortihm 
      tmpcluster = do.call(get(x, asNamespace("igraph")), list(graph,...))
      tmp = data.table(tmpcluster$names,as.factor(tmpcluster$membership))
      colnames(tmp) = c("node", x)
      setkey(tmp, node)
      return(tmp)
    })
    DT_merged = Reduce(merge, DT_list)
    setkey(DT_merged, node)
    ## END MAIN
    return(DT_merged)
}

#' Function computing hub scores for each node. If bycluster = TRUE, hub scores are computed by cluster
#'
#' @param graph An igraph graph
#' @param ... other arguments to be passed to igraph function hub_score()
#' 
#' @seealso \code{\link[igraph]{hub_score}}
#' 
get_hubs_global <-
    function(graph, ...)
{
    hubs = igraph::hub_score(graph, ...)
    DT_hubs = as.data.table(hubs$vector, keep.rownames = T)
    colnames(DT_hubs) = c("node", "hub_score_global")
    setkey(DT_hubs, node)
    return(DT_hubs)
}

#' Function computing hub scores of nodes by group
#' 
#' @param graphs A list of igraph graphs, one for each group within which the hub scores will be computed
#' @param name [character (1)] The name of grouping variable (used only for naming the column of the DT)
#' @param ... Optional arguments to be passed to igraph function 'hub_score()'
#' 
#' @seealso \code{\link[igraph]{hub_score}}
#' 
get_hubs_bycluster <- function(graphs, name, ...)
{
    ## Warning if some clusters have only one member
    vcounts = lapply(graphs, function(g) {
        igraph::vcount(g)
    })
    names(vcounts) = names(graphs)
    lonely = names(vcounts[vcounts == 1])
    if (length(lonely)) {
        warning("Cluster(s) ", paste(lonely, collapse = ", "), " have only one member")
    }
    
    ## Get hub scores for each graph
    hubs = lapply(graphs, function(x) {
        igraph::hub_score(x, ...)
    })   
    ## Create data tables
    tmp = lapply(hubs, function(x) {
        as.data.table(x$vector, keep.rownames = T)
    })
    ## Add clusters names
    DT_hubs = lapply(1:length(tmp), function(x) {
        tmp[[x]][, cluster := names(tmp[x])]
    })
    ## Merge
    DT_merged = rbindlist(DT_hubs)
    setnames(DT_merged,
             old = c("V1","V2"),
             new = c("node", paste0("hub_score_by_", name)))
    setkey(DT_merged, node)

    return(DT_merged)
}

### DEPRECATED, now using get_graph_bycluster
#' Function returning matrices of transfers within each by clusters
#' 
#' @param mat The adjacency matrix of the network
#' @param DT A data table with at least a column 'node' and a factor column identifying the node's cluster
#' @param clusters A unique character vector of the name of the column identifying the nodes' clusters
#' 
get_matrix_bycluster <-
    function(mat, DT, clusters)
{
    ## MAIN
    ## Get list of members of each clusters
    n = 1:length(unique(DT[[clusters]]))
    members = list()
    members[n] = lapply(n, function(x) {
        bool = DT[[clusters]] == x
        return(DT[bool, node])
    })
    ##Select groups of more than one member
    members=members[lapply(members,length)>1]
    ## Get matrices by cluster
    mat_byclust = lapply(members, function(x) mat[x,x])
    ## END OF MAIN
    return(mat_byclust)        
}

get_graph_bycluster <- function(graph, DT, clusters)
{
    ## Get list of members of each clusters
    n = DT[, unique(get(clusters))]
    members = lapply(n, function(x) {
        DT[get(clusters) == x, node]
    })
    names(members) = paste0('clust_', n)

    ## Get graphs by cluster
    graph_byclust = lapply(members, function(x) {
        igraph::induced_subgraph(graph, x,impl = "copy_and_delete")
    })
    return(graph_byclust)        
}


# getAuthorities <-
#     function()
# {
# }
