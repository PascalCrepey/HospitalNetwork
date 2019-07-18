# file: functions_network_analysis.R


### FUNCTIONS ###

#' Function computing different network analysis metrics
#' @param network The network to analyse. Must be a square adjacency matrix (n*n).
#' @param metrics A list of the metrics to compute
#' @param options A named list of options to be passed to the igraph functions
#' 
getMetrics <-
    function(network,
             mode = "directed",
             weighted = TRUE,
             transfers = TRUE,
             metrics = c("degree",
                         "closeness",
                         "betweenness"),
             clusters = c("cluster_fast_greedy",
                          "cluster_infomap"),
             hubs = "all_clusters",
             options = list(
                 degree = list(modes = c("in", "out", "total")),
                 closeness = list(modes = "total"),
                 betweenness = list(),
                 cluster_fast_greedy = list(undirected = "collapse"),
                 cluster_infomap = list(undirected = "collapse")
             )
             )
{
    ## ARGUMENTS CHECK
    coll = makeAssertCollection()
    ## Check network argument
    if (!class(network) %in% c("igraph", "matrix")) {
        stop("Please provide the network in the form of either a square adjacency matrix or an igraph graph.")
    }
    if (class(network) == "matrix") {
        assertMatrix(network, mode = "numeric", add = coll) # matrix must contain numerics
        assertMatrix(network, nrows = ncol(network), add = coll) # matrix must be square
    }
    ## Check metrics argument
    assertCharacter(metrics, unique = T)
    ## Check options argument
    assertList(options, types = "list", add = coll)
    assertNamed(options, type = "unique", add = coll)
    reportAssertions(coll)
    ## END OF ARGUMENT CHECK
        
    if (class(network) == "matrix") {
        graph = graph_from_adjacency_matrix(network,
                                            mode = mode,
                                            weighted = weighted)
    }
    else graph = network

    ## MAIN
    DT_list = list()
    ## transfers
    if (transfers) {
        patients_sent = as.data.table(rowSums(network), keep.rownames = T)
        patients_recieved = as.data.table(colSums(network), keep.rownames = T)
        colnames(patients_sent) = c("node", "patients_sent")
        colnames(patients_recieved) = c("node", "patients_recieved")
        setkey(patients_sent, node)
        setkey(patients_recieved, node)
        DT_list$transfers = merge(patients_recieved, patients_sent)
    }
    ## metrics   
    DT_list[metrics] = lapply(metrics, function(metric) {
        options[[metric]]$graph = graph
        DT = do.call(paste0("get_", metric), options[[metric]])
        setkey(DT, node)
        return(DT)
    })
    ## clusters
    DT_list[clusters] = lapply(clusters, function(algo) {
        options[[algo]]$graph = graph
        options[[algo]]$algo = algo
        DT = do.call(get_clusters, options[[algo]])
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
            ## get matrices by cluster
            mat_byclust = get_matrix_bycluster(mat = network,
                                               DT = DT_list[[g]],
                                               clusters = g)
            ## get graphs by group from the matrices
            graph_byclust = lapply(mat_byclust, function(x) {
                graph_from_adjacency_matrix(x,
                                            mode = mode,
                                            weighted = weighted)
            })
            ## get hub scores by group
            DT = get_hubs_bycluster(graphs = graph_byclust, name = g)
            return(DT)
        })
    }   
    
    DT_merged = Reduce(merge, DT_list)

    return(DT_merged)
}


get_degree <-
    function(graph, modes = c("in", "out", "total"))
{
    ## CHECK ARGUMENTS
    coll = makeAssertCollection()
    assertClass(graph, classes = "igraph", add = coll)
    if(!class(modes) %in% c("list", "character")) {
        stop("Argument 'modes' must be either a character vector or a list of character elements")
    }
    if(class(modes) == "list") {
        assertList(modes, types = "character", unique = T, add = coll)
    }
    if(class(modes) == "character") {
        assertCharacter(modes, unique = T, add = coll)
    }
    ## END OF CHECK
    ## MAIN        
    DT_list = list()
    DT_list[modes] = lapply(modes, function(mode) {
        tmp = as.data.table(degree(graph, mode = mode), keep.rownames = T)
        colnames(tmp) = c("node", paste0("degree_", mode))
        setkey(tmp, node)
        return(tmp)
    })
    DT_merged = Reduce(merge, DT_list)
    ## END OF MAIN
    return(DT_merged)
}


get_closeness <-
    function(graph, modes = "total")
{
    ## CHECK ARGUMENTS
    coll = makeAssertCollection()
    assertClass(graph, classes = "igraph", add = coll)
    if(!class(modes) %in% c("list", "character")) {
        stop("Argument 'modes' must be either a character vector or a list of character elements")
    }
    if(class(modes) == "list") {
        assertList(modes, types = "character", unique = T, add = coll)
    }
    if(class(modes) == "character") {
        assertCharacter(modes, unique = T, add = coll)
    }
    ## END OF CHECK
    ## MAIN        
    DT_list = list()
    DT_list[modes] = lapply(modes, function(mode) {
        tmp = as.data.table(closeness(graph, mode = mode), keep.rownames = T)
        colnames(tmp) = c("node", paste0("closeness_", mode))
        setkey(tmp, node)
        return(tmp)
    })
    DT_merged = Reduce(merge, DT_list)
    ## END OF MAIN
    return(DT_merged)
}

get_betweenness <-
    function(graph)
{
    ## CHECK ARGUMENTS
    coll = makeAssertCollection()
    assertClass(graph, classes = "igraph", add = coll)
    ## END OF CHECK
    ## MAIN
    DT = as.data.table(betweenness(graph), keep.rownames = T)
    colnames(DT) = c("node", "betweenness")
    setkey(DT, node)
    ## END OF MAIN
    return(DT)
}

get_clusters <-
    function(graph,
             algo,
             undirected,
             ...)
{
    ## CHECK ARGUMENTS
    coll = makeAssertCollection()
    assertClass(graph, classes = "igraph", add = coll)
    if(!class(algo) %in% c("list", "character")) {
        stop("Argument 'algo' must be either a character vector or a list of character elements")
    }
    if(class(algo) == "list") {
        assertList(algo, types = "character", unique = T, max.len = 1, add = coll)
    }
    if(class(algo) == "character") {
        assertCharacter(algo, unique = T, max.len = 1, add = coll)
    }
    ## END OF CHECK
    ## MAIN
    if(length(undirected)) {
        graph = as.undirected(graph, mode = undirected)
    }    
    cluster = do.call(algo, list(graph, ...))
    DT_cluster = data.table(cluster$names,
                            as.factor(cluster$membership))
    colnames(DT_cluster) = c("node", algo)
    ## END MAIN
    return(DT_cluster)
}


#' Function computing hub scores for each node. If bycluster = TRUE, hub scores are computed by cluster
#' @param graph An igraph graph
#' 
get_hubs_global <-
    function(graph, ...)
{
    hubs = hub_score(graph, ...)
    DT_hubs = as.data.table(hubs$vector, keep.rownames = T)
    colnames(DT_hubs) = c("node", "hub_score")
    setkey(DT_hubs, node)
    return(DT_hubs)
}

#' Function computing hub scores of nodes by group
#' @param graphs A list of igraph graphs, one for each group within which the hub scores will be computed
#' @param name [character (1)] The name of grouping variable (used only for naming the column of the DT)
#' @param ... Optional arguments to be passed to igraph function 'hub_score()'
#' 
get_hubs_bycluster <-
    function(graphs, name, ...)
{
    ## MAIN
    ## Get hub scores for each graph
    hubs = lapply(graphs, function(x) hub_score(x, ...))
    ## Create data tables and then merge
    tmp = lapply(hubs, function(x) as.data.table(x$vector, keep.rownames = T))
    DT_hubs = lapply(tmp, function(x) {
        colnames(x) = c("node", paste0("hub_score_by_", name))
        setkey(x, node)
    })
    DT_merged = rbindlist(DT_hubs)
    setkey(DT_merged, node)
    ## END OF MAIN
    return(DT_merged)
}


#' Function returning matrices of transfers within each by clusters
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
    ## Get matrices by cluster
    mat_byclust = lapply(members, function(x) mat[x,x])
    ## END OF MAIN
    return(mat_byclust)        
}



getAuthorities <-
    function()
{
}
