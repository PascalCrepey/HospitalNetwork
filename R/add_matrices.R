calculate_reciprocity <- function(graph) {
  if (!igraph::is.directed(graph)) {
    stop("Reciprocity can only be calculated for directed graphs.")
  }
  
  reciprocity_scores <- sapply(igraph::V(graph), function(node) {
    edges_out <- igraph::incident(graph, node, mode = "out")
    edges_in <- igraph::incident(graph, node, mode = "in")
    mutual_edges <- sum(igraph::ends(graph, edges_out)[, 2] %in% igraph::ends(graph, edges_in)[, 1])
    total_edges <- length(edges_out) + length(edges_in)
    if (total_edges == 0) return(NA)
    mutual_edges / total_edges
  })
  
  return(data.table(
    node = igraph::V(graph)$name,
    reciprocity = reciprocity_scores
  ))
}
