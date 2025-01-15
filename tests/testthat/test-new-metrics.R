test_that("Reciprocity calculates", {
  # Create a sample edgelist
  edgelist <- data.table(origin = c("A", "B", "A", "D", "B", "D"), 
                         target = c("B", "A", "C", "A", "D", "B"), 
                         N = c(1, 1, 1, 1, 1, 1))
  hn <- HospiNet$new(edgelist = edgelist, edgelist_long = edgelist, 
                     window_threshold = 365, nmoves_threshold = NULL, noloops = TRUE)
  
  
  # Get the reciprocity from HospiNet
  reciprocity_result <- hn$reciprocity
  
  expected_output <- data.table(node = c("A", "B", "C", "D"), 
                                reciprocity = c(0.25, 0.5, 0, 1/3))  
  
  # Calculate expected global reciprocity directly using igraph
  g <- igraph::graph_from_data_frame(edgelist, directed = TRUE)
  expected_global_reciprocity <- igraph::reciprocity(g, mode = "ratio")
  
  setkey(reciprocity_result, node)
  setkey(expected_output, node)

  # Test if the calculated node level reciprocity matches the expected values
  expect_equal(reciprocity_result[, .(node, reciprocity)], expected_output)
  
  # Test if the global reciprocity matches the expected value
  expect_equal(reciprocity_result$global_reciprocity[1], expected_global_reciprocity)
})








test_that("Eigenvector centrality calculates", {
  # Create a sample edgelist
  edgelist <- data.table(
    origin = c("A", "B", "A", "D", "B", "D"), 
    target = c("B", "A", "C", "A", "D", "B"), 
    N = c(1, 1, 1, 1, 1, 1)
  )
  
  # Initialize the HospiNet object
  hn <- HospiNet$new(
    edgelist = edgelist, 
    edgelist_long = edgelist, 
    window_threshold = 365, 
    nmoves_threshold = NULL, 
    noloops = TRUE
  )
  
  # Check if igraph is correctly initialized
  expect_true(inherits(hn$igraph, "igraph"), "Graph object is not initialized properly.")
  
  # Get the eigenvector centrality from HospiNet
  eigenvector_centrality_result <- hn$eigenvector_centrality
  
  # Create an igraph object from the above edgelist
  g <- igraph::graph_from_data_frame(edgelist, directed = TRUE)
  
  # Calculate expected eigenvector centrality directly using igraph
  expected <- data.table(
    node = igraph::V(g)$name,
    eigenvector_centrality = igraph::eigen_centrality(g, directed = TRUE)$vector
  )
  
  # Set keys for both tables
  setkey(eigenvector_centrality_result, node)
  setkey(expected, node)
  
  # Compare the calculated and expected results
  expect_equal(eigenvector_centrality_result, expected)
})









