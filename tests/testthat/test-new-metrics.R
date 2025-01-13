test_that("Reciprocity calculates", {
  edgelist <- data.table(origin = c("A", "B", "A", "D", "B", "D"), 
                         target = c("B", "A", "C", "A", "D", "B"), 
                         N = c(1, 1, 1, 1, 1, 1))
  hn <- HospiNet$new(edgelist = edgelist, edgelist_long = edgelist, 
                     window_threshold = 365, nmoves_threshold = NULL, noloops = TRUE)
  
  reciprocity <- hn$reciprocity
  
  reciprocity <- reciprocity[order(node)]
  result <- reciprocity
  
  
  expected_output <- data.table(node = c("A", "B", "C", "D"), 
                                reciprocity = c(0.25, 0.5, 0, 1/3))  
  
  expect_equal(result, expected_output)
})




