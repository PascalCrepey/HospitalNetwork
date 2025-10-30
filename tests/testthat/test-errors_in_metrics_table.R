test_that("No errors in metrics table", {
  set.seed(42)
  
  db <- create_fake_subjectDB(n_subjects = 500, n_facilities = 30, with_errors = FALSE)
  checked_db <- checkBase(db)
  
  ## create hospinet from database
  hn <- hospinet_from_subject_database(checked_db)
  
  ## create hospinet from edge list
  el <- edgelist_from_base(checked_db)
  
  hnel <- HospiNet$new(edgelist = el$el_aggr,
                       edgelist_long = el$el_long, 
                       window_threshold = 365,
                       noloops = TRUE, 
                       nmoves_threshold = NULL)
  
  #equal on everything but the data coming from the original base
  expect_equivalent(hn$metricsTable[, -(2:4)], hnel$metricsTable[, -(2:4)])
  
  
  #now include facility summary
  facilitySummary <- per_facility_summary(checked_db)
  
  hnel_withf <- HospiNet$new(edgelist = el$el_aggr,
                       edgelist_long = el$el_long, 
                       window_threshold = 365,
                       fsummary = facilitySummary,
                       noloops = TRUE, 
                       nmoves_threshold = NULL)
  #equal on everything
  expect_equal(hn$metricsTable, hnel_withf$metricsTable)
  
})
