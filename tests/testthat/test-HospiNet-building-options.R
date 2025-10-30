
test_that("No loop option", {
  set.seed(42)
  db <- create_fake_subjectDB(n_subjects = 500, n_facilities = 30, with_errors = FALSE)
  checked_db <- checkBase(db)
  hn <- hospinet_from_subject_database(checked_db)
  hnloop <- hospinet_from_subject_database(checked_db, noloops = FALSE)
  testthat::expect_equal(
    sum(hnloop$matrix),
    sum(hn$matrix) +
      sum(hnloop$matrix * diag(1, nrow = 30, ncol = 30))
  )
})

test_that("Keep nodes is working", {
  set.seed(42)
  db <- create_fake_subjectDB(n_subjects = 100, n_facilities = 10, with_errors = FALSE)
  checked_db <- checkBase(db)
  hn <- hospinet_from_subject_database(checked_db, window_threshold = 7)
  expect_true(all(colSums(hn$matrix) + rowSums(hn$matrix) > 0))
  expect_warning(
    hnkeep <- hospinet_from_subject_database(checked_db, window_threshold = 7, keep_nodes = TRUE)
  )
  expect_true(!all(colSums(hnkeep$matrix) + rowSums(hnkeep$matrix) > 0))
})

