
test_that("Circular plots are created with multiple clusters", {
  set.seed(42)
  db <- create_fake_subjectDB_clustered(n_subjects = 500, n_facilities = 30, n_clusters = 5)
  checked_db <- checkBase(db)
  hn <- hospinet_from_subject_database(checked_db)
  plot_circ_clust = plot(hn, type = "circular_network")
  vdiffr::expect_doppelganger("plot_circ_clust", plot_circ_clust)
})

test_that("Circular plots are created with one cluster", {
  set.seed(42)
  db <- create_fake_subjectDB(n_subjects = 500, n_facilities = 30)
  checked_db <- checkBase(db)
  hn <- hospinet_from_subject_database(checked_db)
  plot_circ_noclust = plot(hn, type = "circular_network")
  vdiffr::expect_doppelganger("plot_circ_noclust", plot_circ_noclust)
})
