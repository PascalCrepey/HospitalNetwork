context("HospiNet object - plotting functions")

#' @importFrom vdiffr expect_doppelganger
test_that("plotting functions on non-clustered network",{
  set.seed(42)
  db = create_fake_subjectDB(n_subjects = 100, with_errors = FALSE)
  checked_db = checkBase(db)
  
  hn = hospinet_from_subject_database(checked_db)
  plot_deg = plot(hn, type = "degree")
  plot_mat100 = plot(hn)
  plot_clust100 = plot(hn, type = "clustered_matrix")
  #plot_circular_network100 = plot(hn, type = "circular_network")
  
  vdiffr::expect_doppelganger("Clustered Matrix plot 100", plot_clust100)
  vdiffr::expect_doppelganger("plot degree 100", plot_deg)
  #vdiffr::expect_doppelganger("Matrix plot 100", plot_mat100)
  
})
#vdiffr::expect_doppelganger("Circular plot 100", plot_circular_network100)

test_that("plotting functions on clustered network", {
  set.seed(42)
  db = create_fake_subjectDB_clustered(n_subjects = 500, n_facilities = 30)
  checked_db = checkBase(db)
  
  hn = hospinet_from_subject_database(checked_db)
  
  plot_mat_net500 = plot(hn)
  plot_deg_net500 = plot(hn, type = "degree")
  plot_clust_net500 = plot(hn, type = "clustered_matrix")
  plot_circular_clust_network500 = plot(hn, type = "circular_network")
  
  vdiffr::expect_doppelganger("Clustered Matrix plot clust 500", plot_clust_net500)
  vdiffr::expect_doppelganger("plot clust degree 500", plot_deg_net500)
  vdiffr::expect_doppelganger("Circular plot clust 500", plot_circular_clust_network500)
  #vdiffr::expect_doppelganger("Matrix plot clust 500", plot_mat_net500)
  
})

