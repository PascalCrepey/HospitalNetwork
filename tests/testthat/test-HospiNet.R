context("HospiNet building options")
test_that("No loop option",{
  set.seed(42)
  db = create_fake_subjectDB(n_subjects = 500, n_facilities = 30, with_errors = FALSE)
  checked_db = checkBase(db)
  hn = hospinet_from_subject_database(checked_db)
  hnloop = hospinet_from_subject_database(checked_db, noloops = FALSE)
  testthat::expect_equal(sum(hnloop$matrix), 
                         sum(hn$matrix) + 
                           sum(hnloop$matrix*diag(1,nrow = 30, ncol = 30)))
})

context("HospiNet plotting")
#' @importFrom vdiffr expect_doppelganger
test_that("plotting non-clustered network",{
  set.seed(42)
  db = create_fake_subjectDB(n_subjects = 100, with_errors = FALSE)
  checked_db = checkBase(db)
  
  hn = hospinet_from_subject_database(checked_db)
  plot_deg = plot(hn, type = "degree")
  plot_mat100 = plot(hn)
  plot_clust100 = plot(hn, type = "clustered_matrix")
  #plot_circular_network100 = plot(hn, type = "circular_network")
  
  vdiffr::expect_doppelganger("ClMat100", plot_clust100)
  vdiffr::expect_doppelganger("Deg100", plot_deg)
  vdiffr::expect_doppelganger("Mat100", plot_mat100)
  
})
#vdiffr::expect_doppelganger("Circular plot 100", plot_circular_network100)

test_that("plotting clustered network", {
  set.seed(42)
  db = create_fake_subjectDB_clustered(n_subjects = 500, n_facilities = 30)
  checked_db = checkBase(db)
  
  hn = hospinet_from_subject_database(checked_db)
  
  plot_mat_net500 = plot(hn)
  plot_deg_net500 = plot(hn, type = "degree")
  plot_clust_net500 = plot(hn, type = "clustered_matrix")
  plot_circular_clust_network500 = plot(hn, type = "circular_network")
  
  vdiffr::expect_doppelganger("ClustMatClust500", plot_clust_net500)
  vdiffr::expect_doppelganger("ClustDeg500", plot_deg_net500)
  vdiffr::expect_doppelganger("CircClust500", plot_circular_clust_network500)
  vdiffr::expect_doppelganger("MatClust500", plot_mat_net500)
  
})




