context("Network analysis")

test_that("check clusters computation",{
  set.seed(42)
  nb_clust = 3
  db = create_fake_subjectDB_clustered(n_subjects = 1000, 
                                       n_clusters = nb_clust,
                                       n_facilities = 30)
  checked_db = checkBase(db)
  hn = hospinet_from_subject_database(checked_db)
  metrics = HospitalNetwork::get_metrics(hn)
  testthat::expect_equal(nb_clust, length(unique(metrics$cluster_fast_greedy)))
  testthat::expect_equal(nb_clust, length(unique(metrics$cluster_infomap)))
})