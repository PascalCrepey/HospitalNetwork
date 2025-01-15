
test_that(" test on transferts2023 dataset", {

  data("transferts2023")
  
  #  Initialize the HospiNet object
  hn <- HospiNet$new(
    edgelist = transferts2023[, .(origin = FINESS_DEF_1, target = FINESS_DEF_2, N = NB_transferts)],
    edgelist_long = transferts2023,
    window_threshold = 365,
    nmoves_threshold = NULL,
    noloops = TRUE
  )
  


  #  Test visualization functions
  degree_plot <- hn$plot(type = "degree")
  circular_network_plot <- hn$plot(type = "circular_network", plotLinks = 10000)
  clustered_matrix_plot <- hn$plot(type = "clustered_matrix")
  
  vdiffr::expect_doppelganger("degree-plot", degree_plot)
  vdiffr::expect_doppelganger("circular-network-plot", circular_network_plot)
  vdiffr::expect_doppelganger("clustered-matrix-plot", clustered_matrix_plot)
  
  
  
  
  })









