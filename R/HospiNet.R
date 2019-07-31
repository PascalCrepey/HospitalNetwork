
#' Constructor for the HospiNet object
#'
#' @param matrix 
#' @param edgelist 
#' @param window_threshold 
#' @param nmoves_threshold 
#' @param noloops 
#'
#' @return
#' @export
#'
HospiNet <- function(matrix, edgelist, window_threshold, nmoves_threshold, noloops){
  structure(list(matrix = matrix, 
            edgelist = edgelist, 
            n_hospitals = nrow(matrix),
            n_movements = sum(matrix),
            window_threshold = window_threshold, 
            nmoves_threshold = nmoves_threshold,
            noloops = noloops),
            class = "HospiNet" )
}

print.HospiNet <- function(hn) {
  #browser()
  cat(paste0(hn$n_hospitals, " hospitals and ", hn$n_movements, " movements.\n"))
  cat(paste0("Movement window is ", hn$window_threshold, " days. \n"))
  
  knitr::knit_print(hn$matrix)
}
