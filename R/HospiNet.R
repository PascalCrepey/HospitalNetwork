
#' Constructor for the HospiNet object
#'
#' @param matrix (matrix) A square matrix of hospitals containing the number of movements between hospitals
#' @param edgelist (data.table) the list of edges (origin, target) and their associated number of movements (N)
#' @param window_threshold the window threshold used to compute the network
#' @param nmoves_threshold the nmoves threshold used to compute the network
#' @param noloops TRUE if loops have been removed
#'
#' @return The constructor returns a HospiNet S3 object.
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
