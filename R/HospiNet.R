
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
# HospiNet <- function(matrix, edgelist, window_threshold, nmoves_threshold, noloops){
#   structure(list(matrix = matrix, 
#             edgelist = edgelist, 
#             n_hospitals = nrow(matrix),
#             n_movements = sum(matrix),
#             window_threshold = window_threshold, 
#             nmoves_threshold = nmoves_threshold,
#             noloops = noloops),
#             class = "HospiNet" )
# }


HospiNet <- R6::R6Class("HospiNet",
  private = list(
    .matrix = NULL,
    .edgelist = NULL,
    .n_hospitals = NULL,
    .n_movements = NULL,
    .window_threshold = NULL,
    .nmoves_threshold = NULL,
    .noloops = NULL
  ),
  public = list(
    initialize = function(edgelist, 
                          window_threshold,
                          nmoves_threshold,
                          noloops
                          ){
      private$.edgelist = edgelist
      private$.window_threshold = window_threshold
      private$.nmoves_threshold = nmoves_threshold
      private$.noloops = noloops
    },
    print = function() {
      cat(paste0(self$n_hospitals, " hospitals and ", self$n_movements, " movements.\n"))
      cat(paste0("Movement window is ", self$window_threshold, " days. \n"))
      if (nrow(self$matrix) <= 10) {
        print(self$matrix)
      }else{
        cat("Matrix too big to be printed on screen.")
      }
    }
  ),
  active = list(
    matrix = function(value) {
      if (missing(value)) {
        if (is.null(private$.matrix)){
          private$.matrix = matrix_from_edgelist(edgelist = private$.edgelist)
          private$.matrix
        }else {
          private$.matrix
        }
      } else {
        stop("`$matrix` is read only", call. = FALSE)
      }
    },
    edgelist = function(value) {
      if (missing(value)) {
        private$.edgelist
      } else {
        stop("`$edgelist` is read only", call. = FALSE)
      }
    },
    n_hospitals = function(value) {
      if (missing(value)) {
        if (is.null(private$.n_hospitals)){
          private$.n_hospitals = nrow(self$matrix)
        } else {
          private$.n_hospitals
        }
      } else {
        stop("`$n_hospitals` is read only", call. = FALSE)
      }
    },
    n_movements = function(value) {
      if (missing(value)) {
        if (is.null(private$.n_movements)){
          private$.n_movements = sum(self$matrix)
        }else{
          private$.n_movements
        }
        
      } else {
        stop("`$n_movements` is read only", call. = FALSE)
      }
    },
    window_threshold = function(value) {
      if (missing(value)) {
        private$.window_threshold
      } else {
        stop("`$window_threshold` is read only", call. = FALSE)
      }
    },
    nmoves_threshold = function(value) {
      if (missing(value)) {
        private$.nmoves_threshold
      } else {
        stop("`$nmoves_threshold` is read only", call. = FALSE)
      }
    },
    noloops = function(value) {
      if (missing(value)) {
        private$.noloops
      } else {
        stop("`$noloops` is read only", call. = FALSE)
      }
    }
  )
)

# print.HospiNet <- function(hn) {
#   #browser()
#   cat(paste0(hn$n_hospitals, " hospitals and ", hn$n_movements, " movements.\n"))
#   cat(paste0("Movement window is ", hn$window_threshold, " days. \n"))
#   
#   knitr::knit_print(hn$matrix)
# }
