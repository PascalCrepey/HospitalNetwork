
#' Class providing the HospiNet object with its methods
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import ggplot2
#' @export
#' @keywords data
#' @return Object of \code{\link{R6Class}} with methods for accessing hospital networks.
#' @format \code{\link{R6Class}} object.
#' @examples
#' mydbsmall = create_fake_patientDB(n_patients = 1000, n_hospital = 10)
#'   
#' hn = hospinet_from_patient_database(base = mydbsmall)
#' 
#' hn
#' 
#' plot(hn)
#' plot(hn, type = "clustered_matrix")
#' 
#' @field edgelist (data.table) the list of edges (origin, target) and their associated number of movements (N) (read-only)
#' @field matrix (matrix) the transfer matrix (active binding, read-only)
#' @field igraph (igraph) the igraph object corresponding to the network (active binding, read-only)
#' @field n_hospitals the number of hospitals in the network (read-only)
#' @field n_movements the total number of patient movements in the network (read-only)
#' @field window_threshold the window threshold used to compute the network (read-only)
#' @field nmoves_threshold the nmoves threshold used to compute the network (read-only)
#' @field noloops TRUE if loops have been removed (read-only)
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(edgelist, 
#' window_threshold,
#' nmoves_threshold,
#' noloops)}}{This method is used to create an object of this class with \code{edgelist} as the necessary information to create the network.
#' The other arguments \code{window_threshold}, \code{nmoves_threshold}, and \code{noloops} are specific to the \code{edgelist} and need to be provided.
#' For ease of use, it is preferable to use the function \code{\link{hospinet_from_patient_database}}}
#'   \item{\code{print()}}{This method prints basic information about the object.}
#'   \item{\code{plot(type = "matrix")}}{This method plots the network matrix by default. 
#'   The argument \code{type} can take the following values: 
#'   \describe{
#'   \item{matrix}{print the network matrix,}
#'   \item{clustered_matrix}{identify and print cluster(s) in the matrix using the infomap algorithm (from igraph).}
#'   }
#'   }
#' }

HospiNet <- R6::R6Class("HospiNet",
  private = list(
    .matrix = NULL,
    .edgelist = NULL,
    .n_hospitals = NULL,
    .n_movements = NULL,
    .window_threshold = NULL,
    .nmoves_threshold = NULL,
    .noloops = NULL,
    .igraph = NULL
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
    },
    plot = function(type = "matrix"){
      if (type == "matrix") {
        ggplot(self$edgelist, aes(x = target, y = origin)) + 
          geom_raster(aes(fill=N)) + 
          scale_fill_gradient(low="grey90", high="red") +
          scale_x_discrete(name = "Origin") + 
          scale_y_discrete(name = "Target") +
          labs(x="Origin", y = "Target", title = "Matrix") +
          theme_bw() + theme(axis.text.x=element_text(size = 8, angle = 90, vjust = 0.3),
                             axis.text.y=element_text(size = 8),
                             plot.title=element_text(size = 12))
      } else if (type == "clustered_matrix") {
        #get the clustering
        cl <- cluster_fast_greedy(self$igraph)
        dt_cl <- data.table(cl$names,cl$membership)
        
        if (max(cl$membership) <= 1) stop("No cluster identified.")
        #put it in a copy of the edgelist
        el = copy(self$edgelist)
        el[dt_cl, origin_c := V2, on = c("origin" = "V1")]
        el[dt_cl, target_c := V2, on = c("target" = "V1")]
        el[, col := ifelse(origin_c == target_c, origin_c, 0)]
        el[, col := factor(col)]
        
        #reorder the axis to make the cluster appear
        el$origin = factor(el$origin, levels = unique(el[order(origin_c), origin])) 
        el$target = factor(el$target, levels = unique(el[order(target_c), target])) 

        #plot it
        ggplot(el, aes(x = target, y = origin)) + 
          geom_raster(aes(fill = col)) + 
          scale_fill_manual(name = NULL, values = c("grey", rainbow(max(el$origin_c)))) +
          scale_x_discrete(name = "Origin") + 
          scale_y_discrete(name = "Target") +
          labs(x = "Origin", y = "Target", title = "Clustered matrix") +
          theme_bw() + theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3),
                             axis.text.y = element_text(size = 8),
                             plot.title = element_text(size = 12))
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
    igraph = function(value) {
      if (missing(value)) {
        if (is.null(private$.igraph)){
          private$.igraph = igraph::graph_from_adjacency_matrix(self$matrix, 
                                                                weighted = TRUE, 
                                                                mode = "undirected")
          private$.igraph
        } else {
          private$.igraph
        }
      }else {
        stop("`$igraph` is read only", call. = FALSE)
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

