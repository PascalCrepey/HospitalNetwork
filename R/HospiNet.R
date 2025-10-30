
#' Class providing the HospiNet object with its methods
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import ggplot2
#' @import ggraph
#' @export
#' @keywords data
#' @return Object of \code{\link[R6:R6Class]{R6::R6Class}} with methods for accessing facility networks.
#' @format \code{\link[R6:R6Class]{R6::R6Class}} object.
#' @examples
#' mydbsmall <- create_fake_subjectDB(n_subjects = 100, n_facilities = 10)
#'
#' hn <- hospinet_from_subject_database(
#'   base = checkBase(mydbsmall),
#'   window_threshold = 10,
#'   count_option = "successive",
#'   condition = "dates"
#' )
#'
#' hn
#'
#' plot(hn)
#' plot(hn, type = "clustered_matrix")
#' @field edgelist (data.table) the list of edges (origin, target) and their associated number of movements (N) (read-only)
#' @field edgelist_long (data.table) edgelist with additional information (read-only)
#' @field matrix (matrix) the transfer matrix (active binding, read-only)
#' @field igraph (igraph) the igraph object corresponding to the network (active binding, read-only)
#' @field n_facilities the number of facilities in the network (read-only)
#' @field n_movements the total number of subject movements in the network (read-only)
#' @field window_threshold the window threshold used to compute the network (read-only)
#' @field nmoves_threshold the nmoves threshold used to compute the network (read-only)
#' @field noloops TRUE if loops have been removed (read-only)

#' @field hist_degrees histogram data of the number of connections per facility
#' @field LOSPerHosp the mean length of stay for each facility (read-only)
#' @field admissionsPerHosp the number of admissions to each facility (read-only)
#' @field subjectsPerHosp the number of unique subjects admitted to each facility (read-only)

#' @field degrees number of connections for each facilities (total, in, and out)(read-only)
#' @field closenesss the closeness centrality of each facility (read-only)
#' @field betweennesss the betweenness centrality of each facility (read-only)
#' @field cluster_infomap the assigned community for each facility, based on the infomap algorithm (read-only)
#' @field cluster_fast_greedy the assigned community for each facility, based on the greedy modularity optimization algorithm (read-only)
#' @field hubs_global Kleinberg's hub centrality scores, based on the entire network (read-only)
#' @field hubs_infomap same as hubs_global, but computed per community based on the infomap algorithm (read-only)
#' @field hubs_fast_greedy same as hubs_global, but computed per community based on the infomap algorithm (read-only)
#' @field metricsTable (data.table) all of the above metrics for each facility (read-only)
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(edgelist,
#' window_threshold,
#' nmoves_threshold,
#' noloops)}}{This method is used to create an object of this class with \code{edgelist} as the necessary information to create the network.
#' The other arguments \code{window_threshold}, \code{nmoves_threshold}, and \code{noloops} are specific to the \code{edgelist} and need to be provided.
#' For ease of use, it is preferable to use the function \code{\link[=hospinet_from_subject_database]{hospinet_from_subject_database()}}.}
#'   \item{\code{print()}}{This method prints basic information about the object.}
#'   \item{\code{plot(type = "matrix")}}{This method plots the network matrix by default.
#'   The argument \code{type} can take the following values:
#'   \describe{
#'   \item{matrix}{plot the network matrix,}
#'   \item{clustered_matrix}{identify and plot cluster(s) in the matrix using the infomap algorithm (from igraph),}
#'   \item{degree}{plot the histogram of the number of neighbors by facility,}
#'   \item{circular_network}{plot the network by clusters using a "spaghetti-like" layout. Only works when there are at least 2 clusters.}
#'   }
#'   }
#' }

HospiNet <- R6::R6Class("HospiNet",
  private = list(
    .matrix = NULL,
    .edgelist = NULL,
    .edgelist_long = NULL,
    .n_facilities = NULL,
    .n_movements = NULL,
    .window_threshold = NULL,
    .nmoves_threshold = NULL,
    .noloops = NULL,
    .igraph = NULL,
    .LOSPerHosp = NULL,
    .admissionsPerHosp = NULL,
    .subjectsPerHosp = NULL,
    .metricsTable = NULL,
    .degrees = NULL,
    .hist_degrees = NULL,
    .closenesss = NULL,
    .betweennesss = NULL,
    .cluster_infomap = NULL,
    .cluster_fast_greedy = NULL,
    .hubs_global = NULL,
    .hubs_infomap = NULL,
    .hubs_fast_greedy = NULL,
    plot_hist_degree = function() {
      plot_hist_degree_fnct(self$hist_degrees, self$degrees)
    },
    plot_matrix = function() {
      plot_matrix_fnct(self$n_facilities, self$edgelist)
    },
    plot_spaghetti = function() {
      plot_spaghetti_fnct(self$matrix, self$cluster_infomap, plotLinks = 5000, alphaSet = 0.1, edgeColourNode = FALSE, facilityColours = NULL, firstCircleCol = NULL, secondCircleCol = NULL)
    },
    plot_clustered_matrix = function() {
      plot_clustered_matrix_fnct(self$igraph, self$edgelist, self$n_facilities)
    },
    create_readonly_active = function(value, field_name, function_point = NULL, args = list()) {
        if (missing(value)) {
          if (is.null(function_point)) {
            private[[field_name]]
          } else {
            if (is.null(private[[field_name]])) {
              private[[field_name]] <- do.call(function_point, args)
            }
            private[[field_name]]
          }
        } else {
          stop(sprintf("`$%s` is read only", field_name), call. = FALSE)
        }
    }
  ),
  public = list(
    #' @description
    #' Create a new HospiNet object.
    #' @param edgelist Short format edgelist
    #' @param edgelist_long Long format edgelist
    #' @param window_threshold The window threshold used to compute the network
    #' @param nmoves_threshold The nmoves threshold used to compute the network
    #' @param noloops TRUE if loops have been removed
    #' @param prob_params Currently unused
    #' @param fsummary A pre-built data.table with the LOSPerHosp, subjectsPerHosp
    #' and admissionsPerHosp that don't need to be recomputed.
    #' @param create_MetricsTable all of the metrics for each facility
    #' @return A new `HospiNet` object
    initialize = function(edgelist,
                          edgelist_long,
                          window_threshold,
                          nmoves_threshold,
                          noloops,
                          prob_params,
                          fsummary = NULL,
                          create_MetricsTable = FALSE) {
      private$.edgelist <- edgelist
      private$.edgelist_long <- edgelist_long
      private$.window_threshold <- window_threshold
      private$.nmoves_threshold <- nmoves_threshold
      private$.noloops <- noloops
      if (!is.null(fsummary)) {
        private$.LOSPerHosp <- fsummary[, .(node, LOS)]
        private$.subjectsPerHosp <- fsummary[, .(node, subjects)]
        private$.admissionsPerHosp <- fsummary[, .(node, admissions)]
      } else {
        private$.LOSPerHosp <- data.table::data.table(node = unique(edgelist$origin), LOS = NA)
        private$.subjectsPerHosp <- data.table::data.table(node = unique(edgelist$origin), subjects = NA)
        private$.admissionsPerHosp <- data.table::data.table(node = unique(edgelist$origin), admissions = NA)
      }
      if (create_MetricsTable) {
        private$.metricsTable <- self$metricsTable
      }
    },
    #' @description
    #' Prints a basic description of the number of facilities and movements of
    #' a HospiNet object.
    #' @return NULL
    print = function() {
      cat(paste0(self$n_facilities, " facilities and ", self$n_movements, " movements.\n"))
      cat(paste0("Movement window is ", self$window_threshold, " days. \n"))
      if (self$n_facilities <= 10) {
        print(self$matrix)
      } else {
        cat("Matrix too big to be printed on screen.")
      }
    },
    #' @description
    #' Plots various representations of the HospiNet network
    #' @param type One of "matrix", "degree", "clustered_matrix", "circular network"
    #' Choose what you would like to plot - the connectivity matrix, degree
    #' distribution, the clusters, or the network in a circle.
    #' @param ... Additional arguments to be provided. Only supported for `type == 'circular_network`'.
    #' @return a `ggplot2` object
    plot = function(type = "matrix", ...) {
      if (type == "degree") {
        private$plot_hist_degree()
      } else if (type == "matrix") {
        private$plot_matrix()
      } else if (type == "clustered_matrix") {
        private$plot_clustered_matrix()
      } else if (type == "circular_network") {
        private$plot_spaghetti(...)
      } else {
        message("Unknown plot type for HospiNet")
      }
    }
  ),
  active = list(
    matrix = function(value) {
      if (missing(value)) {
        if (is.null(private$.matrix)) {
          message("Constructing full matrix")
          private$.matrix <- matrix_from_edgelist(edgelist = private$.edgelist, count = "N")
          private$.matrix
        } else {
          private$.matrix
        }
      } else {
        stop("`$matrix` is read only", call. = FALSE)
      }
    },

    igraph = function(value) {
      if (missing(value)) {
        if (is.null(private$.igraph)) {
          private$.igraph <- igraph::graph_from_data_frame(self$edgelist)
          # weighted = TRUE,
          # mode = "directed")
          #ensure we do not have weights with 0 or negative values
          igraph::E(private$.igraph)$weight <- self$edgelist[, pmax(0.1,N)] 

          private$.igraph
        } else {
          private$.igraph
        }
      } else {
        stop("`$igraph` is read only", call. = FALSE)
      }
    },

    edgelist = function(value) private$create_readonly_active(value, ".edgelist"),
    edgelist_long = function(value) private$create_readonly_active(value, ".edgelist_long"),
    window_threshold = function(value) private$create_readonly_active(value, ".window_threshold"),
    nmoves_threshold = function(value) private$create_readonly_active(value, ".nmoves_threshold"),
    noloops = function(value) private$create_readonly_active(value, ".noloops"),
    LOSPerHosp = function(value) private$create_readonly_active(value, ".LOSPerHosp"),
    subjectsPerHosp = function(value) private$create_readonly_active(value, ".subjectsPerHosp"),
    admissionsPerHosp = function(value) private$create_readonly_active(value, ".admissionsPerHosp"),
    n_facilities = function(value) private$create_readonly_active(value, ".n_facilities", vcount, list(self$igraph)),
    n_movements = function(value) private$create_readonly_active(value, ".n_movements", sum, list(igraph::strength(self$igraph, mode = "in"))),
    degrees = function(value)  private$create_readonly_active(value, ".degrees", get_degree, list(self$igraph, modes = c("total", "in", "out"))),
    closenesss = function(value)private$create_readonly_active(value, ".closenesss", get_closeness, list(self$igraph, modes = "total")),
    betweennesss = function(value)private$create_readonly_active(value, ".betweennesss", get_betweenness, list(self$igraph)),
    cluster_infomap = function(value)private$create_readonly_active(value, ".cluster_infomap", get_clusters, list(self$igraph, "cluster_infomap", undirected = "collapse")),
    cluster_fast_greedy = function(value)private$create_readonly_active(value, ".cluster_fast_greedy", get_clusters, list(self$igraph, "cluster_fast_greedy", undirected = "collapse")),
    hubs_global = function(value)private$create_readonly_active(value, ".hubs_global", get_hubs_global, list(self$igraph)),
    metricsTable = function(value)private$create_readonly_active(value, ".metricsTable", Reduce, list(function(x, y) merge(x = x, y = y, all = TRUE), list(
            self$subjectsPerHosp,
            self$admissionsPerHosp,
            self$LOSPerHosp,
            self$degrees,
            self$betweennesss,
            self$closenesss,
            self$cluster_fast_greedy,
            self$cluster_infomap,
            self$hubs_infomap,
            self$hubs_fast_greedy,
            self$hubs_global
          ))),

    hubs_infomap = function(value) {
      if (missing(value)) {
        if (is.null(private$.hubs_infomap)) {
          graph_byclust <- get_graph_bycluster(
            graph = self$igraph,
            DT = self$cluster_infomap,
            clusters = "cluster_infomap"
          )
          private$.hubs_infomap <- get_hubs_bycluster(graphs = graph_byclust, name = "cluster_infomap")
          #remove cluster column (already in cluster_infomap)
          private$.hubs_infomap <- private$.hubs_infomap[, cluster := NULL]
        }
        private$.hubs_infomap
      } else {
        stop("`$hubs_infomap` is read only", call. = FALSE)
      }
    },
    hubs_fast_greedy = function(value) {
      if (missing(value)) {
        if (is.null(private$.hubs_fast_greedy)) {
          graph_byclust <- get_graph_bycluster(
            graph = self$igraph,
            DT = self$cluster_fast_greedy,
            clusters = "cluster_fast_greedy"
          )
          private$.hubs_fast_greedy <- get_hubs_bycluster(graphs = graph_byclust, name = "cluster_fast_greedy")
          #remove cluster column (already in cluster_fast_greedy)
          private$.hubs_fast_greedy <- private$.hubs_fast_greedy[, cluster := NULL]
        }
        private$.hubs_fast_greedy
      } else {
        stop("`$hubs_fast_greedy` is read only", call. = FALSE)
      }
    },
    
    hist_degrees = function(value) {
      if (missing(value)) {
        d_t <- self$degrees[, .N, by = degree_total]
        d_i <- self$degrees[, .N, by = degree_in]
        d_o <- self$degrees[, .N, by = degree_out]
        max_deg <- max(self$degrees[, -1])
        private$.hist_degrees <- data.table(degree = 1:max_deg)
        private$.hist_degrees <- merge(private$.hist_degrees, d_t, by.x = "degree", by.y = "degree_total", all.x = TRUE)
        private$.hist_degrees <- merge(private$.hist_degrees, d_i, by.x = "degree", by.y = "degree_in", all.x = TRUE)
        private$.hist_degrees <- merge(private$.hist_degrees, d_o, by.x = "degree", by.y = "degree_out", all.x = TRUE)
        data.table::setnames(private$.hist_degrees, 2:4, c("total_degree", "in_degree", "out_degree"))
        private$.hist_degrees
      } else {
        stop("`$hist_degrees` is read only", call. = FALSE)
      }
    }
  )
)