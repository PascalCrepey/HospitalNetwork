
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
#' @field degrees number of connections for each hospitals (total, in, and out)
#' @field hist_degrees histogram data of the number of connections per hospital 
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
    .numHospitals = NULL, # Same as n_hospitals, but with a different source, maybe useful as double-check
    .n_movements = NULL,
    .window_threshold = NULL,
    .nmoves_threshold = NULL,
    .noloops = NULL,
    .igraph = NULL,
    .LOSmean= NULL,
    .TBAmean = NULL,
    .LOSdistribution = NULL,
    .TBAdistribution = NULL,
    .LOSPerHosp = NULL,
    .admissions= NULL, 
    .admissionsPerHosp = NULL,
    .patients= NULL, 
    .patientsPerHosp = NULL,
    .metricsTable=NULL,
    .degrees = NULL,
    .hist_degrees = NULL,
    .closenesss = NULL,
    .betweennesss = NULL,
    .cluster_infomap = NULL,
    .cluster_fast_greedy=NULL,
    .hubs_global=NULL,
    .hubs_infomap=NULL,
    .hubs_fast_greedy=NULL,

    plot_hist_degree = function(){
      hd_long = melt(self$hist_degrees, id.vars = "degree")
      p = ggplot(hd_long[!is.na(value)], aes(x = degree, y = value, fill = variable)) + 
        geom_col(position = "dodge") +
        scale_fill_discrete(name = NULL) +
        scale_x_continuous(name = "Degree", limits = c(min(self$degrees[,-1]) - 1, NA)) +
        scale_y_continuous(name = "Number of hospitals") +
        theme_bw() +
        theme(legend.position = "bottom")
      p
    }, 
    plot_matrix = function(){
      n_h = self$n_hospitals
      ggplot(self$edgelist, aes(x = target, y = origin)) + 
        geom_raster(aes(fill = N)) + 
        scale_fill_gradient(low = "grey90", high = "red") +
        scale_x_discrete(name = "Target", 
                         #take the breaks from origin to get the same order
                         breaks = self$edgelist[, unique(origin)][seq(1,n_h, length.out = 10)] ) + 
        scale_y_discrete(name = "Origin", 
                         breaks = self$edgelist[, unique(origin)][seq(1,n_h, length.out = 10)]) +
        labs(x = "Origin", y = "Target", title = "Matrix") +
        theme_bw() + theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3),
                           axis.text.y = element_text(size = 8),
                           plot.title = element_text(size = 12), 
                           panel.grid = element_blank())
    }, 
    plot_clustered_matrix = function(){
      #get the clustering
      cl <- cluster_infomap(self$igraph, modularity = FALSE)
      
      dt_cl <- data.table(cl$names,cl$membership)
      #put it in a copy of the edgelist
      el = copy(self$edgelist)
      el[dt_cl, origin_c := V2, on = c("origin" = "V1")]
      el[dt_cl, target_c := V2, on = c("target" = "V1")]
      el[, col := ifelse(origin_c == target_c, origin_c, 0)]
      el[, col := factor(col)]
      
      #reorder the axis to make the cluster appear
      el$origin = factor(el$origin, levels = unique(el[order(origin_c), origin])) 
      el$target = factor(el$target, levels = unique(el[order(target_c), target])) 
      
      n_h = self$n_hospitals
      #plot it
      ggplot(el, aes(x = origin, y = target)) + 
        geom_raster(aes(fill = col)) + 
        scale_fill_manual(name = NULL, values = c("grey", rainbow(max(el$origin_c)))) +
        scale_x_discrete(name = "Origin",
                         breaks = levels(el$origin)[round(seq(1,n_h, length.out = 10),0)]) + 
        scale_y_discrete(name = "Target", 
                         breaks = levels(el$target)[round(seq(1,n_h, length.out = 10),0)]) +
        labs(x = "Origin", y = "Target", title = "Clustered matrix") +
        theme_bw() + theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.3),
                           axis.text.y = element_text(size = 8),
                           plot.title = element_text(size = 12), 
                           panel.grid = element_blank())
    }
  ),
  public = list(
    initialize = function(edgelist, 
                          window_threshold,
                          nmoves_threshold,
                          noloops,
                          hsummary=NULL,
                          dsummary=NULL,
                          create_MetricsTable=FALSE
                          ){
      private$.edgelist = edgelist
      private$.window_threshold = window_threshold
      private$.nmoves_threshold = nmoves_threshold
      private$.noloops = noloops
      if (!is.null(dsummary)){
        private$.LOSmean = dsummary$meanLOS
        private$.TBAmean = dsummary$meanTBA
        private$.admissions = dsummary$totalAdmissions
        private$.patients = dsummary$numPatients
        private$.numHospitals = dsummary$numHospitals # Same as n_hospitals, but with a different source, maybe useful as double-check
        private$.LOSdistribution = dsummary$LOSdistribution
        private$.TBAdistribution = dsummary$TBAdistribution
      }
      if (!is.null(hsummary)){
        private$.LOSPerHosp = hsummary[,.(node,LOS)]
        private$.patientsPerHosp = hsummary[,.(node,patients)]
        private$.admissionsPerHosp = hsummary[,.(node,admissions)]
      }
      if(create_MetricsTable){
        private$.metricsTable=self$metricsTable
      }
    },
    print = function() {
      cat(paste0(self$n_hospitals, " hospitals and ", self$n_movements, " movements.\n"))
      cat(paste0("Movement window is ", self$window_threshold, " days. \n"))
      if (self$n_hospitals <= 10) {
        print(self$matrix)
      }else{
        cat("Matrix too big to be printed on screen.")
      }
    },
    plot = function(type = "matrix"){
      if (type == "degree") private$plot_hist_degree()
      else if (type == "matrix") private$plot_matrix()
      else if (type == "clustered_matrix") private$plot_clustered_matrix()
    }
  ),
  active = list(
    matrix = function(value) {
      if (missing(value)) {
        if (is.null(private$.matrix)){
          message("Constructing full matrix")
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
          private$.igraph = igraph::graph_from_data_frame(self$edgelist) 
                                                                #weighted = TRUE, 
                                                                #mode = "directed")
          igraph::E(private$.igraph)$weight <- as.numeric(unlist(self$edgelist[,"N"]))
 
          private$.igraph
        } else {
          private$.igraph
        }
      }else {
        stop("`$igraph` is read only", call. = FALSE)
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
    },
    
    LOSmean = function(value) {
      if (missing(value)) {
        private$.LOSmean
      } else {
        stop("`$LOSmean` is read only", call. = FALSE)
      }
    },
    TBAmean = function(value) {
      if (missing(value)) {
        private$.TBAmean
      } else {
        stop("`$TBAmean` is read only", call. = FALSE)
      }
    },
    admissions = function(value) {
      if (missing(value)) {
        private$.admissions
      } else {
        stop("`$admissions` is read only", call. = FALSE)
      }
    },
    patients = function(value) {
      if (missing(value)) {
        private$.patients
      } else {
        stop("`$patients` is read only", call. = FALSE)
      }
    },
    numHospitals = function(value) {
      if (missing(value)) {
        private$.numHospitals
      } else {
        stop("`$numHospitals` is read only", call. = FALSE)
      }
    },
    LOSdistribution = function(value) {
      if (missing(value)) {
        private$.LOSdistribution
      } else {
        stop("`$LOSdistribution` is read only", call. = FALSE)
      }
    },
    
    TBAdistribution = function(value) {
      if (missing(value)) {
        private$.TBAdistribution
      } else {
        stop("`$noloops` is read only", call. = FALSE)
      }
    },

    LOSPerHosp = function(value) {
      if (missing(value)) {
        private$.LOSPerHosp
      } else {
        stop("`$LOSPerHosp` is read only", call. = FALSE)
      }
    },
    patientsPerHosp = function(value) {
      if (missing(value)) {
        private$.patientsPerHosp
      } else {
        stop("`$patientsPerHosp` is read only", call. = FALSE)
      }
    },
    admissionsPerHosp = function(value) {
      if (missing(value)) {
        private$.admissionsPerHosp
      } else {
        stop("`$admissionsPerHosp` is read only", call. = FALSE)
      }
    },
    
    n_hospitals = function(value) {
      if (missing(value)) {
        if (is.null(private$.n_hospitals)){
          private$.n_hospitals = vcount(self$igraph)
        } else {
          private$.n_hospitals
        }
      } else {
        stop("`$n_hospitals` is read only", call. = FALSE)
      }
    },
    n_movements = function(value) {
      if (missing(value)) {
        if (is.null(private$.n_movements)) {
          private$.n_movements = sum(igraph::strength(self$igraph,mode = "in"))
        }else{
          private$.n_movements
        }
        
      } else {
        stop("`$n_movements` is read only", call. = FALSE)
      }
    },
    
    degrees = function(value) {
      if (missing(value)) {
        if (is.null(private$.degrees)) {
          private$.degrees = get_degree(self$igraph, modes = c("total", "in", "out"))
        }
        private$.degrees
      } else {
        stop("`$noloops` is read only", call. = FALSE)
      }
    },
    closenesss = function(value) {
      if (missing(value)) {
        if (is.null(private$.closenesss)) {
          private$.closenesss = get_closeness(self$igraph, modes = "total")
        }
        private$.closenesss
      } else {
        stop("`$noloops` is read only", call. = FALSE)
      }
    },
    betweennesss = function(value) {
      if (missing(value)) {
        if (is.null(private$.betweennesss)) {
          private$.betweennesss = get_betweenness(self$igraph)
        }
        private$.betweennesss
      } else {
        stop("`$betweennesss` is read only", call. = FALSE)
      }
    },
   cluster_infomap = function(value) {
     if (missing(value)) {
       if (is.null(private$.cluster_infomap)) {
         private$.cluster_infomap = get_clusters(self$igraph,"cluster_infomap",undirected="collapse")
       }
       private$.cluster_infomap
     } else {
       stop("`$cluster_infomap` is read only", call. = FALSE)
     }
   },
   cluster_fast_greedy = function(value) {
     if (missing(value)) {
       if (is.null(private$.cluster_fast_greedy)) {
         private$.cluster_fast_greedy = get_clusters(self$igraph,"cluster_fast_greedy",undirected="collapse")
       }
       private$.cluster_fast_greedy
     } else {
       stop("`$cluster_fast_greedy` is read only", call. = FALSE)
     }
   },
   hubs_global= function(value) {
      if (missing(value)) {
        if (is.null(private$.hubs_global)) {
          private$.hubs_global=get_hubs_global(self$igraph)
        }
        private$.hubs_global
      } else {
        stop("`$hubs_global` is read only", call. = FALSE)
      }
   },
   hubs_infomap = function(value) {
     if (missing(value)) {
       if (is.null(private$.hubs_infomap)) {
         graph_byclust=get_graph_bycluster(graph=self$igraph,
                                         DT = self$cluster_infomap,
                                         clusters = "cluster_infomap")
        private$.hubs_infomap = get_hubs_bycluster(graphs = graph_byclust, name = "cluster_infomap")
       }
       private$.hubs_infomap
     } else {
       stop("`$hubs_infomap` is read only", call. = FALSE)
     }
   },
   hubs_fast_greedy = function(value) {
     if (missing(value)) {
       if (is.null(private$.hubs_fast_greedy)) {
         graph_byclust=get_graph_bycluster(graph=self$igraph,
                                           DT = self$cluster_fast_greedy,
                                           clusters = "cluster_fast_greedy")
         private$.hubs_fast_greedy = get_hubs_bycluster(graphs = graph_byclust, name = "cluster_fast_greedy")
       }
       private$.hubs_fast_greedy
     } else {
       stop("`$hubs_fast_greedy` is read only", call. = FALSE)
     }
   },
   metricsTable = function(value) {
     if (missing(value)) {
       if (is.null(private$.metricsTable)){
         private$.metricsTable = Reduce(function(x,y) merge(x = x, y = y, all=TRUE),list(
           self$patientsPerHosp,
           self$admissionsPerHosp,
           self$LOSPerHosp,
           self$betweennesss,
           self$closenesss,
           self$cluster_fast_greedy,
           self$cluster_infomap,
           self$hubs_infomap,
           self$hubs_fast_greedy,
           self$hubs_global
         ))
         #private$.metricsTable = self$hubs_fast_greedy
         #private$.metricsTable = get_metrics(self$igraph)
       }
       private$.metricsTable
     } else {
       stop("`$metricsTable` is read only", call. = FALSE)
     }
   },
    hist_degrees = function(value) {
      if (missing(value)) {
        d_t = self$degrees[,.N, by = degree_total]
        d_i = self$degrees[,.N, by = degree_in]
        d_o = self$degrees[,.N, by = degree_out]
        max_deg = max(self$degrees[,-1])
        private$.hist_degrees = data.table(degree = 1:max_deg)
        private$.hist_degrees = merge(private$.hist_degrees, d_t, by.x = "degree", by.y = "degree_total", all.x = TRUE)
        private$.hist_degrees = merge(private$.hist_degrees, d_i, by.x = "degree", by.y = "degree_in", all.x = TRUE)
        private$.hist_degrees = merge(private$.hist_degrees, d_o, by.x = "degree", by.y = "degree_out", all.x = TRUE)
        setnames(private$.hist_degrees, 2:4, c("total_degree", "in_degree", "out_degree"))
        private$.hist_degrees
      } else {
        stop("`$hist_degrees` is read only", call. = FALSE)
      }
    }
  )
)

