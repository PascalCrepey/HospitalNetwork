#' @title Build Temporal Edgelist
#' @description Constructs a temporal edgelist from hospital stays.
#' Edges represent patient transfers and have a validity duration.
#' If multiple transfers occur within the validity window, the edge is extended.
#'
#' @param base (data.table) Database with: sID, fID, Adate, Ddate.
#' @param window_threshold (integer) Max days between discharge (A) and admission (B) for a transfer.
#' @param edge_duration (integer) Days an edge remains active after a transfer.
#' @param noloops (logical) If TRUE, remove self-loops.
#'
#' @return A data.table with: origin, target, t_start, t_end, weight, last_event.
#' @export
#' @importFrom data.table data.table setkeyv copy shift as.data.table rbindlist
hospinet_temporal_edgelist <- function(base, 
                                       window_threshold = 365, 
                                       edge_duration = 30,
                                       noloops = TRUE) {
  
  # Check required columns
  req_cols <- c("sID", "fID", "Adate", "Ddate")
  if (!all(req_cols %in% names(base))) {
    stop("Database must contain: sID, fID, Adate, Ddate")
  }
  
  dt <- data.table::copy(base)
  data.table::setorderv(dt, c("sID", "Adate"))
  
  # 1. Detect raw transfers
  # Compare line i (origin) with i+1 (target) for same patient
  dt[, `:=`(
    next_fID = data.table::shift(fID, type = "lead"),
    next_Adate = data.table::shift(Adate, type = "lead")
  ), by = sID]
  
  dt[, gap := as.numeric(difftime(next_Adate, Ddate, units = "days"))]
  
  transfers <- dt[
    !is.na(next_fID) & gap >= 0 & gap <= window_threshold,
    .(
      origin = fID,
      target = next_fID,
      event_date = next_Adate # Link activates upon arrival at target
    )
  ]
  
  if (noloops) {
    transfers <- transfers[origin != target]
  }
  
  if (nrow(transfers) == 0) {
    return(data.table::data.table(origin = character(), target = character(), 
                                  t_start = as.POSIXct(character()), t_end = as.POSIXct(character()), 
                                  weight = integer(), last_event = as.POSIXct(character())))
  }
  
  # Each transfer opens a risk window [event_date, event_date + edge_duration]
  transfers[, `:=`(
    start_raw = event_date,
    end_raw   = event_date + (edge_duration * 24 * 3600) # Add seconds if POSIXct
  )]
  
  data.table::setkeyv(transfers, c("origin", "target", "start_raw"))
  
  # Calculate cumulative max end date to detect breaks in continuity
  transfers[, max_end_so_far := cummax(as.numeric(end_raw)), by = .(origin, target)]
  transfers[, prev_max_end := data.table::shift(max_end_so_far, fill = 0), by = .(origin, target)]
  
  # A new group starts if current start is strictly after previous max end
  transfers[, new_group := as.integer(as.numeric(start_raw) > prev_max_end)]
  
  # First row of group is always new
  transfers[1, new_group := 1, by = .(origin, target)]
  
  # Unique group ID
  transfers[, group_id := cumsum(new_group), by = .(origin, target)]
  
  temporal_edgelist <- transfers[, .(
    t_start = min(start_raw),
    t_end = max(end_raw),       # Full coverage of the merged interval
    weight = .N,                # Number of transfers keeping this link active
    last_event = max(start_raw) # Last actual transfer date
  ), by = .(origin, target, group_id)]
  
  temporal_edgelist[, group_id := NULL]
  
  class(temporal_edgelist) <- c("hospinet.temporal.edgelist", class(temporal_edgelist))
  
  return(temporal_edgelist)
}

#' @title Get Network Snapshot
#' @description Generates a static igraph object at a specific time t.
#'
#' @param temporal_el (data.table) Output from hospinet_temporal_edgelist.
#' @param snapshot_time (POSIXct/Date) Time t to observe.
#' @param weight_mode (character) "active" (binary) or "cumulative" (total transfers in interval).
#'
#' @return An igraph object representing the network at time t.
#' @export
#' @importFrom igraph graph_from_data_frame make_empty_graph
get_network_snapshot <- function(temporal_el, snapshot_time, weight_mode = "active") {
  
  # Edge is active if t_start <= t <= t_end
  active_edges <- temporal_el[t_start <= snapshot_time & t_end >= snapshot_time]
  
  if (nrow(active_edges) == 0) {
    return(igraph::make_empty_graph(n = 0, directed = TRUE))
  }
  
  if (weight_mode == "cumulative") {
    edges_df <- active_edges[, .(origin, target, weight)]
  } else {
    edges_df <- active_edges[, .(origin, target, weight = 1)]
  }
  
  g <- igraph::graph_from_data_frame(d = edges_df, directed = TRUE)
  
  return(g)
}
