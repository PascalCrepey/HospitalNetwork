# fake subject database
# library(data.table)

#' Create a fake subject database
#'
#' @param n_subjects the number of different subjects in the database
#' @param n_facilities the number of facility present in the database
#' @param avg_n_stays the average number of stays per subject
#' @param days_since_discharge the number of days between a discharge date and an admission date (default: max(0, rnorm(1, mean = 30, sd = 10)))
#' @param length_of_stay the length of stay (default: max(1, rnorm(1, mean = 5, sd = 3) )
#' @param start_id_subjects,start_id_facilities change starting ids (used for clustered network)
#' @param with_errors (boolean) introduce or not random errors in the database. Default to FALSE.
#'
#' @return a data.table containing all subjects stays
#' @export
#'
#' @examples
#' mydb <- create_fake_subjectDB(n_subjects = 100, n_facilities = 10)
#' mydb
#' @importFrom stats rnorm
#'
create_fake_subjectDB <- function(n_subjects = 100,
                                  n_facilities = 10,
                                  avg_n_stays = 3,
                                  days_since_discharge = NULL,
                                  length_of_stay = NULL,
                                  start_id_subjects = 1,
                                  start_id_facilities = 1,
                                  with_errors = FALSE) {
  # create subjects IDs
  sIDs <- paste0("s", formatC((1 + start_id_subjects - 1):(start_id_subjects + n_subjects - 1),
    width = nchar(n_subjects), flag = "0"
  ))
  # create facility IDs
  fIDs <- paste0("f", formatC((1 + start_id_facilities - 1):(start_id_facilities + n_facilities - 1),
    width = nchar(n_facilities), flag = "0"
  ))

  all_s_stays <- lapply(sIDs, function(sID) {
    n_moves <- max(1, rnorm(1, mean = avg_n_stays, sd = ceiling(avg_n_stays * 0.3)))
    facilities <- sample(fIDs, n_moves, replace = TRUE)

    s_stays <- NULL
    last_discharge_date <- NULL
    for (fID in facilities) {
      stay <- create_subject_stay(
        sID = sID, fID = fID,
        last_discharge_date = last_discharge_date,
        days_since_discharge = days_since_discharge,
        length_of_stay = length_of_stay
      )
      last_discharge_date <- stay$Ddate

      if (!is.null(s_stays)) {
        s_stays <- rbindlist(list(s_stays, stay))
      } else {
        s_stays <- stay
      }
    }
    s_stays
  })
  all_s_stays <- rbindlist(all_s_stays)

  ## Generate errors in the database
  if (with_errors) {
    N <- nrow(all_s_stays)
    if (N <= 10) stop("Not enough records to introduce errors")
    # Add overlapping stays
    all_s_stays <- rbind(
      all_s_stays,
      data.table(
        sID = c(rep("X", 3), rep("Y", 4)),
        fID = c("a", "b", "a", "a", "b", "c", "c"),
        Adate = lubridate::as_datetime(c(
          Sys.Date(),
          Sys.Date() + 1,
          Sys.Date() + 9,
          Sys.Date(),
          Sys.Date() + 1,
          Sys.Date() + 8,
          Sys.Date() + 9
        )),
        Ddate = lubridate::as_datetime(c(
          Sys.Date() + 7,
          Sys.Date() + 9,
          Sys.Date() + 10,
          Sys.Date() + 7,
          Sys.Date() + 4,
          Sys.Date() + 11,
          Sys.Date() + 13
        ))
      )
    )
    # Mess up dates
    all_s_stays[sample(1:N, N / n_subjects), Adate := Adate + 1000 * 3600 * 24] # Adate > Ddate
    all_s_stays[, Adate := as.character(Adate)] # Wrong format
    # Duplicated rows
    all_s_stays <- rbind(
      all_s_stays,
      all_s_stays[sample(1:N, N / n_subjects), ]
    )
    # Missing values
    all_s_stays[1, Ddate := NA]
    all_s_stays[2, fID := " "]
    all_s_stays[3, sID := "NA"]
    all_s_stays[4, Adate := ""]
    all_s_stays[5, Adate := "na"]
    all_s_stays[6, Adate := "N/A"]
    all_s_stays[7, Adate := "n/a"]
    all_s_stays[8, Adate := "Na"]
    all_s_stays[9, Adate := "N/a"]
    all_s_stays[10, Adate := "NaN"]
    colnames(all_s_stays) <- c("subject", "facility", "admission", "discharge")
  }

  return(all_s_stays)
}

#' Create a fake subject database with clustering
#'
#' @param n_subjects the number of different subjects in the database
#' @param n_facilities the number of facility present in the database
#' @param avg_n_stays the average number of stays per subject
#' @param days_since_discharge the number of days between a discharge date and an admission date (default: max(0, rnorm(1, mean = 30, sd = 10)))
#' @param length_of_stay the length of stay (default: max(1, rnorm(1, mean = 5, sd = 3) )
#' @param n_clusters the number of cluster in the network
#'
#' @return a data.table containing all subjects stays
#' @examples
#' mydb <- create_fake_subjectDB_clustered(n_subjects = 100, n_facilities = 10)
#' mydb
#' @export
#'
create_fake_subjectDB_clustered <- function(n_subjects = 50,
                                            n_facilities = 10,
                                            avg_n_stays = 3,
                                            days_since_discharge = NULL,
                                            length_of_stay = NULL,
                                            n_clusters = 3) {
  # create sub subjectDB
  n_subjects_cl <- round(n_subjects / n_clusters, 0)
  n_facilities_cl <- round(n_facilities / n_clusters, 0)
  s_stays <- lapply(1:n_clusters, function(i) {
    create_fake_subjectDB(
      n_subjects = n_subjects_cl,
      n_facilities = n_facilities_cl,
      avg_n_stays = avg_n_stays,
      days_since_discharge = days_since_discharge,
      length_of_stay = length_of_stay,
      start_id_subjects = (i - 1) * n_subjects_cl + 1,
      start_id_facilities = (i - 1) * n_facilities_cl + 1
    )
  })

  # get the last stays of 20% of subjects in each subnetwork
  connecting_subjects <- lapply(1:n_clusters, function(i) {
    stays <- s_stays[[i]]
    n_p <- length(unique(stays$sID))
    last_stays <- stays[, .SD[.N, ], by = sID][sample(1:n_p, ceiling(n_p * 0.3))]
    last_stays[, cluster_id := i]
  })
  connecting_subjects <- rbindlist(connecting_subjects)

  # and add connections to other networks
  connecting_stays <- lapply(1:connecting_subjects[, .N], function(i) {
    curr_clust <- connecting_subjects[i, cluster_id]
    connecting_f <- sample(connecting_subjects[cluster_id != curr_clust, fID], 1)
    create_subject_stay(
      sID = connecting_subjects[i, sID],
      fID = connecting_f,
      last_discharge_date = connecting_subjects[i, Ddate],
      days_since_discharge = days_since_discharge,
      length_of_stay = length_of_stay
    )
  })
  connecting_stays <- rbindlist(connecting_stays)
  s_stays[[n_clusters + 1]] <- connecting_stays

  # merge all stays
  all_s_stays <- rbindlist(s_stays)

  # randomize facility names
  trFID <- data.table(curr_fid = unique(all_s_stays$fID))
  trFID[, new_fid := sample(curr_fid, .N, replace = FALSE)]
  all_s_stays[trFID, fID := new_fid, on = c("fID" = "curr_fid")]

  return(all_s_stays)
}

#' Create a fake subject stay
#'
#' create_subject_stay is an internal function used by create_fake_subjectDB.
#'
#' @param sID the subject ID
#' @param fID the facility ID
#' @param last_discharge_date the last discharge date
#' @param days_since_discharge the number of days since last discharge (default: max(0, rnorm(1, mean = 30, sd = 10)))
#' @param length_of_stay the length of stay (default: max(1, rnorm(1, mean = 5, sd = 3))
#'
#' @return a one row data.table corresponding to the subject stay.
#' @export
create_subject_stay <- function(sID, fID,
                                last_discharge_date = NULL,
                                days_since_discharge = NULL,
                                length_of_stay = NULL) {
  if (is.null(last_discharge_date)) {
    last_discharge_date <- lubridate::as_datetime("2019-01-01")
  }

  if (is.null(days_since_discharge)) {
    days_since_discharge <- round(max(0, rnorm(1, mean = 30, sd = 10))) * 3600 * 24 # actually seconds since we are in POSIXct
  }

  if (is.null(length_of_stay)) {
    length_of_stay <- round(max(1, rnorm(1, mean = 5, sd = 3))) * 3600 * 24 # actually seconds since we are in POSIXct
  }

  Adate <- last_discharge_date + days_since_discharge

  data.table(
    sID = sID,
    fID = fID,
    Adate = Adate,
    Ddate = Adate + length_of_stay
  )
}