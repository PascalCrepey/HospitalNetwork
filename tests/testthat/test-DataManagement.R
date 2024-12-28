context("Data management")
db = create_fake_subjectDB(n_subjects = 300, with_errors = TRUE)

test_that("errors are raised when needed", {
    expect_error(checkBase(db))
    expect_warning(checkBase(db,
                           convertDates = TRUE,
                           dateFormat = "ymd",
                           deleteErrors = "record",
                           deleteMissing = "record"))
})

suppressWarnings({base = checkBase(db,
                                  convertDates = TRUE,
                                  dateFormat = "ymd",
                                  deleteErrors = "record",
                                  deleteMissing = "record",
                                  subjectID = "sID",
                                  facilityID = "fID",
                                  admDate = "Adate",
                                  disDate = "Ddate")
})

test_that("checkFormat has cleaned the database", {
    cols = c("sID", "fID", "Adate", "Ddate")
    expect_s3_class(base, "data.table")
    expect_true(all(cols %in% colnames(base)))
    expect_type(base$sID, "character")
    expect_type(base$fID, "character")
    expect_true(all(base[, lapply(.SD, function(x) {
                                           trimws(x) == "" | is.na(x)
                                  }),
                         .SDcols = cols] == FALSE))
    expect_equal(anyDuplicated(base), 0)
})

test_that("checkDates has worked properly", {
    cols = c("Adate", "Ddate")
    expect_s3_class(base, "data.table")
    expect_true(all(cols %in% colnames(base)))
    expect_true(all(base[, lapply(.SD, lubridate::is.instant), .SDcols = cols] == TRUE))
    expect_true(all(base[, lapply(.SD, is.na), .SDcols = cols] == FALSE))
    expect_equal(base[Adate > Ddate, .N], 0)
})

test_that("no overlapping stays remain", {
    noOverlap = base[, .SD[-1]$Adate - .SD[-nrow(.SD)]$Ddate >= 0, by = sID]$V1
    expect_true(all(noOverlap))
})

    
    
    
