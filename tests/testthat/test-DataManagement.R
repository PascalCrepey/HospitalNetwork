context("Data management")
db = create_fake_patientDB(n_patients = 100, with_errors = TRUE)
base = checkBase(db,
                 convertDates = TRUE,
                 dateFormat = "ymd",
                 deleteErrors = "record",
                 deleteMissing = "record")

test_that("checkFormat has cleaned the database", {
    cols = c("pID", "hID", "Adate", "Ddate")
    expect_s3_class(base, "data.table")
    expect_true(all(cols %in% colnames(base)))
    expect_type(base$pID, "character")
    expect_type(base$hID, "character")
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
    expect_true(all(base[, lapply(.SD, is.instant), .SDcols = cols] == TRUE))
    expect_true(all(base[, lapply(.SD, is.na), .SDcols = cols] == FALSE))
    expect_equal(base[Adate > Ddate, .N], 0)
})

test_that("no overlapping stays remain", {
    noOverlap = base[, .SD[-1]$Adate - .SD[-nrow(.SD)]$Ddate >= 0, by = pID]$V1
    expect_true(all(noOverlap))
})


    
    
    
