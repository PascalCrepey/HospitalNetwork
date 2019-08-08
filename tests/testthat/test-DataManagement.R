context("Data management")
base = create_fake_patientDB(n_patients=10)
base[, Adate := as.character(Adate)]
cols = c("pID", "hID", "Adate", "Ddate")

test_that("checkFormat has cleaned the database", {
    tmp = checkFormat(base)
    expect_s3_class(tmp, "data.table")
    expect_true(all(cols %in% colnames(tmp)))
    expect_type(tmp$pID, "character")
    expect_type(tmp$hID, "character")
    expect_true(all(base[, lapply(.SD, function(x) {
                                           trimws(x) == "" | is.na(x)
                                  }),
                         .SDcols = cols] == FALSE))
})

test_that("checkDates has worked properly", {
    tmp = checkDates(base, convertDates = T, dateFormat = "ymd")
    cols = c("Adate", "Ddate")
    expect_s3_class(tmp, "data.table")
    expect_true(all(cols %in% colnames(tmp)))
    expect_true(all(base[, lapply(.SD, is.Date), .SDcols = cols] == TRUE))
    expect_true(all(base[, lapply(.SD, is.na), .SDcols = cols] == FALSE))
    expect_equal(base[Adate > Ddate, .N], 0)
})

    
    
    
