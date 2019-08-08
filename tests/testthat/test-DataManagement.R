context("Data management")
base = create_fake_patientDB(n_patients=10)
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

               
    
    
