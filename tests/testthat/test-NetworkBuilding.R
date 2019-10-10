#context("Testing the network building\n")

##--- Create fake base with a specific number of connections -------------------
sID = sort(paste0("s0", rep(1:9,2)))
fID = rep(paste0("f", 1:2), 9)
base = data.table("sID" = sID, "fID" = fID)
base[15:18, sID := "s08"]
base[17:18, fID := c("f3", "f4")]
base[, c("Adate", "Ddate", "modeIN", "modeOUT") := NA_character_]
# (s01) Direct transfer with both dates and flags (transfer, mutation)
base[1, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "transfer")]
base[2, `:=`(Adate = "2019-01-02", Ddate = "2019-01-03",
             modeIN = "transfer", modeOUT = "death")]
# (s02) Direct transfer vis-a-vis dates, but not vis-a-vis flags
base[3, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "dom")]
base[4, `:=`(Adate = "2019-01-02", Ddate = "2019-01-03",
             modeIN = "dom", modeOUT = "death")]
# (s03) Direct transfer vis-a-vis flags, but not vis-a-vis dates
base[5, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "transfer")]
base[6, `:=`(Adate = "2019-01-03", Ddate = "2019-01-04",
             modeIN = "transfer", modeOUT = "death")]
# (s04) Indirect transfer successive < 42 days, not flagged
base[7, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "dom")]
base[8, `:=`(Adate = "2019-02-02", Ddate = "2019-02-03",
             modeIN = "dom", modeOUT = "death")]
# (s05) Indirect transfer successive < 42 days, and flagged
base[9, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "mutation")]
base[10, `:=`(Adate = "2019-02-02", Ddate = "2019-02-03",
             modeIN = "mutation", modeOUT = "death")]
# (s06) Indirect transfer successive > 42 days, not flagged
base[11, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "dom")]
base[12, `:=`(Adate = "2019-03-02", Ddate = "2019-03-03",
             modeIN = "dom", modeOUT = "death")]
# (s07) Indirect transfer successive > 42 days, and flagged
base[13, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "mutation")]
base[14, `:=`(Adate = "2019-03-02", Ddate = "2019-03-03",
             modeIN = "mutation", modeOUT = "death")]
# (s08) Here, multiple scenarii:
## (a)if indirect successive AND < 42 days: 3 transfers (15->16, 16->17, 17->18)
## (b)if indirect successive AND < 365 days: 3 transfers (15->16, 16->17, 17->18)
## (c)if indirect all stays AND < 42 days: 2 + (15->17, 16->18)
## (d)if indirect all stays AND < 365 days: 3 +  (15->17, 15->18, 16->18)
base[15, `:=`(Adate = "2019-01-01", Ddate = "2019-01-05",
              modeIN = "dom", modeOUT = "dom")]
base[16, `:=`(Adate = "2019-01-06", Ddate = "2019-01-10",
              modeIN = "dom", modeOUT = "dom")]
base[17, `:=`(Adate = "2019-02-10", Ddate = "2019-02-11",
              modeIN = "dom", modeOUT = "dom")]
base[18, `:=`(Adate = "2019-02-20", Ddate = "2019-02-21",
              modeIN = "dom", modeOUT = "dom")]
base[, `:=`(Adate = lubridate::parse_date_time(Adate, orders = "ymd"),
            Ddate = lubridate::parse_date_time(Ddate, orders = "ymd"))]
## Number of connections depending on conditions:
## Case A. If window = 0, only dates -> 2 (s01, s02)
## Case B. If window = 0 and flags -> 1 (s01)
### If SUCCESSIVE:
## Case C. If window = 42, not using flags -> 8 (s01, s02, s03, s04, s05, s08*3)
## Case D. If window = 42, and using flags -> 3 (s01, s03, s05)
## Case E. If window = 365, not using flags -> 10 (C + s06 + s07)
## Case F. If window = 365, and using flags -> 4 (D + s07)
## Case G. If only flags -> 4 (s01, s03, s05, s07)
### IF ALL STAYS (flags irrelevant):
## Case H. If window = 42 -> 10 (C + s08*2)
## Case I. If window = 365 -> 13 (E + s08*3)

elA = edgelist_from_base(base,
                         window_threshold = 0,
                         count_option = "successive",
                         condition = "dates")
elB = edgelist_from_base(base,
                         window_threshold = 0,
                         count_option = "successive",
                         condition = "both",
                         flag_vars = list("origin" = "modeOUT",
                                          "target" = "modeIN"),
                         flag_values = list("origin" = c("transfer", "mutation"),
                                            "target" = c("transfer", "mutation")))
elC = edgelist_from_base(base,
                         window_threshold = 42,
                         count_option = "successive",
                         condition = "dates")
elD = edgelist_from_base(base,
                         window_threshold = 42,
                         count_option = "successive",
                         condition = "both",
                         flag_vars = list("origin" = "modeOUT",
                                          "target" = "modeIN"),
                         flag_values = list("origin" = c("transfer", "mutation"),
                                            "target" = c("transfer", "mutation")))
elE = edgelist_from_base(base,
                         window_threshold = 365,
                         count_option = "successive",
                         condition = "dates")
elF = edgelist_from_base(base,
                         window_threshold = 365,
                         count_option = "successive",
                         condition = "both",
                         flag_vars = list("origin" = "modeOUT",
                                          "target" = "modeIN"),
                         flag_values = list("origin" = c("transfer", "mutation"),
                                            "target" = c("transfer", "mutation")))
elG = edgelist_from_base(base,
                         window_threshold = 0,
                         count_option = "successive",
                         condition = "flags",
                         flag_vars = list("origin" = "modeOUT",
                                          "target" = "modeIN"),
                         flag_values = list("origin" = c("transfer", "mutation"),
                                            "target" = c("transfer", "mutation")))
elH = edgelist_from_base(base,
                         window_threshold = 42,
                         count_option = "all",
                         condition = "dates")
elI = edgelist_from_base(base,
                         window_threshold = 365,
                         count_option = "all",
                         condition = "dates")

## TEST EDGELISTS
test_that("edgelist_from_base() computes the right number of connections", {
    expect_equal(elA$el_long$sID, c("s01", "s02")) 
    expect_equal(elB$el_long$sID, "s01") 
    expect_equal(elC$el_long$sID, c("s01", "s02", "s03", "s04", "s05", rep("s08", 3)))
    expect_equal(elD$el_long$sID, c("s01", "s03", "s05"))
    expect_equal(elE$el_long$sID, c("s01", "s02", "s03", "s04", "s05", "s06", "s07", rep("s08", 3)))
    expect_equal(elF$el_long$sID, c("s01", "s03", "s05", "s07"))
    expect_equal(elG$el_long$sID, c("s01", "s03", "s05", "s07"))
    expect_equal(elH$el_long$sID, c("s01", "s02", "s03", "s04", "s05", rep("s08", 5)))
    expect_equal(elI$el_long$sID, c("s01", "s02", "s03", "s04", "s05", "s06", "s07", rep("s08", 6)))
    #
    expect_equal(elA$el_aggr$N, 2)
    expect_equal(elB$el_aggr$N, 1)
    expect_equal(elC$el_aggr$N, c(6,1,1))
    expect_equal(elD$el_aggr$N, 3)
    expect_equal(elE$el_aggr$N, c(8,1,1))
    expect_equal(elF$el_aggr$N, 4)
    expect_equal(elG$el_aggr$N, 4)
    expect_equal(elH$el_aggr$N, c(6,1,1,1,1))
    expect_equal(elI$el_aggr$N, c(8,1,1,1,1,1))
})

## TEST MATRICES
test_that("matrix_from_edgelist() computed the right matrix", {
    suppressWarnings({
        mA = matrix_from_edgelist(elA$el_long, count = NULL, format_long = T)
        mB = matrix_from_edgelist(elB$el_long, count = NULL, format_long = T)
        mC = matrix_from_edgelist(elC$el_long, count = NULL, format_long = T)
        mD = matrix_from_edgelist(elD$el_long, count = NULL, format_long = T)
        mE = matrix_from_edgelist(elE$el_long, count = NULL, format_long = T)
        mF = matrix_from_edgelist(elF$el_long, count = NULL, format_long = T)
        mG = matrix_from_edgelist(elG$el_long, count = NULL, format_long = T)
        mH = matrix_from_edgelist(elH$el_long, count = NULL, format_long = T)
        mI = matrix_from_edgelist(elI$el_long, count = NULL, format_long = T)
        ##
        mA2 = matrix_from_edgelist(elA$el_aggr, count = 'N', format_long = F)
        mB2 = matrix_from_edgelist(elB$el_aggr, count = 'N', format_long = F)
        mC2 = matrix_from_edgelist(elC$el_aggr, count = 'N', format_long = F)
        mD2 = matrix_from_edgelist(elD$el_aggr, count = 'N', format_long = F)
        mE2 = matrix_from_edgelist(elE$el_aggr, count = 'N', format_long = F)
        mF2 = matrix_from_edgelist(elF$el_aggr, count = 'N', format_long = F)
        mG2 = matrix_from_edgelist(elG$el_aggr, count = 'N', format_long = F)
        mH2 = matrix_from_edgelist(elH$el_aggr, count = 'N', format_long = F)
        mI2 = matrix_from_edgelist(elI$el_aggr, count = 'N', format_long = F)
    })
    expect_equal(mA["f1", "f2"], 2)
    expect_equal(mB["f1", "f2"], 1)
    expect_equal(c(mC["f1", "f2"], mC["f2", "f3"], mC["f3", "f4"]), c(6, 1, 1))
    expect_equal(mD["f1", "f2"], 3)
    expect_equal(c(mE["f1", "f2"], mE["f2", "f3"], mE["f3", "f4"]), c(8, 1, 1))
    expect_equal(mF["f1", "f2"], 4)
    expect_equal(mG["f1", "f2"], 4)
    expect_equal(c(mH["f1", "f2"],
                   mH['f1','f3'],
                   mH['f2','f3'],
                   mH['f2','f4'],
                   mH['f3','f4']),
                 c(6, 1, 1, 1, 1))
    expect_equal(c(mI["f1","f2"],
                   mI['f1','f3'],
                   mI['f1','f4'],
                   mI['f2','f3'],
                   mI['f2','f4'],
                   mI['f3','f4']),
                 c(8, 1, 1, 1, 1, 1))
    expect_equal(mA,mA2)
    expect_equal(mB,mB2)
    expect_equal(mC,mC2)
    expect_equal(mD,mD2)
    expect_equal(mE,mE2)
    expect_equal(mF,mF2)
    expect_equal(mG,mG2)
    expect_equal(mH,mH2)
    expect_equal(mI,mI2)
})

test_that("matrix_from_base() computes the right matrix", {
    # only one test because this is a simple wrapper function of the two others
    mC = matrix_from_base(base,
                          window_threshold = 42,
                          count_option = "successive",
                          condition = "dates")
    expect_equal(c(mC["f1", "f2"], mC["f2", "f3"], mC["f3", "f4"]), c(6, 1, 1))
})



## # Test hospinet
## hnet = hospinet_from_subject_database(base = base,
##                                       window_threshold = 0,
##                                       count_option = "all",
##                                       condition = "dates",
##                                       noloops = FALSE)
## test_that("the network contains the right number of facilities", {
##     expect_equal(nrow(hnet$matrix), 4)
## })

## test_that("the network contains the right number of movements", {
##   expect_equal(sum(mydb[, .N - 1, by = sID]$V1), sum(hnet$matrix))
## })

## test_that("elements of the matrix are consistant with the edgelist", {
##   mydb = create_fake_subjectDB(n_subjects = 100, n_facility = 11)
##   hnet = hospinet_from_subject_database(base = mydb, noloops = FALSE)
##   hnetDT = as.data.table(hnet$matrix)
##   hnetDT[, origin := row.names(hnet$matrix)]
##   hnetDT = melt(hnetDT, id.vars = "origin", variable.name = "target", value.name = "N")
##   hnetDT[, target := as.character(target)]

##   setkey(hnetDT, origin, target)
##   hnetDT = hnetDT[N > 0]
  
##   expect_equivalent(hnetDT, hnet$edgelist)
## })
