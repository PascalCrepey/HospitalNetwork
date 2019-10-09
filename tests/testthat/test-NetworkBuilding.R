#context("Testing the network building\n")

##--- Create fake base with a specific number of connections -------------------
pID = sort(paste0("p0", rep(1:9,2)))
hID = rep(paste0("h", 1:2), 9)
base = data.table("pID" = pID, "hID" = hID)
base[15:18, pID := "p08"]
base[17:18, hID := c("h3", "h4")]
base[, c("Adate", "Ddate", "modeIN", "modeOUT") := NA_character_]
# (p01) Direct transfer with both dates and flags (transfer, mutation)
base[1, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "transfer")]
base[2, `:=`(Adate = "2019-01-02", Ddate = "2019-01-03",
             modeIN = "transfer", modeOUT = "death")]
# (p02) Direct transfer vis-a-vis dates, but not vis-a-vis flags
base[3, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "dom")]
base[4, `:=`(Adate = "2019-01-02", Ddate = "2019-01-03",
             modeIN = "dom", modeOUT = "death")]
# (p03) Direct transfer vis-a-vis flags, but not vis-a-vis dates
base[5, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "transfer")]
base[6, `:=`(Adate = "2019-01-03", Ddate = "2019-01-04",
             modeIN = "transfer", modeOUT = "death")]
# (p04) Indirect transfer successive < 42 days, not flagged
base[7, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "dom")]
base[8, `:=`(Adate = "2019-02-02", Ddate = "2019-02-03",
             modeIN = "dom", modeOUT = "death")]
# (p05) Indirect transfer successive < 42 days, and flagged
base[9, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "mutation")]
base[10, `:=`(Adate = "2019-02-02", Ddate = "2019-02-03",
             modeIN = "mutation", modeOUT = "death")]
# (p06) Indirect transfer successive > 42 days, not flagged
base[11, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "dom")]
base[12, `:=`(Adate = "2019-03-02", Ddate = "2019-03-03",
             modeIN = "dom", modeOUT = "death")]
# (p07) Indirect transfer successive > 42 days, and flagged
base[13, `:=`(Adate = "2019-01-01", Ddate = "2019-01-02",
             modeIN = "dom", modeOUT = "mutation")]
base[14, `:=`(Adate = "2019-03-02", Ddate = "2019-03-03",
             modeIN = "mutation", modeOUT = "death")]
# (p08) Here, multiple scenarii:
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
## Case A. If window = 0, only dates -> 2 (p01, p02)
## Case B. If window = 0 and flags -> 1 (p01)
### If SUCCESSIVE:
## Case C. If window = 42, not using flags -> 8 (p01, p02, p03, p04, p05, p08*3)
## Case D. If window = 42, and using flags -> 3 (p01, p03, p05)
## Case E. If window = 365, not using flags -> 10 (C + p06 + p07)
## Case F. If window = 365, and using flags -> 4 (D + p07)
## Case G. If only flags -> 4 (p01, p03, p05, p07)
### IF ALL STAYS (flags irrelevant):
## Case H. If window = 42 -> 10 (C + p08*2)
## Case I. If window = 365 -> 13 (E + p08*3)

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
    expect_equal(elA$el_long$pID, c("p01", "p02")) 
    expect_equal(elB$el_long$pID, "p01") 
    expect_equal(elC$el_long$pID, c("p01", "p02", "p03", "p04", "p05", rep("p08", 3)))
    expect_equal(elD$el_long$pID, c("p01", "p03", "p05"))
    expect_equal(elE$el_long$pID, c("p01", "p02", "p03", "p04", "p05", "p06", "p07", rep("p08", 3)))
    expect_equal(elF$el_long$pID, c("p01", "p03", "p05", "p07"))
    expect_equal(elG$el_long$pID, c("p01", "p03", "p05", "p07"))
    expect_equal(elH$el_long$pID, c("p01", "p02", "p03", "p04", "p05", rep("p08", 5)))
    expect_equal(elI$el_long$pID, c("p01", "p02", "p03", "p04", "p05", "p06", "p07", rep("p08", 6)))
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
    expect_equal(mA["h1", "h2"], 2)
    expect_equal(mB["h1", "h2"], 1)
    expect_equal(c(mC["h1", "h2"], mC["h2", "h3"], mC["h3", "h4"]), c(6, 1, 1))
    expect_equal(mD["h1", "h2"], 3)
    expect_equal(c(mE["h1", "h2"], mE["h2", "h3"], mE["h3", "h4"]), c(8, 1, 1))
    expect_equal(mF["h1", "h2"], 4)
    expect_equal(mG["h1", "h2"], 4)
    expect_equal(c(mH["h1", "h2"],
                   mH['h1','h3'],
                   mH['h2','h3'],
                   mH['h2','h4'],
                   mH['h3','h4']),
                 c(6, 1, 1, 1, 1))
    expect_equal(c(mI["h1","h2"],
                   mI['h1','h3'],
                   mI['h1','h4'],
                   mH['h2','h3'],
                   mH['h2','h4'],
                   mH['h3','h4']),
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
    expect_equal(c(mC["h1", "h2"], mC["h2", "h3"], mC["h3", "h4"]), c(6, 1, 1))
})



## # Test hospinet
## hnet = hospinet_from_patient_database(base = base,
##                                       window_threshold = 0,
##                                       count_option = "all",
##                                       condition = "dates",
##                                       noloops = FALSE)
## test_that("the network contains the right number of hospitals", {
##     expect_equal(nrow(hnet$matrix), 4)
## })

## test_that("the network contains the right number of movements", {
##   expect_equal(sum(mydb[, .N - 1, by = pID]$V1), sum(hnet$matrix))
## })

## test_that("elements of the matrix are consistant with the edgelist", {
##   mydb = create_fake_patientDB(n_patients = 100, n_hospital = 11)
##   hnet = hospinet_from_patient_database(base = mydb, noloops = FALSE)
##   hnetDT = as.data.table(hnet$matrix)
##   hnetDT[, origin := row.names(hnet$matrix)]
##   hnetDT = melt(hnetDT, id.vars = "origin", variable.name = "target", value.name = "N")
##   hnetDT[, target := as.character(target)]

##   setkey(hnetDT, origin, target)
##   hnetDT = hnetDT[N > 0]
  
##   expect_equivalent(hnetDT, hnet$edgelist)
## })
