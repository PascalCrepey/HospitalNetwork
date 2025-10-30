context("Adjusting overlaping stays")

## Fake base with overlapping stays
sID = sort(rep(LETTERS[1:4],6))
fID = rep(c("f1", "f2", "f3", "f4", "f5", "f6"), 4)
baseOver = data.table("sID" = sID, "fID" = fID)
## sID A
baseOver[1, adm := "2019-01-01"]
baseOver[1, dis := "2019-01-03"]
baseOver[2, adm := "2019-01-02"]
baseOver[2, dis := "2019-01-04"]
baseOver[3, adm := "2019-01-04"]
baseOver[3, dis := "2019-01-04"]
baseOver[4, adm := "2019-01-06"]
baseOver[4, dis := "2019-01-10"]
baseOver[5, adm := "2019-01-08"]
baseOver[5, dis := "2019-01-09"]
baseOver[6, adm := "2019-01-10"]
baseOver[6, dis := "2019-01-11"]
## sID B
baseOver[7, adm := "2019-02-02"]
baseOver[7, dis := "2019-02-03"]
baseOver[8, adm := "2019-02-03"]
baseOver[8, dis := "2019-02-05"]
baseOver[9, adm := "2019-02-04"]
baseOver[9, dis := "2019-02-06"]
baseOver[10, adm := "2019-02-05"]
baseOver[10, dis := "2019-02-08"]
baseOver[11, adm := "2019-02-07"]
baseOver[11, dis := "2019-02-09"]
baseOver[12, adm := "2019-02-09"]
baseOver[12, dis := "2019-02-10"]
## sID C
baseOver[13, adm := "2019-03-03"]
baseOver[13, dis := "2019-03-05"]
baseOver[14, adm := "2019-03-04"]
baseOver[14, dis := "2019-03-07"]
baseOver[15, adm := "2019-03-06"]
baseOver[15, dis := "2019-03-08"]
baseOver[16, adm := "2019-03-07"]
baseOver[16, dis := "2019-03-09"]
baseOver[17, adm := "2019-03-09"]
baseOver[17, dis := "2019-03-10"]
baseOver[18, adm := "2019-03-11"]
baseOver[18, dis := "2019-03-12"]
## sID D
baseOver[19, adm := "2019-01-01"]
baseOver[19, dis := "2019-01-25"]
baseOver[20, adm := "2019-01-03"]
baseOver[20, dis := "2019-01-20"]
baseOver[21, adm := "2019-01-04"]
baseOver[21, dis := "2019-01-15"]
baseOver[22, adm := "2019-01-05"]
baseOver[22, dis := "2019-01-06"]
baseOver[23, adm := "2019-01-25"]
baseOver[23, dis := "2019-01-28"]
baseOver[24, adm := "2019-01-26"]
baseOver[24, dis := "2019-01-28"]
baseOver[, `:=`(adm = as.Date(adm),
                dis = as.Date(dis))]

## Correct base if admission is leading
sID = sort(rep(LETTERS[1:4],6))
fID = rep(c("f1", "f2", "f3", "f4", "f5", "f6"), 4)
baseCorrAdm = data.table("sID" = sID, "fID" = fID)
## sID A
baseCorrAdm[1, adm := "2019-01-01"]
baseCorrAdm[1, dis := "2019-01-02"]
baseCorrAdm[2, adm := "2019-01-02"]
baseCorrAdm[2, dis := "2019-01-04"]
baseCorrAdm[3, adm := "2019-01-04"]
baseCorrAdm[3, dis := "2019-01-04"]
baseCorrAdm[4, adm := "2019-01-06"]
baseCorrAdm[4, dis := "2019-01-08"]
baseCorrAdm[5, adm := "2019-01-08"]
baseCorrAdm[5, dis := "2019-01-09"]
baseCorrAdm[6, adm := "2019-01-10"]
baseCorrAdm[6, dis := "2019-01-11"]
## sID B
baseCorrAdm[7, adm := "2019-02-02"]
baseCorrAdm[7, dis := "2019-02-03"]
baseCorrAdm[8, adm := "2019-02-03"]
baseCorrAdm[8, dis := "2019-02-04"]
baseCorrAdm[9, adm := "2019-02-04"]
baseCorrAdm[9, dis := "2019-02-05"]
baseCorrAdm[10, adm := "2019-02-05"]
baseCorrAdm[10, dis := "2019-02-07"]
baseCorrAdm[11, adm := "2019-02-07"]
baseCorrAdm[11, dis := "2019-02-09"]
baseCorrAdm[12, adm := "2019-02-09"]
baseCorrAdm[12, dis := "2019-02-10"]
## sID C
baseCorrAdm[13, adm := "2019-03-03"]
baseCorrAdm[13, dis := "2019-03-04"]
baseCorrAdm[14, adm := "2019-03-04"]
baseCorrAdm[14, dis := "2019-03-06"]
baseCorrAdm[15, adm := "2019-03-06"]
baseCorrAdm[15, dis := "2019-03-07"]
baseCorrAdm[16, adm := "2019-03-07"]
baseCorrAdm[16, dis := "2019-03-09"]
baseCorrAdm[17, adm := "2019-03-09"]
baseCorrAdm[17, dis := "2019-03-10"]
baseCorrAdm[18, adm := "2019-03-11"]
baseCorrAdm[18, dis := "2019-03-12"]
## sID D
baseCorrAdm[19, adm := "2019-01-01"]
baseCorrAdm[19, dis := "2019-01-03"]
baseCorrAdm[20, adm := "2019-01-03"]
baseCorrAdm[20, dis := "2019-01-04"]
baseCorrAdm[21, adm := "2019-01-04"]
baseCorrAdm[21, dis := "2019-01-05"]
baseCorrAdm[22, adm := "2019-01-05"]
baseCorrAdm[22, dis := "2019-01-06"]
baseCorrAdm[23, adm := "2019-01-25"]
baseCorrAdm[23, dis := "2019-01-26"]
baseCorrAdm[24, adm := "2019-01-26"]
baseCorrAdm[24, dis := "2019-01-28"]
## Additional stays
additional_stays = data.table(
    "sID" = c('A', rep('D', 3)),
    "fID" = c("f4", "f3", "f2", "f1"),
    "adm" = c("2019-01-09",
              "2019-01-06",
              "2019-01-15",
              "2019-01-20"),
    "dis" = c("2019-01-10",
              "2019-01-15",
              "2019-01-20",
              "2019-01-25"))
baseCorrAdm = rbind(baseCorrAdm, additional_stays)
baseCorrAdm[, `:=`(adm = as.Date(adm),
                dis = as.Date(dis))]
setkey(baseCorrAdm, sID, adm)

## Correct base if discharge is leading
sID = sort(rep(LETTERS[1:4],6))
fID = rep(c("f1", "f2", "f3", "f4", "f5", "f6"), 4)
baseCorrDis = data.table("sID" = sID, "fID" = fID)
## sID A
baseCorrDis[1, adm := "2019-01-01"]
baseCorrDis[1, dis := "2019-01-03"]
baseCorrDis[2, adm := "2019-01-03"]
baseCorrDis[2, dis := "2019-01-04"]
baseCorrDis[3, adm := "2019-01-04"]
baseCorrDis[3, dis := "2019-01-04"]
baseCorrDis[4, adm := "2019-01-06"]
baseCorrDis[4, dis := "2019-01-08"]
baseCorrDis[5, adm := "2019-01-08"]
baseCorrDis[5, dis := "2019-01-09"]
baseCorrDis[6, adm := "2019-01-10"]
baseCorrDis[6, dis := "2019-01-11"]
## sID B
baseCorrDis[7, adm := "2019-02-02"]
baseCorrDis[7, dis := "2019-02-03"]
baseCorrDis[8, adm := "2019-02-03"]
baseCorrDis[8, dis := "2019-02-05"]
baseCorrDis[9, adm := "2019-02-05"]
baseCorrDis[9, dis := "2019-02-06"]
baseCorrDis[10, adm := "2019-02-06"]
baseCorrDis[10, dis := "2019-02-08"]
baseCorrDis[11, adm := "2019-02-08"]
baseCorrDis[11, dis := "2019-02-09"]
baseCorrDis[12, adm := "2019-02-09"]
baseCorrDis[12, dis := "2019-02-10"]
## sID CDis
baseCorrDis[13, adm := "2019-03-03"]
baseCorrDis[13, dis := "2019-03-05"]
baseCorrDis[14, adm := "2019-03-05"]
baseCorrDis[14, dis := "2019-03-07"]
baseCorrDis[15, adm := "2019-03-07"]
baseCorrDis[15, dis := "2019-03-08"]
baseCorrDis[16, adm := "2019-03-08"]
baseCorrDis[16, dis := "2019-03-09"]
baseCorrDis[17, adm := "2019-03-09"]
baseCorrDis[17, dis := "2019-03-10"]
baseCorrDis[18, adm := "2019-03-11"]
baseCorrDis[18, dis := "2019-03-12"]
## sID DDis
baseCorrDis[19, adm := "2019-01-01"]
baseCorrDis[19, dis := "2019-01-03"]
baseCorrDis[20, adm := "2019-01-03"]
baseCorrDis[20, dis := "2019-01-04"]
baseCorrDis[21, adm := "2019-01-04"]
baseCorrDis[21, dis := "2019-01-05"]
baseCorrDis[22, adm := "2019-01-05"]
baseCorrDis[22, dis := "2019-01-06"]
baseCorrDis[23, adm := "2019-01-25"]
baseCorrDis[23, dis := "2019-01-28"]
baseCorrDis[24, adm := "2019-01-28"]
baseCorrDis[24, dis := "2019-01-28"]
## Additional stays
additional_stays = data.table(
    "sID" = c('A', rep('D', 3)),
    "fID" = c("f4", "f3", "f2", "f1"),
    "adm" = c("2019-01-09",
              "2019-01-06",
              "2019-01-15",
              "2019-01-20"),
    "dis" = c("2019-01-10",
              "2019-01-15",
              "2019-01-20",
              "2019-01-25"))
baseCorrDis = rbind(baseCorrDis, additional_stays)
baseCorrDis[, `:=`(adm = as.Date(adm),
                dis = as.Date(dis))]
setkey(baseCorrDis, sID, adm)

data.table::setnames(baseCorrDis, c("adm", "dis"), c("Adate", "Ddate"))
data.table::setnames(baseOver, c("adm", "dis"), c("Adate", "Ddate"))
data.table::setnames(baseCorrAdm, c("adm", "dis"), c("Adate", "Ddate"))

#=== TESTS ==========================================================
report = list()
report$base = copy(baseOver)

test_that("adjust_overlaps() works ok with admission leading", {
    baseAdm = adjust_overlaps(report, leading = "admission")$base
    expect_identical(baseAdm, baseCorrAdm)
})
report$base = copy(baseOver)
test_that("adjust_overlaps() works ok with discharge leading", {
    baseDis = adjust_overlaps(report, leading = "discharge")$base
    expect_identical(baseDis, baseCorrDis)
})
report$base = copy(baseOver)
test_that("adjust_overlapping_stays() works ok with admission leading", {
    baseOut = HospitalNetwork:::adjust_overlapping_stays(report)$base
    setkey(baseOut, sID, Adate)
    expect_identical(baseCorrAdm, baseOut)
})

    
    
