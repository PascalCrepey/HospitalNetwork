
#context("Testing the network content\n")

test_that("the network contains the right number of hospitals", {
  mydb = create_fake_patientDB(n_patients = 100, n_hospital = 11)
  hnet = hospinet_from_patient_database(base = mydb, noloops = FALSE)
  
  expect_equal(nrow(hnet$matrix), 11)
})

test_that("the network contains the right number of movements", {
  mydb = create_fake_patientDB(n_patients = 100, n_hospital = 11)
  hnet = hospinet_from_patient_database(base = mydb, noloops = FALSE)
  
  expect_equal(sum(mydb[, .N - 1, by = pID]$V1), sum(hnet$matrix))
})

test_that("elements of the matrix are consistant with the edgelist", {
  mydb = create_fake_patientDB(n_patients = 100, n_hospital = 11)
  hnet = hospinet_from_patient_database(base = mydb, noloops = FALSE)
  hnetDT = as.data.table(hnet$matrix)
  hnetDT[, origin := row.names(hnet$matrix)]
  hnetDT = melt(hnetDT, id.vars = "origin", variable.name = "target", value.name = "N")
  hnetDT[, target := as.character(target)]

  setkey(hnetDT, origin, target)
  hnetDT = hnetDT[N > 0]
  
  expect_equivalent(hnetDT, hnet$edgelist)
})