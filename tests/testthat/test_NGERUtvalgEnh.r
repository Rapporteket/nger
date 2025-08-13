test_that("NGERUtvalgEnh filters data correctly", {
  # Create a mock dataset
  RegData <- data.frame(
    ReshId = c(1, 2, 3, 1, 2),
    Alder = c(25, 35, 45, 55, 65),
    InnDato = as.Date(c('2017-01-01', '2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01')),
    OpMetode = c(1, 2, 3, 1, 2),
    LapProsedyre1 = c('LCD01', 'LCD04', 'LCC11', 'LCD11', 'LEF51'),
    LapRobotKirurgi = c(0, 1, 0, 1, 0),
    Opf0AlvorlighetsGrad = c(1, 2, 3, 4, 1),
    Opf0Status = c(1, 1, 1, 1, 1),
    OpBehNivaa = c(1, 2, 3, 1, 2),
    ShNavn = c('Hospital A', 'Hospital B', 'Hospital C', 'Hospital A', 'Hospital B')
  )

  # Test default parameters
  result <- NGERUtvalgEnh(RegData)
  expect_equal(nrow(result$RegData), 5)
  expect_equal(result$utvalgTxt[1], "Operasjonsdato: 2017-01-01 til 2021-01-01")

  # Test filtering by age
  result <- NGERUtvalgEnh(RegData, minald = 30, maxald = 50)
  expect_equal(nrow(result$RegData), 2)
  expect_equal(result$utvalgTxt[2], "Pasienter fra 35 til 45 år")

  # Test filtering by operation method
  result <- NGERUtvalgEnh(RegData, OpMetode = 1)
  expect_equal(nrow(result$RegData), 2)
  expect_equal(result$utvalgTxt[3], "Operasjonstype: Laparoskopi")

  # Test filtering by severity
  result <- NGERUtvalgEnh(RegData, AlvorlighetKompl = c('1', '2'))
  expect_equal(nrow(result$RegData), 2)
  expect_equal(result$utvalgTxt[5], "Alvorlighetsgrad: Liten,Middels")

  # Test filtering by treatment level
  result <- NGERUtvalgEnh(RegData, behNivaa = 2)
  expect_equal(nrow(result$RegData), 2)
  expect_equal(result$utvalgTxt[4], "Behandlingsnivå: Dagkirurgi")

  # Test filtering by reshID
  result <- NGERUtvalgEnh(RegData, reshID = 1)
  expect_equal(nrow(result$RegData), 2)
  expect_equal(result$hovedgrTxt, "Hospital A")

  # Test filtering by diagnosis
  result <- NGERUtvalgEnh(RegData, velgDiag = 1)
  expect_equal(nrow(result$RegData), 1)
  expect_equal(result$utvalgTxt[6], "Diagnose: Godartede ovarialcyster")
})