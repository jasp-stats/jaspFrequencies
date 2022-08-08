context("Multinomial Test")

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$vovkSellke <- TRUE
  options$descriptivesAs <- "proportions"
  options$descriptiveTable <- TRUE
  options$factor <- "facOutlier"
  options$testValues <- "custom"
  options$testValuesCustom <- list(
    list(
      levels = list("f1", "f2", "f3", "totallyridiculoussuperlongfactorme"),
      name = "H1",
      values = list(50, 42, 5, 3)
    )
  )
  results <- jaspTools::runAnalysis("MultinomialTest",
                            "test.csv", options)
  maintable <- results[["results"]][["chisqTable"]][["data"]]
  desctable <- results[["results"]][["descriptivesTable"]][["data"]]

  expected <- jaspTools:::collapseTestTable(
      list(list(case = "H\u2080 (a)",
                chisquare = 5.72,
                df = 3,
                p = 0.126056548007017,
                VovkSellkeMPR = 1.40914224189199)))

  jaspTools::expect_equal_tables(maintable, expected)
  jaspTools::expect_equal_tables(desctable,
                      list("f1", 0.49, 0.5,
                           "f2", 0.49, 0.42,
                           "f3", 0.01, 0.05,
                           "totallyridiculoussuperlongfactorme", 0.01, 0.03))
})

test_that("Descriptives plot matches", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facFive"
  options$descriptivePlot <- TRUE
  results <- jaspTools::runAnalysis("MultinomialTest", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives-1")
})

test_that("Analysis handles errors - Negative Values", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facExperim"
  options$count  <- "contNormal"
  options$testValuesCustom <- list(
    list(
      levels = list("control", "experimental"),
      name = "H1",
      values = list(50, 50)
    )
  )
  results <- jaspTools::runAnalysis("MultinomialTest", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Analysis handles errors - wrong levels", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facExperim"
  options$count  <- "debSame"
  options$testValluesCustom <- list(
    list(
      levels = list("control", "experimental"),
      name = "H1",
      values = list(50, 50)
    )
  )
  results <- jaspTools::runAnalysis("MultinomialTest", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Analysis handles errors - Infinities", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  options$factor <- "facExperim"
  options$count  <- "debInf"
  options$testValuesCustom <- list(
    list(
      levels = list("control", "experimental"),
      name = "H1",
      values = list(50, 50)
    )
  )
  results <- jaspTools::runAnalysis("MultinomialTest", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})
