context("Binomial Test Bayesian")

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("BinomialTestBayesian")
  options$variables <- "contBinom"
  options$bayesFactorType <- "BF01"
  options$alternative <- "twoSided"
  options$priorA <- 1
  options$priorB <- 2
  options$testValue <- 0.2
  results <- jaspTools::runAnalysis("BinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["binomTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 4.32337507642424e-15, "contBinom", 58, 0, 0.58, 100,
         "FALSE", 3.43240614623212e-05, "contBinom", 42, 1, 0.42, 100)
  )
})

test_that("Prior posterior plots match", {
  options <- jaspTools::analysisOptions("BinomialTestBayesian")
  options$priorA <- 1
  options$priorB <- 1
  options$testValue <- 0.5
  options$variables <- "contBinom"
  options$priorPosteriorPlot <- TRUE
  options$priorPosteriorPlotAdditionalInfo <- TRUE
  results <- jaspTools::runAnalysis("BinomialTestBayesian", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-1")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "prior-posterior-2")
})

test_that("Sequential analysis plots match - BF10", {
  options <- jaspTools::analysisOptions("BinomialTestBayesian")
  options$priorA <- 1
  options$priorB <- 1
  options$testValue <- 0.5
  options$variables <- "contBinom"
  options$bfSequentialPlot <- TRUE
  results <- jaspTools::runAnalysis("BinomialTestBayesian", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-1")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-2")
})

test_that("Sequential analysis plots match - BF01", {
  options <- jaspTools::analysisOptions("BinomialTestBayesian")
  options$priorA <- 1
  options$priorB <- 1
  options$testValue <- 0.5
  options$variables <- "contBinom"
  options$bfSequentialPlot <- TRUE
  options$bayesFactorType <- "BF01"
  results <- jaspTools::runAnalysis("BinomialTestBayesian", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-3")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "sequential-analysis-4")
})
