context("Multinomial Test Bayesian")

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("MultinomialTestBayesian")
  options$factor          <- "contBinom"
  options$bayesFactorType <- "BF10"
  options$testValues      <- "equal"
  options$priorCounts <- list(list(levels = paste0('level', 1:2),
                                   name   = c('Counts'),
                                   values = rep(1, 2)))
  results <- jaspTools::runAnalysis("MultinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["multinomialTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.44414455326433, "Multinomial", 2)
  )
})

test_that("Descriptives table results match", {
  options <- jaspTools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "debString"
  options$descriptivesType <- "proportions"
  options$descriptivesTable <- TRUE
  options$descriptivesTableCi <- TRUE
  options$descriptivesTableCiLevel <- 0.10
  options$priorCounts <- list(list(levels =letters,
                                   name   = c('Counts'),
                                   values = rep(1, length(letters))))
  results <- jaspTools::runAnalysis("MultinomialTestBayesian", "test.csv", options)
  table <- results[["results"]][["multinomialDescriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table[[1]],
                      list(0.0384615384615385, "a", 0.0439729779027027, 0.05, 0.0594407448532589)
  )
})

test_that("Descriptives plots match", {
  options <- jaspTools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "contBinom"
  options$descriptivesType <- "proportions"
  options$descriptivesTableCi <- TRUE
  options$descriptivesPlot <- TRUE
  options$priorCounts <- list(list(levels = c("0", "1"),
                                   name   = c('Counts'),
                                   values = rep(1, 2)))
  results <- jaspTools::runAnalysis("MultinomialTestBayesian", "test.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "multinomialBayesianDescriptivesPlot")
})

test_that("Bayesian Multinomial Test table results match in short data format", {
  options <- jaspTools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "Month"
  options$count  <- "Stress.frequency"
  options$testValuesCustom <- list(list(levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "H₀ (a)",
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                              1, 1)))
  options$priorCounts <- list(list(levels = c("1", "2", "3", "4", "5", "6", "7", "8",
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "Counts",
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                              1, 1)))
  set.seed(1)
  results <- jaspTools::runAnalysis("MultinomialTestBayesian", "Memory of Life Stresses.csv", options)
  table <- results[["results"]][["multinomialTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(27.1062505863656, "Multinomial", 18))
})

test_that("Descriptives table correctly shows reordered factor levels", {
  options <- jaspTools::analysisOptions("MultinomialTestBayesian")
  options$factor <- "Month"
  options$count  <- "Stress.frequency"
  options$expectedCount <- "Expected.counts"
  options$descriptivesTable <- TRUE
  options$testValuesCustom <- list(list(levels = c("3", "1", "2", "4", "5", "6", "7", "8",
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "H₀ (a)",
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                              1, 1)))
  options$priorCounts <- list(list(levels = c("3", "1", "2", "4", "5", "6", "7", "8",
                                              "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "Counts",
                                   values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                              1, 1)))
  results <- jaspTools::runAnalysis("MultinomialTestBayesian", "Memory of Life Stresses.csv", options)
  table <- results[["results"]][["multinomialDescriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table[1:4], list(7, 3, 14, 17, 1, 15, 5, 2, 11, 15, 4, 17))
})


