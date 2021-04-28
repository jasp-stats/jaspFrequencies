context("Binomial Test -- Verification project")

# https://jasp-stats.github.io/jasp-verification-project/frequencies.html#binomial-test
test_that("Main table results match R, SPSS, SAS and MiniTab", {
  options <- jaspTools::analysisOptions("BinomialTest")
  options$variables <- "Guesses"
  options$testValue <- 0.25
  
  results <- jaspTools::runAnalysis("BinomialTest", "BinomialTest.csv", options)
  resultTable <- results[["results"]][["binomialTable"]][["data"]]
  
  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("TRUE", 3, 0, 0.718432426452637, 0.3, 10, "Guesses", "FALSE",
               7, 1, 0.00350570678710938, 0.7, 10, "Guesses"))
})