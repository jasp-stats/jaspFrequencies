context("Library: Emily Rosa")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("BinomialTestBayesian results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Emily Rosa.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::BinomialTestBayesian(
          data = NULL,
          version = "0.96.5",
          formula =  ~ Outcome,
          alternative = "greater",
          bayesFactorType = "BF01",
          priorPosteriorPlot = TRUE))

  table <- results[["results"]][["binomTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 16.9549224314849, "Outcome", 70, "Correct", 0.466666666666667,
     150, "FALSE", 4.45085509038257, "Outcome", 80, "Incorrect",
     0.533333333333333, 150))

  plotName <- results[["results"]][["inferentialPlots"]][["collection"]][["inferentialPlots_Outcome - Correct"]][["collection"]][["inferentialPlots_Outcome - Correct_OutcomeCorrectpriorposterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_prior-and-posterior")

  plotName <- results[["results"]][["inferentialPlots"]][["collection"]][["inferentialPlots_Outcome - Incorrect"]][["collection"]][["inferentialPlots_Outcome - Incorrect_OutcomeIncorrectpriorposterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_prior-and-posterior")

})

