context("Library: Hungry Zombies")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("BinomialTestBayesian (analysis 1) results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Hungry Zombies.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::BinomialTestBayesian(
          data = NULL,
          version = "0.96.5",
          formula =  ~ Outcome,
          alternative = "less",
          bayesFactorType = "BF01",
          bfSequentialPlot = TRUE,
          priorPosteriorPlot = TRUE,
          testValue = 1))

  table <- results[["results"]][["binomTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 13, "Outcome", 12, "Hungry", 1, 12))

  plotName <- results[["results"]][["inferentialPlots"]][["collection"]][["inferentialPlots_Outcome - Hungry"]][["collection"]][["inferentialPlots_Outcome - Hungry_OutcomeHungrypriorposterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_prior-and-posterior")

  plotName <- results[["results"]][["inferentialPlots"]][["collection"]][["inferentialPlots_Outcome - Hungry"]][["collection"]][["inferentialPlots_Outcome - Hungry_OutcomeHungrysequential"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_sequential-analysis")

})

test_that("BinomialTestBayesian (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Hungry Zombies.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::BinomialTestBayesian(
          data = NULL,
          version = "0.96.5",
          formula =  ~ Outcome,
          alternative = "less",
          bayesFactorType = "BF01",
          bfSequentialPlot = TRUE,
          priorA = 2,
          priorPosteriorPlot = TRUE,
          testValue = 1))

  table <- results[["results"]][["binomTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 7, "Outcome", 12, "Hungry", 1, 12))

  plotName <- results[["results"]][["inferentialPlots"]][["collection"]][["inferentialPlots_Outcome - Hungry"]][["collection"]][["inferentialPlots_Outcome - Hungry_OutcomeHungrypriorposterior"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-1_prior-and-posterior")

  plotName <- results[["results"]][["inferentialPlots"]][["collection"]][["inferentialPlots_Outcome - Hungry"]][["collection"]][["inferentialPlots_Outcome - Hungry_OutcomeHungrysequential"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-2_figure-2_sequential-analysis")

})

