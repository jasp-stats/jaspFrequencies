context("Library: Death Penalty")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("RegressionLogLinearBayesian results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Death Penalty.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::RegressionLogLinearBayesian(
          data = NULL,
          version = "0.96.5",
          formula =  ~ RaceVictim * RaceDefendant * Death,
          bayesFactorType = "BF01",
          count = ~ Count,
          modelCutOffBestDisplayed = 10,
          modelCutOffPosteriorProbability = 0,
          regressionCoefficientsSubmodelNo = 3))

  table <- results[["results"]][["Container"]][["collection"]][["Container_MainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "RaceVictim + RaceDefendant + Death + RaceVictim<unicode><unicode><unicode>RaceDefendant + RaceVictim<unicode><unicode><unicode>Death",
     1, 0.492, 1.29644268774704, "RaceVictim + RaceDefendant + Death + RaceVictim<unicode><unicode><unicode>RaceDefendant + RaceVictim<unicode><unicode><unicode>Death + RaceDefendant<unicode><unicode><unicode>Death",
     2, 0.3795, 3.8740157480315, "RaceVictim + RaceDefendant + Death + RaceVictim<unicode><unicode><unicode>RaceDefendant + RaceVictim<unicode><unicode><unicode>Death + RaceDefendant<unicode><unicode><unicode>Death + RaceVictim<unicode><unicode><unicode>RaceDefendant<unicode><unicode><unicode>Death",
     3, 0.127, 328, "RaceVictim + RaceDefendant + Death + RaceVictim<unicode><unicode><unicode>RaceDefendant",
     4, 0.0015))

})

