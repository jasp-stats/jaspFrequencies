context("Library: Listerism")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ContingencyTablesBayesian results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Listerism.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::ContingencyTablesBayesian(
          data = NULL,
          version = "0.96.5",
          formula = Method ~ Outcome,
          alternative = "greater",
          columnOrder = "descending",
          counts = ~ Count,
          posteriorOddsRatioPlot = TRUE))

  table <- results[["results"]][["container_Method_Outcome"]][["collection"]][["container_Method_Outcome_contTabBasBF"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF<unicode><unicode> Independent multinomial", "N", 0.551237737262742,
     24))

  table <- results[["results"]][["container_Method_Outcome"]][["collection"]][["container_Method_Outcome_crossTabMain"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(3, 7, "Lister", 10, 5, 9, "Standard", 14, 8, 16, "Total", 24
    ))

  plotName <- results[["results"]][["oddsRatioPlots"]][["collection"]][["oddsRatioPlots_container_Method_Outcome"]][["collection"]][["oddsRatioPlots_container_Method_Outcome_plot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_method-outcome")

})

