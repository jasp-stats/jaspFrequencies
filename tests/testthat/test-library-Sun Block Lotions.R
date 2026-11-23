context("Library: Sun Block Lotions")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("BinomialTest results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Sun Block Lotions.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::BinomialTest(
          data = NULL,
          version = "0.96.5",
          formula =  ~ Product,
          ci = TRUE,
          descriptivesPlot = TRUE))

  table <- results[["results"]][["binomialTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 7, "Theirs", 0.153909204784541, 0.263175964355469, 0.35,
     20, 0.592188534532828, "Product", "FALSE", 13, "Yours", 0.407811465467172,
     0.263175964355469, 0.65, 20, 0.846090795215459, "Product"))

  plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_Product"]][["collection"]][["containerPlots_Product_Theirs"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_theirs")

  plotName <- results[["results"]][["containerPlots"]][["collection"]][["containerPlots_Product"]][["collection"]][["containerPlots_Product_Yours"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_yours")

})

