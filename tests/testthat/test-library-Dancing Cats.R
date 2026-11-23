context("Library: Dancing Cats")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ContingencyTables results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Dancing Cats.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::ContingencyTables(
          data = NULL,
          version = "0.96.5",
          formula = Training ~ Dance,
          chiSquaredContinuityCorrection = TRUE,
          counts = ~ Counts,
          countsExpected = TRUE,
          likelihoodRatio = TRUE,
          percentagesColumn = TRUE,
          percentagesRow = TRUE,
          percentagesTotal = TRUE))

  table <- results[["results"]][["container_Training_Dance"]][["collection"]][["container_Training_Dance_crossTabChisq"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("", 1, 1, 1, "", 1.23604133820171e-06, 4.76743408176905e-07, 5.94011329346777e-07,
     "N", "<unicode><unicode> continuity correction", "<unicode><unicode>",
     "Likelihood ratio", 200, 23.5202780894259, 25.3556925704187,
     24.9315875819403))

  table <- results[["results"]][["container_Training_Dance"]][["collection"]][["container_Training_Dance_crossTabMain"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0806451612903226, 10, 23.56, 0.263157894736842, 0.05, "Food as Reward",
     0.368421052631579, 28, 14.44, 0.736842105263158, 0.14, 0.19,
     38, 38, 1, 0.19, " % within column", "Count", "Expected count",
     " % within row", " % of total", 0.919354838709677, 114, 100.44,
     0.703703703703704, 0.57, "Affection as Reward", 0.631578947368421,
     48, 61.56, 0.296296296296296, 0.24, 0.81, 162, 162, 1, 0.81,
     " % within column", "Count", "Expected count", " % within row",
     " % of total", 1, 124, 124, 0.62, 0.62, "Total", 1, 76, 76,
     0.38, 0.38, 1, 200, 200, 1, 1, " % within column", "Count",
     "Expected count", " % within row", " % of total"))

})

