context("Other: test-contingencytables-mcnemar")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/other/.

test_that("ContingencyTables runs without error", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "other", "test-contingencytables-mcnemar.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("ContingencyTables", encoded$dataset, encoded$options, encodedDataset = TRUE)

  # Check analysis runs without error
  expect_false(isTRUE(results[["status"]] == "error"),
               info = results[["results"]][["error"]])
})

