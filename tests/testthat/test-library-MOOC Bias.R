context("Library: MOOC Bias")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ContingencyTables results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "MOOC Bias.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::ContingencyTables(
          data = NULL,
          version = "0.96.5",
          formula = Group ~ Answer,
          columnOrder = "descending",
          counts = ~ Count,
          percentagesRow = TRUE))

  table <- results[["results"]][["container_Group_Answer"]][["collection"]][["container_Group_Answer_crossTabChisq"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("", 7, "", 0.2382354339069, "N", "<unicode><unicode>", 992, 9.20552074991757
    ))

  table <- results[["results"]][["container_Group_Answer"]][["collection"]][["container_Group_Answer_crossTabMain"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("BF", 115, 0.92741935483871, 9, 0.0725806451612903, 124, 1, "Count",
     " % within row", "BM", 116, 0.935483870967742, 8, 0.0645161290322581,
     124, 1, "Count", " % within row", "CF", 113, 0.911290322580645,
     11, 0.0887096774193548, 124, 1, "Count", " % within row", "CM",
     117, 0.943548387096774, 7, 0.0564516129032258, 124, 1, "Count",
     " % within row", "IF", 116, 0.935483870967742, 8, 0.0645161290322581,
     124, 1, "Count", " % within row", "IM", 120, 0.967741935483871,
     4, 0.032258064516129, 124, 1, "Count", " % within row", "WF",
     117, 0.943548387096774, 7, 0.0564516129032258, 124, 1, "Count",
     " % within row", "WM", 109, 0.879032258064516, 15, 0.120967741935484,
     124, 1, "Count", " % within row", "Total", 923, 0.930443548387097,
     69, 0.0695564516129032, 992, 1, "Count", " % within row"))

})

