context("Library: Memory of Life Stresses")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("MultinomialTest (analysis 1) results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Memory of Life Stresses.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::MultinomialTest(
          data = NULL,
          version = "0.96.5",
          count = ~ Stress.frequency,
          descriptivesPlot = TRUE,
          descriptivesTable = TRUE,
          descriptivesTableCi = TRUE,
          factor = ~ Month,
          plotHeight = 300,
          plotWidth = 450,
          testValues = "custom",
          testValuesCustom = list(list(levels = list("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "H₀ (a)", values = list(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), list(levels = list("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "H₀ (b)", values = list(17, 5, 7, 15, 4, 10, 9, 2, 10, 15, 6, 5, 2, 15, 4, 2, 7, 12)))))

  table <- results[["results"]][["chisqTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("H<unicode> (a)", 45.3673469387755, 17, 0.00021465184864374, "H<unicode> (b)",
     84.6725957049486, 17, 5.60924573036287e-11))

  plotName <- results[["results"]][["descriptivesPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_descriptives-plot")

  table <- results[["results"]][["descriptivesTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(8.16666666666667, 17, 1, 8.56208708277594, 15, 23.9187057467658,
     8.16666666666667, 5, 2, 5.57798183560349, 11, 19.1020451488085,
     8.16666666666667, 7, 3, 7.7985296496181, 14, 22.730574028975,
     8.16666666666667, 15, 4, 10.1181217305864, 17, 26.2681840870811,
     8.16666666666667, 4, 5, 1.63679795662281, 5, 11.4056322643846,
     8.16666666666667, 10, 6, 5.57798183560349, 11, 19.1020451488085,
     8.16666666666667, 9, 7, 4.86590903941694, 10, 17.8671623413048,
     8.16666666666667, 2, 8, 1.09702678695274, 4, 10.0259015819903,
     8.16666666666667, 10, 9, 3.4964429298336, 8, 15.3490410292805,
     8.16666666666667, 15, 10, 4.86590903941694, 10, 17.8671623413048,
     8.16666666666667, 6, 11, 2.84546987329914, 7, 14.0606808642576,
     8.16666666666667, 5, 12, 4.17114015867708, 9, 16.6169059317081,
     8.16666666666667, 2, 13, 5.57798183560349, 11, 19.1020451488085,
     8.16666666666667, 15, 14, 0.62160288211502, 3, 8.59675135766186,
     8.16666666666667, 4, 15, 2.22321174807749, 6, 12.7479827394692,
     8.16666666666667, 2, 16, 0.0253156278665913, 1, 5.48577269028815,
     8.16666666666667, 7, 17, 0.0253156278665913, 1, 5.48577269028815,
     8.16666666666667, 12, 18, 1.09702678695274, 4, 10.0259015819903
    ))

})

test_that("MultinomialTest (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Memory of Life Stresses.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::MultinomialTest(
          data = NULL,
          version = "0.96.5",
          count = ~ Stress.frequency,
          expectedCount = ~ Expected.proportions,
          factor = ~ Month,
          plotHeight = 300,
          plotWidth = 450,
          testValuesCustom = list(list(levels = list("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18"), name = "H1", values = list(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)))))

  table <- results[["results"]][["chisqTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Expected.proportions", 84.6725957049486, 17, 5.60924573036284e-11
    ))

})

