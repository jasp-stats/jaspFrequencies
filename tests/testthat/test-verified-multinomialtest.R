context("Multinomial Test -- Verification project")

# https://jasp-stats.github.io/jasp-verification-project/frequencies.html#multinomial-test-chi-square-goodness-of-fit-test
test_that("Main table results match R, SPSS, SAS", {
  options <- jaspTools::analysisOptions("MultinomialTest")
  
  options$counts <- "Count"
  options$factor <- "Color"
  options$hypothesis <- "Expected"
  options$descriptivesPlot <- TRUE
  options$tableWidget <- list(
    list(
      levels = list("Black", "Red", "White"),
      name = "H1",
      values = list(4, 3, 3)
    )
  )
  
  results <- jaspTools::runAnalysis("MultinomialTest",
                                    "MNT.csv", options)
  
  # Main table
  resultTable <- results$results$chisqTable$data
  
  refTable <- jaspTools:::collapseTestTable(
    list(list(case = "H\u2080 (a)",
              chisquare = 2.33333333333333,
              df = 2,
              p = 0.311403223914598))
  )
  
  jaspTools::expect_equal_tables("test"= resultTable, "ref"=refTable)
  
  # TODO(Alexander) Descriptive Plot put in separate tests
  resultPlot <- results[["state"]][["figures"]][[1]][["obj"]]
})