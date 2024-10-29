context("Contingency Tables -- Verification project")

# does not test
# - row/column order (ascending/descending)

# Testing Chi-squared
options <- jaspTools::analysisOptions("ContingencyTables")
options$rows <- "Preference"
options$columns <- "Sex"
options$counts <- ""

results <- jaspTools::runAnalysis("ContingencyTables", "chisquare2.csv", options)

# https://jasp-stats.github.io/jasp-verification-project/frequencies.html#chi-squared-test
test_that("Main table results match R, SPSS, SAS, Minitab", {
  # Main table
  resultTable   <- results[["results"]][["container_Preference_Sex"]][["collection"]][["container_Preference_Sex_crossTabMain"]][["data"]]

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list( 32, 19, "Fiction", 51, 20, 29, "Nonfiction", 49,
               52, 48, "Total", 100)
  )
})

# https://jasp-stats.github.io/jasp-verification-project/frequencies.html#chi-squared-test
test_that("Chi-squared table results match R, SPSS, SAS, Minitab", {
  # Chi-squared table
  resultTable   <- results[["results"]][["container_Preference_Sex"]][["collection"]][["container_Preference_Sex_crossTabChisq"]][["data"]]

  jaspTools::expect_equal_tables(
    "test"=resultTable,
    "ref"=list("", 1, "", 0.0282214233450228, "N", "<unicode><unicode>", 100,
               4.81448989852351)
  )
})
