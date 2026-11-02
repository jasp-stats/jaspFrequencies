context("Binomial Test")

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("BinomialTest")
  options$variables <- "contBinom"
  options$testValue <- 0.6
  options$alternative <- "greater"
  options$ci <- TRUE
  options$vovkSellke <- TRUE
  results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)
  table <- results[["results"]][["binomialTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
      list("TRUE", 1, 58, 0, 0.492841460660175, 0.696739870156555, 0.58, 100, 1, "contBinom",
           "FALSE", 1, 42, 1, 0.336479745077558, 0.999903917924738, 0.42, 100, 1, "contBinom")
  )
})

test_that("Footnote flags rows where two-sided p-value and CI disagree", {
  # 10/100 vs .05: p = .034 but 95% CI [.049, .176] contains .05
  dataset <- data.frame(x = factor(c(rep("a", 10), rep("b", 90))))
  options <- jaspTools::analysisOptions("BinomialTest")
  options$variables   <- "x"
  options$testValue   <- 0.05
  options$alternative <- "twoSided"
  options$ci          <- TRUE

  getDisagreementFootnotes <- function(options) {
    results   <- jaspTools::runAnalysis("BinomialTest", dataset, options, view = FALSE)
    footnotes <- results[["results"]][["binomialTable"]][["footnotes"]]
    Filter(function(f) grepl("disagree", f[["text"]]), footnotes)
  }

  footnotes <- getDisagreementFootnotes(options)
  expect_length(footnotes, 1)
  expect_equal(unlist(footnotes[[1]][["cols"]]), "p")
  expect_equal(unlist(footnotes[[1]][["rows"]]), "x-a")

  options$alternative <- "greater"
  expect_length(getDisagreementFootnotes(options), 0)

  options$alternative <- "twoSided"
  options$ci          <- FALSE
  expect_length(getDisagreementFootnotes(options), 0)
})

test_that("Descriptives plots match", {
  skip("This test need to be verified")
  options <- jaspTools::analysisOptions("BinomialTest")
  options$variables <- "contBinom"
  options$descriptivesPlot <- TRUE
  options$descriptivesPlotCiLevel <- 0.90
  results <- jaspTools::runAnalysis("BinomialTest", "test.csv", options)

  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives-1")

  testPlot <- results[["state"]][["figures"]][[2]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "descriptives-2")
})
