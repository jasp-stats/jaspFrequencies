context("Informed Binomial Test Bayesian")

test_that("default and explicit model weights produce the expected summary", {
  dataFile <- tempfile(fileext = ".csv")
  on.exit(unlink(dataFile))
  write.csv(
    data.frame(Ref = c("A", "B"), c_i = c(1, 1), t_i = c(2, 2)),
    dataFile,
    row.names = FALSE
  )

  runFixture <- function(priorModelProbability) {
    options <- jaspTools::analysisOptions("InformedBinomialTestBayesian")
    options$factor <- "Ref"
    options$factor.types <- "nominal"
    options$successes <- "c_i"
    options$successes.types <- "scale"
    options$sampleSize <- "t_i"
    options$sampleSize.types <- "scale"
    options$includeNullModel <- TRUE
    options$includeEncompassingModel <- TRUE
    options$bayesFactorType <- "BF10"
    options$bfComparison <- "Encompassing"
    options$models <- list(list(modelName = "Model 1", syntax = ""))
    options$priorCounts <- list(
      list(levels = c("A", "B"), name = "data 1", values = c(1, 1)),
      list(levels = c("A", "B"), name = "data 2", values = c(1, 1))
    )
    options$priorModelProbability <- priorModelProbability

    results <- jaspTools::runAnalysis(
      "InformedBinomialTestBayesian",
      dataFile,
      options,
      view = FALSE
    )
    if (isTRUE(results[["results"]][["error"]]))
      fail(results[["results"]][["errorMessage"]])

    results[["results"]][["summaryTable"]][["data"]]
  }

  emptyGuiWeights <- list(list(
    levels = list(),
    name = "data 1",
    values = list()
  ))
  defaultTable <- runFixture(emptyGuiWeights)

  expect_equal(vapply(defaultTable, `[[`, character(1), "model"), c("Null", "Encompassing"))
  expect_equal(vapply(defaultTable, `[[`, numeric(1), "marglik"), log(c(2 / 15, 1 / 9)), tolerance = 1e-6)
  expect_equal(vapply(defaultTable, `[[`, numeric(1), "priorProb"), c(1 / 2, 1 / 2), tolerance = 1e-6)
  expect_equal(vapply(defaultTable, `[[`, numeric(1), "postProb"), c(6 / 11, 5 / 11), tolerance = 1e-6)
  expect_equal(vapply(defaultTable, `[[`, numeric(1), "bf"), c(6 / 5, 1), tolerance = 1e-6)

  explicitWeights <- list(list(
    levels = c("Null", "Encompassing", "Model 1"),
    name = "data 1",
    values = c("2", "1", "7")
  ))
  explicitTable <- runFixture(explicitWeights)

  expect_equal(vapply(explicitTable, `[[`, character(1), "model"), c("Null", "Encompassing"))
  expect_equal(vapply(explicitTable, `[[`, numeric(1), "priorProb"), c(2 / 3, 1 / 3), tolerance = 1e-6)
  expect_equal(vapply(explicitTable, `[[`, numeric(1), "postProb"), c(12 / 17, 5 / 17), tolerance = 1e-6)
  expect_equal(vapply(explicitTable, `[[`, numeric(1), "bf"), c(6 / 5, 1), tolerance = 1e-6)
})
