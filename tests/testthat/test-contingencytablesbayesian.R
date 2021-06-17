context("Bayesian Contingency Tables")

# does not test
# - row/column order (ascending/descending)
# - different hypothesis options
# - bftype (01, 10, log)
# - log odds for different sampling models
# - error handling in plots

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "facFifty"
  options$layers <- list(list(
    name = "Layer 1",
    variables = "facGender")
  )
  results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabMain"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("TRUE", 320, 495, "control", "f", 815, 334, 202, "experimental",
         "f", 536, "TRUE", 654, 697, "Total", "f", 1351,
         "TRUE", 253, 182, "control", "m", 435, 494, 270, "experimental",
         "m", 764, "TRUE", 747, 452, "Total", "m", 1199,
         "TRUE", 573, 677, "control", "Total", 1250, 828, 472,
         "experimental", "Total", 1300, "TRUE", 1401, 1149,
         "Total", "Total", 2550)
  )
})

test_that("Multiple row and column variables give multiple main tables", {
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- c("facExperim", "facGender")
  options$columns <- c("contBinom", "facFive")
  results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)

  table1 <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabMain"]][["data"]]
  table2 <- results[["results"]][["container_facExperim_facFive"]][["collection"]][["container_facExperim_facFive_crossTabMain"]][["data"]]
  table3 <- results[["results"]][["container_facGender_contBinom"]][["collection"]][["container_facGender_contBinom_crossTabMain"]][["data"]]
  table4 <- results[["results"]][["container_facGender_facFive"]][["collection"]][["container_facGender_facFive_crossTabMain"]][["data"]]

  expect_is(table1, "list", label = "facExperim-contBinom table")
  expect_is(table2, "list", label = "facExperim-Facfive table")
  expect_is(table3, "list", label = "facGender-contBinom table")
  expect_is(table4, "list", label = "facGender-facFive table")
})

test_that("Bayesian Contingency Tables Tests table results match", {
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$priorConcentration <- 1.5

  samplingModels <- c("poisson", "jointMultinomial", "independentMultinomialRowsFixed",
                      "independentMultinomialColumnsFixed", "hypergeometric")
  refTables <- list(
    poisson = list("BF<unicode><unicode> Poisson", "N", 100, 0.523118843924781),
    jointMultinomial = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.440084106793853),
    independentMultinomialRowsFixed = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.35545254779504),
    independentMultinomialColumnsFixed = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.364069579256729),
    hypergeometric = list("BF<unicode><unicode> hypergeometric", "N", 100, 0.269648117146104)
  )

  for (samplingModel in samplingModels) {
    options$samplingModel <- samplingModel
    results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)
    table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_contTabBasBF"]][["data"]]
    jaspTools::expect_equal_tables(table, refTables[[samplingModel]], label=paste("Sampling model", samplingModel))
  }
})

test_that("Bayesian Contingency Tables Tests table results match - different hypotheses", {
  set.seed(0)
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$priorConcentration <- 1.5

  samplingModels <- c("poisson", "jointMultinomial", "independentMultinomialRowsFixed",
                      "independentMultinomialColumnsFixed")
  hypotheses <- c("groupsNotEqual", "groupOneGreater", "groupTwoGreater")

  refTables <- list(
    poisson = list(
      groupsNotEqual  = list("BF<unicode><unicode> Poisson", "N", 100, 0.523118843924781),
      groupOneGreater = list("BF<unicode><unicode> Poisson", "N", 100, 0.224941102887656),
      groupTwoGreater = list("BF<unicode><unicode> Poisson", "N", 100, 0.818576366973497)),
    jointMultinomial = list(
      groupsNotEqual  = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.440084106793853),
      groupOneGreater = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.181490685641785),
      groupTwoGreater = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.683978718779006)),
    independentMultinomialRowsFixed = list(
      groupsNotEqual  = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.35545254779504),
      groupOneGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.149361160583476),
      groupTwoGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.560761939401455)),
    independentMultinomialColumnsFixed = list(
      groupsNotEqual  = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.364069579256729),
      groupOneGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.153564548530488),
      groupTwoGreater = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.571734867264767))
  )

  for (samplingModel in samplingModels) {
    options$samplingModel <- samplingModel
    for(hypothesis in hypotheses) {
      options$hypothesis <- hypothesis
      results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options, view = FALSE)
      table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_contTabBasBF"]][["data"]]
      #makeTestTable(table)
      jaspTools::expect_equal_tables(table, refTables[[samplingModel]][[hypothesis]], label=paste("Sampling model", samplingModel, "; hypothesis", hypothesis))
    }
  }
})

test_that("Log Odds Ratio table results match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$oddsRatio <- TRUE
  options$oddsRatioCredibleIntervalInterval <- 0.90
  results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_contTabBasLogOdds"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.322626350332064, -0.98462219921522, 0.339369498551093)
  )
})

test_that("Cramer's V table results match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$effectSize <- TRUE
  options$effectSizeCredibleIntervalInterval <- 0.90
  results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_contTabBasCramersV"]][["data"]]
  jaspTools::expect_equal_tables(table,
                      list(0.0698837782777569, 1.02065076979155e-16, 0.216657039422164)
  )
})

test_that("Log Odds Ratio Plot matches", {
  set.seed(0)
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$plotPosteriorOddsRatio <- TRUE
  options$plotPosteriorOddsRatioAdditionalInfo <- TRUE
  results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)
  jaspTools::expect_equal_plots(results[["state"]][["figures"]][[1]][["obj"]], "log-odds-ratio")
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "contNormal"
  results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)
  errorMsg <- results[["results"]][["errorMessage"]]
  expect_is(errorMsg, "character")
})
