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
    list( 320, 495, "control", "f", 815, 334, 202, "experimental",
         "f", 536,  654, 697, "Total", "f", 1351,
          253, 182, "control", "m", 435, 494, 270, "experimental",
         "m", 764,  747, 452, "Total", "m", 1199,
          573, 677, "control", "Total", 1250, 828, 472,
         "experimental", "Total", 1300,  1401, 1149,
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
  alternatives <- c("twoSided", "greater", "less")

  refTables <- list(
    poisson = list(
      twoSided  = list("BF<unicode><unicode> Poisson", "N", 100, 0.523118843924781),
      greater   = list("BF<unicode><unicode> Poisson", "N", 100, 0.224941102887656),
      less      = list("BF<unicode><unicode> Poisson", "N", 100, 0.818576366973497)),
    jointMultinomial = list(
      twoSided  = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.440084106793853),
      greater   = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.181490685641785),
      less      = list("BF<unicode><unicode> joint multinomial", "N", 100, 0.683978718779006)),
    independentMultinomialRowsFixed = list(
      twoSided  = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.35545254779504),
      greater   = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.149361160583476),
      less      = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.560761939401455)),
    independentMultinomialColumnsFixed = list(
      twoSided  = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.364069579256729),
      greater   = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.153564548530488),
      less      = list("BF<unicode><unicode> independent multinomial", "N", 100, 0.571734867264767))
  )

  for (samplingModel in samplingModels) {
    options$samplingModel <- samplingModel
    for(alternative in alternatives) {
      options$alternative <- alternative
      results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options, view = FALSE)
      table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_contTabBasBF"]][["data"]]
      #makeTestTable(table)
      jaspTools::expect_equal_tables(table, refTables[[samplingModel]][[alternative]], label=paste("Sampling model", samplingModel, "; hypothesis", alternative))
    }
  }
})

test_that("Log Odds Ratio table results match", {
  set.seed(0)
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$oddsRatio <- TRUE
  options$oddsRatioCiLevel <- 0.90
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
  options$cramersV <- TRUE
  options$cramersVCiLevel <- 0.90
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
  options$posteriorOddsRatioPlot <- TRUE
  options$posteriorOddsRatioPlotAdditionalInfo <- TRUE
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

test_that("Hypergeometric contingency table does not crash for not 2x2 tables", {
  options <- jaspTools::analysisOptions("ContingencyTablesBayesian")
  options$rows <- "facFive"
  options$columns <- "facGender"
  options$samplingModel <- "hypergeometric"
  results <- jaspTools::runAnalysis("ContingencyTablesBayesian", "test.csv", options)

  expect(is.null(results[["results"]][["errorMessage"]]),
         "Hypergeometric contingency table errored out with not 2x2 table!")

  bfFootnote <- results[["results"]][["container_facFive_facGender"]][["collection"]][["container_facFive_facGender_contTabBasBF"]][["footnotes"]][[1]][["text"]]
  expect_equal(bfFootnote,
               "Hypergeometric contingency tables test restricted to 2 x 2 tables")
})
