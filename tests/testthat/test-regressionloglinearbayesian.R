context("Bayesian Log-Linear Regression")

initOpts <- function() {
  options <- jaspTools::analysisOptions("RegressionLogLinearBayesian")
  options$samplingMethod <- "manual"
  options$samplingMethodManualSamples <- 100
  # move factors before count for jaspTools preloadData type detection
  factorsVal <- options[["factors"]]
  options[["factors"]] <- NULL
  options <- c(list(factors = factorsVal), options)
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$count <- "facFifty"
  options$factors <- c("contBinom", "facGender")
  options$count.types <- "scale"
  options$factors.types <- c("nominal", "nominal")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facGender"),
    list(components=c("contBinom", "facGender"))
  )
  options$priorScale <- 1
  options$priorShape <- 0
  options$modelCutOffBestDisplayed <- 2
  options$modelCutOffPosteriorProbability <- 0.001
  results <- jaspTools::runAnalysis("RegressionLogLinearBayesian", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_MainTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(1, "contBinom + facGender", 0.963333333333333, 1, 2,
         "contBinom + facGender + contBinom<unicode><unicode><unicode>facGender",
         0.0366666666666667, 0.0380622837370242)
  )
})

test_that("General summary statistics table matches", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
  options$factors.types <- c("nominal", "nominal")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facFive"),
    list(components=c("contBinom", "facFive"))
  )
  options$regressionCoefficientsEstimates <- TRUE
  options$regressionCoefficientsCi <- TRUE
  options$regressionCoefficientsCiLevel <- 0.90
  results <- jaspTools::runAnalysis("RegressionLogLinearBayesian", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_SummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 2.12177466183418, 2.28941355597128, 1, 0.0114477565469203,
                                      2.45495750281602, "contBinom = 0", -0.0378564465631074, 0.0303566571915709,
                                      1, 0.00356265251723892, 0.144271031109651, "facFive = 1", -0.130345670875488,
                                      -0.00552214172853818, 1, 0.00720162344071975, 0.138580850953899,
                                      "facFive = 2", -0.121705317175141, -0.00240692298068445, 1,
                                      0.00784162472732632, 0.165537141029262, "facFive = 3", -0.0900898563442231,
                                      0.00293548076176616, 1, 0.00745020526106459, 0.187558384342406,
                                      "facFive = 4", -0.122842441315214, 0.00138448207875664, 1, 0.00779485819569607,
                                      0.155851155312605, "contBinom = 0*facFive = 1", -0.125500914290162,
                                      0.00197519410432227, 0.571428571428572, 0.00569094808602156,
                                      0.103584632765005, "contBinom = 0*facFive = 2", -0.18074633235348,
                                      -0.027036768615536, 0.571428571428572, 0.00624820819787772,
                                      0.0610871410555315, "contBinom = 0*facFive = 3", -0.145370964229943,
                                      0.00343309539396841, 0.571428571428572, 0.00598953855547498,
                                      0.0973020746027853, "contBinom = 0*facFive = 4", -0.131413707533983,
                                      -0.0218844778500033, 0.571428571428572, 0.00501003480774966,
                                      0.101000867077933)
  )
})

test_that("Submodel summary statistics table matches", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
  options$factors.types <- c("nominal", "nominal")
  options$modelTerms <- list(
    list(components="contBinom"),
    list(components="facFive"),
    list(components=c("contBinom", "facFive"))
  )
  options$regressionCoefficientsSubmodel <- TRUE
  options$regressionCoefficientsSubmodelCi <- TRUE
  options$regressionCoefficientsSubmodelEstimates <- TRUE
  options$regressionCoefficientsSubmodelNo <- 2
  results <- jaspTools::runAnalysis("RegressionLogLinearBayesian", "test.csv", options)
  table <- results[["results"]][["Container"]][["collection"]][["Container_SubSummaryTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list("(Intercept)", 2.12809954463568, 2.29560729883006, 0.00945972825329097,
                                      2.52220705393406, "contBinom = 0", -0.0378564465631074, 0.0457579623532093,
                                      0.00487119274553834, 0.235518792759572, "facFive = 1", -0.189678327954176,
                                      -0.00411654631578645, 0.00919225838347484, 0.225524507296169,
                                      "facFive = 2", -0.230464286621306, -0.00755884938209818, 0.0107688538955323,
                                      0.203303292163532, "facFive = 3", -0.151633708350594, 0.00693535371456421,
                                      0.00958840576151793, 0.242411127225972, "facFive = 4", -0.234264000793186,
                                      0.00306711588492556, 0.0107120223942475, 0.230287649489839))
})

test_that("Analysis handles errors - Infinity", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
  options$factors.types <- c("nominal", "nominal")
  options$count <- "debInf"
  options$count.types <- "scale"
  results <- jaspTools::runAnalysis("RegressionLogLinearBayesian", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Analysis handles errors - Missing values (factor)", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("debBinMiss20", "contBinom")
  options$factors.types <- c("nominal", "nominal")
  options$modelTerms <- list(
    list(components="debBinMiss20")
  )
  results <- jaspTools::runAnalysis("RegressionLogLinearBayesian", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})
