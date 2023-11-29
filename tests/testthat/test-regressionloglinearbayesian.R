context("Bayesian Log-Linear Regression")

initOpts <- function() {
  options <- jaspTools::analysisOptions("RegressionLogLinearBayesian")
  options$samplingMethod <- "manual"
  options$samplingMethodManualSamples <- 100
  return(options)
}

test_that("Main table results match", {
  set.seed(0)
  options <- initOpts()
  options$count <- "facFifty"
  options$factors <- c("contBinom", "facGender")
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
                                 list("(Intercept)", 2.12746929269443, 2.28118363693666, 1, 0.00959232762655234,
                                      2.43233357540468, "contBinom = 1", -0.1140552657318, -0.0335221198542702,
                                      1, 0.002756395445979, 0.0475020520551498, "facFive = 2", -0.195453489777316,
                                      -0.00282655167800188, 1, 0.0102248921960175, 0.155047320962659,
                                      "facFive = 3", -0.164264352414958, 0.000154400018643484, 1,
                                      0.00940048687056697, 0.161656677341903, "facFive = 4", -0.14973534001884,
                                      0.00369626302015217, 1, 0.00974587350979484, 0.189539723263921,
                                      "facFive = 5", -0.155273680656904, 0.0016839574489477, 1, 0.0108516685524461,
                                      0.166831941585999, "contBinom = 1*facFive = 2", -0.0819863860420921,
                                      0.0507141965203517, 0.447023809523808, 0.0117408712962367, 0.225608774669159,
                                      "contBinom = 1*facFive = 3", -0.172855306187846, -0.0133761570465655,
                                      0.447023809523808, 0.00900801924647208, 0.144539535306823, "contBinom = 1*facFive = 4",
                                      -0.0818337842044022, 0.0427001178126396, 0.447023809523808,
                                      0.00995671151898941, 0.241519319460282, "contBinom = 1*facFive = 5",
                                      -0.289531236785577, -0.0761428540661306, 0.447023809523808,
                                      0.0132782059648042, 0.0542974169718703)
  )
})

test_that("Submodel summary statistics table matches", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
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
                                 list("(Intercept)", 2.12746929269443, 2.27823739725555, 0.00954231418605421,
                                      2.47945144261042, "contBinom = 1", -0.152571436550678, -0.0308665527935553,
                                      0.00251737809993112, 0.0428060084103773, "facFive = 2", -0.172121681790293,
                                      0.00224525307377201, 0.00878383066077853, 0.230919007122397,
                                      "facFive = 3", -0.196710458257331, -0.0044781058298241, 0.00835331749889998,
                                      0.208709120970735, "facFive = 4", -0.241047231509551, 0.00449593617720325,
                                      0.00801996356268645, 0.162572205036627, "facFive = 5", -0.226987657684912,
                                      0.000215113407450374, 0.011947132025284, 0.232769962304245,
                                      "contBinom = 1*facFive = 2", -0.153533409328021, 0.0507141965203517,
                                      0.0117408712962367, 0.307061040408055, "contBinom = 1*facFive = 3",
                                      -0.213739201091917, -0.0133761570465655, 0.00900801924647208,
                                      0.184451333453313, "contBinom = 1*facFive = 4", -0.148024819514679,
                                      0.0427001178126396, 0.00995671151898941, 0.248503021545585,
                                      "contBinom = 1*facFive = 5", -0.344769999413831, -0.0761428540661306,
                                      0.0132782059648042, 0.0669949682186052))
})

test_that("Analysis handles errors - Infinity", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("contBinom", "facFive")
  options$count <- "debInf"
  results <- jaspTools::runAnalysis("RegressionLogLinearBayesian", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})

test_that("Analysis handles errors - Missing values (factor)", {
  set.seed(0)
  options <- initOpts()
  options$factors <- c("debBinMiss20", "contBinom")
  options$modelTerms <- list(
    list(components="debBinMiss20")
  )
  results <- jaspTools::runAnalysis("RegressionLogLinearBayesian", "test.csv", options)
  status <- results[["status"]]
  expect_identical(status, "validationError")
})
