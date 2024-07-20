context("AB Test Bayesian")

# abtest will fail on the first run, but not on subsequent runs.
# Clearly something requires loading (options?), but it is not fixed by loadNamespace("abtest").
# This ensures the subsequent runs pass:
options <- jaspTools::analysisOptions("ABTestBayesian")
options$n1 <- "n1"
options$y1 <- "y1"
options$n2 <- "n2"
options$y2 <- "y2"
jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
# Up to here.


test_that("Main table results match", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"

  results  <- jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianTable"]][["data"]]
  refTable <- list("Log odds ratio = 0", 1                , 0.517752872701024, 0.5,
                   "Log odds ratio &gt; 0", 1.23201979244712 , 0.318940893382007, 0.25,
                   "Log odds ratio &lt; 0", 0.630826954431096, 0.163306233916969, 0.25)

  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Descriptives table results match", {
  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$descriptivesTable <- TRUE

  results  <- jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianDescriptivesTable"]][["data"]]

  refTable <- list("Group 1", 2, 4, 0.500,
                   "Group 2", 3, 4, 0.750)

  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Main table results, log odds neq 0", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$priorModelProbabilityEqual     <- 0.495
  options$priorModelProbabilityGreater   <- 0.168
  options$priorModelProbabilityLess      <- 0.168
  options$priorModelProbabilityTwoSided  <- 0.168

  results  <- jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianTable"]][["data"]]

  refTable <- list("Log odds ratio = 0"        , 1                , 0.51649969761886 , 0.495495495495495,
                   "Log odds ratio &gt; 0"     , 1.23201979244712 , 0.215969209785004, 0.168168168168168,
                   "Log odds ratio <unicode> 0", 0.895333193589823, 0.15694910382065 , 0.168168168168168,
                   "Log odds ratio &lt; 0"     , 0.630826954431096, 0.110581988775487, 0.168168168168168)

  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Main table results, only 2 hypotheses", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$priorModelProbabilityEqual     <- 0.5
  options$priorModelProbabilityGreater   <- 0.5
  options$priorModelProbabilityLess      <- 0
  options$priorModelProbabilityTwoSided  <- 0

  results  <- jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
  table    <- results[["results"]][["abTestBayesianTable"]][["data"]]

  refTable <- list("Log odds ratio &gt; 0", 1                , 0.55197529906147, 0.5,
                   "Log odds ratio = 0"   , 0.811675271883202, 0.44802470093853, 0.5)

  jaspTools::expect_equal_tables(table, refTable)
})


test_that("Prior plot matches", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$priorModelProbabilityEqual     <- 0.5
  options$priorModelProbabilityGreater   <- 0.5
  options$priorModelProbabilityLess      <- 0
  options$priorModelProbabilityTwoSided  <- 0
  options$priorPlot      <- TRUE
  options$priorPlotType  <- "logOddsRatio"

  results  <- jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "prioronly")
})


test_that("Posterior plot matches", {
  skip("Needs to be verified.")
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$priorModelProbabilityEqual     <- 0.5
  options$priorModelProbabilityGreater <- 0.5
  options$priorModelProbabilityLess    <- 0
  options$priorModelProbabilityTwoSided  <- 0
  options$priorPosteriorPlot <- TRUE
  options$priorPosteriorPlotType <- "logOddsRatio"

  results  <- jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]]

  jaspTools::expect_equal_plots(testPlot, "posterior")
})


test_that("Sequential plot matches", {
  set.seed(0)

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$priorModelProbabilityEqual     <- 0.5
  options$priorModelProbabilityGreater <- 0.5
  options$priorModelProbabilityLess    <- 0
  options$priorModelProbabilityTwoSided  <- 0
  options$bfSequentialPlot <- TRUE

  results  <- jaspTools::runAnalysis("ABTestBayesian", "ab_data.csv", options)
  testPlot <- results[["state"]][["figures"]][[1]][["obj"]]

  jaspTools::expect_equal_plots(testPlot, "sequential")
})


test_that("plotRobustness plot matches", {
  set.seed(0)
  testthat::skip_on_os("mac")

  options <- jaspTools::analysisOptions("ABTestBayesian")
  options$n1 <- "n1"
  options$y1 <- "y1"
  options$n2 <- "n2"
  options$y2 <- "y2"
  options$priorModelProbabilityEqual     <- 0.5
  options$priorModelProbabilityGreater   <- 0.5
  options$priorModelProbabilityLess      <- 0
  options$priorModelProbabilityTwoSided  <- 0
  options$bfRobustnessPlot     <- TRUE
  options$bfRobustnessPlotType <- "BF10"

  results  <- jaspTools::runAnalysis("ABTestBayesian", testthat::test_path("ab_data.csv"), options)
  testPlot <- results[["state"]][["figures"]][[1]]

  jaspTools::expect_equal_plots(testPlot, "robustness")
})
