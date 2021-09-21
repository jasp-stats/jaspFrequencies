context("Contingency Tables")

# does not test
# - row/column order (ascending/descending)

test_that("Main table results match", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "facFifty"
  options$layers <- list(list(
    name = "Layer 1",
    variables = "facGender"
  ))
  options$countsExpected <- TRUE
  options$percentagesRow <- TRUE
  options$percentagesColumn <- TRUE
  options$percentagesTotal <- TRUE
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)
  table   <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabMain"]][["data"]]
  jaspTools::expect_equal_tables(table,
      list(0.489296636085627, 320, 394.529977794227, 0.236861584011843, 0.392638036809816,
           0.710186513629842, 495, 420.470022205773, 0.36639526276832,
           0.607361963190184, "control", "f", 0.603256846780163, 815, 815,
           0.603256846780163, 1, " % within column", "Count", "Expected count",
           " % of total", " % within row", 0.510703363914373, 334, 259.470022205773,
           0.247224278312361, 0.623134328358209, 0.289813486370158, 202,
           276.529977794227, 0.149518874907476, 0.376865671641791, "experimental",
           "f", 0.396743153219837, 536, 536, 0.396743153219837, 1, " % within column",
           "Count", "Expected count", " % of total", " % within row", 1,
           654, 654, 0.484085862324204, 0.484085862324204, 1, 697, 697,
           0.515914137675796, 0.515914137675796, "Total", "f", 1, 1351, 1351,
           1, 1, " % within column", "Count", "Expected count", " % of total",
           " % within row", 0.338688085676037, 253, 271.013344453712, 0.211009174311927,
           0.581609195402299, 0.402654867256637, 182, 163.986655546289,
           0.151793160967473, 0.418390804597701, "control", "m", 0.362802335279399,
           435, 435, 0.362802335279399, 1, " % within column", "Count",
           "Expected count", " % of total", " % within row", 0.661311914323963,
           494, 475.986655546288, 0.412010008340284, 0.646596858638743,
           0.597345132743363, 270, 288.013344453712, 0.225187656380317,
           0.353403141361257, "experimental", "m", 0.6371976647206, 764,
           764, 0.6371976647206, 1, " % within column", "Count", "Expected count",
           " % of total", " % within row", 1, 747, 747, 0.62301918265221,
           0.62301918265221, 1, 452, 452, 0.37698081734779, 0.37698081734779,
           "Total", "m", 1, 1199, 1199, 1, 1, " % within column", "Count", "Expected count",
           " % of total", " % within row", 0.408993576017131, 573, 686.764705882353,
           0.224705882352941, 0.4584, 0.589208006962576, 677, 563.235294117647,
           0.265490196078431, 0.5416, "control", "Total", 0.490196078431372,
           1250, 1250, 0.490196078431373, 1, " % within column", "Count",
           "Expected count", " % of total", " % within row", 0.591006423982869,
           828, 714.235294117647, 0.324705882352941, 0.636923076923077,
           0.410791993037424, 472, 585.764705882353, 0.185098039215686,
           0.363076923076923, "experimental", "Total", 0.509803921568627,
           1300, 1300, 0.509803921568627, 1, " % within column", "Count",
           "Expected count", " % of total", " % within row", 1, 1401, 1401,
           0.549411764705882, 0.549411764705882, 1, 1149, 1149, 0.450588235294118,
           0.450588235294118, "Total", "Total", 1, 2550, 2550, 1, 1, " % within column",
           "Count", "Expected count", " % of total", " % within row")
  )
})

test_that("Multiple row and column variables give multiple main tables", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- c("facExperim", "facGender")
  options$columns <- c("contBinom", "facFive")
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)

  table1 <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabMain"]][["data"]]
  table2 <- results[["results"]][["container_facExperim_facFive"]][["collection"]][["container_facExperim_facFive_crossTabMain"]][["data"]]
  table3 <- results[["results"]][["container_facGender_contBinom"]][["collection"]][["container_facGender_contBinom_crossTabMain"]][["data"]]
  table4 <- results[["results"]][["container_facGender_facFive"]][["collection"]][["container_facGender_facFive_crossTabMain"]][["data"]]

  expect_is(table1, "list", label = "facExperim-contBinom table")
  expect_is(table2, "list", label = "facExperim-Facfive table")
  expect_is(table3, "list", label = "facGender-contBinom table")
  expect_is(table4, "list", label = "facGender-facFive table")
})

test_that("Chi-Squared test table results match", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "facFifty"
  options$chiSquared <- TRUE
  options$chiSquaredContinuityCorrection <- TRUE
  options$likelihoodRatio <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabChisq"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("N", "", "", "", 2550,
         "<unicode>", 82.0397085317219, 1, 1.33379878452991e-19, 63462127883801120,
         "<unicode><unicode> continuity correction", 81.3201582621313, 1, 1.91958529099645e-19, 44468347240355080,
         "Likelihood ratio", 82.4643894680383, 1, 0, "<unicode><unicode>")
  )
})

test_that("Nominal table results match", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$contingencyCoefficient <- TRUE
  options$phiAndCramersV <- TRUE
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabNominal"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Contingency coefficient", 0.0807792391722019, "Phi-coefficient",
         -0.0810440898473108, "Cramer's V ", 0.0810440898473108)
  )
})

test_that("Log Odds Ratio table results match", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$oddsRatio <- TRUE
  options$oddsRatioConfidenceIntervalInterval <- 0.90
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabLogOdds"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Odds ratio", -0.329205575243527, -0.998167649205055, 0.339756498718001,"",
         "Fisher's exact test ", -0.325882968750928, -1.07370478788709,
         0.415368461868818, 0.5435617)
  )
})

test_that("Ordinal Gamma table results match", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$gamma <- TRUE
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabGamma"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.163132137030995, 0.197938461395245, -0.551084392520947, 0.224820118458957)
  )
})

test_that("Kendall's Tau table results match", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$kendallsTauB <- TRUE
  options$VovkSellkeMPR <- TRUE
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)
  table <- results[["results"]][["container_facExperim_contBinom"]][["collection"]][["container_facExperim_contBinom_crossTabKendallTau"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.0810440898473108, 0.420024632711394, 1, -0.806378512498144)
  )
})

test_that("Goodman-Kruskal lambda and Cramer's V results match", {
  # test based on example data from p. 1130
  # Kvålseth, T. O. (2018) Measuring association between nominal categorical variables: an alternative to the Goodman–Kruskal lambda, Journal of Applied Statistics, 45:6,1118-1132, DOI: 10.1080/02664763.2017.1346066

  comb <- expand.grid(var1=letters[1:3], var2=letters[1:2], KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  counts <- c(40, 15, 5, 5, 25, 10)
  df <- data.frame(var1 = rep(comb$var1, counts), var2 = rep(comb$var2, counts))

  options                <- jaspTools::analysisOptions("ContingencyTables")
  options$rows           <- "var1"
  options$columns        <- "var2"
  options$lambda         <- TRUE
  options$phiAndCramersV <- TRUE

  results <- jaspTools::runAnalysis("ContingencyTables", df, options)
  table <- results[["results"]][["container_var1_var2"]][["collection"]][["container_var1_var2_crossTabNominal"]][["data"]][[1]]

  # reported 0.3636
  testthat::expect_equal(table[["value[LambdaC]"]], 0.363636363636364)
  # reported 0.37
  testthat::expect_equal(table[["value[LambdaS]"]], 0.369318181818182)
  # reported 0.53
  testthat::expect_equal(table[["value[CramerV]"]], 0.534135681195261)
})

test_that("Analysis handles errors", {
  options <- jaspTools::analysisOptions("ContingencyTables")
  options$rows <- "facExperim"
  options$columns <- "contBinom"
  options$counts <- "contNormal"
  results <- jaspTools::runAnalysis("ContingencyTables", "test.csv", options)
  errorMsg <- results[["results"]][["errorMessage"]]
  expect_is(errorMsg, "character")
})


options <- analysisOptions("ContingencyTables")
options$columns <- "V2"
options$contingencyCoefficient <- TRUE
options$phiAndCramersV <- TRUE
options$rows <- "V1"
set.seed(1)
# data is a random sample from https://github.com/jasp-stats/jasp-issues/issues/811
dataset <- structure(list(V1 = c(1L, 2L, 1L, 2L, 1L, 0L, 2L, 0L, 0L, 1L), V2 = c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L)), row.names = c(NA, -10L), class = "data.frame")
results <- jaspTools::runAnalysis("ContingencyTables", dataset, options)

test_that("Phi coefficient is only available for 2 by 2 contingency tables", {
  tb <- results[["results"]][["container_V1_V2"]][["collection"]][["container_V1_V2_crossTabNominal"]]
  table <- tb[["data"]]
  jaspTools::expect_equal_tables(
    test  = table,
    ref   = list("Contingency coefficient", "Cramer's V ", "Phi-coefficient", 0.638284738504225, 0.82915619758885, "NaN"),
    label = "values in the table"
  )

  footnote <- tb[["footnotes"]]
  jaspTools::expect_equal_tables(
    test  = footnote,
    ref   = list("value[PhiCoef]", 15, 0, "Phi coefficient is only available for 2 by 2 contingency Tables"),
    label = "footnote under the table"
  )
})
