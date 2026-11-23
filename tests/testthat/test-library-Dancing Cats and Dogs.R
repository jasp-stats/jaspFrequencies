context("Library: Dancing Cats and Dogs")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("RegressionLogLinear (analysis 2) results match", {

  # Run the R syntax of analysis 2 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Dancing Cats and Dogs.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::RegressionLogLinear(
          data = NULL,
          version = "0.96.5",
          formula =  ~ Animal * Training * Dance,
          regressionCoefficientsEstimates = TRUE))

  table <- results[["results"]][["AnovaTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(" ", " ", "NULL", " ", 200.163068756161, 7, 65.267900149373, 1,
     "Animal", 6.5377639661852e-16, 134.895168606788, 6, 61.145284936596,
     1, "Training", 5.30145830199366e-15, 73.7498836701916, 5, 1.4828392707899,
     1, "Dance", 0.223330819899114, 72.2670443994017, 4, 21.6815520568862,
     1, "Animal<unicode><unicode><unicode>Training", 3.21871844136864e-06,
     50.5854923425154, 3, 21.6699853182522, 1, "Animal<unicode><unicode><unicode>Dance",
     3.23818553120677e-06, 28.9155070242632, 2, 8.61059821734605,
     1, "Training<unicode><unicode><unicode>Dance", 0.00334212513760805,
     20.3049088069172, 1, 20.3049088069172, 1, "Animal<unicode><unicode><unicode>Training<unicode><unicode><unicode>Dance",
     6.60309588877715e-06, 0, 0))

  table <- results[["results"]][["CoefficientsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.30258509299405, 0.316227765824378, 7.28141340464337, "(Intercept)",
     3.30340669817947e-13, 0.336472236621213, 0.41403933543088, 0.812657657927731,
     "Animal = Dog", 0.416414363984732, 2.43361335540045, 0.329805897010405,
     7.37892614249912, "Training = Affection as Reward", 1.59571315386023e-13,
     1.02961941718116, 0.36839419864032, 2.79488499270973, "Dance = Yes",
     0.00519181813139431, -3.1267605359604, 0.568381600027356, -5.50116424565803,
     "Animal = Dog*Training = Affection as Reward", 3.77291532433613e-08,
     -0.672944473242428, 0.507092552690095, -1.32706439815079, "Animal = Dog*Dance = Yes",
     0.184487380976988, -1.89461685466776, 0.406595067296308, -4.65971431298023,
     "Training = Affection as Reward*Dance = Yes", 3.16648558237037e-06,
     2.95932759166019, 0.68124006114786, 4.3440304826963, "Animal = Dog*Training = Affection as Reward*Dance = Yes",
     1.39892091261504e-05))

})

test_that("ContingencyTables (analysis 3) results match", {

  # Run the R syntax of analysis 3 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Dancing Cats and Dogs.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::ContingencyTables(
          data = NULL,
          version = "0.96.5",
          formula = Training ~ Dance,
          countsExpected = TRUE,
          layers = list(list(name = "Layer 1", variables = "Animal")),
          oddsRatio = TRUE,
          rowOrder = "descending"))

  table <- results[["results"]][["container_Training_Dance"]][["collection"]][["container_Training_Dance_crossTabChisq"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cat", "", 1, "", 4.76743408176905e-07, "N", "<unicode><unicode>",
     200, 25.3556925704187, "Dog", "", 1, "", 0.0473625609080163,
     "N", "<unicode><unicode>", 70, 3.93246187363835, "Total", "",
     1, "", 5.16597929731571e-05, "N", "<unicode><unicode>", 270,
     16.3862068965517))

  table <- results[["results"]][["container_Training_Dance"]][["collection"]][["container_Training_Dance_crossTabLogOdds"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cat", 1.04301968310191, 1.09770516618013, 1.31170926130053e-06,
     "", "Fisher's exact test ", "Log Odds Ratio", 2.79908980356416,
     2.6915285431554, 1.88392309014989, 1.89461685466776, "Dog",
     -2.30072582647103, -2.13602151617446, 0.0679742247527316, "",
     "Fisher's exact test ", "Log Odds Ratio", 0.119581160105319,
     0.00660004218960641, -1.04905742130482, -1.06471073699243, "Total",
     0.543578114321138, 0.577921721321454, 5.88684395505268e-05,
     "", "Fisher's exact test ", "Log Odds Ratio", 1.75846206147188,
     1.71234288728455, 1.14080194821479, 1.145132304303))

  table <- results[["results"]][["container_Training_Dance"]][["collection"]][["container_Training_Dance_crossTabMain"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Cat", 114, 100.44, "Affection as Reward", 48, 61.56, 162, 162,
     "Count", "Expected count", "Cat", 10, 23.56, "Food as Reward",
     28, 14.44, 38, 38, "Count", "Expected count", "Cat", 124, 124,
     "Total", 76, 76, 200, 200, "Count", "Expected count", "Dog",
     7, 10.8, "Affection as Reward", 29, 25.2, 36, 36, "Count", "Expected count",
     "Dog", 14, 10.2, "Food as Reward", 20, 23.8, 34, 34, "Count",
     "Expected count", "Dog", 21, 21, "Total", 49, 49, 70, 70, "Count",
     "Expected count", "Total", 121, 106.333333333333, "Affection as Reward",
     77, 91.6666666666667, 198, 198, "Count", "Expected count", "Total",
     24, 38.6666666666667, "Food as Reward", 48, 33.3333333333333,
     72, 72, "Count", "Expected count", "Total", 145, 145, "Total",
     125, 125, 270, 270, "Count", "Expected count"))

})

