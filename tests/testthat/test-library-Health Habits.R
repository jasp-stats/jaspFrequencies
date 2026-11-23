context("Library: Health Habits")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("ContingencyTables results match", {

  # Run the R syntax of analysis 1 of the JASP example file on the data of the file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Health Habits.jasp")
  set.seed(1)
  results <- jaspTools::runAnalysisSyntax(jaspFile = jaspFile, jaspFrequencies::ContingencyTables(
          data = NULL,
          version = "0.96.5",
          formula = `Fruit Consumption` ~ `Physical Activity`,
          countsExpected = TRUE,
          gamma = TRUE,
          kendallsTauB = TRUE,
          percentagesColumn = TRUE,
          vovkSellke = TRUE))

  table <- results[["results"]][["container_Fruit Consumption_Physical Activity"]][["collection"]][["container_Fruit Consumption_Physical Activity_crossTabChisq"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("", 10.8094306534633, "", 4, "", 0.00682394816704282, "N", "<unicode><unicode>",
     1184, 14.1524879485903))

  table <- results[["results"]][["container_Fruit Consumption_Physical Activity"]][["collection"]][["container_Fruit Consumption_Physical Activity_crossTabGamma"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0441220473554492, 0.0162160866692839, 0.189171334150986, 0.102693710410135
    ))

  table <- results[["results"]][["container_Fruit Consumption_Physical Activity"]][["collection"]][["container_Fruit Consumption_Physical Activity_crossTabKendallTau"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(4.42653897617164, 0.0216954352476952, 2.29565814720867, 0.0611732179429381
    ))

  table <- results[["results"]][["container_Fruit Consumption_Physical Activity"]][["collection"]][["container_Fruit Consumption_Physical Activity_crossTabMain"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list("Low", 0.638888888888889, 69, 51.902027027027, 0.465011286681716,
     206, 212.894425675676, 0.464454976303318, 294, 304.203547297297,
     0.480574324324324, 569, 569, " % within column", "Count", "Expected count",
     "Medium", 0.231481481481481, 25, 29.2804054054054, 0.284424379232506,
     126, 120.103885135135, 0.268562401263823, 170, 171.615709459459,
     0.271114864864865, 321, 321, " % within column", "Count", "Expected count",
     "High", 0.12962962962963, 14, 26.8175675675676, 0.250564334085779,
     111, 110.001689189189, 0.266982622432859, 169, 157.180743243243,
     0.248310810810811, 294, 294, " % within column", "Count", "Expected count",
     "Total", 1, 108, 108, 1, 443, 443, 1, 633, 633, 1, 1184, 1184,
     " % within column", "Count", "Expected count"))

})

