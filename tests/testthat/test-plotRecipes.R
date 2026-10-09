test_that("binomial producer stores a redrawable environment-free recipe", {
  holder <- new.env(parent = emptyenv())
  data <- data.frame(label = "yes", proportion = .6, lowerCI = .4, upperCI = .8)
  jaspFrequencies:::.binomFillDescriptivesPlot(holder, data, .5)
  expect_s3_class(holder$plotObject, "jaspPlotRecipe")
  recipe <- unserialize(serialize(holder$plotObject, NULL))
  set.seed(731)
  seed <- .Random.seed
  first <- suppressWarnings(jaspGraphs::materializeJaspPlotRecipe(recipe))
  second <- suppressWarnings(jaspGraphs::materializeJaspPlotRecipe(recipe))
  expect_identical(.Random.seed, seed)
  expect_equal(suppressWarnings(ggplot2::ggplot_build(first))$data, suppressWarnings(ggplot2::ggplot_build(second))$data)
  expect_equal(suppressWarnings(ggplot2::ggplot_build(first))$data[[1]]$ymin, .4)
  expect_equal(suppressWarnings(ggplot2::ggplot_build(first))$data[[1]]$ymax, .8)
})

test_that("informed summary and shared sequential recipes survive serialization", {
  data <- data.frame(fact = c("A", "B"), observed = c(.3, .7), lowerCI = c(.2, .6), upperCI = c(.4, .8), size = c(10, 10))
  for (fun in c(".createInformedBinBayesPlot", ".createInformedMultPlot")) {
    for (descriptives in c(TRUE, FALSE)) {
      recipe <- jaspGraphs::createJaspPlotRecipe(
        fun = paste0("jaspFrequencies:::" , fun),
        args = list(plotData = data, options = list(factor = "Category", descriptivesDisplay = "proportions"), descriptives = descriptives)
      )
      plot <- suppressWarnings(jaspGraphs::materializeJaspPlotRecipe(unserialize(serialize(recipe, NULL))))
      expect_true(inherits(plot, "ggplot"))
      expect_equal(suppressWarnings(ggplot2::ggplot_build(plot))$data[[1]]$y, data$observed)
    }
  }
  recipe <- jaspGraphs::createJaspPlotRecipe(
    fun = "jaspFrequencies:::.createInformedMultPlotSequentialProb",
    args = list(plotData = data.frame(step = 1:2, postProb = c(.4, .6), model = "H1"))
  )
  expect_true(inherits(jaspGraphs::materializeJaspPlotRecipe(unserialize(serialize(recipe, NULL))), "ggplot"))
})

test_that("Bayesian binomial label is mathematical after recipe roundtrip", {
  recipe <- jaspGraphs::createJaspPlotRecipe(
    fun = "jaspFrequencies:::.drawBayesBinomPriorPosterior",
    args = list(dfLinesPP = jaspFrequencies:::.dfLinesPP(a = 1, b = 1, hyp = "two.sided", theta0 = .5, counts = 3, n = 4),
                dfPointsPP = jaspFrequencies:::.dfPointsPP(a = 1, b = 1, hyp = "two.sided", theta0 = .5, counts = 3, n = 4),
                label = "Population proportion", additionalInfo = FALSE, BF10 = 2,
                quantiles = list(ci.lower = .2, ci.upper = .8, ci.median = .6), hypForPlots = "equal")
  )
  plot <- suppressWarnings(jaspGraphs::materializeJaspPlotRecipe(unserialize(serialize(recipe, NULL))))
  expect_identical(plot$scales$get_scales('x')$name, bquote(paste("Population proportion", ~theta)))
})
