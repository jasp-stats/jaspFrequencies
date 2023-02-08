#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#


ABTestBayesianInternal <- function(jaspResults, dataset = NULL, options) {

  ready <- (options$n1 != "" && options$y1 != "" && options$n2 != "" && options$y2 != "")

  ### READ DATA                ###
  if (ready)
    dataset <- .abTestBayesianReadData(dataset, options)

  ### This function is shared with summary stats a/b test
  .abTestMain(jaspResults, dataset, options, ready)
}

.abTestMain <- function(jaspResults, dataset, options, ready) {
  ## COMPUTE AB OBJECT         ###
  ab_obj <- .calcModel.abTest(jaspResults, dataset, options, ready)

  ### RESULTS - MAIN TABLE     ###
  .abTestBayesianTableMain(jaspResults, ab_obj, options, ready, position = 1)

  ### DESCRIPTIVES TABLE       ###
  if (options$descriptivesTable)
    .abTestBayesianDescriptivesTable(jaspResults, dataset, options, ready, position = 2)

  ### PRIOR AND POSTERIOR PLOT ###
  if (options$priorPosteriorPlot)
    .abTestPlotPriorPosterior(jaspResults, ab_obj, options, ready, position = 3)

  ### SEQUENTIAL PLOT          ###
  if (options$bfSequentialPlot)
    .abTestPlotSequential(jaspResults, ab_obj, ready, position = 4)

  ### ROBUSTNESS PLOT          ###
  if (options$bfRobustnessPlot)
    .abTestPlotRobustness(jaspResults, ab_obj, options, ready, position = 5)

  ### PRIOR PLOT               ###
  if (options$priorPlot)
    .abTestPlotPriorOnly(jaspResults, options, position = 6)
}


.abTestBayesianReadData <- function(dataset, options) {
  # Get the relevant data
  #
  # Args:
  #   dataset: dataset object
  #   options: a list of user options
  #
  # Return:
  #   The (numeric) columns given as dependent/covariates/wlsWeights

  if (!is.null(dataset))
    return(dataset)

  asNum             <- c(options$n1, options$y1, options$n2, options$y2)
  dataset           <- .readDataSetToEnd(columns.as.numeric = asNum, excluse.na.listwise = asNum)
  new_names         <- setNames(c("n1", "n2", "y1", "y2"),
                                c(.v(options$n1), .v(options$n2), .v(options$y1), .v(options$y2)))
  colnames(dataset) <- stringr::str_replace_all(colnames(dataset), new_names)

  return(dataset)
}


.abTestBayesianTableMain <- function(jaspResults, ab_obj, options, ready, position) {

  if (!is.null(jaspResults[["abTestBayesianTable"]]))
    return()

  # Create the main table
  abTestBayesianTable <- createJaspTable(title = gettext("Bayesian A/B Test"))
  abTestBayesianTable$dependOn(options = c("bayesFactorType", "bayesFactorOrder"), optionsFromObject = jaspResults[["model"]])
  abTestBayesianTable$position <- position

  # abTestBayesianTable$addCitation("JASP Team (2018). JASP (Version 0.9.3) [Computer software].")

  if (options$bayesFactorType == "BF10") {
    bf.title <- "BF<sub>10</sub>"
  } else if (options$bayesFactorType == "BF01") {
    bf.title <- "BF<sub>01</sub>"
  } else if (options$bayesFactorType == "LogBF10") {
    bf.title <- "Log(BF<sub>10</sub>)"
  }

  abTestBayesianTable$addColumnInfo(name = "Models",    title = gettext("Models"),    type = "string")
  abTestBayesianTable$addColumnInfo(name = "P(M)",      title = gettext("P(M)"),      type = "number", format = "sf:4;dp:3")
  abTestBayesianTable$addColumnInfo(name = "P(M|data)", title = gettext("P(M|data)"), type = "number", format = "sf:4;dp:3")
  abTestBayesianTable$addColumnInfo(name = "BF",        title = bf.title,    type = "number")

  jaspResults[["abTestBayesianTable"]] <- abTestBayesianTable

  if (!ready)
    return()

  .abTestBayesianFillTableMain(abTestBayesianTable, ab_obj, options)

  abTestBayesianTable$addFootnote(gettext("A positive log odds ratio means that the success rate in Group 2 is higher than in Group 1."))
}


.abTestBayesianFillTableMain <- function(abTestBayesianTable, ab_obj, options) {

  # Normalize prior probabilities
  sum_logor          <- options$priorModelProbabilityGreater + options$priorModelProbabilityLess + options$priorModelProbabilityEqual + options$priorModelProbabilityTwoSided
  priorModelProbabilityGreater   <- options$priorModelProbabilityGreater / sum_logor
  priorModelProbabilityLess      <- options$priorModelProbabilityLess / sum_logor
  priorModelProbabilityEqual     <- options$priorModelProbabilityEqual / sum_logor
  priorModelProbabilityTwoSided  <- options$priorModelProbabilityTwoSided / sum_logor

  if (inherits(ab_obj, "try-error")) {
    errorMessage <- as.character(ab_obj)
    abTestBayesianTable$setError(errorMessage)

    return()
  }

  output.rows <- list()
  rowCount    <- 0

  if (priorModelProbabilityEqual > 0) {
    rowCount = rowCount + 1
    output.rows[[rowCount]] <- list(
      "Models"    = gettext("Log odds ratio = 0"),
      "BF"        = 1.00,
      "P(M|data)" = ab_obj$post_prob[["H0"]],
      "P(M)"      = priorModelProbabilityEqual
    )
  }

  if (priorModelProbabilityGreater > 0) {
    rowCount = rowCount + 1
    output.rows[[rowCount]] <- list(
      "Models"    = gettext("Log odds ratio > 0"),
      "BF"        = ab_obj$bf[["bfplus0"]],
      "P(M|data)" = ab_obj$post_prob[["H+"]],
      "P(M)"      = priorModelProbabilityGreater
    )
  }

  if (priorModelProbabilityLess > 0) {
    rowCount = rowCount + 1
    output.rows[[rowCount]] <- list(
      "Models"    = gettext("Log odds ratio < 0"),
      "BF"        = ab_obj$bf[["bfminus0"]],
      "P(M|data)" = ab_obj$post_prob[["H-"]],
      "P(M)"      = priorModelProbabilityLess
    )
  }

  if (priorModelProbabilityTwoSided > 0) {
    rowCount = rowCount + 1
    output.rows[[rowCount]] <- list(
      "Models"    = gettextf("Log odds ratio %s 0","\u2260"),
      "BF"        = ab_obj$bf[["bf10"]],
      "P(M|data)" = ab_obj$post_prob[["H1"]],
      "P(M)"      = priorModelProbabilityTwoSided
    )
  }

  if (options$bayesFactorOrder == "bestModelTop") {
    ordered       <- output.rows[order(sapply(output.rows, "[[", "P(M|data)"), decreasing = TRUE)]
    best_model_bf <- ordered[[1]]$BF
    output.rows   <- list()

    for (r in 1:rowCount) {
      ordered[[r]]$BF  <- ordered[[r]]$BF / best_model_bf
    }
    output.rows   <- ordered
  }

  for (r in 1:rowCount) {
    if (options$bayesFactorType == "BF01") {
      output.rows[[r]]$BF <- 1 / output.rows[[r]]$BF
    } else if (options$bayesFactorType == "LogBF10") {
      output.rows[[r]]$BF <- base::log(output.rows[[r]]$BF)
    }
  }

  abTestBayesianTable$addRows(output.rows)
}


.calcModel.abTest <- function(jaspResults, dataset, options, ready) {

  if (!is.null(jaspResults[["model"]]))
    return(jaspResults[["model"]]$object)

  # we copy dependencies from this state object in a few places, so it must always exist
  jaspResults[["model"]] <- createJaspState()
  jaspResults[["model"]]$dependOn(c("n1", "y1", "n2", "y2", "normalPriorMean", "normalPriorSd", "priorModelProbabilityEqual",
                                    "priorModelProbabilityLess", "priorModelProbabilityGreater", "priorModelProbabilityTwoSided", "samples", "setSeed", "seed"))

  if (!ready)
    return(NULL)

  prior_par <- list(mu_psi = options$normalPriorMean, sigma_psi = options$normalPriorSd, mu_beta = 0, sigma_beta = 1)

  # Normalize prior probabilities
  sum_logor          <- options$priorModelProbabilityGreater + options$priorModelProbabilityLess + options$priorModelProbabilityEqual + options$priorModelProbabilityTwoSided
  priorModelProbabilityGreater   <- options$priorModelProbabilityGreater / sum_logor
  priorModelProbabilityLess      <- options$priorModelProbabilityLess / sum_logor
  priorModelProbabilityEqual     <- options$priorModelProbabilityEqual / sum_logor
  priorModelProbabilityTwoSided  <- options$priorModelProbabilityTwoSided / sum_logor

  prior_prob <- c(priorModelProbabilityTwoSided, priorModelProbabilityGreater, priorModelProbabilityLess, priorModelProbabilityEqual)
  names(prior_prob) <- c("H1", "H+", "H-", "H0")

  .setSeedJASP(options)
  ab <- try(abtest::ab_test(data = dataset, prior_par = prior_par, prior_prob = prior_prob,
                            posterior = TRUE, nsamples = options$samples))

  jaspResults[["model"]]$object <- ab

  return(ab)
}


.abTestBayesianDescriptivesTable <- function(jaspResults, dataset, options, ready, position) {

  if (!is.null(jaspResults[["abTestBayesianDescriptivesTable"]]))
    return()

  abTestBayesianDescriptivesTable <- createJaspTable(title = gettext("Descriptives"))

  abTestBayesianDescriptivesTable$dependOn(c("n1", "y1", "n2", "y2", "descriptivesTable"))
  abTestBayesianDescriptivesTable$position <- position

  abTestBayesianDescriptivesTable$addColumnInfo(name = "group",      title = "",                    type = "string")
  abTestBayesianDescriptivesTable$addColumnInfo(name = "counts",     title = gettext("Counts"),     type = "integer")
  abTestBayesianDescriptivesTable$addColumnInfo(name = "total",      title = gettext("Total"),      type = "integer")
  abTestBayesianDescriptivesTable$addColumnInfo(name = "proportion", title = gettext("Proportion"), type = "number", format = "sf:4;dp:3")

  jaspResults[["abTestBayesianDescriptivesTable"]] <- abTestBayesianDescriptivesTable

  if (!ready)
    return()

  .abTestBayesianFillDescriptivesTable(abTestBayesianDescriptivesTable, dataset)

  return()
}


.abTestBayesianFillDescriptivesTable <- function(abTestBayesianDescriptivesTable, dataset) {

  output.rows <- list()

  num_rows = length(dataset$y1)
  counts = dataset$y1[num_rows]
  total = dataset$n1[num_rows]
  output.rows[[1]] <- list(group = gettext("Group 1"), counts = counts, total = total, proportion = counts / total)

  counts = dataset$y2[num_rows]
  total = dataset$n2[num_rows]
  output.rows[[2]] <- list(group = gettext("Group 2"), counts = counts, total = total, proportion = counts / total)

  abTestBayesianDescriptivesTable$addRows(output.rows)
}


.abTestPlotPriorPosterior <- function(jaspResults, ab_obj, options, ready, position) {

  abTestPriorAndPosteriorPlot <- createJaspPlot(title = gettext("Prior and Posterior"),  width = 530, height = 400)
  abTestPriorAndPosteriorPlot$dependOn(c("n1", "y1", "n2", "y2", "normalPriorMean", "normalPriorSd", "samples", "priorPosteriorPlotType", "priorPosteriorPlot",
                                         "setSeed", "seed"))
  abTestPriorAndPosteriorPlot$position <- position

  jaspResults[["abTestPriorAndPosteriorPlot"]] <- abTestPriorAndPosteriorPlot

  if (!ready)
    return()

  abTestPriorAndPosteriorPlot$plotObject <- .plotPosterior.abTest(ab_obj, options$priorPosteriorPlotType)
}


.plotPosterior.abTest <- function(ab_obj, posteriorPlotType) {
  # Plot the posterior for different models
  #
  # Args:
  #   ab_obj: ab test object
  #   posteriorPlotType
  what <- switch(
    posteriorPlotType,
    "logOddsRatio" = "logor",
    "oddsRatio"    = "or",
    "relativeRisk" = "rrisk",
    "absoluteRisk" = "arisk",
    "p1P2"         = "p1p2"
  )

  plotFunc <- function() {
    # abtest::plot_posterior(x = ab_obj, what = what, hypothesis = "H1")
    abtest_plot_posterior(x = ab_obj, what = what, hypothesis = "H1")
  }

  return(plotFunc)
}


.abTestPlotSequential <- function(jaspResults, ab_obj, ready, position) {

  abTestSequentialPlot <- createJaspPlot(title = gettext("Sequential Analysis"),  width = 530, height = 400)
  abTestSequentialPlot$dependOn(options = "bfSequentialPlot", optionsFromObject = jaspResults[["model"]])
  abTestSequentialPlot$position <- position

  jaspResults[["abTestSequentialPlot"]] <- abTestSequentialPlot

  if (!ready)
    return()

  plotObject <- try(.plotSequentialAnalysis.abTest(ab_obj))
  if (jaspBase::isTryError(plotObject)) {
    abTestSequentialPlot$setError(jaspBase::.extractErrorMessage(plotObject))
  } else {
    abTestSequentialPlot$plotObject <- plotObject
  }
}


.abTestPlotRobustness <- function(jaspResults, ab_obj, options, ready, position) {

  abTestRobustnessPlot <- createJaspPlot(title = gettext("Bayes Factor Robustness Check"),  width = 530, height = 400)
  abTestRobustnessPlot$dependOn(c("n1", "y1", "n2", "y2", "normalPriorMean", "normalPriorSd", "bfRobustnessPlotStepsPriorMean", "bfRobustnessPlotStepsPriorSd", "bfRobustnessPlotLowerPriorMean", "bfRobustnessPlotUpperPriorMean", "bfRobustnessPlotLowerPriorSd", "bfRobustnessPlotUpperPriorSd", "bfRobustnessPlotType", "samples", "bfRobustnessPlot", "setSeed", "seed"))
  abTestRobustnessPlot$position <- position

  jaspResults[["abTestRobustnessPlot"]] <- abTestRobustnessPlot

  if (!ready)
    return()

  abTestRobustnessPlot$plotObject <- .plotRobustness.abTest(ab_obj, options)
}


.plotRobustness.abTest <- function(ab_obj, options) {
  # Args:
  #   ab_obj: ab test object

  mu_range    = c(options$bfRobustnessPlotLowerPriorMean,    options$bfRobustnessPlotUpperPriorMean)
  sigma_range = c(options$bfRobustnessPlotLowerPriorSd, options$bfRobustnessPlotUpperPriorSd)

  plotFunc <- function() {

    abtest::plot_robustness(
      x           = ab_obj,
      mu_steps    = options$bfRobustnessPlotStepsPriorMean,
      sigma_steps = options$bfRobustnessPlotStepsPriorSd,
      mu_range    = mu_range,
      sigma_range = sigma_range,
      bftype      = options$bfRobustnessPlotType
    )
  }

  return (plotFunc)
}


.abTestPlotPriorOnly <- function(jaspResults, options, position) {

  abTestPriorPlot <- createJaspPlot(title = gettext("Prior"),  width = 530, height = 400)
  abTestPriorPlot$dependOn(c("normalPriorMean", "normalPriorSd", "priorPlotType", "priorPlot"))
  abTestPriorPlot$position <- position

  jaspResults[["abTestPriorPlot"]] <- abTestPriorPlot

  abTestPriorPlot$plotObject <- .plotPrior.abTest(options)
}


.plotPrior.abTest <- function(options) {
  # Plot the prior
  #
  # Args:
  #   options

  prior_par <- list(mu_psi = options$normalPriorMean, sigma_psi = options$normalPriorSd, mu_beta = 0, sigma_beta = 1)

  what <- switch(
    options$priorPlotType,
    "logOddsRatio" = "logor",
    "oddsRatio"    = "or",
    "relativeRisk" = "rrisk",
    "absoluteRisk" = "arisk",
    "p1P2"         = "p1p2",
    "p1"           = "p1",
    "p2"           = "p2"
  )

  plotFunc <- function() {
    abtest::plot_prior(prior_par = prior_par, what = what, hypothesis = "H1")
  }

  return(plotFunc)
}


.plotSequentialAnalysis.abTest <- function(abObj, thin = 1) {

  # ggplot2 version of abtest::plot_sequential
  # wherever code is copied from abtest::plot_sequential this is indicated with a start and end

  # start abtest::plot_sequential
  data <- abObj$input$data
  ntotal <- unique(vapply(data, length, 0))
  index <- seq(thin, ntotal, thin)
  if (index[length(index)] != ntotal) {
    warning("thinning hides the last observation", call. = FALSE)
  }
  nsteps <- length(index)
  n <- vapply(index, function(i) data$n1[i] + data$n2[i], 0)

  out <- lapply(index, FUN = abtest:::compute_ab_seq, ab = abObj, data = data, ntotal = ntotal)

  col <- RColorBrewer::brewer.pal(8, name = "Dark2")[c(2, 8, 1, 6)]

  xticks <- pretty(c(0, data$n1[length(data$n1)] + data$n2[length(data$n2)]))
  xlim <- range(xticks)
  ylim <- c(0, 1)
  yticks <- pretty(ylim)
  hyp_index <- abObj$input$prior_prob != 0

  # end abtest::plot_sequential

  names(col) <- c("H-", "H0", "H+", "H1")

  hyp_index2 <- hyp_index[hyp_index]
  nLen <- length(n)
  dfLines <- data.frame(
    x = rep(c(0, n), length(hyp_index2)),
    y = numeric((1 + nLen) * length(hyp_index2)),
    g = rep(factor(names(hyp_index2)), each = nLen + 1)
  )

  for (i in seq_along(hyp_index2)) {
    hyp <- names(hyp_index2)[i]
    values <- c(abObj$input$prior_prob[hyp], vapply(out, FUN = function(y) y$post_prob[hyp], FUN.VALUE = 0))
    dfLines$y[dfLines[["g"]] == hyp] <- values
  }

  mainPlot <- ggplot2::ggplot(data = dfLines, mapping = ggplot2::aes(x = x, y = y, group = g, color = g, fill = g)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(name = gettext("n"), breaks = xticks, limits = xlim) +
    ggplot2::scale_y_continuous(name = gettext("Posterior Probability"), breaks = yticks, limits = ylim) +
    ggplot2::scale_color_manual(values = col) +
    ggplot2::scale_fill_manual(values = col) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw()

  # start abtest::plot_sequential
  p_prior <- abObj$input$prior_prob
  p_prior <- p_prior[c("H-", "H0", "H+", "H1")]
  p_prior[p_prior == 0] <- 1e-100

  p_post <- abObj$post_prob
  p_post <- p_post[c("H-", "H0", "H+", "H1")]
  p_post[p_post == 0] <- 1e-100
  col_rgb <- col2rgb(col)
  col_trans <- rgb(t(col_rgb), alpha = max(col_rgb)/3, maxColorValue = max(col_rgb))

  legend_order <- c("H1", "H+", "H-", "H0")
  p_prior <- p_prior[legend_order]
  p_post <- p_post[legend_order]

  # end abtest::plot_sequential

  names(col_trans) <- names(col)

  # unlike abtest::plot_sequential, do not draw any absent hypotheses
  p_prior <- p_prior[names(hyp_index2)]
  p_post  <- p_post [names(hyp_index2)]

  priorPieChart <- jaspGraphs::plotPieChart(p_prior, names(p_prior), showAxisText = FALSE) +
    ggplot2::scale_fill_manual(values = col_trans) +
    ggplot2::ylab("Prior Probabilities") +
    ggplot2::coord_polar("y", start = pi / 2) +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = jaspGraphs::getGraphOption("fontsize") * .75),
                   plot.margin = ggplot2::margin())

  posteriorPieChart <- jaspGraphs::plotPieChart(p_post, names(p_post), showAxisText = FALSE) +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::ylab("Posterior Probabilities") +
    ggplot2::coord_polar("y", start = pi / 2) +
    ggplot2::theme(legend.position = "none",
                   axis.title.x = ggplot2::element_text(size = jaspGraphs::getGraphOption("fontsize") * .75),
                   plot.margin = ggplot2::margin())

  legendText <- names(hyp_index2)
  legendText[legendText == "H-"] <- "H−" # abtest also does this and yes the two '-' are different...

  priorLabels <- .abTestmakeLabels(legendText, p_prior)
  dfPriorLabels <- data.frame(x = 1, y = rev(seq_along(priorLabels)), label = priorLabels, g = names(p_prior))
  priorText <- ggplot2::ggplot(data = dfPriorLabels, mapping = ggplot2::aes(x = x, y = y, label = label)) +
    ggplot2::geom_point(data = dfPriorLabels, mapping = ggplot2::aes(x = x - 0.5, y = y, group = g, fill = g), inherit.aes = FALSE, shape = 21, size = 3) +
    ggplot2::scale_fill_manual(values = col_trans) +
    ggplot2::geom_text(hjust = "left") +
    ggplot2::xlim(c(0, 15)) +
    ggplot2::ylim(range(seq_along(priorLabels)) + c(-2, 1)) +
    ggplot2::xlab("") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  postLabels <- .abTestmakeLabels(legendText, p_post)
  dfPostLabels <- data.frame(x = 1, y = rev(seq_along(postLabels)), label = postLabels, g = names(p_post))
  postText <- ggplot2::ggplot(data = dfPostLabels, mapping = ggplot2::aes(x = x, y = y, label = label)) +
    ggplot2::geom_point(data = dfPostLabels, mapping = ggplot2::aes(x = x - 0.5, y = y, group = g, fill = g), inherit.aes = FALSE, shape = 21, size = 3) +
    ggplot2::scale_fill_manual(values = col) +
    ggplot2::geom_text(hjust = "left") +
    ggplot2::xlim(c(0, 15)) +
    ggplot2::ylim(range(seq_along(postLabels)) + c(-2, 1)) +
    ggplot2::xlab("") +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position = "none")

  arrowPlot <- ggplot2::ggplot(data = data.frame(x = c(.5, 1), y = c(0, 0)), mapping = ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_line(arrow = ggplot2::arrow(length = grid::unit(0.3, "cm")), size = 1.05) +
    ggplot2::xlim(c(0, 1)) +
    ggplot2::ylim(c(-1, .5)) +
    ggplot2::theme_void() +
    ggplot2::xlab("")

  widths <- c(3, 4, 1, 3, 4) # relative widths of the figures above
  layout <- rbind(rep(1:5, widths), matrix(6, 4, sum(widths)))

  combinedPlot <- jaspGraphs:::jaspGraphsPlot$new(
    subplots = list(
      priorPieChart, priorText, arrowPlot, posteriorPieChart, postText, mainPlot
    ), layout = layout, #widths = rep(1, 6), heights = c(.2, .8)
  )

  return(combinedPlot)

}


.abTestmakeLabels <- function(legendText, probs) {
  # so it is only translated once, .3f is inherited from abtest
  gettextf("P(%1$s | data) = %2$.3f", legendText, probs)
}


# copied from abtest because the following crashes (see https://github.com/jasp-stats/INTERNAL-jasp/issues/2238)
# f <- tempfile()
# grDevices::png(f)
# abtest::plot_posterior(abtest::ab_test(list(y1=3,y2=3,n1=5,n2=5)))
# grDevices::dev.control("enable")
# out <- grDevices::recordPlot()
# grDevices::dev.off()
# if (file.exists(f)) file.remove(f)
# gridGraphics::grid.echo(out)
abtest_plot_posterior <- function (x, what = "logor", hypothesis = "H1", ci = 0.95, p1lab = "p1",
          p2lab = "p2", p1adj = 0.44, p2adj = 0.56, ...)
{
  fitdist    <- abtest:::fitdist
  dposterior <- abtest:::dposterior
  qposterior <- abtest:::qposterior
  dprior     <- abtest:::dprior
  qprior     <- abtest:::qprior

  if (!(what %in% c("logor", "or", "rrisk", "arisk", "p1p2"))) {
    stop("what needs to be either \"logor\", \"or\", \"rrisk\", \"arisk\",\n         or \"p1p2",
         call. = FALSE)
  }
  if (!(hypothesis %in% c("H1", "H+", "H-"))) {
    stop("hypothesis needs to be either \"H1\", \"H+\", or \"H-\"",
         call. = FALSE)
  }
  lwd <- 2
  cexPoints <- 1.5
  cexAxis <- 1.2
  cexYlab <- 1.5
  cexXlab <- 1.5
  cexTextBF <- 1.4
  cexCI <- 1.1
  cexLegend <- 1.2
  lwdAxis <- 1.2
  if (x$input$posterior) {
    post_samples <- x$post$H1
  }
  else {
    x <- abtest:::ab_test(data = x$input$data, prior_par = x$input$prior_par,
                 prior_prob = x$input$prior_prob, nsamples = x$input$nsamples,
                 is_df = x$input$is_df, posterior = TRUE)
  }
  if (what == "p1p2") {
    post_samples <- switch(hypothesis, H1 = x$post$H1, `H+` = x$post$Hplus,
                           `H-` = x$post$Hminus)
    fit1 <- fitdist(post_samples = post_samples, what = "p1")
    fit2 <- fitdist(post_samples = post_samples, what = "p2")
    pci <- c((1 - ci)/2, 1 - (1 - ci)/2)
    CIlow1 <- qposterior(p = pci[1], what = "p1", hypothesis = hypothesis,
                         fit = fit1)
    CIhigh1 <- qposterior(p = pci[2], what = "p1", hypothesis = hypothesis,
                          fit = fit1)
    medianPosterior1 <- qposterior(p = 0.5, what = "p1",
                                   hypothesis = hypothesis, fit = fit1)
    CIlow2 <- qposterior(p = pci[1], what = "p2", hypothesis = hypothesis,
                         fit = fit2)
    CIhigh2 <- qposterior(p = pci[2], what = "p2", hypothesis = hypothesis,
                          fit = fit2)
    medianPosterior2 <- qposterior(p = 0.5, what = "p2",
                                   hypothesis = hypothesis, fit = fit2)
    if (abs(CIhigh1 - CIlow1) < 0.01 && abs(CIhigh2 - CIlow2) <
        0.01) {
      qlow1 <- qposterior(p = 0.001, what = "p1", hypothesis = hypothesis,
                          fit = fit1)
      qhigh1 <- qposterior(p = 0.999, what = "p1", hypothesis = hypothesis,
                           fit = fit1)
      qlow2 <- qposterior(p = 0.001, what = "p2", hypothesis = hypothesis,
                          fit = fit2)
      qhigh2 <- qposterior(p = 0.999, what = "p2", hypothesis = hypothesis,
                           fit = fit2)
      xlim <- c(min(CIlow1, CIlow2, qlow1, qlow2), max(CIhigh1,
                                                       CIhigh2, qhigh1, qhigh2))
    }
    else {
      xlim <- c(0, 1)
    }
    xticks <- pretty(xlim, n = 5)
    xlim <- range(xticks)
    stretch <- ifelse(hypothesis == "H1", 1.2, 1.32)
    xx <- seq(min(xticks), max(xticks), length.out = 1000)
    priorLine1 <- dprior(x1 = xx, x2 = NULL, prior_par = x$input$prior_par,
                         what = "p1", hypothesis = hypothesis)
    priorLine2 <- dprior(x1 = xx, x2 = NULL, prior_par = x$input$prior_par,
                         what = "p2", hypothesis = hypothesis)
    postLine1 <- dposterior(x = xx, what = "p1", hypothesis = hypothesis,
                            fit = fit1)
    postLine2 <- dposterior(x = xx, what = "p2", hypothesis = hypothesis,
                            fit = fit2)
    dpriormax1 <- max(priorLine1)
    dpriormax2 <- max(priorLine2)
    dpostmax1 <- max(postLine1)
    dpostmax2 <- max(postLine2)
    ylim <- c(0, stretch * max(c(dpriormax1, dpriormax2,
                                 dpostmax1, dpostmax2)))
    yticks <- pretty(ylim)
    yticks <- c(yticks, yticks[length(yticks)] + diff(yticks[(length(yticks) -
                                                                1):length(yticks)]), yticks[length(yticks)] + 2 *
                  diff(yticks[(length(yticks) - 1):length(yticks)]))
    ylim <- range(yticks)
  }
  else {
    post_samples <- x$post$H1
    fit <- fitdist(post_samples = post_samples, what = what)
    prior_range <- qprior(p = c(0.05, 0.95), prior_par = x$input$prior_par,
                          what = what, hypothesis = "H1")
    post_range <- qposterior(p = c(0.001, 0.999), what = what,
                             hypothesis = "H1", fit = fit)
    xticks <- pretty(c(prior_range, post_range))
    xlim <- range(xticks)
    stretch <- ifelse(hypothesis == "H1", 1.2, 1.32)
    xx <- seq(min(xticks), max(xticks), length.out = 1000)
    priorLine <- dprior(x1 = xx, x2 = NULL, prior_par = x$input$prior_par,
                        what = what, hypothesis = hypothesis)
    postLine <- dposterior(x = xx, what = what, hypothesis = hypothesis,
                           fit = fit)
    dpriormax <- max(priorLine)
    dpostmax <- max(postLine)
    pci <- c((1 - ci)/2, 1 - (1 - ci)/2)
    CIlow <- qposterior(p = pci[1], what = what, hypothesis = hypothesis,
                        fit = fit)
    CIhigh <- qposterior(p = pci[2], what = what, hypothesis = hypothesis,
                         fit = fit)
    ylim <- c(0, max(stretch * c(dpriormax, dpostmax)))
    yticks <- pretty(ylim)
    xlim <- c(min(CIlow, range(xticks)[1]), max(range(xticks)[2],
                                                CIhigh))
    plot(1, 1, xlim = xlim, ylim = ylim, ylab = "", xlab = "",
         type = "n", axes = FALSE)
    yCI <- grconvertY(max(dpriormax, dpostmax), "user",
                      "ndc") + 0.04
    yCI <- grconvertY(yCI, "ndc", "user")
    medianPosterior <- qposterior(p = 0.5, what = what,
                                  hypothesis = hypothesis, fit = fit)
    if ((yCI > yticks[length(yticks) - 2] && diff(yticks[(length(yticks) -
                                                          1):length(yticks)])/diff(range(yticks)) < 1/6) ||
        (yCI > yticks[length(yticks) - 1])) {
      yticks <- c(yticks, yticks[length(yticks)] + diff(yticks[(length(yticks) -
                                                                  1):length(yticks)]))
      ylim <- range(yticks)
    }
  }
  yticks <- pretty(ylim)
  ylim <- range(yticks)
  ylabels <- as.character(yticks)
  xlab <- switch(what, logor = "Log Odds Ratio", or = "Odds Ratio",
                 rrisk = "Relative Risk", arisk = "Absolute Risk")
  op <- par(mar = c(5.6, 5, 7, 4) + 0.1, las = 1, xpd = TRUE)
  plot(1, 1, xlim = xlim, ylim = ylim, ylab = "", xlab = "",
       type = "n", axes = FALSE)
  if (what == "p1p2") {
    col_trans1 <- rgb(0, 0, 0, alpha = 0.5)
    col_trans2 <- rgb(0, 0, 0, alpha = 0.8)
    lines(xx, postLine1, lwd = lwd, col = col_trans1)
    lines(xx, postLine2, lwd = lwd, col = col_trans2)
    lines(xx, priorLine1, lwd = lwd, lty = 3, col = col_trans1)
    lines(xx, priorLine2, lwd = lwd, lty = 3, col = col_trans2)
    yCI1 <- grconvertY(max(dpriormax1, dpriormax2, dpostmax1,
                           dpostmax2), "user", "ndc") + 0.03
    yCI1 <- grconvertY(yCI1, "ndc", "user")
    yCI2 <- grconvertY(max(dpriormax1, dpriormax2, dpostmax1,
                           dpostmax2), "user", "ndc") + 0.06
    yCI2 <- grconvertY(yCI2, "ndc", "user")
    arrows(CIlow1, yCI1, CIhigh1, yCI1, angle = 90, code = 3,
           length = 0.1, lwd = lwd, col = col_trans1)
    arrows(CIlow2, yCI2, CIhigh2, yCI2, angle = 90, code = 3,
           length = 0.1, lwd = lwd, col = col_trans2)
    medianText1 <- formatC(medianPosterior1, digits = 3,
                           format = "f")
    medianText2 <- formatC(medianPosterior2, digits = 3,
                           format = "f")
    offsetTopPart <- 0.06
    yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
    textci <- as.character(ci)
    percentage <- strsplit(textci, split = "0.")[[1]][2]
    CIText1 <- paste0(percentage, "% CI: [", bquote(.(formatC(CIlow1,
                                                              3, format = "f"))), ", ", bquote(.(formatC(CIhigh1,
                                                                                                         3, format = "f"))), "]")
    CIText2 <- paste0(percentage, "% CI: [", bquote(.(formatC(CIlow2,
                                                              3, format = "f"))), ", ", bquote(.(formatC(CIhigh2,
                                                                                                         3, format = "f"))), "]")
    medianLegendText1 <- paste("median =", medianText1)
    medianLegendText2 <- paste("median =", medianText2)
    text(min(xticks), yy2, medianLegendText1, cex = 1.1,
         pos = 4, col = col_trans1)
    text(min(xticks), yy, CIText1, cex = 1.1, pos = 4, col = col_trans1)
    text(max(xticks), yy2, medianLegendText2, cex = 1.1,
         pos = 2, col = col_trans2)
    text(max(xticks), yy, CIText2, cex = 1.1, pos = 2, col = col_trans2)
    index <- which.min(c(medianPosterior1, medianPosterior2,
                         1 - medianPosterior1, 1 - medianPosterior2))
    legendPosition <- switch(index, `1` = max(xticks), `2` = max(xticks),
                             `3` = min(xticks), `4` = min(xticks))
    legend(legendPosition, max(yticks), legend = c("Posterior",
                                                   "Prior"), lty = c(1, 3), bty = "n", lwd = rep(lwd,
                                                                                                 2), cex = cexLegend, xjust = ifelse(all.equal(legendPosition,
                                                                                                                                               max(xticks)), 1, 0), yjust = 1, x.intersp = 0.6,
           seg.len = 1.2)
    # START OF CHANGES
    # original
    # mtext(c(p1lab, "&", p2lab), col = c(col_trans1, "black", col_trans2), adj = c(p1adj, 0.5, p2adj), side = 1, cex = cexXlab, line = 2.5)
    mtext(p1lab, col = col_trans1, adj = p1adj, side = 1, cex = cexXlab, line = 2.5)
    mtext("&",   col = "black",    adj = 0.5,   side = 1, cex = cexXlab, line = 2.5)
    mtext(p2lab, col = col_trans2, adj = p2adj, side = 1, cex = cexXlab, line = 2.5)
    # END OF CHANGES
  }
  else {
    lines(xx, postLine, lwd = lwd)
    lines(xx, priorLine, lwd = lwd, lty = 3)
    arrows(CIlow, yCI, CIhigh, yCI, angle = 90, code = 3,
           length = 0.1, lwd = lwd)
    medianText <- formatC(medianPosterior, digits = 3, format = "f")
    offsetTopPart <- 0.06
    yy <- grconvertY(0.756 + offsetTopPart, "ndc", "user")
    yy2 <- grconvertY(0.812 + offsetTopPart, "ndc", "user")
    textci <- as.character(ci)
    percentage <- strsplit(textci, split = "0.")[[1]][2]
    CIText <- paste0(percentage, "% CI: [", bquote(.(formatC(CIlow,
                                                             3, format = "f"))), ", ", bquote(.(formatC(CIhigh,
                                                                                                        3, format = "f"))), "]")
    medianLegendText <- paste("median =", medianText)
    text(max(xticks), yy2, medianLegendText, cex = 1.1,
         pos = 2)
    text(max(xticks), yy, CIText, cex = 1.1, pos = 2)
    if (medianPosterior <= mean(xlim)) {
      legendPosition <- max(xticks)
      legend(legendPosition, max(yticks), legend = c("Posterior",
                                                     "Prior"), lty = c(1, 3), bty = "n", lwd = rep(lwd,
                                                                                                   2), cex = cexLegend, xjust = 1, yjust = 1, x.intersp = 0.6,
             seg.len = 1.2)
    }
    else {
      legendPosition <- min(xticks)
      legend(legendPosition, max(yticks), legend = c("Posterior",
                                                     "Prior"), lty = c(1, 3), bty = "n", lwd = rep(lwd,
                                                                                                   2), cex = cexLegend, xjust = 0, yjust = 1, x.intersp = 0.6,
             seg.len = 1.2)
    }
    mtext(xlab, side = 1, cex = cexXlab, line = 2.5)
  }
  axis(1, at = xticks, cex.axis = cexAxis, lwd = lwdAxis)
  axis(2, at = yticks, labels = ylabels, cex.axis = cexAxis,
       lwd = lwdAxis)
  if (nchar(ylabels[length(ylabels)]) > 4) {
    mtext(text = "Density", side = 2, las = 0, cex = cexYlab,
          line = 4)
  }
  else if (nchar(ylabels[length(ylabels)]) == 4) {
    mtext(text = "Density", side = 2, las = 0, cex = cexYlab,
          line = 3.25)
  }
  else if (nchar(ylabels[length(ylabels)]) < 4) {
    mtext(text = "Density", side = 2, las = 0, cex = cexYlab,
          line = 2.85)
  }
  par(op)
}
