#
# Copyright (C) 2018 University of Amsterdam
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

InformedBinomialTestBayesianInternal <- function(jaspResults, dataset, options, ...) {

  dataset <- .informedBinReadData(dataset, options)
  .informedBinCheckErrors(dataset, options)
  .computeInformedBinResults(jaspResults, dataset, options)
  .createInformedBayesMainTable(jaspResults, options, type = "binomial")

  if (options[["descriptivesTable"]])
    .createInformedBinBayesDescriptivesTable(jaspResults, dataset, options)

  if (options[["descriptivesPlot"]])
    .createInformedBinBayesDescriptivesPlot(jaspResults, dataset, options)

  if (options[["posteriorPlot"]])
    .createInformedBinBayesPosteriorPlot(jaspResults, dataset, options)

  return()
}

.informedBinDependency <- c("factor", "successes", "sampleSize", "priorCounts", "models", "syntax",
                                    "bridgeSamples", "mcmcBurnin", "mcmcSamples", "setSeed", "seed")
.informedBinReadData    <- function(dataset, options) {

  fact <- asnum <- NULL
  if (options$factor != "") {
    fact <- options$factor
    if (options$successes != "")
      asnum <- options$successes
    if (options$sampleSize != "")
      asnum <- c(asnum, options$sampleSize)
  }

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = asnum, columns.as.factor = fact,
                                 exclude.na.listwise = NULL)
  } else {
    dataset <- .vdf(dataset, columns.as.numeric = asnum, columns.as.factor = fact)
  }

  # Reorder the rows of the factor and the successes (and expected probabilities) if the user changes the factor level order in JASP.
  # This ensures the ordering in tables and plots also changes appropriately.
  if (options$factor != "" && options$successes != "") {
    factLevelOrder        <- as.character(dataset[[.v(options$factor)]])

    # the following condition holds when `successes` are specified but the data set is not in aggregated form
    # the error is subsequently caught in  .multinomCheckErrors
    # we need to escape this function early because the operations under this check assume that the data set is already in aggregated form
    if (length(unique(factLevelOrder)) != length(factLevelOrder)) return(dataset)

    levelOrderUserWants   <- options$tableWidget[[1]]$levels
    whatUserWantsToWhatIs <- match(levelOrderUserWants, factLevelOrder)

    if (!identical(sort(whatUserWantsToWhatIs), whatUserWantsToWhatIs))
      dataset[seq_along(factLevelOrder), ] <- dataset[whatUserWantsToWhatIs, ]

    # For syntax mode the analysis will be called from RStudio and the factor levels may not match the tableWidget.
    factValues <- as.character(dataset[[.v(options$factor)]])
    facLevels  <- levels(dataset[[.v(options$factor)]])
    if (length(facLevels) == length(factValues) && !identical(factValues, facLevels))
      levels(dataset[[.v(options$factor)]]) <- factValues
  }

  return(dataset)
}
.informedBinCheckErrors <- function(dataset, options) {

  if (options[["factor"]] == "" || options[["successes"]] == "" || options[["sampleSize"]] == "")
    return()

  customChecks <- list(
    checkInput = function() {

      successes   <- dataset[[options[["successes"]]]]
      sampleSize  <- dataset[[options[["sampleSize"]]]]

      if (any(successes != round(successes)))
        return(gettext("Invalid successes: variable must contain only integer values."))

      if (any(sampleSize != round(sampleSize)))
        return(gettext("Invalid sample size: variable must contain only integer values."))

      if (any(successes > sampleSize))
        return(gettext("Sample size must be larger then the number of successes."))

      if (length(dataset[[options[["factor"]]]]) != length(levels(dataset[[options[["factor"]]]])))
        return(gettext("Factor must contain unique levels that map the successes to sample size."))

    }
  )

  .hasErrors(dataset,
             type = c("factorLevels", "negativeValues", "infinity"),
             negativeValues.target = c(options[["successes"]], options[["sampleSize"]]),
             infinity.target = c(options[["successes"]], options[["sampleSize"]]),
             factorLevels.target  = options[["factor"]],
             factorLevels.amount  = "< 1",
             custom = customChecks,
             exitAnalysisIfErrors = TRUE)
}
.computeInformedBinResults                <- function(jaspResults, dataset, options) {

  # skip if there is nothing new
  if (!is.null(jaspResults[["models"]]))
    return()

  # skip if the input is not specified
  if (options[["factor"]] == "" || options[["successes"]] == "" || options[["sampleSize"]] == "")
    return()

  models <- createJaspState()
  models$dependOn(.informedBinDependency)
  jaspResults[["models"]] <- models

  if (length(options[["models"]]) > 0)
    startProgressbar(length(options[["models"]]))


  modelsList <- list()

  # fit an overall unrestricted model (for plotting the posterior)
  modelsList[[1]] <- list(
    "model" = try(multibridge::binom_bf_informed(
      x             = dataset[[options[["successes"]]]],
      n             = dataset[[options[["sampleSize"]]]],
      Hr            = paste0(levels(dataset[[options[["factor"]]]]), collapse = ","),
      a             = options[["priorCounts"]][[1]][["values"]],
      b             = options[["priorCounts"]][[2]][["values"]],
      factor_levels = dataset[[options[["factor"]]]],
      bf_type       = "BF0r",
      nburnin       = options[["mcmcBurnin"]],
      niter         = options[["mcmcBurnin"]] + options[["mcmcSamples"]],
      maxiter       = options[["bridgeSamples"]],
      seed          = if (options[["setSeed"]]) .getSeedJASP(options) else sample.int(.Machine$integer.max, 1),
    )),
    "name"  = "unrestricted"
  )

  # estimate the restricted models
  for(i in seq_along(options[["models"]])) {

    if (nchar(options[["models"]][[i]][["syntax"]]) == 0) {

      modelsList[[i+1]] <- list(
        "model" = NULL,
        "name"  = options[["models"]][[i]][["modelName"]]
      )

    } else {

      modelsList[[i+1]] <- list(
        "model" = try(multibridge::binom_bf_informed(
          x             = dataset[[options[["successes"]]]],
          n             = dataset[[options[["sampleSize"]]]],
          Hr            = options[["models"]][[i]][["syntax"]],
          a             = options[["priorCounts"]][[1]][["values"]],
          b             = options[["priorCounts"]][[2]][["values"]],
          factor_levels = dataset[[options[["factor"]]]],
          bf_type       = "BF0r",
          nburnin       = options[["mcmcBurnin"]],
          niter         = options[["mcmcBurnin"]] + options[["mcmcSamples"]],
          maxiter       = options[["bridgeSamples"]],
          seed          = if (options[["setSeed"]]) .getSeedJASP(options) else sample.int(.Machine$integer.max, 1),
        )),
        "name"  = options[["models"]][[i]][["modelName"]]
      )
    }

    progressbarTick()
  }

  models$object <- modelsList

  return()
}
.createInformedBinBayesDescriptivesTable  <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  # Create Table
  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  descriptivesTable$position <- 2
  descriptivesTable$dependOn(c("factor", "descriptivesDisplay", "descriptivesTable", "successes", "sampleSize"))

  if (options[["descriptivesDisplay"]] == "counts") {
    outType <- "integer"
    outText <- gettext("Observed counts")
  } else {
    outType <- "number"
    outText <- gettext("Observed proportions")
  }


  descriptivesTable$addColumnInfo(name = "fact",     title = options[["factor"]],    type = "string")
  descriptivesTable$addColumnInfo(name = "observed", title = outText,                type = outType)
  descriptivesTable$addColumnInfo(name = "size",     title = gettext("Sample size"), type = "integer")

  jaspResults[["descriptivesTable"]] <- descriptivesTable


  # show empty Table if no variable is selected
  if (is.null(jaspResults[["models"]]))
    return()

  tempTable <- .createInformedBinBayesDescriptivesData(jaspResults, options)

  descriptivesTable$setData(tempTable)

  return()
}
.createInformedBinBayesDescriptivesPlot   <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["descriptivesPlot"]]))
    return()

  # Create Plot
  descriptivesPlot <- createJaspPlot(title = gettext("Descriptives plot"), width = 480, height = 320)
  descriptivesPlot$position <- 3
  descriptivesPlot$dependOn(c("factor", "descriptivesDisplay", "descriptivesPlot", "successes", "sampleSize"))

  jaspResults[["descriptivesPlot"]] <- descriptivesPlot

  # show empty plot if no variable is selected
  if (is.null(jaspResults[["models"]]))
    return()

  plotData <- .createInformedBinBayesDescriptivesData(jaspResults, options)

  descriptivesPlot$plotObject <- .createInformedBinBayesPlot(plotData, options, descriptives = TRUE)

  return()
}
.createInformedBinBayesPosteriorPlot      <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["posteriorPlot"]]))
    return()

  posteriorPlot <- createJaspPlot(title = gettext("Unrestricted posterior plot"), width = 480, height = 320)
  posteriorPlot$position <- 4
  posteriorPlot$dependOn(c(.informedBinDependency,  "display", "posteriorPlot", "posteriorPlotCiCoverage"))
  jaspResults[["posteriorPlot"]] <- posteriorPlot

  if (is.null(jaspResults[["models"]]))
    return()

  # extract posterior summary from the unrestricted model and format it for the plotting function
  tempSummary           <- data.frame(
    fact     = levels(dataset[[options[["factor"]]]]),
    observed = (options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]]) / (options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]] + options[["priorCounts"]][[2]][["values"]] + (dataset[[options[["sampleSize"]]]]-dataset[[options[["successes"]]]])),
    lowerCI  = stats::qbeta(p = (1-options[["posteriorPlotCiCoverage"]])/2, shape1 = options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]], shape2 = options[["priorCounts"]][[2]][["values"]] + (dataset[[options[["sampleSize"]]]]-dataset[[options[["successes"]]]])),
    upperCI  = stats::qbeta(p = 1-(1-options[["posteriorPlotCiCoverage"]])/2, shape1 = options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]], shape2 = options[["priorCounts"]][[2]][["values"]] + (dataset[[options[["sampleSize"]]]]-dataset[[options[["successes"]]]]))
  )

  posteriorPlot$plotObject <- .createInformedBinBayesPlot(tempSummary, options, descriptives = FALSE)

  return()
}
.createInformedBinBayesDescriptivesData   <- function(jaspResults, options) {

  model <- jaspResults[["models"]]$object[[1]]$model

  tempTable <- data.frame(
    "fact"     = model[["restrictions"]][["full_model"]][["parameters_full"]],
    "observed" = model[["restrictions"]][["full_model"]][["counts_full"]],
    "size"     = model[["restrictions"]][["full_model"]][["total_full"]]
  )

  if (options[["descriptivesDisplay"]] == "proportions")
    tempTable[["observed"]] <- tempTable[["observed"]] / tempTable[["size"]]

  return(tempTable)
}
.createInformedBinBayesPlot               <- function(plotData, options, descriptives = TRUE) {

  base_breaks_y <- function(x) {
    b <- pretty(c(0,x))
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), size = 0.75, inherit.aes=FALSE),
         ggplot2::scale_y_continuous(breaks=b))
  }

  # Counts or props
  yname <- sprintf("%1$s %2$s",
                   if (descriptives) gettext("Observed") else gettext("Estimated"),
                   if (descriptives && options[["descriptivesDisplay"]] == "counts") gettext("Counts") else gettext("Proportions")
  )

  # Prepare data for plotting
  plotFrame <- plotData
  # We need to reverse the factor's levels because of the coord_flip later
  plotFrame$fact <- factor(plotFrame$fact, levels = rev(plotFrame$fact))

  # Determine y-axis margin: If CIs could not be computed, use observed counts
  if (descriptives && options[["descriptivesDisplay"]] == "counts")
    plotFrame$yAxisMargin <- plotFrame$size
  else if (!is.null(plotFrame$upperCI))
    plotFrame$yAxisMargin <- plotFrame$upperCI
  else
    plotFrame$yAxisMargin <- plotFrame$observed

  if (descriptives && options[["descriptivesDisplay"]] == "counts") {
    plotFrameMax <- plotFrame
    plotFrameMax$observed <- plotFrameMax$size - plotFrameMax$observed
    plotFrameMax$split <- "max"
    plotFrame$split    <- "obs"
    plotFrame <- rbind(plotFrame, plotFrameMax)
    plotFrame$split <- factor(plotFrame$split, levels = c("max", "obs"))
  }

  # Create plot
  p <- ggplot2::ggplot(data = plotFrame)

  if (descriptives && options[["descriptivesDisplay"]] == "counts")
    p <- p + ggplot2::geom_bar(mapping = ggplot2::aes(x = fact, y = observed, fill = split),
                               stat = "identity", size = 0.75, colour = "black") +
    ggplot2::scale_discrete_manual(aesthetics = "fill", values  = c("obs" = "grey", "max" = "white"))
  else
    p <- p + ggplot2::geom_bar(mapping = ggplot2::aes(x = fact, y = observed),
                           stat = "identity", size = 0.75, colour="black", fill = "grey")


  if (!descriptives)
    p <-  p + ggplot2::geom_errorbar(ggplot2::aes(x = fact, ymin = plotFrame[["lowerCI"]], ymax = plotFrame[["upperCI"]]),
                                     size = 0.75, width = 0.3)

  p <- p + base_breaks_y(plotFrame$yAxisMargin) +
    ggplot2::xlab(options[["factor"]]) +
    ggplot2::ylab(yname) +
    ggplot2::coord_flip()

  p <- p + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw()

  return(p)
}
.binComputeCIs                            <- function(successes, sampleSize, CI, ifErrorReturn = NaN, scale) {

  # set function behaviour, if analysis crashes
  errorReturn <- rep(ifErrorReturn, 2)

  ciDf   <- data.frame(lowerCI = NA, upperCI = NA)
  for(i in seq_along(successes)) {
    # return results on count scale. If function crashes, return table with NaN's
    tryCatch(binomResult <- binom.test(successes[i], sampleSize[i], conf.level = CI)$conf.int * ifelse(scale == 'descCounts', sampleSize[i], 1),
             error = function(e) {binomResult <<- errorReturn})
    ciDf[i, ] <- binomResult
  }

  return(ciDf)
}
