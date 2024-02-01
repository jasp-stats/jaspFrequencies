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
  options <- .informedBayesParsePriorModelProbability(options)
  dataset <- .binomialAggregateData(dataset, options)

  .computeInformedBinResults(jaspResults, dataset, options)
  .createInformedBayesMainTable(jaspResults, options, type = "binomial")

  if (options[["descriptivesTable"]])
    .createInformedBinBayesDescriptivesTable(jaspResults, dataset, options)

  if (options[["descriptivesPlot"]])
    .createInformedBinBayesDescriptivesPlot(jaspResults, dataset, options)

  if (options[["posteriorPlot"]])
    .createInformedBinBayesPosteriorPlot(jaspResults, dataset, options)

  if (options[["sequentialAnalysisPlot"]])
    .createInformedBinSequentialAnalysisPlot(jaspResults, dataset, options)

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
.binomialAggregateData  <- function(dataset, options) {

  if (length(dataset[[options[["factor"]]]]) != length(levels(dataset[[options[["factor"]]]])) && options[["successes"]] != "") {

    if (options[["sampleSize"]] != "")
      .quitAnalysis("No `Sample Size` should be provided in case the individual successes for a given factor level are specified.")

    individualDataFactor    <- dataset[[options[["factor"]]]]
    individualDataSuccesses <- dataset[[options[["successes"]]]]

    if (length(unique(individualDataSuccesses)) != 2)
      .quitAnalysis(gettext("The `Success` must have two levels if the `Sample Size` is not specified."))

    frequencies       <- table(individualDataFactor, individualDataSuccesses)
    sampleSize        <- rowSums(frequencies)
    dataset           <- data.frame(factor(rownames(frequencies), levels = levels(dataset[[options[["factor"]]]])))
    colnames(dataset) <- options[["factor"]]

    attr(dataset, "individual") <- TRUE
    attr(dataset, "successes")  <- frequencies[,2]
    attr(dataset, "sampleSize") <- sampleSize
    attr(dataset, "individualDataFactor")    <- individualDataFactor
    attr(dataset, "individualDataSuccesses") <- individualDataSuccesses
  } else {
    attr(dataset, "individual") <- FALSE
  }

  return(dataset)
}
.informedBinCheckErrors <- function(dataset, options) {

  if (options[["factor"]] == "" || options[["successes"]] == "" || (options[["sampleSize"]] == "" && is.null(attr(dataset, "sampleSize"))))
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
  if (options[["factor"]] == "" || options[["successes"]] == "" || (options[["sampleSize"]] == "" && is.null(attr(dataset, "sampleSize"))))
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
      x             = .informedExtractSuccesses(dataset, options),
      n             = .informedExtractSampleSize(dataset, options),
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
          x             = .informedExtractSuccesses(dataset, options),
          n             = .informedExtractSampleSize(dataset, options),
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
.computeInformedBinSequentialResults      <- function(jaspResults, dataset, options) {

  # skip if there is nothing new
  if (!is.null(jaspResults[["sequentialAnalysisResults"]]))
    return()

  # skip if the input is not specified
  # skip if the input is not specified
  if (options[["factor"]] == "" || options[["successes"]] == "" || (options[["sampleSize"]] == "" && is.null(attr(dataset, "sampleSize"))))
    return()

  # skip if the data are only aggregated
  if (!attr(dataset, "individual"))
    return()

  sequential <- createJaspState()
  sequential$dependOn(c(.informedMultDependency, "sequentialAnalysisNumberOfSteps"))
  jaspResults[["sequentialAnalysisResults"]] <- sequential

  # extract data
  individualDataFactor    <- attr(dataset, "individualDataFactor")
  individualDataSuccesses <- attr(dataset, "individualDataSuccesses")

  # specify sequential steps (do all if 0)
  if(options[["sequentialAnalysisNumberOfSteps"]] == 0)
    steps <- 1:length(individualDataFactor)
  else
    steps <- unique(round(c(seq.int(0, length(individualDataFactor), length.out = options[["sequentialAnalysisNumberOfSteps"]]), length(individualDataFactor))))[-1]

  startProgressbar(length(steps), label = gettext("Performing sequential analysis."))

  out <- list()
  for (step in steps){

    tempOutput <- list()

    ### prepare data
    frequencies          <- table(individualDataFactor[1:step], individualDataSuccesses[1:step])
    sampleSize           <- rowSums(frequencies)
    seqDataset           <- data.frame(factor(rownames(frequencies), levels = levels(dataset[[options[["factor"]]]])), frequencies[,2], sampleSize)
    colnames(seqDataset) <- c(options[["factor"]], "successes", "sampleSize")


    ### fit models & extract margliks
    # fit an overall unrestricted model (for plotting the posterior)
    model0 <- try(multibridge::binom_bf_informed(
      x             = seqDataset[["successes"]],
      n             = seqDataset[["sampleSize"]],
      Hr            = paste0(levels(dataset[[options[["factor"]]]]), collapse = ","),
      a             = options[["priorCounts"]][[1]][["values"]],
      b             = options[["priorCounts"]][[2]][["values"]],
      factor_levels = seqDataset[[options[["factor"]]]],
      bf_type       = "BF0r",
      nburnin       = options[["mcmcBurnin"]],
      niter         = options[["mcmcBurnin"]] + options[["mcmcSamples"]],
      maxiter       = options[["bridgeSamples"]],
      seed          = if (options[["setSeed"]]) .getSeedJASP(options) else sample.int(.Machine$integer.max, 1),
    ))

    # extract margliks
    if (jaspBase::isTryError(model0)) {
      tempOutput[[1]] <- data.frame(
        model         = "Null",
        marglik       = NA
      )
      tempOutput[[2]] <- data.frame(
        model         = "Encompassing",
        marglik       = NA
      )
    } else {
      tempOutput[[1]] <- data.frame(
        model         = "Null",
        marglik       = model0$logml[["logmlH0"]]
      )
      tempOutput[[2]] <- data.frame(
        model         = "Encompassing",
        marglik       = model0$logml[["logmlHe"]]
      )
    }


    # estimate the restricted models & extract margliks
    for(i in seq_along(options[["models"]])) {

      if (nchar(options[["models"]][[i]][["syntax"]]) == 0) {

        next

      } else {

        model1 <- try(multibridge::binom_bf_informed(
          x             = seqDataset[["successes"]],
          n             = seqDataset[["sampleSize"]],
          Hr            = options[["models"]][[i]][["syntax"]],
          a             = options[["priorCounts"]][[1]][["values"]],
          b             = options[["priorCounts"]][[2]][["values"]],
          factor_levels = seqDataset[[options[["factor"]]]],
          bf_type       = "BF0r",
          nburnin       = options[["mcmcBurnin"]],
          niter         = options[["mcmcBurnin"]] + options[["mcmcSamples"]],
          maxiter       = options[["bridgeSamples"]],
          seed          = if (options[["setSeed"]]) .getSeedJASP(options) else sample.int(.Machine$integer.max, 1),
        ))

        # extract margliks
        if (jaspBase::isTryError(model1)) {
          tempOutput[[i+2]] <- data.frame(
            model         = options[["models"]][[i]][["modelName"]],
            marglik       = NA
          )
        } else {
          tempOutput[[i+2]] <- data.frame(
            model         = options[["models"]][[i]][["modelName"]],
            marglik       = model1$logml[["logmlHr"]]
          )
        }
      }
    }

    ### store sequential summary
    tempOutput      <- do.call(rbind, tempOutput)
    tempOutput$step <- step
    out[[step]]     <- tempOutput

    progressbarTick()
  }

  out <- do.call(rbind, out)
  sequential$object <- out

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
  if (is.null(jaspResults[["models"]]) || jaspBase::isTryError(jaspResults[["models"]]$object[[1]]$model))
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
  if (is.null(jaspResults[["models"]]) || jaspBase::isTryError(jaspResults[["models"]]$object[[1]]$model))
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

  if (is.null(jaspResults[["models"]]) || jaspBase::isTryError(jaspResults[["models"]]$object[[1]]$model))
    return()

  successes  <- .informedExtractSuccesses(dataset, options)
  sampleSize <- .informedExtractSampleSize(dataset, options)
  priorAlpha <- options[["priorCounts"]][[1]][["values"]]
  priorBeta  <- options[["priorCounts"]][[2]][["values"]]

  # extract posterior summary from the unrestricted model and format it for the plotting function
  tempSummary           <- data.frame(
    fact     = levels(dataset[[options[["factor"]]]]),
    observed = (priorAlpha + successes) / (priorAlpha + successes + priorBeta + (sampleSize-successes)),
    lowerCI  = stats::qbeta(p = (1-options[["posteriorPlotCiCoverage"]])/2,   shape1 = priorAlpha + successes, shape2 = priorBeta + (sampleSize-successes)),
    upperCI  = stats::qbeta(p = 1-(1-options[["posteriorPlotCiCoverage"]])/2, shape1 = priorAlpha + successes, shape2 = priorBeta + (sampleSize-successes))
  )

  posteriorPlot$plotObject <- .createInformedBinBayesPlot(tempSummary, options, descriptives = FALSE)

  return()
}
.createInformedBinSequentialAnalysisPlot  <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["sequentialAnalysisPlot"]]))
    return()


  # create/obtain sequential analysis
  .computeInformedBinSequentialResults(jaspResults, dataset, options)
  sequentialAnalysisResults <- jaspResults[["sequentialAnalysisResults"]]$object

  # create an empty plot in case the selection is restricted
  if (is.null(jaspResults[["models"]]$object) || .informedBayesNumberOfModels(jaspResults, options) < 2) {
    tempPlot <- createJaspPlot(title = gettext("Sequential analysis"), width = 480, height = 320)
    tempPlot$dependOn(c(.informedMultDependency, "includeNullModel", "includeEncompassingModel"))
    tempPlot$position <- 5
    jaspResults[["sequentialAnalysisPlot"]] <- tempPlot
    tempPlot$setError(gettext("At least two models need to be specified."))
    return()
  }

  # remove unused hypotheses
  if (!options[["includeNullModel"]])
    sequentialAnalysisResults <- sequentialAnalysisResults[sequentialAnalysisResults$model != "Null",]
  if (!options[["includeEncompassingModel"]])
    sequentialAnalysisResults <- sequentialAnalysisResults[sequentialAnalysisResults$model != "Encompassing",]

  if (options[["sequentialAnalysisPlotType"]] == "bayesFactor") {

    # create plot container
    sequentialAnalysisPlot <- createJaspContainer(gettext("Sequential analysis"))
    sequentialAnalysisPlot$dependOn(c(.informedBinDependency, "bayesFactorType", "bfComparison", "bfVsHypothesis",
                                      "sequentialAnalysisPlot", "sequentialAnalysisPlotType", "sequentialAnalysisNumberOfSteps",
                                      "includeNullModel", "includeEncompassingModel"))
    sequentialAnalysisPlot$position <- 5
    jaspResults[["sequentialAnalysisPlot"]] <- sequentialAnalysisPlot

    # extract BF type and Bayes factor comparison
    bfTypeIgnoreLog <- if (options[["bayesFactorType"]] == "BF01") "BF01" else "BF10"
    bfComparison    <- .selectAvailableBfComparison(options, unique(sequentialAnalysisResults$model))

    for (hypothesis in unique(sequentialAnalysisResults$model)) {

      if (hypothesis == bfComparison)
        next

      # create sequential figures for individual hypotheses
      tempPlot <- createJaspPlot(title = gettext(paste0(hypothesis, " vs. ", bfComparison)), width = 480, height = 320)
      sequentialAnalysisPlot[[hypothesis]] <- tempPlot

      tempData <- data.frame(
        x = c(0, sequentialAnalysisResults$step[sequentialAnalysisResults$model == bfComparison]),
        y = c(0, sequentialAnalysisResults$marglik[sequentialAnalysisResults$model == hypothesis] -
                sequentialAnalysisResults$marglik[sequentialAnalysisResults$model == bfComparison])
      )
      if (bfTypeIgnoreLog == "BF01")
        tempData$y <- - tempData$y

      tempPlot$plotObject <- jaspGraphs::PlotRobustnessSequential(
        dfLines    = tempData,
        xName      = "n",
        BF         = exp(tempData$y[nrow(tempData)]),
        bfType     = bfTypeIgnoreLog,
        hypothesis = "equal")
    }

    return()

  } else if(options[["sequentialAnalysisPlotType"]] == "posteriorProbability") {

    # compute posterior probabilities
    priorProb <- options[["priorModelProbability"]][[1]][["valuesParsed"]][options[["priorModelProbability"]][[1]][["levels"]] %in% unique(sequentialAnalysisResults$model)]
    priorProb <- priorProb / sum(priorProb)
    postProb  <- do.call(rbind, lapply(unique(sequentialAnalysisResults$step), function(step) {

      logLik    <- sequentialAnalysisResults$marglik[sequentialAnalysisResults$step == step]
      postProb  <- bridgesampling::post_prob(logLik, prior_prob = priorProb)

      return(data.frame(
        model    = sequentialAnalysisResults$model[sequentialAnalysisResults$step == step],
        step     = step,
        postProb = postProb
      ))
    }))
    postProb  <- rbind(
      data.frame(
        model    = unique(sequentialAnalysisResults$model),
        step     = 0,
        postProb = priorProb
      ),
      postProb
    )

    # create plot
    tempPlot <- createJaspPlot(title = gettext("Sequential analysis"), width = 480, height = 320)
    tempPlot$dependOn(c(.informedMultDependency, "bayesFactorType", "bfComparison", "bfVsHypothesis",
                        "sequentialAnalysisPlot", "sequentialAnalysisPlotType", "priorModelProbability", "sequentialAnalysisNumberOfSteps",
                        "includeNullModel", "includeEncompassingModel"))
    tempPlot$position <- 5
    jaspResults[["sequentialAnalysisPlot"]] <- tempPlot
    tempPlot$plotObject <- .createInformedMultPlotSequentialProb(postProb)


    return()
  }
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
.informedExtractColumn     <- function(dataset, options, column) {
  if (attr(dataset, "individual"))
    return(attr(dataset, column))
  else
    return(dataset[[options[[column]]]])
}
.informedExtractSampleSize <- function(dataset, options) {
  .informedExtractColumn(dataset, options, "sampleSize")
}
.informedExtractSuccesses  <- function(dataset, options) {
  .informedExtractColumn(dataset, options, "successes")
}
.informedExtractCount      <- function(dataset, options) {
  .informedExtractColumn(dataset, options, "count")
}
