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

InformedMultinomialTestBayesianInternal <- function(jaspResults, dataset, options, ...) {

  dataset <- .multinomReadData(dataset, options)
  .multinomCheckErrors(dataset, options)
  dataset <- .multinomAggregateData(dataset, options)

  .computeInformedMultResults(jaspResults, dataset, options)
  .createInformedBayesMainTable(jaspResults, options, type = "multinomial")

  if (options[["descriptivesTable"]])
    .createInformedMultBayesDescriptivesTable(jaspResults, dataset, options)

  if (options[["descriptivesPlot"]])
    .createInformedMultBayesDescriptivesPlot(jaspResults, dataset, options)

  if (options[["posteriorPlot"]])
    .createInformedMultBayesPosteriorPlot(jaspResults, dataset, options)

  return()
}

.informedMultDependency <- c("factor", "count", "priorCounts", "models", "syntax",
                                    "bridgeSamples", "mcmcBurnin", "mcmcSamples", "setSeed", "seed")

.multinomAggregateData                    <- function(dataset, options){

  if(length(dataset[[options[["factor"]]]]) != length(levels(dataset[[options[["factor"]]]]))){

    frequencies <- table(dataset[[options[["factor"]]]])
    dataset     <- cbind.data.frame(factor(names(frequencies), levels = levels(dataset[[options[["factor"]]]])), as.numeric(frequencies))
    colnames(dataset) <- c(options[["factor"]], "__jaspComputedCounts")

  }

  return(dataset)
}
.computeInformedMultResults               <- function(jaspResults, dataset, options) {

  # skip if there is nothing new
  if (!is.null(jaspResults[["models"]]))
    return()

  # skip if the input is not specified
  if (options[["factor"]] == "" || (options[["count"]] == "" && is.null(dataset[["__jaspComputedCounts"]])))
    return()

  models <- createJaspState()
  models$dependOn(.informedMultDependency)
  jaspResults[["models"]] <- models

  if (length(options[["models"]]) > 0)
    startProgressbar(length(options[["models"]]))


  modelsList <- list()

  # fit an overall unrestricted model (for plotting the posterior)
  modelsList[[1]] <- list(
    "model" = try(multibridge::mult_bf_informed(
      x             = if(options[["count"]] != "") dataset[[options[["count"]]]] else dataset[["__jaspComputedCounts"]],
      Hr            = paste0(levels(dataset[[options[["factor"]]]]), collapse = ","),
      a             = options[["priorCounts"]][[1]][["values"]],
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
        "model" = try(multibridge::mult_bf_informed(
          x             = if(options[["count"]] != "") dataset[[options[["count"]]]] else dataset[["__jaspComputedCounts"]],
          Hr            = options[["models"]][[i]][["syntax"]],
          a             = options[["priorCounts"]][[1]][["values"]],
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
.createInformedBayesMainTable             <- function(jaspResults, options, type) {

  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  models <- jaspResults[["models"]]$object

  summaryTable <- createJaspTable(title = gettextf("Bayesian evaluation of %1$s order constraints", switch(
    type,
    multinomial = gettext("multinomial"),
    binomial    = gettext("binomial")
  )))
  summaryTable$position <- 1
  summaryTable$dependOn(c(.informedDependencies(type), "bayesFactorType", "bfComparison", "bfVsHypothesis"))

  if (options$bayesFactorType == "BF10")
    bfTitle <- gettextf("BF%s%s", "\u2081", "\u2080")
  else if (options$bayesFactorType == "BF01")
    bfTitle <- gettextf("BF%s%s", "\u2080", "\u2081")
  else
    bfTitle <- gettextf("Log(BF%s%s)", "\u2081", "\u2080")

  summaryTable$addColumnInfo(name = "model",         title = "",                      type = "string")
  summaryTable$addColumnInfo(name = "marglik",       title = gettext("Log marglik"),  type = "number")
  summaryTable$addColumnInfo(name = "marglikError",  title = gettext("Error"),        type = "number")
  summaryTable$addColumnInfo(name = "marglikPrec",   title = gettext("Error %"),      type = "number")
  summaryTable$addColumnInfo(name = "bf",            title = bfTitle,                 type = "number")

  jaspResults[["summaryTable"]] <- summaryTable

  if (is.null(models))
    return()
  else if (any(unlist(lapply(models, jaspBase::isTryError)))) {
    errors <- models[unlist(lapply(models, jaspBase::isTryError))]
    summaryTable$setError(paste0("Error in ", errors[[1]][["name"]], ": ", errors[[1]][["model"]]))
    return()
  }

  # extract marginal likelihoods
  rowsList <- list()
  for (i in seq_along(models)) {

    if (is.null(models[[i]][["model"]]))
      next

    # extract marginal likelihood for the null and encompassing models from the first fit object
    # (and check that they match on all subsequent ones)
    if (i == 1) {
      rowsList[[1]] <- data.frame(
        model         = "Null",
        marglik       = models[[i]]$model$logml[["logmlH0"]],
        marglikError  = NA,
        marglikPrec   = NA
      )
      rowsList[[2]] <- data.frame(
        model         = "Encompassing",
        marglik       = models[[i]]$model$logml[["logmlHe"]],
        marglikError  = NA,
        marglikPrec   = NA
      )
    } else if (!all.equal(rowsList[[1]][["marglik"]], models[[i]]$model$logml[["logmlH0"]]) ||
               !all.equal(rowsList[[2]][["marglik"]], models[[i]]$model$logml[["logmlHe"]])) {
      stop("Marginal likelihoods of different models do not match.")
    } else {
      # add the alternative hypotheses
      rowsList[[i + 2]] <- data.frame(
        model        = models[[i]][["name"]],
        marglik      = models[[i]]$model$logml[["logmlHr"]],
        marglikError = if(length(models[[i]]$model$bridge_output) == 0) NA else models[[i]]$model$bridge_output[[1]]$post$error_measures$re2,
        marglikPrec  = if(length(models[[i]]$model$bridge_output) == 0) NA else as.numeric(gsub("%", "", models[[i]]$model$bridge_output[[1]]$post$error_measures$percentage, fixed = TRUE))
      )
    }
  }

  rowsFrame <- do.call(rbind, rowsList)

  # extract the Bayes factor comparison
  if (options[["bfComparison"]]== "encompassing")
    bfComparison <- "Encompassing"
  else if (options[["bfComparison"]]== "null")
    bfComparison <- "Null"
  else if (options[["bfVsHypothesis"]] %in% rowsFrame$model)
    bfComparison <- options[["bfVsHypothesis"]]
  else
    bfComparison <- "Encompassing"

  # compute Bayes factors
  rowsFrame$bf <- exp(rowsFrame$marglik - rowsFrame$marglik[rowsFrame$model == bfComparison])
  rowsFrame$bf <- .recodeBFtype(rowsFrame$bf, options[["bayesFactorType"]])

  summaryTable$setData(rowsFrame)
  summaryTable$addFootnote(gettextf("Model in each row (denoted as '1') is compared to the %1$s (denoted as 0).", bfComparison))
  if (.chechIfAllRestrictedModelsNull(options))
    summaryTable$addFootnote(gettext("Specify informed hypothesis tests in the `Order Restricted Hypotheses` section."))

  return()
}
.createInformedMultBayesDescriptivesTable <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["descriptivesTable"]]))
    return()

  # Create Table
  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  descriptivesTable$position <- 2
  descriptivesTable$dependOn(c("factor", "descriptivesDisplay", "descriptivesTable", "count"))

  if (options[["descriptivesDisplay"]] == "counts") {
    outType <- "integer"
    outText <- gettext("Observed counts")
  } else {
    outType <- "number"
    outText <- gettext("Observed proportions")
  }

  descriptivesTable$addColumnInfo(name = "fact",     title = options[["factor"]], type = "string")
  descriptivesTable$addColumnInfo(name = "observed", title = outText,             type = outType)

  jaspResults[["descriptivesTable"]] <- descriptivesTable


  # show empty Table if no variable is selected
  if (is.null(jaspResults[["models"]]) || jaspBase::isTryError(jaspResults[["models"]]$object[[1]]$model))
    return()

  tempTable <- .createInformedMultDescriptivesData(jaspResults, options)

  descriptivesTable$setData(tempTable)

  return()
}
.createInformedMultBayesDescriptivesPlot  <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["descriptivesPlot"]]))
    return()

  # Create Plot
  descriptivesPlot <- createJaspPlot(title = gettext("Descriptives plot"), width = 480, height = 320)
  descriptivesPlot$position <- 3
  descriptivesPlot$dependOn(c("factor", "descriptivesDisplay", "descriptivesPlot", "count"))
  jaspResults[["descriptivesPlot"]] <- descriptivesPlot

  # show empty plot if no variable is selected
  if (is.null(jaspResults[["models"]]) || jaspBase::isTryError(jaspResults[["models"]]$object[[1]]$model))
    return()

  plotData <- .createInformedMultDescriptivesData(jaspResults, options)

  descriptivesPlot$plotObject <- .createInformedMultPlot(plotData, options, descriptives = TRUE)

  return()
}
.createInformedMultBayesPosteriorPlot     <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["posteriorPlot"]]))
    return()

  posteriorPlot <- createJaspPlot(title = gettext("Unrestricted posterior plot"), width = 480, height = 320)
  posteriorPlot$position <- 4
  posteriorPlot$dependOn(c(.informedMultDependency,  "display", "posteriorPlot", "posteriorPlotCiCoverage"))
  jaspResults[["posteriorPlot"]] <- posteriorPlot

  if (is.null(jaspResults[["models"]]) || jaspBase::isTryError(jaspResults[["models"]]$object[[1]]$model))
    return()

  # extract posterior summary from the unrestricted model and format it for the plotting function
  tempModel             <- jaspResults[["models"]]$object[[1]]$model
  tempModel$cred_level  <- options[["posteriorPlotCiCoverage"]]
  tempSummary           <- summary(tempModel)[["estimates"]][,c("factor_level", "median", "lower", "upper")]
  colnames(tempSummary) <- c("fact", "observed", "lowerCI", "upperCI")


  posteriorPlot$plotObject <- .createInformedMultPlot(tempSummary, options, descriptives = FALSE)

  return()
}
.createInformedMultBayesPosteriorPlots    <- function(jaspResults, dataset, options) {
  # currently not used -- might be added later
  if (!is.null(jaspResults[["posteriorPlots"]]))
    return()

  posteriorPlots <- createJaspContainer("Posterior plots")
  posteriorPlots$dependOn(c(.informedMultDependency,  "display", "posteriorPlot", "posteriorPlotCiCoverage"))
  posteriorPlots$position <- 4
  jaspResults[["posteriorPlots"]] <- posteriorPlots

  models <- jaspResults[["models"]]$object

  if (is.null(models)) {
    tempPlot <- createJaspPlot(title = "", width = 480, height = 320)
    posteriorPlots[["waitingPlot"]] <- tempPlot
    return()
  }

  for (i in seq_along(models)) {

    if (is.null(models[[i]][["model"]]))
      next

    # extract posterior summary and format it for the plotting function
    tempModel             <- models[[i]]$model
    tempModel$cred_level  <- options[["posteriorPlotCiCoverage"]]
    tempSummary           <- summary(tempModel)[["estimates"]][,c("factor_level", "median", "lower", "upper")]
    colnames(tempSummary) <- c("fact", "observed", "lowerCI", "upperCI")

    if (options[["display"]] == "counts")
      tempSummary[,2:4] <- tempSummary[,2:4] * sum(if(options[["count"]] != "") dataset[[options[["count"]]]] else dataset[["__jaspComputedCounts"]])

    tempPlot <- createJaspPlot(title = models[[i]]$name, width = 480, height = 320)
    tempPlot$position <- i
    posteriorPlots[[models[[i]]$name]] <- tempPlot

    tempPlot$plotObject <- .informedPlot(tempSummary, options, descriptives = FALSE)
  }

  return()
}
.createInformedMultBayesDescriptivesData  <- function(dataset, options, table = TRUE) {

  counts <- if(options[["count"]] != "") dataset[[options[["count"]]]] else dataset[["__jaspComputedCounts"]]

  # Compute CI
  if (table && options[["descriptivesTableCi"]])
    tempCI <- .multComputeCIs(counts, options[["descriptivesTableCiCoverage"]], ifErrorReturn = 0, scale = .decodeOptionsDisplay(options))
  else if (!table)
    tempCI <- .multComputeCIs(counts, options[["posteriorPlotCiCoverage"]], ifErrorReturn = 0, scale = .decodeOptionsDisplay(options))
  else
    tempCI <- NULL

  if (options[["display"]] == "counts")
    stdConst <- 1
  else
    stdConst <- sum(counts)

  rowsList <- list()

  # Add rows
  for (i in 1:nrow(dataset)) {

    tempRow <- list(fact = dataset[i,options[["factor"]]])

    # skip if the input is not specified
    if (!(options[["count"]] != "" && is.null(dataset[["__jaspComputedCounts"]]))) {
      tempRow[["observed"]] <- counts[i] / stdConst
      if (!is.null(tempCI)) {
        tempRow[["lowerCI"]] <- tempCI[i,"lowerCI"]
        tempRow[["upperCI"]] <- tempCI[i,"upperCI"]
      }
    }

    rowsList[[i]] <- as.data.frame(tempRow)
  }

  rowsFrame <- do.call(rbind, rowsList)
  return(rowsFrame)
}
.createInformedMultDescriptivesData       <- function(jaspResults, options) {

  model <- jaspResults[["models"]]$object[[1]]$model

  tempTable <- data.frame(
    "fact"     = model[["restrictions"]][["full_model"]][["parameters_full"]],
    "observed" = model[["restrictions"]][["full_model"]][["counts_full"]]
  )

  if (options[["descriptivesDisplay"]] == "proportions")
    tempTable[["observed"]] <- tempTable[["observed"]] / sum(tempTable[["observed"]])

  return(tempTable)
}
.createInformedMultPlot                   <- function(plotData, options, descriptives = TRUE) {

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
  if (!is.null(plotFrame$upperCI))
    plotFrame$yAxisMargin <-  plotFrame$upperCI
  else
    plotFrame$yAxisMargin <-  plotFrame$observed


  # Create plot
  p <- ggplot2::ggplot(data = plotFrame,
                       mapping = ggplot2::aes(x = fact, y = observed)) +
    ggplot2::geom_bar(stat = "identity", size = 0.75, colour="black",
                      fill = "grey")

  if (!descriptives)
    p <-  p + ggplot2::geom_errorbar(ggplot2::aes(ymin = plotFrame[["lowerCI"]],
                                                  ymax = plotFrame[["upperCI"]]),
                                     size = 0.75, width = 0.3)

  p <- p + base_breaks_y(plotFrame$yAxisMargin) +
    ggplot2::xlab(options[["factor"]]) +
    ggplot2::ylab(yname) +
    ggplot2::coord_flip()

  p <- p + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw()

  return(p)
}
.chechIfAllRestrictedModelsNull           <- function(options) {
  return(all(unlist(lapply(options[["models"]], function(x) nchar(trimws(x[["syntax"]], "both")) == 0))))
}
.decodeOptionsDisplay                     <- function(options) {
  switch(
    options[["display"]],
    "counts"      = "descCounts",
    "proportions" = "descProp"
  )
}
.informedDependencies                     <- function(type) {
  return(switch(
    type,
    "binomial"    = .informedBinDependency,
    "multinomial" = .informedMultDependency
  ))
}
