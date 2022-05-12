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

# TODO:
# - dynamically fill comparison dropdown
# - how to deal with restricted hypotheses with different number of elements?
# - scale proportions for posterior when restricted?
# - bug with empty model
# - x-axis label for posterior: change to estimated
# - fit the null and encompassing models separately (there should be a function for that)

InformedMultinomialTestBayesian <- function(jaspResults, dataset, options, ...) {

  dataset            <- .multinomReadData(dataset, options)

  .multinomCheckErrors(dataset, options)

  saveRDS(dataset, file = "C:/JASP/jaspFrequencies/dataset.RDS")
  saveRDS(options, file = "C:/JASP/jaspFrequencies/options.RDS")

  .computeInformedMultinomialResults(jaspResults, dataset, options)
  .createInformedMultBayesMainTable(jaspResults, options)

  if (options[["descriptives"]])
    .createInformedMultBayesDescriptivesTable(jaspResults, dataset, options)

  if (options[["descriptivesPlot"]])
    .createInformedMultBayesDescriptivesPlot(jaspResults, dataset, options)

  if (options[["posteriorPlot"]])
    .createInformedMultBayesPosteriorPlot(jaspResults, dataset, options)

  return()
}

.informedMultinomialDependency <- c("factor", "counts", "priorCounts", "restrictedModels", "restrictionSyntax",
                                    "bridgeIter", "mcmcBurnin", "mcmcIter", "setSeed", "seed")

.computeInformedMultinomialResults        <- function(jaspResults, dataset, options){

  # skip if there is nothing new
  if (!is.null(jaspResults[["models"]]))
    return()

  # skip if all models are empty
  if (all(unlist(lapply(options[["restrictedModels"]], function(x) nchar(x[["restrictionSyntax"]]) == 0))))
    return()

  # skip if the input is not specified
  if (options[["factor"]] == "" || options[["counts"]] == "")
    return()

  models <- createJaspState()
  models$dependOn(.informedMultinomialDependency)
  jaspResults[["models"]] <- models

  if(length(options[["restrictedModels"]]) > 0)
    startProgressbar(length(options[["restrictedModels"]]))


  modelsList <- list()

  for(i in seq_along(options[["restrictedModels"]])) {

    if (nchar(options[["restrictedModels"]][[i]][["restrictionSyntax"]]) == 0) {

      modelsList[[i]] <- list(
        "model" = NULL,
        "name"  = options[["restrictedModels"]][[i]][["modelName"]]
      )

    } else {

      tempFit <- try(multibridge::mult_bf_informed(
        x             = dataset[,options[["counts"]]],
        Hr            = options[["restrictedModels"]][[i]][["restrictionSyntax"]],
        a             = options[["priorCounts"]][[1]][["values"]],
        factor_levels = dataset[,options[["factor"]]],
        bf_type       = "BF0r",
        nburnin       = options[["mcmcBurnin"]],
        niter         = options[["mcmcBurnin"]] + options[["mcmcIter"]],
        maxiter       = options[["bridgeIter"]],
        seed          = if (options[["setSeed"]]) .getSeedJASP(options) else sample.int(.Machine$integer.max, 1),
      ))


      modelsList[[i]] <- list(
        "model" = tempFit,
        "name"  = options[["restrictedModels"]][[i]][["modelName"]]
      )

    }

    progressbarTick()
  }

  models$object <- modelsList



  saveRDS(modelsList, file = "C:/JASP/jaspFrequencies/models.RDS")

  return()
}
.createInformedMultBayesMainTable         <- function(jaspResults, options){


  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  models <- jaspResults[["models"]]$object

  summaryTable <- createJaspTable(title = gettext("Bayesian evaluation of multinomial order constraints "))
  summaryTable$position <- 1
  summaryTable$dependOn(c(.informedMultinomialDependency, "bayesFactorType", "testAgainst"))

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

    if (i == 1) {
      rowsList[[1]] <- data.frame(
        model         = "Null",
        marglik       = models[[i]]$model$bridge_output[[1]]$post$logml - models[[i]]$model$bf_list$bf[1, "LogBFr0"],
        marglikError  = NA,
        marglikPrec   = NA
      )
      rowsList[[2]] <- data.frame(
        model         = "Encompasing",
        marglik       = models[[i]]$model$bridge_output[[1]]$post$logml + models[[i]]$model$bf_list$bfr_table[1, "LogBFer"],
        marglikError  = NA,
        marglikPrec   = NA
      )

    }

    rowsList[[i + 2]] <- data.frame(
      model        = models[[i]][["name"]],
      marglik      = models[[i]]$model$bridge_output[[1]]$post$logml,
      marglikError = models[[i]]$model$bridge_output[[1]]$post$error_measures$re2,
      marglikPrec  = as.numeric(gsub("%", "", models[[i]]$model$bridge_output[[1]]$post$error_measures$percentage, fixed = TRUE))
    )
  }

  rowsFrame <- do.call(rbind, rowsList)

  # compute Bayes factors
  rowsFrame$bf <- exp(rowsFrame$marglik[rowsFrame$model == options[["testAgainst"]]] - rowsFrame$marglik)
  rowsFrame$bf <- .recodeBFtype(rowsFrame$bf, options[["bayesFactorType"]])

  summaryTable$setData(rowsFrame)
  summaryTable$addFootnote(gettextf(
    "Model in each row (denoted as '1') is compared to the %1$s (denoted as 0).",
    if (options[["testAgainst"]] %in% c("Encompasing", "Null")) paste0(options[["testAgainst"]], " model") else options[["testAgainst"]]
  ))

  return()
}
.createInformedMultBayesDescriptivesTable <- function(jaspResults, dataset, options){

  if(!is.null(jaspResults[["descriptivesTable"]]))
    return()

  # Create Table
  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  descriptivesTable$position <- 2
  descriptivesTable$dependOn(c("factor", "counts", "countProp", "descriptives", "credibleInterval", "credibleIntervalInterval"))

  if (options[["countProp"]] == "descCounts")
    outType <- "integer"
  else
    outType <- "number"

  descriptivesTable$addColumnInfo(name = "fact",     title = options[["factor"]], type = "string")
  descriptivesTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = outType)

  if (options[["credibleInterval"]]) {
    overTitle <- gettextf("%1$s%% Credible Interval", paste0(100 * options[["credibleIntervalInterval"]]))
    descriptivesTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "number", overtitle = overTitle)
    descriptivesTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "number", overtitle = overTitle)
  }

  jaspResults[["descriptivesTable"]] <- descriptivesTable

  # Show empty Table if no variable is selected
  if(options[["factor"]] == "")
    return()

  # compute and fill the table
  descriptivesData <- .createInformedMultBayesDescriptivesData(dataset, options)
  descriptivesTable$setData(descriptivesData)

  if (options[["counts"]] != "" && options[["credibleInterval"]]){
    descriptivesTable$addFootnote(gettext("Credible intervals are based on independent binomial distributions with flat priors."))
  }

  return()
}
.createInformedMultBayesDescriptivesPlot  <- function(jaspResults, dataset, options){

  if(!is.null(jaspResults[["descriptivesPlot"]]))
    return()

  # Create Plot
  descriptivesPlot <- createJaspPlot(title = gettext("Descriptives plot"), width = 480, height = 320)
  descriptivesPlot$position <- 3
  descriptivesPlot$dependOn(c(.informedMultinomialDependency, "countProp", "descriptivesPlot", "credibleIntervalPlot"))
  jaspResults[["descriptivesPlot"]] <- descriptivesPlot

  # Show empty Plot if no variable is selected
  if(options[["factor"]] == "" && options[["counts"]] == "")
    return()

  plotData <- .createInformedMultBayesDescriptivesData(dataset, options, table = FALSE)
  descriptivesPlot$plotObject <- .informedMultinomialPlot(plotData, options, descriptives = TRUE)

  return()
}
.createInformedMultBayesPosteriorPlot     <- function(jaspResults, dataset, options){

  if(!is.null(jaspResults[["posteriorPlots"]]))
    return()

  posteriorPlots <- createJaspContainer("Posterior plots")
  posteriorPlots$dependOn(c(.informedMultinomialDependency,  "countProp", "posteriorPlot", "credibleIntervalPlot"))
  posteriorPlots$position <- 4
  jaspResults[["posteriorPlots"]] <- posteriorPlots

  models <- jaspResults[["models"]]$object

  if (is.null(models)) {
    tempPlot <- createJaspPlot(title = "", width = 480, height = 320)
    posteriorPlots[["waitingPlot"]] <- tempPlot
    return()
  }

  for (i in seq_along(models)) {

    # extract posterior summary and format it for the plotting function
    tempModel             <- models[[i]]$model
    tempModel$cred_level  <- options[["credibleIntervalPlot"]]
    tempSummary           <- summary(tempModel)[["estimates"]][,c("factor_level", "median", "lower", "upper")]
    colnames(tempSummary) <- c("fact", "observed", "lowerCI", "upperCI")

    if (options[["countProp"]] == "descCounts")
      tempSummary[,2:4] <- tempSummary[,2:4] * sum(dataset[,options[["counts"]]])

    tempPlot <- createJaspPlot(title = models[[i]]$name, width = 480, height = 320)
    tempPlot$position <- i
    posteriorPlots[[models[[i]]$name]] <- tempPlot

    tempPlot$plotObject <- .informedMultinomialPlot(tempSummary, options, descriptives = FALSE)
  }

  return()
}
.createInformedMultBayesDescriptivesData  <- function(dataset, options, table = TRUE){

  # Compute CI
  if (table && options[["counts"]] != "" && options[["credibleInterval"]])
    tempCI <- .multComputeCIs(dataset[,options[["counts"]]], options[["credibleIntervalInterval"]], ifErrorReturn = 0, scale = options[["countProp"]])
  else if (!table && options[["counts"]] != "")
    tempCI <- .multComputeCIs(dataset[,options[["counts"]]], options[["credibleIntervalPlot"]], ifErrorReturn = 0, scale = options[["countProp"]])
  else
    tempCI <- NULL

  if (options[["countProp"]] == "descCounts")
    stdConst <- 1
  else
    stdConst <- sum(dataset[,options[["counts"]]])

  rowsList <- list()

  # Add rows
  for (i in 1:nrow(dataset)){

    tempRow <- list(fact = dataset[i,options[["factor"]]])

    # skip if the input is not specified
    if (options[["counts"]] != "") {
      tempRow[["observed"]] <- dataset[i,options[["counts"]]] / stdConst
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
.informedMultinomialPlot                  <- function(plotData, options, descriptives = TRUE){

  base_breaks_y <- function(x) {
    b <- pretty(c(0,x))
    d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
    list(ggplot2::geom_segment(data=d, ggplot2::aes(x=x, y=y, xend=xend, yend=yend), size = 0.75, inherit.aes=FALSE),
         ggplot2::scale_y_continuous(breaks=b))
  }

  # Counts or props
  yname <- sprintf("%1$s %2$s",
    if (descriptives) gettext("Observed") else gettext("Estimated"),
    if (options[["countProp"]] == "descCounts") gettext("Counts") else gettext("Proportions")
  )

  # Prepare data for plotting
  plotFrame <- plotData
  # We need to reverse the factor's levels because of the coord_flip later
  plotFrame$fact <- factor(plotFrame$fact, levels = rev(plotFrame$fact))

  # Determine y-axis margin: If CIs could not be computed, use observed counts
  plotFrame$yAxisMargin <- plotFrame$upperCI
  for(i in 1:nrow(plotFrame)){
    if(plotFrame$upperCI[i] == 0){
      plotFrame$yAxisMargin[i] <- plotFrame$obs[i]
    }
  }

  # Create plot
  p <- ggplot2::ggplot(data = plotFrame,
                       mapping = ggplot2::aes(x = fact, y = observed)) +
    ggplot2::geom_bar(stat = "identity", size = 0.75, colour="black",
                      fill = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = plotFrame[["lowerCI"]],
                                        ymax = plotFrame[["upperCI"]]),
                           size = 0.75, width = 0.3) +
    base_breaks_y(plotFrame$yAxisMargin) +
    ggplot2::xlab(options[["factor"]]) +
    ggplot2::ylab(yname) +
    ggplot2::coord_flip()

  p <- p + jaspGraphs::geom_rangeframe(sides = "b") + jaspGraphs::themeJaspRaw()

  return(p)
}
