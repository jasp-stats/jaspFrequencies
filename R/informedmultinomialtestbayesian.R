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
  options <- .informedBayesParsePriorModelProbability(options)
  dataset <- .multinomAggregateData(dataset, options)

  .computeInformedMultResults(jaspResults, dataset, options)
  .createInformedBayesMainTable(jaspResults, options, type = "multinomial")

  if (options[["descriptivesTable"]])
    .createInformedMultBayesDescriptivesTable(jaspResults, dataset, options)

  if (options[["descriptivesPlot"]])
    .createInformedMultBayesDescriptivesPlot(jaspResults, dataset, options)

  if (options[["posteriorPlot"]])
    .createInformedMultBayesPosteriorPlot(jaspResults, dataset, options)

  if (options[["sequentialAnalysisPlot"]])
    .createInformedMultSequentialAnalysisPlot(jaspResults, dataset, options)

  return()
}

.informedMultDependency <- c("factor", "count", "priorCounts", "models", "syntax",
                                    "bridgeSamples", "mcmcBurnin", "mcmcSamples", "setSeed", "seed")

.informedBayesParsePriorModelProbability  <- function(options) {

  # prepare output holder
  options[["priorModelProbability"]][[1]][["valuesParsed"]] <- rep(NA, length(options[["priorModelProbability"]][[1]][["levels"]]))

  # parse the settings
  for(i in seq_along(options[["priorModelProbability"]][[1]][["levels"]])){

    tempVal <- eval(parse(text = options[["priorModelProbability"]][[1]][["values"]][i]))

    # check the input is valid
    if(is.numeric(tempVal) && length(tempVal) == 1 && tempVal > 0)
      options[["priorModelProbability"]][[1]][["valuesParsed"]][i] <- tempVal
    else
      .quitAnalysis(gettextf(
        "Prior model probability input for the '%1$s' hypothesis (%2$s) could not be parsed into a positive number. Please, check the input.",
        options[["priorModelProbability"]][[1]][["levels"]][i],
        options[["priorModelProbability"]][[1]][["values"]][i]
      ))
  }
  return(options)
}
.informedBayesNumberOfModels              <- function(jaspResults, options) {

  models <- jaspResults[["models"]]$object

  if(length(models) == 1){
    return(options[["includeNullModel"]] + options[["includeEncompassingModel"]])
  }else{
    return(options[["includeNullModel"]] + options[["includeEncompassingModel"]] + sum(sapply(2:length(models), function(m) !is.null(models[[m]][["model"]]))))
  }
}
.multinomAggregateData                    <- function(dataset, options) {

  if (length(dataset[[options[["factor"]]]]) != length(levels(dataset[[options[["factor"]]]]))) {

    individualData    <- dataset[[options[["factor"]]]]
    frequencies       <- table(individualData)
    dataset           <- data.frame(factor(names(frequencies), levels = levels(dataset[[options[["factor"]]]])))
    colnames(dataset) <- options[["factor"]]

    attr(dataset, "individual")     <- TRUE
    attr(dataset, "count")          <- as.numeric(frequencies)
    attr(dataset, "individualData") <- individualData

  } else {
    attr(dataset, "individual") <- FALSE
  }

  return(dataset)
}
.computeInformedMultResults               <- function(jaspResults, dataset, options) {

  # skip if there is nothing new
  if (!is.null(jaspResults[["models"]]))
    return()

  # skip if the input is not specified
  if (options[["factor"]] == "" || (options[["count"]] == "" && is.null(attr(dataset, "count"))))
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
      x             = .informedExtractCount(dataset, options),
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
          x             = .informedExtractCount(dataset, options),
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
.computeInformedMultSequentialResults     <- function(jaspResults, dataset, options) {

  # skip if there is nothing new
  if (!is.null(jaspResults[["sequentialAnalysisResults"]]))
    return()

  # skip if the input is not specified
  if (options[["factor"]] == "" || (options[["count"]] == "" && is.null(attr(dataset, "count"))))
    return()

  # skip if the data are only aggregated
  if (!attr(dataset, "individual"))
    return()


  sequential <- createJaspState()
  sequential$dependOn(c(.informedMultDependency, "sequentialAnalysisNumberOfSteps"))
  jaspResults[["sequentialAnalysisResults"]] <- sequential

  individualData <- attr(dataset, "individualData")
  levelsData     <- levels(individualData)

  # specify sequential steps (do all if 0)
  if(options[["sequentialAnalysisNumberOfSteps"]] == 0)
    steps <- 1:length(individualData)
  else
    steps <- unique(round(c(seq.int(0, length(individualData), length.out = options[["sequentialAnalysisNumberOfSteps"]]), length(individualData))))[-1]

  startProgressbar(length(steps), label = gettext("Performing sequential analysis."))

  out <- list()
  for (step in steps){

    tempOutput <- list()

    ### prepare data
    frequencies <- table(individualData[1:step])
    seqDataset  <- cbind.data.frame(factor(names(frequencies), levels = levels(dataset[[options[["factor"]]]])), as.numeric(frequencies))
    colnames(seqDataset) <- c(options[["factor"]], "count")

    ### fit models & extract margliks
    # fit an overall unrestricted model (for plotting the posterior)
    model0 <- try(multibridge::mult_bf_informed(
        x             = seqDataset[["count"]],
        Hr            = paste0(levels(dataset[[options[["factor"]]]]), collapse = ","),
        a             = options[["priorCounts"]][[1]][["values"]],
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

        model1 <- try(multibridge::mult_bf_informed(
            x             = seqDataset[["count"]],
            Hr            = options[["models"]][[i]][["syntax"]],
            a             = options[["priorCounts"]][[1]][["values"]],
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
  summaryTable$dependOn(c(.informedDependencies(type), "bayesFactorType", "bfComparison", "bfVsHypothesis", "priorModelProbability", "includeNullModel", "includeEncompassingModel"))

  if (options$bayesFactorType == "BF10")
    bfTitle <- gettextf("BF%1$s%2$s", "\u2081", "\u2080")
  else if (options$bayesFactorType == "BF01")
    bfTitle <- gettextf("BF%1$s%2$s", "\u2080", "\u2081")
  else
    bfTitle <- gettextf("Log(BF%1$s%2$s)", "\u2081", "\u2080")

  summaryTable$addColumnInfo(name = "model",         title = "",                        type = "string")
  summaryTable$addColumnInfo(name = "marglik",       title = gettext("Log marglik"),    type = "number")
  summaryTable$addColumnInfo(name = "marglikError",  title = gettext("Error"),          type = "number")
  summaryTable$addColumnInfo(name = "marglikPrec",   title = gettextf("Error %%"),      type = "number")
  summaryTable$addColumnInfo(name = "priorProb",     title = gettext("P(M)"),           type = "number")
  summaryTable$addColumnInfo(name = "postProb",      title = gettext("P(M|Data)"),      type = "number")
  summaryTable$addColumnInfo(name = "bfInclusion",   title = gettext("BF<sub>M</sub>"), type = "number")
  summaryTable$addColumnInfo(name = "bf",            title = bfTitle,                   type = "number")

  jaspResults[["summaryTable"]] <- summaryTable

  if (is.null(models) || .informedBayesNumberOfModels(jaspResults, options) == 0)
    return()
  else if (any(unlist(lapply(models, jaspBase::isTryError)))) {
    errors <- models[unlist(lapply(models, jaspBase::isTryError))]

    if(any(grepl("checkFactorLevelsInOR", sapply(errors, function(e) e[["model"]]))))
      summaryTable$setError(gettext("Please double check if the factor level names in the 'Order Restricted Hypotheses' section match the factor levels in your data set."))
    else
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
      if (options[["includeNullModel"]])
        rowsList[[length(rowsList) + 1]] <- data.frame(
          model         = "Null",
          marglik       = models[[i]]$model$logml[["logmlH0"]],
          marglikError  = NA,
          marglikPrec   = NA,
          priorProb     = options[["priorModelProbability"]][[1]][["valuesParsed"]][options[["priorModelProbability"]][[1]][["levels"]] == "Null"]
        )
      if (options[["includeEncompassingModel"]])
        rowsList[[length(rowsList) + 1]] <- data.frame(
          model         = "Encompassing",
          marglik       = models[[i]]$model$logml[["logmlHe"]],
          marglikError  = NA,
          marglikPrec   = NA,
          priorProb     = options[["priorModelProbability"]][[1]][["valuesParsed"]][options[["priorModelProbability"]][[1]][["levels"]] == "Encompassing"]
        )
    } else {
      # add the alternative hypotheses
      rowsList[[length(rowsList) + 1]] <- data.frame(
        model        = models[[i]][["name"]],
        marglik      = models[[i]]$model$logml[["logmlHr"]],
        marglikError = if(length(models[[i]]$model$bridge_output) == 0) NA else models[[i]]$model$bridge_output[[1]]$post$error_measures$re2,
        marglikPrec  = if(length(models[[i]]$model$bridge_output) == 0) NA else as.numeric(gsub("%", "", models[[i]]$model$bridge_output[[1]]$post$error_measures$percentage, fixed = TRUE)),
        priorProb    = options[["priorModelProbability"]][[1]][["valuesParsed"]][options[["priorModelProbability"]][[1]][["levels"]] == models[[i]][["name"]]]
      )
    }
  }

  rowsFrame <- do.call(rbind, rowsList)

  # compute posterior probabilities
  rowsFrame$priorProb <- rowsFrame$priorProb / sum(rowsFrame$priorProb)
  rowsFrame$postProb  <- bridgesampling::post_prob(rowsFrame$marglik, prior_prob = rowsFrame$priorProb)

  # extract the Bayes factor comparison (select comparison that's specified AND not ommitted)
  bfComparison    <- .selectAvailableBfComparison(options, rowsFrame$model)

  # compute Bayes factors
  rowsFrame$bfInclusion <- (rowsFrame$postProb / (1-rowsFrame$postProb)) / (rowsFrame$priorProb / (1 - rowsFrame$priorProb))
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

  if (length(jaspResults[["models"]]) == 0 || jaspBase::isTryError(jaspResults[["models"]]$object[[1]]$model))
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
      tempSummary[,2:4] <- tempSummary[,2:4] * sum(.informedExtractCount(dataset, options))

    tempPlot <- createJaspPlot(title = models[[i]]$name, width = 480, height = 320)
    tempPlot$position <- i
    posteriorPlots[[models[[i]]$name]] <- tempPlot

    tempPlot$plotObject <- .informedPlot(tempSummary, options, descriptives = FALSE)
  }

  return()
}
.createInformedMultSequentialAnalysisPlot <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["sequentialAnalysisPlot"]]))
    return()


  # create/obtain sequential analysis
  .computeInformedMultSequentialResults(jaspResults, dataset, options)
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
    sequentialAnalysisPlot$dependOn(c(.informedMultDependency, "bayesFactorType", "bfComparison", "bfVsHypothesis",
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
.createInformedMultBayesDescriptivesData  <- function(dataset, options, table = TRUE) {

  counts <- .informedExtractCount(dataset, options)

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
    if (!(options[["count"]] != "" && is.null(attr(dataset, "count")))) {
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
.createInformedMultPlotSequentialProb     <- function(plotData) {

  ggplot2::ggplot(data = plotData, mapping = ggplot2::aes(x = step, y = postProb, color = model)) +
    jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous("Observation", limits = range(jaspGraphs::getPrettyAxisBreaks(range(plotData$step))), breaks = jaspGraphs::getPrettyAxisBreaks(range(plotData$step))) +
    ggplot2::scale_y_continuous("Posterior probability", limits = c(0, 1),  breaks = jaspGraphs::getPrettyAxisBreaks(c(0,1))) +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Model")) +
    jaspGraphs::geom_rangeframe() +
    jaspGraphs::themeJaspRaw(legend.position = "right")

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
.selectAvailableBfComparison              <- function(options, models){

  # extract the Bayes factor comparison (select comparison that's specified AND not ommitted)
  if (options[["bfComparison"]] == "Encompassing" && "Encompassing" %in% models)
    bfComparison <- "Encompassing"
  else if (options[["bfComparison"]] == "Null" && "Null" %in% models)
    bfComparison <- "Null"
  else if (options[["bfVsHypothesis"]] %in% models)
    bfComparison <- options[["bfVsHypothesis"]]
  else
    bfComparison <- models[1]

  return(bfComparison)
}
