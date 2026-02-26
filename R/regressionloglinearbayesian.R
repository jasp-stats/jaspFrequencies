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

RegressionLogLinearBayesianInternal <- function(jaspResults, dataset = NULL, options, ...) {
  ready <- length(options$factors) > 1
  if(ready){
    dataset <- .basRegLogLinReadData(dataset, options)
    .basRegLogLinCheckErrors(dataset, options)
  }
  # Container
  .basRegLogLinContainer(jaspResults, dataset, options)
  container <- jaspResults[["Container"]]
  # Output tables (each calls its own results function)
  .basRegLogLinMainTable(      container, dataset, options, ready)
  .basRegLogLinSummaryTable(   container, dataset, options, ready)
  .basRegLogLinSubSummaryTable(container, dataset, options, ready)

  return()
}

# Preprocessing functions
.basRegLogLinReadData <- function(dataset, options) {
  if (is.null(dataset)) {
    counts <- factors <- NULL
    if(options$count != "")
      counts <- options$count
    if(length(options$modelTerms) > 0)
      factors <- options$factors
    dataset <- .readDataSetToEnd(columns.as.factor = factors,
                                 columns.as.numeric = counts)

    # conting uses the last level as the reference level,
    # but elsewhere we use the first level instead.
    # So, we shift the first level to be the last level to keep the output consistent
    for (fac in factors) {
      var <- dataset[[fac]]
      if (nlevels(var) > 1) {
        lev <- levels(var)
        dataset[[fac]] <- factor(var, levels = c(lev[-1], lev[1]))
      }
    }
  }

  return(dataset)
}

.basRegLogLinCheckErrors <- function(dataset, options) {

  if (options$count != "") {
    .hasErrors(
      dataset = dataset,
      type    = c("missingValues", "modelInteractions", "factorLevels", "infinity", "negativeValues"),
      infinity.target     = options$count,
      negativeValues.target = options$count,
      factorLevels.target = options$factors,
      factorLevels.amount = " < 2",
      modelInteractions.modelTerms = options$modelTerms,
      exitAnalysisIfErrors = TRUE
    )
  } else {
    .hasErrors(
      dataset = dataset,
      type    = c("missingValues", "modelInteractions", "factorLevels"),
      factorLevels.target = options$factors,
      factorLevels.amount = " < 2",
      modelInteractions.modelTerms = options$modelTerms,
      exitAnalysisIfErrors = TRUE
    )

  }
}

# Compute results
.basRegLogLinComputeBFObject <- function(container, dataset, options) {
  if(!is.null(container[["bfObject"]]))
    return(container[["bfObject"]]$object)

  bfObject <- list("bcctObj"        = NULL,
                   "variables"      = c(".", "."),
                   "nModelsVisited" = NULL,
                   "nBurnIn"        = NULL,
                   "bf10s"          = rep(".", length = 2),
                   "postModelProbs" = rep(".", length = 2),
                   "modelNames"     = NULL)
  numberOfModels   <- length(options$modelTerms)
  variablesInModel <- bcctObj <- NULL

  if (options$count == "")
    dataset <- plyr::count(dataset)

  # Extract models needed to be compared
  if (options$count == "")
    dependentVariable <- "freq"
  else
    dependentVariable <- unlist(options$count)

  dependentBase64 <- .v(dependentVariable)
  if (length(options$modelTerms) > 0) {
    variablesInModel <- variablesInModelBase64 <- NULL

    for (i in seq_along(options$modelTerms)) {
      components <- options$modelTerms[[i]]$components
      if (length(components) == 1) {
        term <- components[[1]]
        termBase64 <- .v(components[[1]])
      } else {
        componentsUnlisted <- unlist(components)
        term       <- paste0(componentsUnlisted, collapse = ":")
        termBase64 <- paste0(.v(componentsUnlisted), collapse = ":")
      }
      # Add to tally
      variablesInModel       <- c(variablesInModel, term)
      variablesInModelBase64 <- c(variablesInModelBase64, termBase64)

      # Remove empty stuff
      variablesInModel <- variablesInModel[variablesInModel != ""]
    }
  }

  # Prune the variables
  if (length(variablesInModel) == 0) {
    variablesInModel <- c("...", "... ")
    modelDefinition  <- NULL #this model has no parameters
  } else if (length(variablesInModel) == 1 && options$count == "") {
    variablesInModel <- c(variablesInModel, "... ")
    modelDefinition  <- NULL #this model has only one parameter

  } else if (length(variablesInModel) > 1 || options$count != "") {
    modelDefinition <- paste(dependentBase64, "~",
                             paste(variablesInModelBase64, collapse = "+"))
  } else {
    # Nothing worked out:
    modelDefinition <- NULL #this model has no parameters
    stop(gettext("variables cannot be read"))
  }

  # Save in object
  bfObject$variables <- variablesInModel
  # START analysis Bayesian Log Linear regression
  if (!is.null(modelDefinition)) {
    modelFormula <- as.formula(modelDefinition)

    if (options$count == "")
      names(dataset)[names(dataset) == "freq"] <- dependentBase64
    # Calculate here
    #gives an object computed using Bayesian Analysis of Complete Contingency Tables
    .setSeedJASP(options)
    bcctObj <- try(conting::bcct(formula = modelFormula, data = dataset,
                                 prior = "SBH", n.sample = 2000,
                                 a = options$priorShape, b = options$priorScale),
                   silent = TRUE)
    bfObject$nBurnIn <- 2000 * 0.2

    # Always do auto and then manual adds additional samples
    if (options$samplingMethod == "manual"){
      .setSeedJASP(options)
      bcctObj <- try(conting::bcctu(object = bcctObj,
                                    n.sample = options$samplingMethodManualSamples),
                     silent = TRUE)
      bfObject$nBurnIn <- (2000 + options$samplingMethodManualSamples) * 0.2
    }

    # bcct object checking
    if(inherits(bcctObj, "bcct"))
      bfObject$bcctObj <- bcctObj
    else if(isTryError(bcctObj)) {
      msg <- .extractErrorMessage(bcctObj)
      if(grepl(pattern = "the leading minor of order [0-9]+ is not positive definite", x = msg)) {
        msg <- gettext("Cannot compute the results; a numerical error occurred during sampling. Try to change (e.g., simplify) the model or adjust priors.")
      }
      stop(gettextf("R package 'conting' error: %s <br> It is possible that a numerical error occured during sampling. Try to change (e.g., simplify) the model, adjust priors, change seed, or change the number of MCMC samples.", msg))
    }
  }

  # Post processing
  if (inherits(bfObject$bcctObj, "bcct")) {
    # Good case
    # TODO: Here check bcctSummary$totmodsvisit if this is one,
    # then nothing going on, resample
    #
    bcctSummary <- try(conting::mod_probs(bfObject$bcctObj, scale = 0,
                                          best = options$modelCutOffBestDisplayed), silent = TRUE)

    if (inherits(bcctSummary, "modprobs")) {
      # Good case
      bfObject$nModelsVisited <- bcctSummary$totmodsvisit
      modelnames <- bcctSummary$table$model_formula
      bfObject$modelNames <- substring(as.character(modelnames), first = 2)

      if (bfObject$nModelsVisited == 1)
        bfObject$postModelProbs <- bfObject$bf10s <- 1
      else if (bfObject$nModelsVisited > 1) {
        # Note the following BFs are based on a uniform prior on the models
        if (!is.null(bcctSummary$table$prob.Freq)) {
          postModelProbs <- bcctSummary$table$prob.Freq
          bfObject$postModelProbs <- postModelProbs
          bfObject$bf10s <-  postModelProbs/ max(postModelProbs)
        } else {
          # NAs: nModelsVisited, bf10s, postModelProbs
          stop(gettext("R Package error: Cannot retrieve table probabilities"))
        }
      }
    }
  }
  container[["bfObject"]] <- createJaspState(bfObject)
  container[["bfObject"]]$dependOn(c("samplingMethodManualSamples", "samplingMethod", "seed", "setSeed", "modelCutOffBestDisplayed"))
  return(bfObject)
}

.basRegLogLinMainResults <- function(container, dataset, options) {
  # Compute/get the model
  bfObject <- .basRegLogLinComputeBFObject(container, dataset, options)

  posteriorTableRows <- list()
  results <- list()

  nModelsReport <- try(min(bfObject$nModelsVisited, options$modelCutOffBestDisplayed))
  if (!is.null(bfObject$modelNames))
    reportNames <- .unvf(bfObject$modelNames)
  else if (!is.null(bfObject$variables))
    reportNames <- bfObject$variables
  else
    reportNames <- c("...", "... ")

  if (!is.null(bfObject$bf10s))
    reportBfs <- bfObject$bf10s
  else
    reportBfs <- rep(NA, length = nModelsReport)
  if (is.numeric(reportBfs)) {
    if (options$bayesFactorType == "BF01")
      reportBfs <- 1/reportBfs
    else if (options$bayesFactorType == "LogBF10")
      reportBfs <- log(reportBfs)
  }
  if (!is.null(bfObject$postModelProbs))
    reportPostModelProbs <- bfObject$postModelProbs
  else
    reportPostModelProbs <- rep(NA, length = nModelsReport)

  for (i in 1:nModelsReport){
    posteriorTableRows[[i]] <- list()
    posteriorTableRows[[i]]$"number"  <- as.integer(i)
    posteriorTableRows[[i]]$"model"   <- reportNames[i]
    posteriorTableRows[[i]]$"pMdata"  <- reportPostModelProbs[i]
    posteriorTableRows[[i]]$"bf"      <- reportBfs[i]
  }

  message <- gettextf("Total number of models visited = %i", bfObject$nModelsVisited)
  container[["MainTable"]]$addFootnote(message)
  container[["MainTable"]]$addRows(posteriorTableRows)
}

.basRegLogLinSummaryResults <- function(container, dataset, options) {
  # Compute/get the model
  bfObject <- .basRegLogLinComputeBFObject(container, dataset, options)

  results <- list()

  lookup.table <- .regressionLogLinearBayesianBuildLookup(dataset, options$factors)
  lookup.table[["(Intercept)"]] <- gettext("(Intercept)")

  if(inherits(bfObject$bcctObj, "bcct")) {
    probLevel <- options$regressionCoefficientsCiLevel
    logBlm.summary   <- summary(bfObject$bcctObj,
                                n.burnin = bfObject$nBurnIn,
                                cutoff = options$modelCutOffPosteriorProbability,
                                prob.level = probLevel)
    logBlm.estimates <- logBlm.summary$int_stats

    len.Blogreg <- length(results) + 1
    term.names  <- logBlm.estimates$term

    if (length(bfObject$variables) > 0) {

      variablesInModel <- bfObject$variables
      terms <- as.character(logBlm.estimates$term)
      coef  <- base::strsplit (terms, split = ":", fixed = TRUE)

      for (var in seq_along(coef)) {

        terms <- coef[[var]]
        actualName <- list()

        for (j in seq_along(terms))
          actualName[[j]] <- paste(lookup.table[[ terms[j] ]], collapse = " = ")
        varName <- paste0(actualName, collapse = "*")

        results[[ len.Blogreg ]]             <- list()
        results[[ len.Blogreg ]]$"Name"      <- varName
        results[[ len.Blogreg ]]$"post_prob" <- as.numeric(logBlm.estimates$prob[var])
        results[[ len.Blogreg ]]$"post_mean" <- as.numeric(logBlm.estimates$post_mean[var])
        results[[ len.Blogreg ]]$"post_var"  <- as.numeric(logBlm.estimates$post_var[var])

        if (options$regressionCoefficientsCi == TRUE){
          results[[ len.Blogreg ]]$"lower_lim" <- as.numeric(logBlm.estimates$lower[var])
          results[[ len.Blogreg ]]$"upper_lim" <- as.numeric(logBlm.estimates$upper[var])
        }

        len.Blogreg <- len.Blogreg + 1
      }
    }

  } else {

    len.Blogreg <- length(results) + 1

    if (length(bfObject$variables) > 0) {

      variablesInModel <- bfObject$variables

      len.Blogreg <- len.Blogreg + 1

      for (var in 1:length(variablesInModel)) {

        if (base::grepl(":", variablesInModel[var])) {

          # if interaction term
          vars <- unlist(strsplit(variablesInModel[var], split = ":"))
          name <- paste0(vars, collapse = "\u2009\u273b\u2009")

        } else
          name <- as.character(variablesInModel[ var])

        results[[ len.Blogreg ]]$"Name" <- name
        len.Blogreg <- len.Blogreg + 1
      }
    }
  }
  container[["SummaryTable"]]$addRows(results)
}

.basRegLogLinSubSummaryResults <- function(container, dataset, options) {
  if(!is.null(container[["SubSummaryResults"]]) || !options$regressionCoefficientsSubmodel)
    return()
  # Compute/get the model
  bfObject <- .basRegLogLinComputeBFObject(container, dataset, options)
  if(!(options$regressionCoefficientsSubmodelNo %in% 1:bfObject$nModelsVisited))
    stop(gettext("Submodel specified is not within possible submodels"))

  results <- list()
  lookup.table <- .regressionLogLinearBayesianBuildLookup(dataset, options$factors)
  lookup.table[["(Intercept)"]] <- gettext("(Intercept)")

  if (!is.null(bfObject$bcctObj)) {
    probLevel <- options$regressionCoefficientsSubmodelCiLevel
    order     <- options$regressionCoefficientsSubmodelNo
    logBlm.subestimates = try(conting::sub_model(bfObject$bcctObj,
                                                 n.burnin   = bfObject$nBurnIn,
                                                 order      = order,
                                                 prob.level = probLevel),
                              silent = TRUE)

    if (inherits(logBlm.subestimates, "submod")){

      len.Blogreg <- length(results) + 1
      term.names  <- logBlm.subestimates$term

      extractedModelFormula <- logBlm.subestimates$formula

      extractedModelFormula <- as.character(extractedModelFormula)
      extractedModelFormula <- substring(extractedModelFormula, first = 2) # trim leading ~
      extractedModelFormula <- .unvf(extractedModelFormula)
      container[["SubSummaryTable"]]$addFootnote(extractedModelFormula, symbol = gettext("<em>Model formula:</em>"))
      container[["SubSummaryTable"]]$addFootnote(paste(round(logBlm.subestimates$post_prob, 3)), symbol = gettext("<em>Posterior model probability =</em>"))

      if (length(bfObject$variables) > 0) {

        variablesInModel <- bfObject$variables
        terms <- as.character(logBlm.subestimates$term)
        coef  <- base::strsplit (terms, split = ":", fixed = TRUE)

        for (var in seq_along(coef)) {

          terms      <- coef[[var]]
          actualName <- list()

          for (j in seq_along(terms))
            actualName[[j]] <- paste(lookup.table[[ terms[j] ]], collapse = " = ")
          varName <- paste0(actualName, collapse = "*")

          results[[ len.Blogreg ]] <- list()
          results[[ len.Blogreg ]]$"Name"      <- varName
          results[[ len.Blogreg ]]$"post_mean" <- as.numeric(logBlm.subestimates$post_mean[var])
          results[[ len.Blogreg ]]$"post_var"  <- as.numeric(logBlm.subestimates$post_var[var])


          if (options$regressionCoefficientsSubmodelCi){
            results[[ len.Blogreg ]]$"lower_lim" <- as.numeric(logBlm.subestimates$lower[var])
            results[[ len.Blogreg ]]$"upper_lim" <- as.numeric(logBlm.subestimates$upper[var])
          }
          len.Blogreg <- len.Blogreg + 1
        }
      }

    } else {

      len.Blogreg <- length(results) + 1
      if (length(bfObject$variables) > 0) {

        variablesInModel <- bfObject$variables

        len.Blogreg <- len.Blogreg + 1

        for (var in 1:length(variablesInModel)) {

          if (base::grepl(":", variablesInModel[var])) {

            # if interaction term
            vars <- unlist(strsplit(variablesInModel[var], split = ":"))
            name <- paste0(vars, collapse = "\u2009\u273b\u2009")

          } else
            name <- as.character(variablesInModel[ var])

          results[[ len.Blogreg ]]$"Name" <- name
          len.Blogreg <- len.Blogreg + 1
        }
      }
    }

  } else {

    len.Blogreg <- length(results) + 1

    if (length(bfObject$variables) > 0)
      variablesInModel <- bfObject$variables

    results[[ len.Blogreg ]]$"Model" <- 1
  }
  container[["SubSummaryTable"]]$addRows(results)
}

# Container
.basRegLogLinContainer <- function(jaspResults, dataset, options) {
  if(is.null(jaspResults[["Container"]])) {
    jaspResults[["Container"]] <- createJaspContainer()
    jaspResults[["Container"]]$dependOn(c("count", "modelTerms", "priorShape",
                                          "priorScale", "samplingMethod", "samplingMethodManualSamples", "seed", "setSeed"))
  }
}

# Tables
.basRegLogLinMainTable <- function(container, dataset, options, ready) {
  if (!is.null(container[["MainTable"]]))
    return()

  # Create table
  mainTable <- createJaspTable(title = gettext("Model Comparison"))
  mainTable$dependOn(c("bayesFactorType", "modelCutOffBestDisplayed", "modelCutOffPosteriorProbability"))
  .basRegLogLinCitation(mainTable)
  mainTable$showSpecifiedColumnsOnly <- TRUE
  mainTable$position <- 1
  if (options$bayesFactorType == "BF10")
    bfTitle <- gettext("BF<sub>10</sub>")
  else if (options$bayesFactorType == "BF01")
    bfTitle <- gettext("BF<sub>01</sub>")
  else
    bfTitle <- gettext("Log(BF<sub>10</sub>)")

  # Add columns to table
  mainTable$addColumnInfo(name = "number",  title = " ",                  type = "integer")
  mainTable$addColumnInfo(name = "model",   title = gettext("Models"),    type = "string")
  mainTable$addColumnInfo(name = "pMdata",  title = gettext("P(M|data)"), type = "number", format = "dp:3")
  mainTable$addColumnInfo(name = "bf",      title = bfTitle,              type = "number")

  container[["MainTable"]] <- mainTable
  if (!ready)
    return()
  res <- try(.basRegLogLinMainResults(container, dataset, options))
  .basRegLogLinSetError(res, mainTable)
}

.basRegLogLinSummaryTable <- function(container, dataset, options, ready){
  if (!is.null(container[["SummaryTable"]]) ||
      !options$regressionCoefficientsEstimates)
    return()

  # Create table
  summaryTable <- createJaspTable(title = gettext("Posterior Summary Statistics"))
  summaryTable$dependOn(c("regressionCoefficientsEstimates",
                          "regressionCoefficientsCi",
                          "regressionCoefficientsCiLevel"))
  .basRegLogLinCitation(summaryTable)
  summaryTable$showSpecifiedColumnsOnly <- TRUE
  summaryTable$position <- 2

  # Add columns to table
  summaryTable$addColumnInfo(name = "Name",      title = " ",                     type = "string")
  summaryTable$addColumnInfo(name = "post_prob", title = gettext("P(incl|data)"), type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "post_mean", title = gettext("Mean"),         type = "number", format = "dp:3")
  summaryTable$addColumnInfo(name = "post_var",  title = gettext("Variance"),     type = "number", format = "dp:3")
  if(options$regressionCoefficientsCi){
    ci.label <- gettextf("%s%% Credible intervals", 100*options$regressionCoefficientsCiLevel)
    summaryTable$addColumnInfo(name = "lower_lim", title = gettext("Lower"), type = "number", overtitle = ci.label)
    summaryTable$addColumnInfo(name = "upper_lim", title = gettext("Upper"), type = "number", overtitle = ci.label)
  }

  container[["SummaryTable"]] <- summaryTable

  if (!ready)
    return()

  res <- try(.basRegLogLinSummaryResults(container, dataset, options))

  .basRegLogLinSetError(res, summaryTable)
}

.basRegLogLinSubSummaryTable <- function(container, dataset, options, ready){
  if (!is.null(container[["SubSummaryTable"]]) ||
      !options$regressionCoefficientsSubmodel)
    return()

  # Create table
  title <- gettextf("Posterior Summary Statistics For Submodel %s", options$regressionCoefficientsSubmodelNo)

  subSummaryTable <- createJaspTable(title = title)
  subSummaryTable$dependOn(c("regressionCoefficientsSubmodel",
                             "regressionCoefficientsSubmodelCi",
                             "regressionCoefficientsSubmodelCiLevel",
                             "regressionCoefficientsSubmodelNo"))
  .basRegLogLinCitation(subSummaryTable)
  subSummaryTable$showSpecifiedColumnsOnly <- TRUE
  subSummaryTable$position <- 3

  # Add columns to table
  subSummaryTable$addColumnInfo(name = "Name",      title = " ",                 type = "string")
  subSummaryTable$addColumnInfo(name = "post_mean", title = gettext("Mean"),     type = "number", format = "dp:3")
  subSummaryTable$addColumnInfo(name = "post_var",  title = gettext("Variance"), type = "number", format = "dp:3")

  if(options$regressionCoefficientsSubmodelCi){
    ciVal    <- options$regressionCoefficientsSubmodelCiLevel
    ci.label <- gettextf("%.0f%% Credible intervals", 100*ciVal)

    subSummaryTable$addColumnInfo(name = "lower_lim", title = gettext("Lower"), type = "number", overtitle = ci.label)
    subSummaryTable$addColumnInfo(name = "upper_lim", title = gettext("Upper"), type = "number", overtitle = ci.label)
  }

  container[["SubSummaryTable"]] <- subSummaryTable
  if (!ready)
    return()
  res <- try(.basRegLogLinSubSummaryResults(container, dataset, options))
  .basRegLogLinSetError(res, subSummaryTable)
}

# Other
.basRegLogLinCitation <- function(table) {
  citation <- ("Overstall, A., & King, R. (2014). conting: an R package for
                Bayesian analysis of complete and incomplete contingency tables.
                Journal of Statistical Software, 58(7), 1-27.")
  table$addCitation(citation)
}

.basRegLogLinSetError <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}

.regressionLogLinearBayesianBuildLookup <- function(dataset, factors) {
  table <- list()

  for (factorName in factors) {
    levels <- base::levels(dataset[[ .v(factorName) ]])

    for (i in seq_along(levels)) {
      levelName <- levels[i]
      base64Name <- paste(.v(factorName), i, sep="")
      actualName <- c(factorName, levelName)
      table[[base64Name]] <- actualName
    }
  }
  return(table)
}

