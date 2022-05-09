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

# TODO: add comparison to null / one of the levels

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


  return()
}

.informedMultinomialDependency <- c("factor", "counts", "priorCounts", "restrictedModels", "restrictionSyntax")

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
        bf_type       = "BFre",
        seed          = 2020
      ))


      modelsList[[i]] <- list(
        "model" = tempFit,
        "name"  = options[["restrictedModels"]][[i]][["modelName"]]
      )

    }

    progressbarTick()
  }

  models$object <- modelsList



  saveRDS(modelsList, file = "C:/JASP/jaspFrequencies/fits.RDS")

  return()
}
.createInformedMultBayesMainTable         <- function(jaspResults, options){


  if (!is.null(jaspResults[["summaryTable"]]))
    return()

  models <- jaspResults[["models"]]$object

  summaryTable <- createJaspTable(title = gettext("Bayesian evaluation of multinomial order constraints "))
  summaryTable$position <- 1
  summaryTable$dependOn(c(.informedMultinomialDependency, "bayesFactorType"))

  if (options$bayesFactorType == "BF10")
    bfTitle <- gettextf("BF%s%s", "\u2081", "\u2080")
  else if (options$bayesFactorType == "BF01")
    bfTitle <- gettextf("BF%s%s", "\u2080", "\u2081")
  else
    bfTitle <- gettextf("Log(BF%s%s)", "\u2081", "\u2080")

  summaryTable$addColumnInfo(name = "model",    title = "",                                   type = "string")
  summaryTable$addColumnInfo(name = "marglik",  title = gettext("log(Marginal likelihood)"),  type = "number")
  summaryTable$addColumnInfo(name = "bf",       title = bfTitle,                              type = "number")
  summaryTable$addColumnInfo(name = "bfError",  title = gettext("Error"),                     type = "number")
  summaryTable$addColumnInfo(name = "bfPrec",   title = gettext("Error %"),                   type = "number")

  jaspResults[["summaryTable"]] <- summaryTable

  if (is.null(models))
    return()
  else if (any(unlist(lapply(models, jaspBase::isTryError)))) {
    errors <- models[unlist(lapply(models, jaspBase::isTryError))]
    summaryTable$setError(paste0("Error in ", errors[[1]][["name"]], ": ", errors[[1]][["model"]]))
    return()
  }

  for (i in 1:seq_along(models)) {

    tempRow <- list(
      model    = models[[i]][["name"]],
      marglik  = models[[i]]$model$bf_list$logBFe_inequalities[1,"logml_post"],
      bf       = .recodeBFtype(models[[i]]$model$bf_list$bf[1,"BFer"], options[["bayesFactorType"]]),
      bfError  = models[[i]]$model$bf_list$error_measures[1,"re2"],
      bfPrec   = as.numeric(gsub("%", "", models[[i]]$model$bf_list$error_measures["percentage"], fixed = TRUE))
    )

    summaryTable$addRows(tempRow)
  }

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

  # Compute CI
  if (options[["counts"]] != "" && options[["credibleInterval"]]){
    tempCI <- .multComputeCIs(dataset[,options[["counts"]]], options[["credibleIntervalInterval"]], ifErrorReturn = 0, scale = options[["countProp"]])
    jaspResults[["descriptivesTable"]]$addFootnote(gettext("Credible intervals are based on independent binomial distributions with flat priors."))
  }

  if (options[["countProp"]] == "descCounts")
    stdConst <- 1
  else
    stdConst <- sum(dataset[,options[["counts"]]])

  # Add rows
  for (i in 1:nrow(dataset)){

    tempRow <- list(fact = dataset[i,options[["factor"]]])

    # skip if the input is not specified
    if (options[["counts"]] != "") {
      tempRow[["observed"]] <- dataset[i,options[["counts"]]] / stdConst
      if (options[["credibleInterval"]]) {
        tempRow[["lowerCI"]] <- tempCI[i,"lowerCI"]
        tempRow[["upperCI"]] <- tempCI[i,"upperCI"]
      }
    }

    descriptivesTable$addRows(tempRow)
  }

  return()
}
.createInformedMultBayesDescriptivesPlot  <- function(jaspResults, dataset, options){

  if(!is.null(jaspResults[["descriptivesPlot"]]))
    return()

  # Create Plot
  descriptivesPlot <- createJaspPlot(title = gettext("Descriptives plot"), width = 480, height = 320)
  descriptivesPlot$position <- 3
  descriptivesPlot$dependOn(c("factor", "counts", "countProp", "descriptivesPlot", "descriptivesPlotCredibleInterval"))
  jaspResults[["descriptivesPlot"]] <- descriptivesPlot

  # Show empty Plot if no variable is selected
  if(options[["factor"]] == "" && options[["counts"]])
    return()

  # creates summary output for the descriptive
  options$exProbVar  <- ""
  options$hypothesis <- "multinomialTest"
  .computeMultinomialResults(jaspResults, dataset, options)

  descriptivesPlot$plotObject <- .multBayesPlotHelper(options[["factorVariable"]], options, jaspResults[["stateMultinomialBayesianResults"]]$object)

  return()
}
