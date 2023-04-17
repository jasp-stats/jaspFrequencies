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
    .createInformedBayesDescriptivesTable(jaspResults, dataset, options, type = "binomial")

  if (options[["descriptivesPlot"]])
    .createInformedBayesDescriptivesPlot(jaspResults, dataset, options, type = "binomial")

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
.createInformedBinBayesPosteriorPlot      <- function(jaspResults, dataset, options) {

  if (!is.null(jaspResults[["posteriorPlot"]]))
    return()

  # extract posterior summary from the unrestricted model and format it for the plotting function
  tempSummary           <- data.frame(
    fact     = levels(dataset[[options[["factor"]]]]),
    observed = (options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]]) / (options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]] + options[["priorCounts"]][[2]][["values"]] + (dataset[[options[["sampleSize"]]]]-dataset[[options[["successes"]]]])),
    lowerCI  = stats::qbeta(p = (1-options[["descriptivesAndPosteriorPlotCiCoverage"]])/2, shape1 = options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]], shape2 = options[["priorCounts"]][[2]][["values"]] + (dataset[[options[["sampleSize"]]]]-dataset[[options[["successes"]]]])),
    upperCI  = stats::qbeta(p = 1-(1-options[["descriptivesAndPosteriorPlotCiCoverage"]])/2, shape1 = options[["priorCounts"]][[1]][["values"]] + dataset[[options[["successes"]]]], shape2 = options[["priorCounts"]][[2]][["values"]] + (dataset[[options[["sampleSize"]]]]-dataset[[options[["successes"]]]]))
  )

  if (options[["display"]] == "counts")
    tempSummary[,2:4] <- tempSummary[,2:4] * matrix(dataset[[options[["sampleSize"]]]], nrow = nrow(dataset), ncol = 3, byrow = FALSE)

  posteriorPlot <- createJaspPlot(title = gettext("Unrestricted posterior plot"), width = 480, height = 320)
  posteriorPlot$position <- 4
  posteriorPlot$dependOn(c(.informedBinDependency,  "display", "posteriorPlot", "descriptivesAndPosteriorPlotCiCoverage"))
  jaspResults[["posteriorPlot"]] <- posteriorPlot

  posteriorPlot$plotObject <- .informedPlot(tempSummary, options, descriptives = FALSE)

  return()
}
.createInformedBinBayesDescriptivesData   <- function(dataset, options, table = TRUE) {

  # Compute CI
  if (table && options[["successes"]] != "" && options[["sampleSize"]] != "" && options[["descriptivesTableCi"]])
    tempCI <- .binComputeCIs(dataset[[options[["successes"]]]], dataset[[options[["sampleSize"]]]], options[["descriptivesTableCiCoverage"]], ifErrorReturn = 0, scale = .decodeOptionsDisplay(options))
  else if (!table && options[["successes"]] != "" && options[["sampleSize"]] != "")
    tempCI <- .binComputeCIs(dataset[[options[["successes"]]]], dataset[[options[["sampleSize"]]]], options[["descriptivesAndPosteriorPlotCiCoverage"]], ifErrorReturn = 0, scale = .decodeOptionsDisplay(options))
  else
    tempCI <- NULL

  # Add rows
  if (options[["successes"]] != "" && options[["sampleSize"]] != "") {

    rowsFrame <- data.frame(
      fact     = dataset[[options[["factor"]]]],
      observed = if (options[["display"]] == "counts") dataset[[options[["successes"]]]] else dataset[[options[["successes"]]]] / dataset[[options[["sampleSize"]]]]
    )

    if (!is.null(tempCI)) {
      rowsFrame$lowerCI <- tempCI[,"lowerCI"]
      rowsFrame$upperCI <- tempCI[,"upperCI"]
    }

  } else {
    rowsFrame <- data.frame()
  }

  return(rowsFrame)
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
