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

MultinomialTestInternal <- function(jaspResults, dataset, options, ...) {
  # Read dataset
  dataset <- .multinomReadData(dataset, options)

  ready   <- options$factor != ""

  # Error checking
  .multinomCheckErrors(dataset, options)

  # Output tables and plots
  .multinomialMainTable(        jaspResults, dataset, options, ready)
  .multinomialDescriptivesTable(jaspResults, dataset, options, ready)
  .multinomialDescriptivesPlot( jaspResults, dataset, options, ready)

  return()
}

# Preprocessing functions ----
.multinomReadData <- function(dataset, options) {
  fact <- asnum <- NULL
  if (options$factor != "") {
    fact <- options$factor
    if (options$count != "") {
      asnum <- options$count
      if (!is.null(options$expectedCount) && options$expectedCount != "")
        asnum <- c(asnum, options$expectedCount)
    }
  }

  if (is.null(dataset)) {
    dataset <- .readDataSetToEnd(columns.as.numeric = asnum, columns.as.factor = fact,
                                 exclude.na.listwise = NULL)
  } else {
    dataset <- .vdf(dataset, columns.as.numeric = asnum, columns.as.factor = fact)
  }

  # Reorder the rows of the factor and the counts (and expected probabilities) if the user changes the factor level order in JASP.
  # This ensures the ordering in tables and plots also changes appropriately.
  if (options$factor != "" && options$count != "") {
    factLevelOrder        <- as.character(dataset[[.v(options$factor)]])

    # the following condition holds when `count` are specified but the data set is not in aggregated form
    # the error is subsequently caught in  .multinomCheckErrors
    # we need to escape this function early because the operations under this check assume that the data set is already in aggregated form
    if(length(unique(factLevelOrder)) != length(factLevelOrder)) return(dataset)

    levelOrderUserWants   <- options$testValuesCustom[[1]]$levels
    whatUserWantsToWhatIs <- match(levelOrderUserWants, factLevelOrder)

    if (!identical(sort(whatUserWantsToWhatIs), whatUserWantsToWhatIs))
      dataset[seq_along(factLevelOrder), ] <- dataset[whatUserWantsToWhatIs, ]

    # For syntax mode the analysis will be called from RStudio and the factor levels may not match the testValuesCustom.
    factValues <- as.character(dataset[[.v(options$factor)]])
    facLevels  <- levels(dataset[[.v(options$factor)]])
    if (length(facLevels) == length(factValues) && !identical(factValues, facLevels))
      levels(dataset[[.v(options$factor)]]) <- factValues
  }

  return(dataset)
}

.multinomCheckErrors <- function(dataset, options) {
  if (options$factor == "")
    return()

  customChecks <- list(
    checkExpecAndObs = function() {
      if (options$expectedCount != "" && options$count == "")
        return(gettext("Expected counts not supported without observed counts."))
    },

    checkExpecNeeded = function() {
      if (options$expectedCount != "" || options$testValues != "equal")
        if (options$expectedCount == "" && length(options$testValuesCustom) == 0)
          return(gettext("No expected counts entered."))
    },

    checkCounts = function() {
      if (options$count != "") {
        dataset <- na.omit(dataset)
        nlevels <- nlevels(as.factor(dataset[[.v(options$factor)]]))
        counts  <- dataset[[.v(options$count)]]

        if (nlevels != length(counts))
          return(gettext("Invalid counts: variable does not match the number of levels of the factor. When counts are specified, each row of the data set must represent a unique level of the factor."))

        if (options$expectedCount != "" && nlevels != length(dataset[[.v(options$expectedCount)]]))
          return(gettext("Invalid expected counts: variable does not match the number of levels of the factor."))

        # only applies for observed counts, expected counts can be proportions
        if (any(counts != round(counts)))
          return(gettext("Invalid counts: variable must contain only integer values."))

        if (sum(counts) == 0)
          return(gettext("Invalid counts: less than 1 observation."))

        if (max(counts) > 10e7)
          return(gettext("Invalid counts: maximum value (10e7) exceeded."))

      }
    }
  )

  .hasErrors(dataset,
             type = c("factorLevels", "negativeValues", "infinity"),
             negativeValues.target = c(options$count, options$expectedCount),
             infinity.target = c(options$count, options$expectedCount),
             factorLevels.target  = options$factor,
             factorLevels.amount  = "< 1",
             custom = customChecks,
             exitAnalysisIfErrors = TRUE)
}

# Results function ----
.chisquareTest <- function(jaspResults, dataset, options) {
  # Run chi-square test and return jaspResults object
  #
  # Args:
  #   jaspResults:
  #   dataset: input dataset
  #   options: user options
  #
  # Return:
  #   No return, stores jaspResults Object (chisquare test)

  # Take results from state if possible
  if (!is.null(jaspResults[["stateChisqResults"]]))
    return(jaspResults[["stateChisqResults"]]$object)

  chisqResults <- NULL
  fact <- options$factor

  if(fact != ""){
    # first determine the hypotheses
    factorVariable <- dataset[[.v(fact)]]
    factorVariable <- factorVariable[!is.na(factorVariable)]
    factorVariable <- as.factor(factorVariable)
    nlev <- nlevels(factorVariable)

    if (options$count != "") {
      counts <- dataset[[.v(options$count)]]
      # omit count entries for which factor variable is NA
      counts <- counts[!is.na(factorVariable)]
      factorVariable <- factor(rep(factorVariable, counts),
                               levels = levels(factorVariable))
    }

    dataTable <- table(factorVariable)

    hyps <- .multinomialHypotheses(dataset, options, nlev)

    # create a named list with as values the chi-square result objects
    chisqResults <- lapply(hyps, function(h) {
      # catch warning message and append to object if necessary
      csr  <- warn <- NULL
      # need to improve this try statement
      csr  <- withCallingHandlers(
                chisq.test(x = dataTable, p = h, rescale.p = TRUE,
                           simulate.p.value = FALSE),
                warning = function(w) warn <<- w$message
      )
      csr[["warn"]] <- warn
      return(csr)
    })
  }
  # Save results to state
  jaspResults[["stateChisqResults"]] <- createJaspState(chisqResults)
  jaspResults[["stateChisqResults"]]$dependOn(c("factor", "count", "testValues",
                                                "expectedCount", "testValuesCustom"))
  return(chisqResults)
}

# Filling Tables ----
.multinomMainTableFill <- function(jaspResults, dataset, options) {
  # Turn chi-square object into jaspResults
  # object and creates results for descripties table
  #
  # Args:
  #   jaspResults:
  #   dataset: input dataset
  #   options: user options
  #   chisqResults:
  #
  # Return:
  #   No return, stores jaspResults Object (main table results)

  results <- list()

  # Compute Results
  chisqResults <- .chisquareTest(jaspResults, dataset, options)

  if(!is.null(chisqResults)){
    # extract relevant objects from chisqResults (this looks so convoluted is because its stored in an inconvenient way, but that's probably a legacy code thingy).
    dataframe <- do.call(rbind.data.frame,
                         lapply(chisqResults, function(x) c(x[["statistic"]][["X-squared"]], x[["parameter"]][["df"]], x[["p.value"]])))
    colnames(dataframe) <- c("chisquare", "df", "p")
    dataframe <- cbind(case = names(chisqResults), dataframe)

    if (options$vovkSellke)
      dataframe <- cbind(dataframe, vovkSellke = VovkSellkeMPR(dataframe$p))

    jaspResults[["chisqTable"]]$setData(dataframe)

    for (r in 1:length(chisqResults)) {
      if (!is.null(chisqResults[[r]][["warn"]]))
        jaspResults[["chisqTable"]]$addFootnote(chisqResults[[r]][["warn"]])
    }
  }
}

.multinomDescriptivesTableFill <- function(jaspResults, options, chisqResults){
  # Turn chi-square object into jaspResults
  # object and creates results for descripties table
  #
  # Args:
  #   jaspResults:
  #   dataset: input dataset
  #   options: user options
  #
  # Return:
  #   No return, stores jaspResults Object (descriptives table results)

  results <- list()
  footnotes <- list()

  observed <- chisqResults[[1]][["observed"]]
  if (options$descriptivesType == "counts")
    observed <- as.integer(observed)
  else
    observed <- as.numeric(observed)/sum(observed)

  nms <- names(chisqResults)
  tableFrame <- data.frame(
    factor   = names(chisqResults[[1]][["observed"]]),
    observed = observed,
    stringsAsFactors = FALSE
  )
  if (options$descriptivesType == "counts")
    for (r in chisqResults)
      tableFrame <- cbind(tableFrame, r[["expected"]])
  else
    for (r in chisqResults){
      div <- sum(chisqResults[[1]][["observed"]])
      tableFrame <- cbind(tableFrame, r[["expected"]]/div)
    }

  if (length(nms) == 1)
    colnames(tableFrame)[-(1:2)] <- "expected"
  else
    colnames(tableFrame)[-(1:2)] <- nms

  # Add confidenceInterval to the tableFrame
  if (options$descriptivesTableCi){
    ciDf <- .multComputeCIs(chisqResults[[1]][["observed"]],
                            options$descriptivesTableCiLevel,
                            scale = options$descriptivesType)
    tableFrame <- cbind(tableFrame, ciDf)
    message <- gettext("Confidence intervals are based on independent binomial distributions.")
    results[["footnotes"]][["CImessage"]] <- message
    if (anyNA(unlist(tableFrame[, c('lowerCI', 'upperCI')]))){
      message <- gettext("Could not compute confidence intervals.")
      results[["footnotes"]][["ciComputeError"]] <- message
    }
  }
  jaspResults[["descriptivesTable"]]$setData(tableFrame)
}

# Output functions ----
.multinomialMainTable <- function(jaspResults, dataset, options, ready) {
  # Transform chi-square test object into table for JASP
  # chisqResults = list(H1 = obj, H2 = obj, ....)
  #
  # Args:
  #   jaspResults:
  #   dataset:
  #   options: input options
  #   ready: need factor field filled in order to compute results
  #
  # Return:
  #   Chi square table

  if (!is.null(jaspResults[["chisqTable"]])) return()

  # Create table
  chisqTable <- createJaspTable(title = "Multinomial Test")
  chisqTable$dependOn(c("factor", "count", "expectedCount", "testValuesCustom",
                        "vovkSellke", "testValues"))
  chisqTable$showSpecifiedColumnsOnly <- TRUE

  # Add columns to table
  chisqTable$addColumnInfo(name = "case",      title = "", type = "string", combine = TRUE)
  chisqTable$addColumnInfo(name = "chisquare", title = "\u03C7\u00B2", type = "number")
  chisqTable$addColumnInfo(name = "df",        title = gettext("df"), type = "integer")
  chisqTable$addColumnInfo(name = "p",         title = gettext("p"),  type = "pvalue")

  # include Vovk-Selke p-ratio as columns
  if (options$vovkSellke) {
    chisqTable$addColumnInfo(name = "vovkSellke", title =  gettextf("VS-MPR%s", "\u002A"), type = "number")
    chisqTable$addFootnote(.messages("footnote", "VovkSellkeMPR"), symbol = "\u002A")
  }

  jaspResults[["chisqTable"]] <- chisqTable
  if (!ready)
    return()
  res <- try(.multinomMainTableFill(jaspResults, dataset, options))
  .multinomialSetError(res, chisqTable)
}

.multinomialDescriptivesTable <- function(jaspResults, dataset, options, ready) {
  # Create multinomial descriptives table
  #
  # Args:
  #   jaspResults
  #   options: user options
  #   ready: need factor field filled in order to compute results
  #
  # Return:
  #   Descriptives table

  if(!options$descriptivesTable || !is.null(jaspResults[["descriptivesTable"]]))
    return()

  # Compute/get Results
  chisqResults <- .chisquareTest(jaspResults, dataset, options)

  descriptivesTable <- createJaspTable(title = gettext("Descriptives"))
  descriptivesTable$dependOn(c("factor", "count", "expectedCount",  "testValues", "descriptivesType", "descriptivesTable",
                               "descriptivesTableCi", "testValuesCustom", "descriptivesTableCiLevel"))

  if(options$factor == ""){
    descriptivesTable$addColumnInfo(name = "factor", title = gettext("Factor"), type = "string")
    if (options$descriptivesType == "counts")
      descriptivesTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "integer")
    else
      descriptivesTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "number")
  } else {
    descriptivesTable$addColumnInfo(name = "factor", title = options$factor, type = "string")
    if (options$descriptivesType == "counts")
      descriptivesTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "integer")
    else
      descriptivesTable$addColumnInfo(name = "observed", title = gettext("Observed"), type = "number")

    nms <- names(chisqResults)

    if (length(nms) == 1) {
      descriptivesTable$addColumnInfo(name = "expected", title = gettextf("Expected: %s", nms),
                                      type = "number")
    } else {
      for (i in 1:length(nms)) {
        descriptivesTable$addColumnInfo(name = nms[i], title = nms[i],
                                        type = "number", overtitle = gettext("Expected"))
      }
    }
  }
  if (options$descriptivesTableCi){
    interval <- 100 * options$descriptivesTableCiLevel
    title <- gettextf("%s%% Confidence Interval", interval)
    descriptivesTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"),
                                    type = "number", overtitle = title)
    descriptivesTable$addColumnInfo(name = "upperCI", title = gettext("Upper"),
                                    type = "number", overtitle = title)
  }
  jaspResults[["descriptivesTable"]] <- descriptivesTable
  if (!ready)
    return()
  res <- try(.multinomDescriptivesTableFill(jaspResults, options, chisqResults))
  .multinomialSetError(res, descriptivesTable)
  if(options$descriptivesTableCi) {
    message <- gettext("Confidence intervals are based on independent binomial distributions.")
    descriptivesTable$addFootnote(message)
    if (anyNA(unlist(descriptivesTable[["data"]][, c('lowerCI', 'upperCI')]))){
      message <- gettext("Could not compute confidence intervals.")
      descriptivesTable$addFootnote(message)
    }
  }
}

.multinomialDescriptivesPlot <- function(jaspResults, dataset, options, ready) {
  if(!options$descriptivesPlot || !is.null(jaspResults[["descriptivesPlot"]]))
    return()

  descriptivesPlot <- createJaspPlot(title = gettext("Descriptives Plot"), width = 500, aspectRatio = 0.7)
  descriptivesPlot$dependOn(c("factor", "count", "descriptivesPlotCiLevel",
                              "descriptivesType", "descriptivesPlot"))

  jaspResults[["descriptivesPlot"]] <- descriptivesPlot

  if (!ready)
    return()

  .multinomialDescriptivesPlotFill(jaspResults, dataset, options, descriptivesPlot)

  return()
}

.multinomialDescriptivesPlotFill <- function(jaspResults, dataset, options, descriptivesPlot) {
  # Compute/get Results
  chisqResults <- .chisquareTest(jaspResults, dataset, options)
  # Generate the plot
  f <- names(chisqResults[[1]][["observed"]])
  plotFrame <- data.frame(factor = factor(f, levels = rev(f)))
  # Counts or props
  if (options$descriptivesType == "counts") {
    yname <- gettext("Observed counts")
    obs   <- as.integer(chisqResults[[1]][["observed"]])
  } else {
    div   <- sum(chisqResults[[1]][["observed"]])
    yname <- gettext("Observed proportions")
    obs   <- as.numeric(chisqResults[[1]][["observed"]])/div
  }
  plotFrame <- cbind(plotFrame, obs)

  # Calculate confidence interval
  if (options$descriptivesPlotCiLevel){
    ciDf       <- .multComputeCIs(chisqResults[[1]][["observed"]],
                                  options$descriptivesPlotCiLevel,
                                  ifErrorReturn = 0, scale = options$descriptivesType)
    plotFrame  <- cbind(plotFrame, ciDf)
  }

  # Determine y-axis margin: If CIs could not be computed, use observed counts
  plotFrame$yAxisMargin <- plotFrame$upperCI
  for(i in 1:nrow(plotFrame))
    if(abs(plotFrame$upperCI[i]) <= sqrt(.Machine$double.eps)) #near-zero value
      plotFrame$yAxisMargin[i] <- plotFrame$obs[i]

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(0, plotFrame[["yAxisMargin"]]))
  # Create plot
  p <- ggplot2::ggplot(data = plotFrame,
                       mapping = ggplot2::aes(x = factor, y = obs)) +
    ggplot2::geom_bar(stat = "identity", size = 0.75, colour = "black",
                      fill = "grey") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = ciDf$lowerCI, ymax = ciDf$upperCI),
                           size = 0.75, width = 0.3) +
    ggplot2::xlab(options$factor) +
    ggplot2::scale_y_continuous(name = yname, breaks = yBreaks) +
    jaspGraphs::geom_rangeframe(sides = "b") +
    ggplot2::coord_flip() +
    jaspGraphs::themeJaspRaw(axis.title.cex = jaspGraphs::getGraphOption("axis.title.cex"))

  descriptivesPlot$plotObject <- p
}

# Extra results functions
.multinomialHypotheses <- function(dataset, options, nlevels) {
  # Transform input into a list of hypotheses
  # This function transforms the input into a list of hypotheses
  #
  # Args:
  #   dataset: input dataset
  #   options: user options
  #   nlevels:  number of levels of the factor variable
  #
  # Return:
  #   hypotheses

  hyps <- list()
  if (options$expectedCount == "" && options$testValues == "equal") {
    # Expected probabilities are simple now
    hyps[["Multinomial"]] <- rep(1/nlevels, nlevels)
  } else {
    # First, generate a table with expected probabilities based on input
    expectedDf <- .generateExpectedProps(dataset, options, nlevels)
    # assign each hypothesis to the hyps object
    hyps <- as.list(expectedDf)
  }
  return(hyps)
}

.generateExpectedProps <- function(dataset, options, nlevels) {
  # Parse expected probabilities/counts
  # This function returns a data frame with in each named column the expected
  # probabilities. The column names are the hypothesis names.
  #
  # Args:
  #   dataset: input dataset
  #   options: user options
  #   nlevels: number of levels of the factor variable
  #
  # Return:
  #   expected Probabilities

  if (options$expectedCount != "") {
    # use only expectedCount
    fact   <- dataset[[.v(options$factor)]]
    eProps <- dataset[.v(options$expectedCount)]
    colnames(eProps) <- options$expectedCount
    rownames(eProps) <- fact

    return(na.omit(eProps))

  } else if (length(options$testValuesCustom) > 0) {
    eProps <- sapply(options$testValuesCustom, function(x) {
      vals <- unlist(x$values)
      if (sum(vals) == 0)
        vals <- rep(1, length(vals))
      return(vals)
    })

    colnames(eProps) <- sapply(seq_along(options$testValuesCustom),
                               function(x) paste0("H\u2080 (", letters[x], ")"))
    rownames(eProps) <- options$testValuesCustom[[1]]$levels

    return(as.data.frame(eProps))
  }
}

.multinomialSetError <- function(res, table){
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
}

