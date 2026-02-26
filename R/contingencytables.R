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

ContingencyTablesInternal <- function(jaspResults, dataset, options, ...) {
  # Read dataset
  dataset <- .crossTabReadData(dataset, options)

  ready <- !(length(options$rows) == 0 || length(options$columns) == 0)

  # Error checking
  .crossTabCheckErrors(dataset, options)

  # Compute the combinations of rows, columns, layers
  analyses <- .crossTabComputeAnalyses(dataset, options, ready)

  # Tables container
  .crossTabContainer(jaspResults, options, analyses, ready)

  # Output tables (each calls its own results function)
  .crossTabMain(      jaspResults, dataset, options, analyses, ready)
  .crossTabChisq(     jaspResults, dataset, options, analyses, ready)
  .crossTabOdds(      jaspResults, dataset, options, analyses, ready)
  .crossTabNominal(   jaspResults, dataset, options, analyses, ready)
  .crossTabGamma(     jaspResults, dataset, options, analyses, ready)
  .crossTabKendallTau(jaspResults, dataset, options, analyses, ready)

  return()
}

# Preprocessing functions
.crossTabReadData <- function(dataset, options) {
  if (!is.null(dataset))
    return(dataset)
  else {
    layer.variables <- c()
    for (layer in options$layers)
      layer.variables <- c(layer.variables, unlist(layer$variables))
    counts.var <- options$counts
    if (counts.var == "")
      counts.var <- NULL

    factors <- c(unlist(options$rows), unlist(options$columns), layer.variables)

    return(.readDataSetToEnd(columns.as.factor  = factors,
                             columns.as.numeric = counts.var))
  }
}

.crossTabCheckErrors <- function(dataset, options) {
  .hasErrors(dataset,
             type = c('negativeValues', 'infinity', 'missingValues'),
             all.target = c(options$counts),
             exitAnalysisIfErrors = TRUE)

  .hasErrors(dataset,
             type = "factorLevels",
             factorLevels.target = c(options[["rows"]], options[["columns"]]),
             factorLevels.amount  = "< 2",
             exitAnalysisIfErrors = TRUE)
}

# Combinations of rows, columns, layers
.crossTabComputeAnalyses <- function(dataset, options, ready) {
  rows    <- as.character(options$rows)
  columns <- as.character(options$columns)

  if (length(rows) == 0)
    rows <- ""

  if (length(columns) == 0)
    columns <- ""
  analyses <- list()
  analyses <- data.frame("columns" = columns, stringsAsFactors = FALSE)
  analyses <- cbind(analyses, "rows" = rep(rows, each = nrow(analyses)),
                    stringsAsFactors = FALSE)

  for (layer in options$layers) {
    layer.vars <- as.character(layer$variables)
    analyses <- cbind(analyses, rep(layer.vars, each = nrow(analyses)),
                      stringsAsFactors = FALSE)
    names(analyses)[ncol(analyses)] <- layer$name
  }
  return(analyses)
}

.crossTabCreateContainerName <- function(analysis, prefix = "container") {
  if (nrow(analysis) != 1)
    stop("Attempting to create container name for multiple analyses. Each analysis has its own container.")

  return(paste(prefix, analysis$rows, analysis$columns, sep = "_"))
}

# Container
.crossTabContainer <- function(jaspResults, options, analyses, ready) {
  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    containerName <- .crossTabCreateContainerName(analysis)
    if (is.null(jaspResults[[containerName]])) {
      container <- createJaspContainer()
      container$dependOn(options              = c("layers", "counts"),
                         optionContainsValue  = list(rows     = analysis$rows,
                                                     columns  = analysis$columns))
      container$position <- 1
      jaspResults[[containerName]] <- container
    }
  }
}

# Output Tables
.crossTabMain <- function(jaspResults, dataset, options, analyses, ready) {

  if (!(options$countsObserved || options$countsExpected || options$percentagesRow || options$percentagesColumn ||
      options$percentagesTotal || options$residualsUnstandardized || options$residualsPearson || options$residualsStandardized))
    return()

  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[.crossTabCreateContainerName(analysis)]]
    if (!is.null(analysisContainer[["crossTabMain"]]))
      next

    # Create table
    crossTabMain <- createJaspTable(title = gettext("Contingency Tables"))
    crossTabMain$dependOn(c("countsExpected", "countsObserved", "marginShowTotals", "percentagesRow",  "percentagesColumn",
                            "percentagesTotal", "rowOrder", "columnOrder", "residualsUnstandardized",
                            "residualsPearson", "residualsStandardized"))
    crossTabMain$showSpecifiedColumnsOnly <- TRUE
    crossTabMain$position <- 1

    .crossTabLayersColumns(crossTabMain, analysis)

    colTitleHere <- analysis$rows
    if(analysis$rows == "")  colTitleHere <- " "

    crossTabMain$addColumnInfo(name = analysis$rows, title = colTitleHere, type = "string", combine = TRUE)


    counts.fp <- .crossTabCountsFp(dataset, options)

    if (sum(options$countsObserved, options$countsExpected, options$percentagesRow, options$percentagesColumn,
            options$percentagesTotal, options$residualsUnstandardized, options$residualsPearson, options$residualsStandardized) > 1) {
      if (options$countsObserved)           crossTabMain$addColumnInfo(name = "type[counts]",                  title = "", type = "string")
      if (options$countsExpected)           crossTabMain$addColumnInfo(name = "type[expected]",                title = "", type = "string")
      if (options$percentagesRow)           crossTabMain$addColumnInfo(name = "type[row.proportions]",         title = "", type = "string")
      if (options$percentagesColumn)        crossTabMain$addColumnInfo(name = "type[col.proportions]",         title = "", type = "string")
      if (options$percentagesTotal)         crossTabMain$addColumnInfo(name = "type[total.proportions]",       title = "", type = "string")
      if (options$residualsUnstandardized)  crossTabMain$addColumnInfo(name = "type[unstandardized.residuals]",title = "", type = "string")
      if (options$residualsPearson)         crossTabMain$addColumnInfo(name = "type[pearson.residuals]",       title = "", type = "string")
      if (options$residualsStandardized)    crossTabMain$addColumnInfo(name = "type[standardized.residuals]",  title = "", type = "string")
    }

    .crossTabMainOvertitle(dataset, options, crossTabMain, analysis, counts.fp)

    # Totals columns
    totalTitle <- gettext("Total")

    if (options$marginShowTotals && (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn ||
         options$percentagesTotal || options$residualsUnstandardized || options$residualsPearson || options$residualsStandardized)) {
      if (options$countsObserved)          crossTabMain$addColumnInfo(name = "total[counts]",                   title = totalTitle, type = "number",  format = "sf:4;dp:2")
      if (options$countsExpected)          crossTabMain$addColumnInfo(name = "total[expected]",                 title = totalTitle, type = "number",  format = "sf:4;dp:2")
      if (options$percentagesRow)          crossTabMain$addColumnInfo(name = "total[row.proportions]",          title = totalTitle, type = "number",  format = "dp:1;pc")
      if (options$percentagesColumn)       crossTabMain$addColumnInfo(name = "total[col.proportions]",          title = totalTitle, type = "number",  format = "dp:1;pc")
      if (options$percentagesTotal)        crossTabMain$addColumnInfo(name = "total[total.proportions]",        title = totalTitle, type = "number",  format = "dp:1;pc")
      if (options$residualsUnstandardized) crossTabMain$addColumnInfo(name = "total[unstandardized.residuals]", title = totalTitle, type = "number",  format = "sf:4;dp:2")
      if (options$residualsPearson)        crossTabMain$addColumnInfo(name = "total[pearson.residuals]",        title = totalTitle, type = "number",  format = "sf:4;dp:2")
      if (options$residualsStandardized)   crossTabMain$addColumnInfo(name = "total[standardized.residuals]",   title = totalTitle, type = "number",  format = "sf:4;dp:2")
    } else if (options$marginShowTotals)
      if (options$countsObserved)          crossTabMain$addColumnInfo(name = "total[counts]",                   title = totalTitle, type = "integer")

    analysisContainer[["crossTabMain"]] <- crossTabMain
    analysis                            <- as.list(analysis)
    groupList                           <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                 <- try(.crossTabCountsRows(analysisContainer, analysis$rows, groupList, options, ready, counts.fp))

    if (sum(options$countsObserved, options$countsExpected, options$percentagesRow, options$percentagesColumn,
            options$percentagesTotal, options$residualsUnstandardized, options$residualsPearson, options$residualsStandardized) == 1)
      crossTabMain$addFootnote(.crossTabMainNote(options))

    .crossTabSetErrorOrFill(res, crossTabMain)
  }
}

#All the following .crossTabBlabla functions look suspiciously similar, they can probably be all merged to a very large extent...
.crossTabChisq <- function(jaspResults, dataset, options, analyses, ready) {
  if(!(options$chiSquared || options$chiSquaredContinuityCorrection || options$likelihoodRatio))
    return()

  for (i in 1:nrow(analyses)){
    analysis          <- analyses[i,]
    analysisContainer <- jaspResults[[.crossTabCreateContainerName(analysis)]]
    if (!is.null(analysisContainer[["crossTabChisq"]]))
      next

    # Create table
    crossTabChisq <- createJaspTable(title = gettext("Chi-Squared Tests"))
    crossTabChisq$dependOn(c("chiSquared", "chiSquaredContinuityCorrection", "likelihoodRatio", "vovkSellke"))
    crossTabChisq$showSpecifiedColumnsOnly <- TRUE
    crossTabChisq$position <- 2

    counts.fp <- .crossTabCountsFp(dataset, options)

    # Add columns to table
    .crossTabLayersColumns(crossTabChisq, analysis)

    if (options$chiSquared)                       .crossTabChisqAddColInfo(fold = "chiSquared",     crossTabChisq, options)
    if (options$chiSquaredContinuityCorrection)   .crossTabChisqAddColInfo(fold = "chiSquared-cc",  crossTabChisq, options)
    if (options$likelihoodRatio)                  .crossTabChisqAddColInfo(fold = "likelihood",     crossTabChisq, options)
                                                  .crossTabChisqAddColInfo(fold = "N",              crossTabChisq, options, counts.fp)

    if(options$vovkSellke){
      message <- gettextf("Vovk-Sellke Maximum  <em>p</em>-Ratio: Based the <em>p</em>-value,
      the maximum possible odds in favor of H%1$s over H%2$s equals %3$s
      (Sellke, Bayarri, & Berger, 2001).", "\u2081", "\u2080", "1/(-e <em>p</em> log(<em>p</em>)) for <em>p</em> \u2264 .37")
      crossTabChisq$addFootnote(message, symbol = "\u002A")
    }

    analysisContainer[["crossTabChisq"]]  <- crossTabChisq
    analysis                              <- as.list(analysis)
    groupList                             <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                   <- try(.crossTabTestsRows(analysisContainer, groupList$rows, groupList, options, ready, counts.fp))

    if (ready && !.crossTabIs2x2(table(dataset[[analysis[["columns"]]]], dataset[[analysis[["rows"]]]])))
      crossTabChisq$addFootnote(gettext("Continuity correction is available only for 2x2 tables."))

    .crossTabSetErrorOrFill(res, crossTabChisq)
  }
}

.crossTabOdds <- function(jaspResults, dataset, options, analyses, ready){
  if (!options$oddsRatio)
    return ()
  for (i in 1:nrow(analyses)) {
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[.crossTabCreateContainerName(analysis)]]
    if (!is.null(analysisContainer[["crossTabLogOdds"]]))
      next

    # Create table
    title <- if (options[["oddsRatioAsLogOdds"]]) gettext("Log Odds Ratio") else gettext("Odds Ratio")
    crossTabOdds <- createJaspTable(title = title)
    crossTabOdds$dependOn(c("oddsRatioAsLogOdds","oddsRatio", "oddsRatioCiLevel",
                            "oddsRatioAlternative", "rowOrder", "columnOrder"))
    crossTabOdds$showSpecifiedColumnsOnly <- TRUE
    crossTabOdds$position <- 3

    ci.label <- gettextf("%.0f%% Confidence Intervals", 100*options$oddsRatioCiLevel)

    # Add columns to table
    .crossTabLayersColumns( crossTabOdds, analysis)
    .crossTabOddsAddColInfo(crossTabOdds, fold = "oddsRatio",  ci.label, oddsTitle = "Estimate")
    .crossTabOddsAddColInfo(crossTabOdds, fold = "FisherTest", ci.label, oddsTitle = "Estimate")

    analysisContainer[["crossTabLogOdds"]] <- crossTabOdds
    analysis                               <- as.list(analysis)
    groupList                              <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis,
                                                                     ready) # Compute/get Group List

    # Add note for one-sided tests
    .crossTabOddsNote(crossTabOdds, groupList, options, ready)

    res <- try(.crossTabOddsRatioRows(analysisContainer, analysis$rows, groupList, options, ready))

    .crossTabSetErrorOrFill(res, crossTabOdds)
  }
}

.crossTabNominal <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$contingencyCoefficient && !options$phiAndCramersV && !options$lambda)
    return()

  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[.crossTabCreateContainerName(analysis)]]
    if (!is.null(analysisContainer[["crossTabNominal"]]))
      next
    # Create table
    crossTabNominal <- createJaspTable(title = gettext("Nominal"))
    crossTabNominal$dependOn(c("contingencyCoefficient", "phiAndCramersV", "lambda"))
    crossTabNominal$showSpecifiedColumnsOnly <- TRUE
    crossTabNominal$position <- 4
    # Add columns to table
    .crossTabLayersColumns(crossTabNominal, analysis)
    if (options$contingencyCoefficient)
      .crossTabNominalAddColInfo(crossTabNominal, "ContCoef")

    if (options$phiAndCramersV) {
      .crossTabNominalAddColInfo(crossTabNominal, "PhiCoef")
      .crossTabNominalAddColInfo(crossTabNominal, "CramerV")
    }

    if (options$lambda) {
      .crossTabNominalAddColInfo(crossTabNominal, "LambdaR")
      .crossTabNominalAddColInfo(crossTabNominal, "LambdaC")
      .crossTabNominalAddColInfo(crossTabNominal, "LambdaS")
    }

    analysisContainer[["crossTabNominal"]]  <- crossTabNominal
    analysis                                <- as.list(analysis)
    groupList                               <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                     <- try(.crossTabNominalRows(analysisContainer, analysis$rows, groupList, options, ready))

    .crossTabSetErrorOrFill(res, crossTabNominal)
  }
}

.crossTabGamma <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$gamma)
    return()

  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[.crossTabCreateContainerName(analysis)]]
    if (!is.null(analysisContainer[["crossTabGamma"]]))
      next

    # Create table
    crossTabGamma <- createJaspTable(title = gettext("Ordinal Gamma"))
    crossTabGamma$dependOn(c("gamma", "rowOrder", "columnOrder"))
    crossTabGamma$showSpecifiedColumnsOnly <- TRUE
    crossTabGamma$position <- 5

    ci.label <- gettextf("95%% Confidence Intervals")

    # Add columns to table
    .crossTabLayersColumns(crossTabGamma, analysis)
    crossTabGamma$addColumnInfo(name = "value[gammaCoef]", title = gettext("Gamma"),          type = "number")
    crossTabGamma$addColumnInfo(name = "Sigma[gammaCoef]", title = gettext("Standard Error"), type = "number", format = "dp:3")
    crossTabGamma$addColumnInfo(name = "low[gammaCoef]",   title = gettext("Lower"),          type = "number", format = "dp:3", overtitle = ci.label)
    crossTabGamma$addColumnInfo(name = "up[gammaCoef]",    title = gettext("Upper"),          type = "number", format = "dp:3", overtitle = ci.label)

    analysisContainer[["crossTabGamma"]]  <- crossTabGamma
    analysis                              <- as.list(analysis)
    groupList                             <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                   <- try(.crossTabGammaRows(analysisContainer, analysis$rows, groupList, options, ready))

    .crossTabSetErrorOrFill(res, crossTabGamma)
  }
}

.crossTabKendallTau <- function(jaspResults, dataset, options, analyses, ready) {
  if (!options$kendallsTauB)
    return()

  for (i in 1:nrow(analyses)){
    analysis <- analyses[i,]
    analysisContainer <- jaspResults[[.crossTabCreateContainerName(analysis)]]
    if (!is.null(analysisContainer[["crossTabKendallTau"]]))
      next

    # Create table
    crossTabKendallTau <- createJaspTable(title = "Kendall's Tau")
    crossTabKendallTau$dependOn(c("kendallsTauB", "vovkSellke", "rowOrder", "columnOrder"))
    crossTabKendallTau$showSpecifiedColumnsOnly <- TRUE
    crossTabKendallTau$position <- 6

    # Add columns to table
    .crossTabLayersColumns(crossTabKendallTau, analysis)
                                crossTabKendallTau$addColumnInfo(name = "value[kTauB]",     title = gettext("Kendall's Tau-b "),    type = "number")
                                crossTabKendallTau$addColumnInfo(name = "statistic[kTauB]", title = gettext("Z"),                   type = "number", format = "dp:3")
                                crossTabKendallTau$addColumnInfo(name = "p[kTauB]",         title = gettext("p"),                   type = "pvalue")
    if (options$vovkSellke)  crossTabKendallTau$addColumnInfo(name = "MPR[kTauB]",       title = gettextf("VS-MPR%s", "\u002A"), type = "number")

    analysisContainer[["crossTabKendallTau"]] <- crossTabKendallTau
    analysis                                  <- as.list(analysis)
    groupList                                 <- .crossTabComputeGroups(dataset, options, analysisContainer, analysis, ready) # Compute/get Group List
    res                                       <- try(.crossTabKendallsTauRows(analysisContainer, analysis$rows, groupList, options, ready))

    .crossTabSetErrorOrFill(res, crossTabKendallTau)
  }
}

# Table functions
.crossTabLayersColumns <- function(table, analysis) {
  if ((length(analysis)) >= 3)
    for (j in (length(analysis)):3)
      table$addColumnInfo(name = analysis[[j]], type = "string", combine = TRUE)
}

.crossTabMainOvertitle <- function(dataset, options, table, analysis, counts.fp) {

  lvls                 <- c("a", "b")
  useColumnNameAsTitle <- FALSE
  overTitle            <- "."


  if(analysis$columns != "") {
    if (is.factor(dataset[[ .v(analysis$columns) ]] ))
          lvls <- levels(dataset[[ .v(analysis$columns) ]])
    else  lvls <- unique(dataset[[ .v(analysis$columns) ]])

    if (options$columnOrder == "descending")
        lvls <- rev(lvls)

    overTitle            <- unlist(analysis$columns)
    useColumnNameAsTitle <- TRUE
  }

  for (column.name in lvls) {

    if(useColumnNameAsTitle) myTitle <- column.name
    else                     myTitle <- "."

    pr.format <- NULL
    pr.type   <- "integer"
    if (counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn ||
        options$percentagesTotal || options$residualsUnstandardized || options$residualsPearson ||
        options$residualsStandardized)
    {
      pr.format <- "sf:4;dp:2"
      pr.type   <- "number"
    }

    if (options$countsObserved)          table$addColumnInfo(name = paste0(column.name,"[counts]"),                  title = myTitle, type = pr.type,  format = pr.format,   overtitle = overTitle)
    if (options$countsExpected)          table$addColumnInfo(name = paste0(column.name,"[expected]"),                title = myTitle, type = "number", format = "sf:4;dp:2", overtitle = overTitle)
    if (options$percentagesRow)          table$addColumnInfo(name = paste0(column.name,"[row.proportions]"),         title = myTitle, type = "number", format = "dp:1;pc",   overtitle = overTitle)
    if (options$percentagesColumn)       table$addColumnInfo(name = paste0(column.name,"[col.proportions]"),         title = myTitle, type = "number", format = "dp:1;pc",   overtitle = overTitle)
    if (options$percentagesTotal)        table$addColumnInfo(name = paste0(column.name,"[total.proportions]"),       title = myTitle, type = "number", format = "dp:1;pc",   overtitle = overTitle)
    if (options$residualsUnstandardized) table$addColumnInfo(name = paste0(column.name,"[unstandardized.residuals]"),title = myTitle, type = "number", format = "sf:4;dp:2", overtitle = overTitle)
    if (options$residualsPearson)        table$addColumnInfo(name = paste0(column.name,"[pearson.residuals]"),       title = myTitle, type = "number", format = "sf:4;dp:2", overtitle = overTitle)
    if (options$residualsStandardized)   table$addColumnInfo(name = paste0(column.name,"[standardized.residuals]"),  title = myTitle, type = "number", format = "sf:4;dp:2", overtitle = overTitle)
  }
}

.crossTabSetErrorOrFill <- function(res, table) {
  if(isTryError(res))
    table$setError(.extractErrorMessage(res))
  else
    for (level in 1:length(res$rows))
      table$addRows(res$rows[[level]], rowNames = res$rownames)
}

.crossTabChisqAddColInfo <- function(fold, table, options, counts.fp = FALSE) {
  if(fold == "N" && counts.fp == FALSE) valueFoldType <- "integer"
  else                                  valueFoldType <- "number"

                              table$addColumnInfo(name = paste0("type[",  fold, "]"),   title = "",                             type = "string")
                              table$addColumnInfo(name = paste0("value[", fold, "]"),   title = gettext("Value"),               type = valueFoldType)
                              table$addColumnInfo(name = paste0("df[",    fold, "]"),   title = gettext("df"),                  type = "integer")
                              table$addColumnInfo(name = paste0("p[",     fold, "]"),   title = gettext("p"),                   type = "pvalue")
  if (options$vovkSellke)  table$addColumnInfo(name = paste0("MPR[",   fold, "]"),   title = gettextf("VS-MPR%s", "\u002A"), type = "number")
}

.crossTabOddsAddColInfo <- function(table, fold, ci.label, oddsTitle) {
                            table$addColumnInfo(name = paste0("type[",  fold, "]"),   title = "",                        type = "string")
                            table$addColumnInfo(name = paste0("value[", fold, "]"),   title = oddsTitle,                 type = "number")
                            table$addColumnInfo(name = paste0("low[",   fold, "]"),   title = gettext("Lower"),          type = "number", overtitle = ci.label, format = "dp:3")
                            table$addColumnInfo(name = paste0("up[",    fold, "]"),   title = gettext("Upper"),          type = "number", overtitle = ci.label, format = "dp:3")
                            table$addColumnInfo(name = paste0("p[",     fold, "]"),   title = gettext("p"),              type = "pvalue")
}

.crossTabNominalAddColInfo <- function(table, fold){
  table$addColumnInfo(name = paste0("type[",  fold, "]"),  title = "",                type = "string")
  table$addColumnInfo(name = paste0("value[", fold, "]"),  title = gettext("Value"),  type = "number")
}

.crossTabLayerNames <- function(row, group) {
  for (layer in names(group)) {
    level <- group[[layer]]

    if (level == "")
      row[[layer]] <- "Total"
    else
      row[[layer]] <- level
  }
  return(row)
}

.crossTabComputeGroups <- function(dataset, options, analysisContainer, analysis, ready) {
  if(!is.null(analysisContainer[["groupList"]]))
    return(analysisContainer[["groupList"]]$object)
  groupsList <- list() #list of groups, group.matrices
  counts.var <- options$counts
  if (counts.var == "")
    counts.var <- NULL
  if(ready) {
    all.vars   <- c(unlist(analysis), counts.var)
    subdataset <- subset(dataset, select = .v(all.vars))
  }
  else
    subdataset <- dataset
  # the following creates a 'groups' list
  # a 'group' represents a combinations of the levels from the layers
  # if no layers are specified, groups is null
  if (length(analysis) >= 3) {  # if layers are specified

    lvls <- levels(subdataset[[ .v(analysis[[3]]) ]])

    if (length(lvls) < 2)
      lvls <- ""
    else
      lvls <- c(lvls, "")  # blank means total

    # here we create all combinations of the levels from the layers
    # it is easiest to do this with a data frame
    # at the end we convert this to a list of rows

    groups <- data.frame(lvls, stringsAsFactors=FALSE)
    names(groups) <- analysis[[3]]

    if (length(analysis) >= 4) {

      for (j in 4:(length(analysis))) {
        lvls <- levels(subdataset[[ .v(analysis[[j]]) ]])
        lvls <- c(lvls, "")  # blank means total

        groups <- cbind(rep(lvls, each=dim(groups)[1]), groups,
                        stringsAsFactors=FALSE)
        names(groups)[1] <- analysis[[j]]
      }
    }
    # convert all the combinations to a list of rows
    groups <- .dataFrameToRowList(groups)

  } else  # if layers are not specified
    groups <- NULL
  groupsList$groups <- groups
  if (!is.null(counts.var))
    counts <- stats::na.omit(subdataset[[ .v(counts.var) ]])
  grp.mat <- .crossTabGroupMatrices(subdataset, analysis$rows,
                                      analysis$columns, groups,
                                      counts.var,
                                      options$rowOrder   =="descending",
                                      options$columnOrder=="descending",
                                      ready)
  groupsList$group.matrices <- grp.mat
  analysisContainer[["groupList"]] <- createJaspState(groupsList)
  analysisContainer[["groupList"]]$dependOn(c("rowOrder", "columnOrder"))
  return(groupsList)
}

.crossTabCountsMatrixToRow <- function(type.matrix, counts.matrix, j, type) {
  row.list              <- as.list(type.matrix[j,])
  total                 <- paste0("total[", type, "]")
  row.list[[total]]     <- NULL
  names(row.list)       <- paste0(names(row.list), "[", type, "]")

  if(type == "expected" || type == "row.proportions" || type == "total.proportions"){
    if (is.character(type.matrix[1,1])) row.list[[total]] <- ""
    else                                row.list[[total]] <- base::sum(type.matrix[j,])
  }

  if(type == "col.proportions"){
    if (is.character(type.matrix[1,1])) row.list[[total]] <- ""
    else {
      row.sum  <- margin.table(counts.matrix, 1)
      row.prop <- as.list(prop.table(row.sum))
      row.list[[total]] <- row.prop[[j]]
    }
  }
  row.type              <- list()
  row.title             <- paste0("type[", type, "]")
  row.type[[row.title]] <- switch(type,
                                  "expected" = gettext("Expected count"),
                                  "row.proportions" = gettextf(" %% within row"),
                                  "col.proportions" = gettextf(" %% within column"),
                                  "total.proportions" = gettextf(" %% of total"),
                                  "unstandardized.residuals" = gettext("Unstandardized residuals"),
                                  "pearson.residuals" = gettext("Pearson residuals"),
                                  "standardized.residuals" = gettext("Standardized residuals"))
  row.list              <- c(row.type, row.list)
  return(row.list)
}

.crossTabCountsColumnTotalsMatrixToRow <- function(matrix, counts.matrix, type) {
  if (is.character(matrix[1,1]))
    return(NULL)
  else {
    if(type %in% c("expected", "col.proportions", "total.proportions"))
      row     <- colSums(matrix)
    else if(type == "row.proportions"){
      m       <- margin.table(counts.matrix, 2)
      rowprop <- prop.table(m)
      row     <- rowprop
    }
  }

  row <- as.list(row)
  names(row) <- paste0(names(row),"[", type, "]")

  if (is.character(matrix[1,1]))
    row[[paste0("total[", type, "]")]]   <- ""
  else {
    if(type == "col.proportions") {
      row.sum                            <- margin.table(matrix, 1)
      row.prop                           <- prop.table(row.sum)
      col.prop                           <- sum(row.prop)
      row[[paste0("total[", type, "]")]] <- col.prop
    } else if(type == "row.proportions")
      row[[paste0("total[", type, "]")]] <- sum(rowprop)
    else
      row[[paste0("total[", type, "]")]] <- sum(matrix)
  }
  row.type              <- list()
  row.title             <- paste0("type[", type, "]")
  row.type[[row.title]] <- switch(type,
                                  "expected" = gettext("Expected count"),
                                  "row.proportions" = gettextf(" %% within row"),
                                  "col.proportions" = gettextf(" %% within column"),
                                  "total.proportions" = gettextf(" %% of total"))
  row                   <- c(row.type, row)
  return(row)
}

.crossTabNominalMatrixToRow <- function(analysisContainer, row, counts.matrix, type, ready, rowname) {

  switch(type,
  ContCoef= {
    row[["type[ContCoef]"]] <- gettext("Contingency coefficient")
    val                     <- "contingency"
  },
  PhiCoef= {
    row[["type[PhiCoef]"]]  <- gettext("Phi-coefficient")
    val                     <- "phi"
  },
  CramerV= {
    row[["type[CramerV]"]]  <- gettext("Cramer's V ")
    val                     <- "cramer"
  })

  if (ready) {

    chi.result <- try({

      res <- vcd::assocstats(counts.matrix)
      # vcd::assocstats only returns absolute the value
      if (val == "phi" && (counts.matrix[1, 1] * counts.matrix[2, 2] - counts.matrix[1, 2] * counts.matrix[2, 1]) < 0) {
        res[[val]] <- -1 * res[[val]]
      }
      res
    })

    colName <- paste0("value[", type, "]")

    if (isTryError(chi.result))
      row[[colName]] <- NaN
    else if (is.na(chi.result[[val]])) {
      row[[colName]] <- NaN

      message <- if (val == "phi" && !all(dim(counts.matrix) == 2L))
        gettext("Phi coefficient is only available for 2 by 2 contingency Tables")
      else
        gettext("Value could not be calculated - At least one row or column contains all zeros")

      analysisContainer[["crossTabNominal"]]$addFootnote(message, rowNames = rowname, colNames = colName)
    } else
      row[[colName]] <- chi.result[[val]]
  }

  return(row)
}

.crossTabNominalLambdaMatrixToRow <- function(analysisContainer, row, counts.matrix, ready) {
  row[["type[LambdaR]"]] <- gettext("Lambda (rows)")
  row[["type[LambdaC]"]] <- gettext("Lambda (columns)")
  row[["type[LambdaS]"]] <- gettext("Lambda (symmetric)")

  if(ready) {
    n <- sum(counts.matrix)
    csum <- colSums(counts.matrix)
    rsum <- rowSums(counts.matrix)
    rmax <- apply(counts.matrix, 1, max)
    cmax <- apply(counts.matrix, 2, max)
    max.rsum <- max(rsum)
    max.csum <- max(csum)

    row[["value[LambdaR]"]] <- (sum(rmax) - max.csum)/(n - max.csum)
    row[["value[LambdaC]"]] <- (sum(cmax) - max.rsum)/(n - max.rsum)
    row[["value[LambdaS]"]] <- 0.5 * (row[["value[LambdaR]"]] + row[["value[LambdaC]"]])
  } else {
    row[["value[LambdaR]"]] <- row[["value[LambdaC]"]] <- row[["value[LambdaCS]"]] <- NA
  }

  return(row)
}

.crossTabCountsFp <- function(dataset, options) {
  # check if the counts column has floating point numbers
  if (options$counts != "") {
    counts <- dataset[[ .v(options$counts) ]]
    return(!all((counts %% 1) == 0))
  }
  return(FALSE)
}

.crossTabOddsNote <- function(crossTabOdds, groupList, options, ready){
  if(ready){
    row_group_levels <- dimnames(groupList$group.matrices[[1]])[[1]]
    col_group_levels <- dimnames(groupList$group.matrices[[1]])[[2]]
    if(!options$oddsRatioAsLogOdds) {
      message <- gettextf("Odds ratio indicates the following: <br> (Ratio > 1): Group <em>%1$s</em> is more likely in group <em>%2$s</em><br>(Ratio < 1): Group <em>%1$s</em> is less likely in group <em>%2$s</em>",
                          col_group_levels[1], row_group_levels[1])
      crossTabOdds$addFootnote(message)
    }
    else {
      message <- gettextf("Log odds ratio indicates the following:<br>(Ratio > 0): Group <em>%1$s</em> is more likely in group <em>%2$s</em> <br>(Ratio < 0): Group <em>%1$s</em> is less likely in group <em>%2$s</em>",
                          col_group_levels[1], row_group_levels[1])
      crossTabOdds$addFootnote(message)
    }
    if(length(groupList$group.matrices) >= 1  & options[["oddsRatioAlternative"]] != "twoSided"){
      if(options[["oddsRatioAlternative"]] == "less") lessIsMore <- gettext("is less than")
      else                                      lessIsMore <- gettext("is greater than")

      message <- gettextf("For all tests, the alternative hypothesis specifies that group <em>%1$s</em> %2$s <em>%3$s</em>.", row_group_levels[1], lessIsMore, row_group_levels[2])
      crossTabOdds$addFootnote(message)
    }
  }
}

.crossTabRowName <- function(groups) {
  rowNames <- lapply(groups, function(x) paste(c("row", unlist(x)), collapse="-"))
  return(unlist(rowNames))
}

.crossTabIs2x2 <- function(counts.matrix) {
  return(all(dim(counts.matrix) == 2L))
}

.crossTabMainNote <- function(options) {

  if (options$countsObserved)               return(gettext("Each cell displays the observed counts"))
  else if (options$countsExpected)          return(gettext("Each cell displays the expected counts"))
  else if (options$percentagesRow)          return(gettext("Each cell displays the row percentages"))
  else if (options$percentagesColumn)       return(gettext("Each cell displays column percentages"))
  else if (options$percentagesTotal)        return(gettext("Each cell displays total percentages"))
  else if (options$residualsUnstandardized) return(gettext("Each cell displays unstandardized residuals"))
  else if (options$residualsPearson)        return(gettext("Each cell displays Pearson residuals"))
  else if (options$residualsStandardized)   return(gettext("Each cell displays standardized residuals"))

  stop("unreachable point in .crossTabMainNote was reached!")
}

# Group matrix
.crossTabGroupMatrices <- function(dataset, rows, columns, groups, counts = NULL,
                                   rowOrderDescending = FALSE,
                                   columnOrderDescending = FALSE, ready) {
  # this creates count matrices for each of the groups
  matrices <- list()

  if (is.null(groups)) {

    if (!ready) {

      row.levels <- c(" .", " . ")
      col.levels <- c(" .", " . ")
      if (rows != "")
        row.levels <- levels(dataset[[ .v(rows) ]])
      if (columns != "")
        col.levels <- levels(dataset[[ .v(columns) ]])
      ss.matrix <- matrix(0,
                          nrow     = length(row.levels),
                          ncol     = length(col.levels),
                          dimnames = list(row.levels, col.levels))
    } else if (is.null(counts)) {
      ss.dataset <- subset(dataset, select = .v(c(rows, columns)))
      ss.table   <- table(ss.dataset)
      ss.matrix  <- matrix(ss.table, nrow = dim(ss.table)[1],
                           ncol = dim(ss.table)[2],
                           dimnames = dimnames(ss.table))
    } else {
      ss.dataset <- subset(dataset, select = .v(c(rows, columns, counts)))
      ss.matrix  <- tapply(ss.dataset[[ .v(counts) ]],
                           list(ss.dataset[[ .v(rows) ]],
                                ss.dataset[[ .v(columns) ]]),
                           sum)
      ss.matrix[is.na(ss.matrix)] <- 0
    }
    if (rowOrderDescending)
      ss.matrix <- apply(ss.matrix, 2, rev)
    if (columnOrderDescending)
      ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]

    ss.matrix[is.na(ss.matrix)] <- 0

    matrices[[1]] <- ss.matrix
  } else {
    for (group in groups) {
      group <- group[group != ""]
      if (!ready) {} # do nothing
      else if (length(group) == 0)
        ss.dataset <- subset(dataset, select = .v(c(rows, columns, counts)))
      else {
        ss.filter.string <- paste(.v(names(group)), "==\"", group, "\"",
                                        sep = "", collapse = "&")
        ss.expression    <- parse(text = ss.filter.string)
        ss.dataset	     <- subset(dataset,
                                   select = .v(c(rows, columns, counts)),
                                   subset = eval(ss.expression))
      }
      if (!ready)
        ss.matrix <- matrix(c(0,0,0,0), nrow = 2, ncol = 2)
      else if (is.null(counts)) {
        ss.table  <- table(ss.dataset)
        ss.matrix <- matrix(ss.table,
                            nrow     = dim(ss.table)[1],
                            ncol     = dim(ss.table)[2],
                            dimnames = dimnames(ss.table))
      } else {
        ss.matrix <- tapply(ss.dataset[[ .v(counts) ]],
                            list(ss.dataset[[ .v(rows) ]],
                                 ss.dataset[[ .v(columns) ]]),
                            sum)
      }

      ss.matrix[is.na(ss.matrix)] <- 0

      if (rowOrderDescending)
        ss.matrix <- apply(ss.matrix, 2, rev)
      if (columnOrderDescending)
        ss.matrix <- ss.matrix[ , ncol(ss.matrix):1]
      matrices[[length(matrices) + 1]] <- ss.matrix
    }
  }
  return(matrices)
}

# Table Results
.crossTabCountsRows <- function(analysisContainer, var.name, groupList, options, ready, counts.fp) {
  if(!is.null(analysisContainer[["resultsMain"]]))
    return()
  counts.rows    <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL

    rows                          <- list()
    row.count                     <- list()
    row.count[["type[counts]"]]   <- gettext("Count")

    if (ready) {

      chisqResults <- try({
        stats::chisq.test(counts.matrix, correct = FALSE)
      })

      if (isTryError(chisqResults)) {
        expected.matrix                    <- counts.matrix
        expected.matrix[,]                 <- ""
        unstandardized.residuals.matrix    <- counts.matrix
        unstandardized.residuals.matrix[,] <- ""
        pearson.residuals.matrix           <- counts.matrix
        pearson.residuals.matrix[,]        <- ""
        standardized.residuals.matrix      <- counts.matrix
        standardized.residuals.matrix[,]   <- ""
      } else {
        expected.matrix                    <- chisqResults$expected
        unstandardized.residuals.matrix    <- counts.matrix - chisqResults$expected
        pearson.residuals.matrix           <- chisqResults$residuals
        standardized.residuals.matrix      <- chisqResults$stdres
        }

      row.proportions.matrix <- try({
        prop.table(counts.matrix, 1)
      })
      if (isTryError(row.proportions.matrix)) {
        row.proportions.matrix    <- counts.matrix
        row.proportions.matrix[,] <- ""
      }

      col.proportions.matrix <- try({
        prop.table(counts.matrix, 2)
      })
      if (isTryError(col.proportions.matrix)) {
        col.proportions.matrix    <- counts.matrix
        col.proportions.matrix[,] <- ""
      }

      total.proportions.matrix <- try({
        prop.table(counts.matrix, margin = NULL)
      })
      if (isTryError(total.proportions.matrix)) {
        total.proportions.matrix    <- counts.matrix
        total.proportions.matrix[,] <- ""
      }

    } else {
      expected.matrix                  <- counts.matrix
      row.proportions.matrix           <- counts.matrix
      col.proportions.matrix           <- counts.matrix
      total.proportions.matrix         <- counts.matrix
      unstandardized.residuals.matrix  <- counts.matrix
      pearson.residuals.matrix         <- counts.matrix
      standardized.residuals.matrix    <- counts.matrix
    }

    for (j in 1:dim(counts.matrix)[[1]]) {

      if (ready) {

        row        <- as.list(counts.matrix[j,])
        names(row) <- paste0(names(row),"[counts]")
        sum        <- sum(counts.matrix[j,])

        if(counts.fp || options$countsExpected || options$percentagesRow || options$percentagesColumn ||
           options$percentagesTotal || options$residualsUnstandardized || options$residualsPearson ||
           options$residualsStandardized)
          row[["total[counts]"]] <- sum
        else  row[["total[counts]"]] <- as.integer(sum)

        if (options$countsObserved)
          row <- c(row.count, row)

        if (options$countsExpected)
          row <- c(row, .crossTabCountsMatrixToRow(expected.matrix,
                                                   counts.matrix,
                                                   j,
                                                   "expected"))
        if (options$percentagesRow)
          row <- c(row, .crossTabCountsMatrixToRow(row.proportions.matrix,
                                                   counts.matrix,
                                                   j,
                                                   "row.proportions"))
        if (options$percentagesColumn)
          row <- c(row, .crossTabCountsMatrixToRow(col.proportions.matrix,
                                                   counts.matrix,
                                                   j,
                                                   "col.proportions"))
        if (options$percentagesTotal)
          row <- c(row, .crossTabCountsMatrixToRow(total.proportions.matrix,
                                                   counts.matrix,
                                                   j,
                                                   "total.proportions"))
        if (options$residualsUnstandardized)
          row <- c(row, .crossTabCountsMatrixToRow(unstandardized.residuals.matrix,
                                                   counts.matrix,
                                                   j,
                                                   "unstandardized.residuals"))
        if (options$residualsPearson)
          row <- c(row, .crossTabCountsMatrixToRow(pearson.residuals.matrix,
                                                   counts.matrix,
                                                   j,
                                                   "pearson.residuals"))
        if (options$residualsStandardized)
          row <- c(row, .crossTabCountsMatrixToRow(standardized.residuals.matrix,
                                                   counts.matrix,
                                                   j,
                                                   "standardized.residuals"))
      } else
        row <- list()
      row[[var.name]] <- dimnames(counts.matrix)[[1]][j]
      row <- .crossTabLayerNames(row, group)

      if (j == 1 && !options$countsObserved && !options$countsExpected && !options$percentagesRow &&
          !options$percentagesCol &&  !options$percentagesTotal &&
          !options$residualsUnstandardized && !options$residualsPearson && !options$residualsStandardized)
        row[[".isNewGroup"]] <- TRUE
      rows[[length(rows) + 1]] <- row
    }

    if (ready) {

      row         <- as.list(colSums(counts.matrix))
      names(row)  <- paste0(names(row),"[counts]")
      sum         <- sum(counts.matrix)

      if(options$marginShowTotals && (counts.fp || options$countsExpected || options$percentagesRow ||
         options$percentagesColumn || options$percentagesTotal || options$residualsUnstandardized ||
         options$residualsPearson || options$residualsStandardized))
        row[["total[counts]"]] <- sum
      else  if (options$marginShowTotals)
        row[["total[counts]"]] <- as.integer(sum)

      if (options$countsObserved)
        row <- c(row.count, row)

      if (options$countsExpected) {
        expected  <- .crossTabCountsColumnTotalsMatrixToRow(expected.matrix, counts.matrix, type = "expected")
        row       <- c(row,  expected)
      }

      if (options$percentagesRow) {
        row.proportions <- .crossTabCountsColumnTotalsMatrixToRow(row.proportions.matrix, counts.matrix, type = "row.proportions")
        row             <- c(row,  row.proportions)
      }

      if (options$percentagesColumn) {
        col.proportions <- .crossTabCountsColumnTotalsMatrixToRow(col.proportions.matrix, counts.matrix, type = "col.proportions")
        row             <- c(row,  col.proportions)
      }

      if (options$percentagesTotal) {
        total.proportions <- .crossTabCountsColumnTotalsMatrixToRow(total.proportions.matrix, counts.matrix, type = "total.proportions")
        row               <- c(row,  total.proportions)
      }



    } else
      row <- list()

    if(var.name != "")
      row[[var.name]] <- gettext("Total")

    if (!(options$countsObserved || options$countsExpected || options$percentagesRow || options$percentagesCol ||
          options$percentagesTotal || options$residualsUnstandardized || options$residualsPearson ||
          options$residualsStandardized))
      row[[".isNewGroup"]] <- TRUE

    if (options$marginShowTotals) {
      row                       <- .crossTabLayerNames(row, group)
      rows[[length(rows) + 1]]  <- row
    }
    counts.rows               <- c(counts.rows, rows)
  }

  main                                <- createJaspState(counts.rows)
  table                               <- analysisContainer[["crossTabMain"]]
  analysisContainer[["resultsMain"]]  <- main

  main$dependOn(optionsFromObject = table)

  return(list(rows = counts.rows, rownames = row.rownames))
}

.crossTabTestsRows <- function(analysisContainer, var.name, groupList, options, ready, counts.fp) {
  tests.rows     <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {

    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL

    row <- list()
    row[["type[N]"]] <- gettext("N")
    row[["df[N]"]]   <- ""
    row[["p[N]"]]    <- ""
    row[["MPR[N]"]]  <- ""

    if (ready){
      sum <- sum(counts.matrix)
      if(counts.fp)
        row[["value[N]"]] <- sum
      else
        row[["value[N]"]] <- as.integer(sum)
    }
    else
      row[["value[N]"]] <- "."

    if (options$chiSquared) {

      row[["type[chiSquared]"]] <- "\u03A7\u00B2"

      if (ready) {

        chi.result <- try({
          chi.result <- stats::chisq.test(counts.matrix, correct = FALSE)
        })

        if (isTryError(chi.result) || is.na(chi.result$statistic)) {
          row[["value[chiSquared]"]] <- NaN
          row[["df[chiSquared]"]]    <- " "
          row[["p[chiSquared]"]]     <- " "
          row[["MPR[chiSquared]"]]   <- " "

          message <- gettextf("%s could not be calculated - At least one row or column contains all zeros", "\u03A7\u00B2")
          analysisContainer[["crossTabChisq"]]$addFootnote(message, rowNames = row.rownames[g], colNames = "value[chiSquared]")
        } else {
          row[["value[chiSquared]"]] <- unname(chi.result$statistic)
          row[["df[chiSquared]"]]    <- unname(chi.result$parameter)
          row[["p[chiSquared]"]]     <- unname(chi.result$p.value)
          row[["MPR[chiSquared]"]]   <- VovkSellkeMPR(row[["p[chiSquared]"]])
        }
      } else
        row[["value[chiSquared]"]] <- "."
    }

    if (options$chiSquaredContinuityCorrection) {

      row[["type[chiSquared-cc]"]] <- gettextf("%s continuity correction", "\u03A7\u00B2")

      if (ready && .crossTabIs2x2(counts.matrix)) {

        chi.result <- try({
          chi.result <- stats::chisq.test(counts.matrix)
          #row <- list(Method = "Pearson's Chi-squared",
                       #X2 = unname(chi$statistic),
                       #df = unname(chi$parameter),
                       #p  = chi$p.value)
        })

        if (isTryError(chi.result) || is.na(chi.result$statistic)) {
          row[["value[chiSquared-cc]"]] <- NaN
          row[["df[chiSquared-cc]"]]    <- " "
          row[["p[chiSquared-cc]"]]     <- " "
          row[["MPR[chiSquared-cc]"]]   <- " "

          message <- gettextf("%s could not be calculated - At least one row or column contains all zeros", "\u03A7\u00B2")
          analysisContainer[["crossTabChisq"]]$addFootnote(message, rowNames = row.rownames[g], colNames = "value[chiSquared-cc]")
        } else {
          row[["value[chiSquared-cc]"]] <- unname(chi.result$statistic)
          row[["df[chiSquared-cc]"]]    <- unname(chi.result$parameter)
          pVal                          <- unname(chi.result$p.value)
          row[["p[chiSquared-cc]"]]     <- pVal
          row[["MPR[chiSquared-cc]"]]   <- VovkSellkeMPR(pVal)
        }

      } else
        row[["value[chiSquared-cc]"]] <- "."
    }

    if (options$likelihoodRatio) {

      row[["type[likelihood]"]] <- gettext("Likelihood ratio")

      if (ready) {

        chi.result <- try(vcd::assocstats(counts.matrix))

        if (isTryError(chi.result)) {
          row[["value[likelihood]"]] <- NaN
          row[["df[likelihood]"]]    <- ""
          row[["p[likelihood]"]]     <- ""
          if (options$vovkSellke)
            row[["MPR[likelihood]"]]   <- ""
        } else {
          row[["value[likelihood]"]] <- chi.result$chisq_tests[1]
          row[["df[likelihood]"]]    <- chi.result$chisq_tests[3]
          pVal                       <- chi.result$chisq_tests[5]
          row[["p[likelihood]"]]     <- pVal
          if (options$vovkSellke)
            row[["MPR[likelihood]"]]   <- VovkSellkeMPR(pVal)
        }
      } else
        row[["value[likelihood]"]] <- "."
    }
    row <- .crossTabLayerNames(row, group)
    tests.rows[[length(tests.rows) + 1]] <- row
  }
  return(list(rows = tests.rows, rownames = row.rownames))
}

.crossTabOddsRatioRows <- function(analysisContainer, var.name, groupList, options, ready) {
  odds.ratio.rows <- list()
  group.matrices  <- groupList$group.matrices
  groups          <- groupList$groups
  row.rownames    <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))
      group <- groups[[g]]
    else
      group <- NULL

    row <- list()
    row[["type[oddsRatio]"]] <- analysisContainer[["crossTabLogOdds"]]$title
    if (ready) {
      if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {
        row[["value[oddsRatio]"]] <- NaN
        row[["low[oddsRatio]"]]   <- ""
        row[["up[oddsRatio]"]]    <- ""
        row[["p[oddsRatio]"]]     <- ""
        stop(gettext("Odds ratio restricted to 2 x 2 tables"))
      } else {
        chi.result <- try({
          chi.result  <- vcd::oddsratio(counts.matrix)
          level       <- options$oddsRatioCiLevel
          CI          <- stats::confint(chi.result, level = level)
          if (options[["oddsRatioAsLogOdds"]]){
            OR          <- unname(chi.result$coefficients)
            CI.low      <- CI[1]
            CI.high     <- CI[2]
          } else {
            OR          <- exp(unname(chi.result$coefficients))
            CI.low      <- exp(CI[1])
            CI.high     <- exp(CI[2])
          }
        })

        if (isTryError(chi.result))
          row[["value[oddsRatio]"]] <- NaN
        else if (is.na(chi.result))
          row[["value[oddsRatio]"]] <- NaN
        else {
          row[["value[oddsRatio]"]] <- OR
          row[["low[oddsRatio]"]]   <- CI.low
          row[["up[oddsRatio]"]]    <- CI.high
          row[["p[oddsRatio]"]]     <- ""
        }
        row[["value[oddsRatio]"]] <- OR
        row[["low[oddsRatio]"]]   <- CI.low
        row[["up[oddsRatio]"]]    <- CI.high
        row[["p[oddsRatio]"]]     <- ""
      }
    }

    row[["type[FisherTest]"]] <- gettext("Fisher's exact test ")
    if (ready) {

      if ( ! identical(dim(counts.matrix),as.integer(c(2,2)))) {
        row[["value[FisherTest]"]] <- NaN
        row[["low[FisherTest]"]]   <- ""
        row[["up[FisherTest]"]]    <- ""
        row[["p[FisherTest]"]]     <- ""
      } else {
        chi.result <- try({
          conf.level  <- options$oddsRatioCiLevel
          alternative <- if(options[["oddsRatioAlternative"]] == "twoSided") "two.sided" else options[["oddsRatioAlternative"]]
          chi.result  <- stats::fisher.test(counts.matrix, conf.level = conf.level, alternative = alternative)
          if (options[["oddsRatioAsLogOdds"]]){
            OR          <- log(unname(chi.result$estimate))
            CI.low      <- log(chi.result$conf.int[1])
            CI.high     <- log(chi.result$conf.int[2])
            p           <- chi.result$p.value
          } else {
            OR          <- unname(chi.result$estimate)
            CI.low      <- chi.result$conf.int[1]
            CI.high     <- chi.result$conf.int[2]
            p           <- chi.result$p.value
          }
        })

        if (isTryError(chi.result))
          row[["value[FisherTest]"]] <- NaN
        else if (is.na(chi.result))
          row[["value[FisherTest]"]] <- NaN
        else {
          row[["value[FisherTest]"]] <- OR
          row[["low[FisherTest]"]]   <- CI.low
          row[["up[FisherTest]"]]    <- CI.high
          row[["p[FisherTest]"]]     <- p
        }
        row[["value[FisherTest]"]]   <- OR
        row[["low[FisherTest]"]]     <- CI.low
        row[["up[FisherTest]"]]      <- CI.high
        row[["p[FisherTest]"]]       <- p
      }
    }

    row <- .crossTabLayerNames(row, group)
    odds.ratio.rows[[length(odds.ratio.rows) + 1]] <- row
  }
  return(list(rows = odds.ratio.rows, rownames = row.rownames))
}

.crossTabNominalRows <- function(analysisContainer, var.name, groupList, options, ready) {
  nominal.rows   <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]

    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL

    row <- list()

    if (options$contingencyCoefficient) {
      row          <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "ContCoef", ready, row.rownames[g])
    }

    if (options$phiAndCramersV) {
      row          <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "PhiCoef", ready, row.rownames[g])
      row          <- .crossTabNominalMatrixToRow(analysisContainer, row, counts.matrix, type = "CramerV", ready, row.rownames[g])
    }

    if (options$lambda) {
      row <- .crossTabNominalLambdaMatrixToRow(analysisContainer, row, counts.matrix, ready)
    }

    nominal.rows[[length(nominal.rows) + 1]] <- .crossTabLayerNames(row, group)
  }
  return(list(rows = nominal.rows, rownames = row.rownames))
}

# See https://github.com/cran/vcdExtra/blob/10bd55cf7f74fe4b42ff154228b58d4ed885dfad/R/GKgamma.R#L47
# vcdExtra imports rgl, which needs some (big) X11 libraries. But as only GKgamma is needed from vcdExtra, we just copy the code here.
GKgamma<-function(x, level=0.95)
{
    # x is a matrix of counts.  You can use output of crosstabs or xtabs in R.
    # Confidence interval calculation and output from Greg Rodd

    # Check for using S-PLUS and output is from crosstabs (needs >= S-PLUS 6.0)
    if(is.null(version$language) && inherits(x, "crosstabs")) { oldClass(x)<-NULL; attr(x, "marginals")<-NULL}

    ## TODO: add tests for matrix or table

    n <- nrow(x)
    m <- ncol(x)
    pi.c<-pi.d<-matrix(0, nrow=n, ncol=m)

    row.x<-row(x)
    col.x<-col(x)

    for(i in 1:(n)){
        for(j in 1:(m)){
            pi.c[i, j]<-sum(x[row.x<i & col.x<j]) + sum(x[row.x>i & col.x>j])
            pi.d[i, j]<-sum(x[row.x<i & col.x>j]) + sum(x[row.x>i & col.x<j])
        }
    }

    C <- sum(pi.c*x)/2
    D <- sum(pi.d*x)/2

    psi<-2*(D*pi.c-C*pi.d)/(C+D)^2
    sigma2<-sum(x*psi^2)-sum(x*psi)^2

    gamma <- (C - D)/(C + D)
    pr2 <- 1 - (1 - level)/2
    CI <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + gamma

    result <- list(gamma = gamma, C = C, D = D, sigma = sqrt(sigma2),
        CIlevel = level,
        CI = c(max(CI[1], -1), min(CI[2], 1))
        )
    class(result) <- "GKgamma"
    result
}


.crossTabGammaRows <- function(analysisContainer, var.name, groupList, options, ready) {
  ordinal.rows   <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]
    if (!is.null(groups))  group <- groups[[g]]
    else                   group <- NULL

    row <- list()
    if(ready) {
      chi.result <- try({ chi.result <- GKgamma(counts.matrix) }) # in for a penny in for a dime I guess
      if (isTryError(chi.result)) {
        row[["value[gammaCoef]"]] <- NaN
      } else {
        row[["value[gammaCoef]"]] <- chi.result$gamma
        row[["Sigma[gammaCoef]"]] <- chi.result$sigma
        row[["low[gammaCoef]"]]   <- chi.result$CI[1]
        row[["up[gammaCoef]"]]    <- chi.result$CI[2]
      }
    }
    else {
      row[["value[gammaCoef]"]] <- "."
      row[["Sigma[gammaCoef]"]] <- "."
      row[["low[gammaCoef]"]]   <- "."
      row[["up[gammaCoef]"]]    <- "."
    }

    ordinal.rows[[length(ordinal.rows) + 1]] <- .crossTabLayerNames(row, group)
  }
  return(list(rows = ordinal.rows, rownames = row.rownames))
}

.crossTabKendallsTauRows <- function(analysisContainer, var.name, groupList, options, ready) {
  kendalls.rows  <- list()
  group.matrices <- groupList$group.matrices
  groups         <- groupList$groups
  row.rownames   <- .crossTabRowName(groups)

  for (g in seq_along(group.matrices)) {
    counts.matrix <- group.matrices[[g]]

    if (!is.null(groups)) group <- groups[[g]]
    else                  group <- NULL

    row <- list()
    if (ready) {
      chi.result <- try({
        count.dat  <- stats::ftable(counts.matrix)
        count.dat  <- as.data.frame(count.dat)
        Var1       <- rep(count.dat[,1],times = count.dat$Freq)
        Var2       <- rep(count.dat[,2],times = count.dat$Freq)
        chi.result <- stats::cor.test(as.numeric(Var1),
                                      as.numeric(Var2),
                                      method = "kendall")
      })

      if (isTryError(chi.result))
                                    row[["value[kTauB]"]] <- NaN
      else {
                                    row[["value[kTauB]"]]     <- unname(chi.result$estimate)
                                    row[["p[kTauB]"]]         <- chi.result$p.value
        if (options$vovkSellke)     row[["MPR[kTauB]"]]       <- VovkSellkeMPR(row[["p[kTauB]"]])
                                    row[["statistic[kTauB]"]] <- unname(chi.result$statistic)
      }
    } else {
                                    row[["value[kTauB]"]]     <- "."
                                    row[["p[kTauB]"]]         <- "."
      if (options$vovkSellke)    row[["MPR[kTauB]"]]       <- "."
                                    row[["statistic[kTauB]"]] <- "."
    }

    kendalls.rows[[length(kendalls.rows) + 1]] <- .crossTabLayerNames(row, group)
  }
  return(list(rows = kendalls.rows, rownames = row.rownames))
}

.dataFrameToRowList <- function(df, discard.column.names=FALSE) {

  if (dim(df)[1] == 0 || dim(df)[2] == 0)
    return(list())

  column.names <- names(df)
  rows <- list()

  for (i in 1:dim(df)[1]) {

    row <- list()

    for (j in 1:length(column.names))
      row[[j]] <- df[i,j]

    if ( ! discard.column.names)
      names(row) <- column.names

    rows[[i]] <- row
  }

  rows
}

