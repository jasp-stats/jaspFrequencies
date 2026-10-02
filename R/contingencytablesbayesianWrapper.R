#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Bayesian Contingency Tables
#'
#' @param countsExpected, Show counts expected under the null hypothesis.
#'    Defaults to \code{FALSE}.
#' @param countsObserved, Show actual counts from the data.
#'    Defaults to \code{TRUE}.
#' @param marginShowTotals, Shows row and column totals.
#'    Defaults to \code{TRUE}.
#' @param oddsRatio, Displays the odds ratio for 2×2 tables (ad/bc) with a credible interval.
#'    Defaults to \code{FALSE}.
#' @param oddsRatioCiLevel, Range where the true log odds ratio likely falls, given the data.
#' @param percentagesColumn, Shows column-wise percentages.
#'    Defaults to \code{FALSE}.
#' @param percentagesRow, Shows row-wise percentages.
#'    Defaults to \code{FALSE}.
#' @param percentagesTotal, Shows percentages of total.
#'    Defaults to \code{FALSE}.
#' @param posteriorOddsRatioPlot, Displays a graphical summary of the log odds ratio.
#'    Defaults to \code{FALSE}.
#' @param priorConcentration, Controls how strongly the prior favours equal proportions.
ContingencyTablesBayesian <- function(
          data = NULL,
          version = "1",
          formula = NULL,
          alternative = "twoSided",
          bayesFactorType = "BF10",
          columnOrder = "ascending",
          columns = list(types = list(), value = list()),
          counts = list(types = list(), value = ""),
          countsExpected = FALSE,
          countsObserved = TRUE,
          cramersV = FALSE,
          cramersVCiLevel = 0.95,
          cramersVPlot = FALSE,
          layers = list(),
          marginShowTotals = TRUE,
          oddsRatio = FALSE,
          oddsRatioCiLevel = 0.95,
          percentagesColumn = FALSE,
          percentagesRow = FALSE,
          percentagesTotal = FALSE,
          plotHeight = 240,
          plotWidth = 320,
          posteriorOddsRatioPlot = FALSE,
          posteriorOddsRatioPlotAdditionalInfo = TRUE,
          priorConcentration = 1,
          residualsPearson = FALSE,
          residualsStandardized = FALSE,
          residualsUnstandardized = FALSE,
          rowOrder = "ascending",
          rows = list(types = list(), value = list()),
          samplingModel = "independentMultinomialRowsFixed",
          seed = 1,
          setSeed = FALSE) {

   defaultArgCalls <- formals(jaspFrequencies::ContingencyTablesBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }
   optionsWithFormula <- c("columns", "counts", "layers", "rows")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "ContingencyTablesBayesian", "ContingencyTablesBayesian.qml", options, version, TRUE))
}