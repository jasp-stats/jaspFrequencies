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

#' Contingency Tables
#'
#' Contingency tables allow the user to identify how the frequencies of one categorical variable relate to another, helping to determine associations between variables.
#'
#' @param chiSquared, Pearson’s chi-squared test for independence.
#'    Defaults to \code{TRUE}.
#' @param chiSquaredContinuityCorrection, Applies Yates’ correction for continuity (for 2×2 tables).
#'    Defaults to \code{FALSE}.
#' @param contingencyCoefficient, Measure of association for nominal variables (based on χ²).
#'    Defaults to \code{FALSE}.
#' @param countsExpected, Show counts expected under the null hypothesis.
#'    Defaults to \code{FALSE}.
#' @param countsObserved, Show actual counts from the data.
#'    Defaults to \code{TRUE}.
#' @param gamma, Measure of ordinal association based on concordant/discordant pairs.
#'    Defaults to \code{FALSE}.
#' @param kendallsTauB, Ordinal correlation adjusting for ties.
#'    Defaults to \code{FALSE}.
#' @param lambda, Proportion reduction in error measure for nominal data.
#'    Defaults to \code{FALSE}.
#' @param likelihoodRatio, Calculates the likelihood of the data under the alternative hypothesis divided by the likelihood of the data under the null hypothesis.
#'    Defaults to \code{FALSE}.
#' @param marginShowTotals, Shows row and column totals.
#'    Defaults to \code{TRUE}.
#' @param mcNemarChiSquared, Tests equality of two marginal proportions in paired nominal data.
#'    Defaults to \code{FALSE}.
#' @param mcNemarChiSquaredContinuityCorrection, Corrects for error introduced when approximating a discrete distribution with a continuous one in McNemar's test.
#'    Defaults to \code{FALSE}.
#' @param oddsRatio, Displays the odds ratio for 2×2 tables (ad/bc).
#'    Defaults to \code{FALSE}.
#' @param oddsRatioAlternative, Specify the direction of the alternative hypothesis for Fisher's exact test.
#' \itemize{
#'   \item \code{"twoSided"}: Two-sided alternative hypothesis that the proportion of group 1 is not equal to the proportion of group 2.
#'   \item \code{"greater"}: One-sided alternative hypothesis that the proportion of group 1 is greater than the proportion of group 2.
#'   \item \code{"less"}: One-sided alternative hypothesis that the proportion of group 1 is less than the proportion of group 2.
#' }
#' @param oddsRatioAsLogOdds, Shows the log-transformed odds ratio.
#'    Defaults to \code{TRUE}.
#' @param oddsRatioCiLevel, Coverage of the confidence intervals in percentages. The default value is 95.
#' @param percentagesColumn, Shows column-wise percentages.
#'    Defaults to \code{FALSE}.
#' @param percentagesRow, Shows row-wise percentages.
#'    Defaults to \code{FALSE}.
#' @param percentagesTotal, Shows percentages of total.
#'    Defaults to \code{FALSE}.
#' @param phiAndCramersV, Effect size for nominal association; Phi (2×2), Cramer’s V (larger tables).
#'    Defaults to \code{FALSE}.
#' @param residualsPearson, Standardized residuals; computed by (observed - expected) / √(expected).
#'    Defaults to \code{FALSE}.
#' @param residualsStandardized, Accounts for row/column totals; computed by (observed - expected) / √(expected × (1 - row marginal proportion) × (1 - column marginal proportion)).
#'    Defaults to \code{FALSE}.
#' @param residualsUnstandardized, Computed by (observed - expected).
#'    Defaults to \code{FALSE}.
#' @param vovkSellke, An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.
#'    Defaults to \code{FALSE}.
ContingencyTables <- function(
          data = NULL,
          version = "1",
          formula = NULL,
          byIntervalEta = FALSE,
          chiSquared = TRUE,
          chiSquaredContinuityCorrection = FALSE,
          cochranAndMantel = FALSE,
          columnOrder = "ascending",
          columns = list(types = list(), value = list()),
          contingencyCoefficient = FALSE,
          counts = list(types = list(), value = ""),
          countsExpected = FALSE,
          countsHiddenSmallCounts = FALSE,
          countsHiddenSmallCountsThreshold = 5,
          countsObserved = TRUE,
          gamma = FALSE,
          kendallsTauB = FALSE,
          kendallsTauC = FALSE,
          lambda = FALSE,
          layers = list(),
          likelihoodRatio = FALSE,
          marginShowTotals = TRUE,
          mcNemarChiSquared = FALSE,
          mcNemarChiSquaredContinuityCorrection = FALSE,
          oddsRatio = FALSE,
          oddsRatioAlternative = "twoSided",
          oddsRatioAsLogOdds = TRUE,
          oddsRatioCiLevel = 0.95,
          percentagesColumn = FALSE,
          percentagesRow = FALSE,
          percentagesTotal = FALSE,
          phiAndCramersV = FALSE,
          plotHeight = 320,
          plotWidth = 480,
          residualsPearson = FALSE,
          residualsStandardized = FALSE,
          residualsUnstandardized = FALSE,
          rowOrder = "ascending",
          rows = list(types = list(), value = list()),
          somersD = FALSE,
          testOddsRatioEquals = 1,
          uncertaintyCoefficient = FALSE,
          vovkSellke = FALSE,
          zTestAdjustedPValues = FALSE,
          zTestColumnComparison = FALSE) {

   defaultArgCalls <- formals(jaspFrequencies::ContingencyTables)
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

   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "ContingencyTables", "ContingencyTables.qml", options, version, TRUE))
}