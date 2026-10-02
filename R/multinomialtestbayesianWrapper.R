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

#' Bayesian Multinomial Test
#'
#' The Bayesian multinomial test allows the user to test whether an observed distribution of cell counts corresponds to an expected distribution.
#' ## Assumptions
#' - The variable of interest should be categorical.
#'
#' @param count, The variable that contains the count data.
#' @param descriptivesPlot, Displays the frequency of the reported counts and the corresponding credible intervals for every level of the variable of interest.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlotCiLevel, A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.
#' @param descriptivesTable, Displays a table containing the categories of interest, the observed values and the expected values under the specified hypotheses.
#'    Defaults to \code{FALSE}.
#' @param descriptivesTableCi, Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.
#'    Defaults to \code{FALSE}.
#' @param expectedCount, If the data includes a variable representing expected cell counts, enter it here; its values define the null hypothesis.
#' @param factor, The categorical variable we are interested in.
MultinomialTestBayesian <- function(
          data = NULL,
          version = "1",
          bayesFactorType = "BF10",
          count = list(types = list(), value = ""),
          descriptivesPlot = FALSE,
          descriptivesPlotCiLevel = 0.95,
          descriptivesTable = FALSE,
          descriptivesTableCi = FALSE,
          descriptivesTableCiLevel = 0.95,
          descriptivesType = "counts",
          expectedCount = list(types = list(), value = ""),
          factor = list(types = list(), value = ""),
          plotHeight = 320,
          plotWidth = 480,
          priorCounts = list(list(levels = list(), name = "data 1", values = list())),
          testValues = "equal",
          testValuesCustom = list(list(levels = list(), name = "data 1", values = list()))) {

   defaultArgCalls <- formals(jaspFrequencies::MultinomialTestBayesian)
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

   optionsWithFormula <- c("count", "expectedCount", "factor", "priorCounts", "testValuesCustom")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "MultinomialTestBayesian", "MultinomialTestBayesian.qml", options, version, TRUE))
}