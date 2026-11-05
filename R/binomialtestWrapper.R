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

#' Binomial Test
#'
#' The binomial test allows the user to test whether a proportion of a dichotomous variable is equal to a test value (=presumed population value). The analysis returns a binomial test for each level of the dependent variable against all other levels, so it will also work for variables with more than two levels.
#' ## Assumptions
#' - The variable should be a dichotomous scale.
#' - Observations should be independent.
#'
#' @param ci, Coverage of the confidence intervals in percentages. The default value is 95. The interval is the Clopper-Pearson interval from stats::binom.test. For two-sided tests, the p-value uses the minimum-likelihood method (summing the probabilities of all outcomes that are no more likely than the observed one), whereas the interval places equal probability in each tail. Because the interval does not invert the same test, the p-value and the interval can occasionally disagree about whether the test value is plausible. This can only happen for two-sided tests with a test value other than 0.5, where the binomial distribution is asymmetric.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlot, Visualises the proportion and confidence interval of the two different values of your dichotomous variable.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlotCiLevel, Coverage of the confidence intervals in percentages. The default value is 95.
#' @param testValue, The proportion of the variable under the null hypothesis - the baseline for comparison.
#' @param vovkSellke, An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.
#'    Defaults to \code{FALSE}.
BinomialTest <- function(
          data = NULL,
          version = "1",
          formula = NULL,
          alternative = "twoSided",
          ci = FALSE,
          ciLevel = 0.95,
          counts = list(types = list(), value = ""),
          descriptivesPlot = FALSE,
          descriptivesPlotCiLevel = 0.95,
          plotHeight = 320,
          plotWidth = 480,
          testValue = 0.5,
          variables = list(types = list(), value = list()),
          vovkSellke = FALSE) {

   options <- mget(names(formals(sys.function())), envir = environment(), inherits = FALSE)
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
   optionsWithFormula <- c("counts", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "BinomialTest", "BinomialTest.qml", options, version, TRUE))
}