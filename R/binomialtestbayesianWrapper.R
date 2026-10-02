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

#' Bayesian Binomial Test
#'
#' The Bayesian binomial test allows you to test whether a proportion of a dichotomous variable is equal to a test value (presumed population value). The analysis returns a binomial test for each level of the dependent variable against all other levels, so it will also work for variables with more than two levels.
#' ## Assumptions
#' - The variable should be a dichotomous scale.
#' - Observations should be independent.
#'
#' @param bfSequentialPlot, Displays the development of the Bayes factor as the data come in using the user-defined prior.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlot, Display descriptives plots.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlotCiLevel, Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.
#' @param priorA, Sets how much prior belief you have in success. When a = b = 1, this corresponds to a uniform prior distribution.
#' @param priorB, Sets how much prior belief you have in failure. When a = b = 1, this corresponds to a uniform prior distribution.
#' @param priorPosteriorPlot, Displays the prior and posterior density of the population proportion under the alternative hypothesis.
#'    Defaults to \code{FALSE}.
#' @param priorPosteriorPlotAdditionalInfo, Shows the Bayes factor using the chosen prior, a probability wheel showing evidence for each hypothesis, and the median with 95% credible interval of the effect size.
#'    Defaults to \code{TRUE}.
#' @param testValue, The proportion of the variable under the null hypothesis - the baseline for comparison.
BinomialTestBayesian <- function(
          data = NULL,
          version = "1",
          formula = NULL,
          alternative = "twoSided",
          bayesFactorType = "BF10",
          bfSequentialPlot = FALSE,
          counts = list(types = list(), value = ""),
          descriptivesPlot = FALSE,
          descriptivesPlotCiLevel = 0.95,
          plotHeight = 320,
          plotWidth = 480,
          priorA = 1,
          priorB = 1,
          priorPosteriorPlot = FALSE,
          priorPosteriorPlotAdditionalInfo = TRUE,
          testValue = 0.5,
          variables = list(types = list(), value = list())) {

   defaultArgCalls <- formals(jaspFrequencies::BinomialTestBayesian)
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
   optionsWithFormula <- c("counts", "variables")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "BinomialTestBayesian", "BinomialTestBayesian.qml", options, version, TRUE))
}