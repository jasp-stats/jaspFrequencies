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

#' Bayesian A/B Test
#'
#' The Bayesian A/B test allows one to monitor the evidence for the hypotheses that an intervention or treatment has either a positive effect, a negative effect or no effect.
#'
#' @param bayesFactorOrder, Compares each model against the model selected
#' \itemize{
#'   \item \code{"bestModelTop"}
#'   \item \code{"nullModelTop"}
#' }
#' @param bfRobustnessPlot, Displays the prior sensitivity analysis.
#'    Defaults to \code{FALSE}.
#' @param bfRobustnessPlotStepsPriorMean, Specifies in how many discrete steps the μ step range is partitioned.
#' @param bfRobustnessPlotStepsPriorSd, Specifies in how many discrete steps the σ step range is partitioned.
#' @param bfSequentialPlot, Displays the development of posterior probabilities as the data come in. The probability wheels visualize prior and posterior probabilities of the hypotheses.
#'    Defaults to \code{FALSE}.
#' @param descriptivesTable, Displays the counts and proportion for each group.
#'    Defaults to \code{FALSE}.
#' @param n1, Number of trials in group 1 (control condition).
#' @param n2, Number of trials in group 2 (experimental condition).
#' @param normalPriorMean, Specifies the mean for the normal prior on the test-relevant log odds ratio.
#' @param normalPriorSd, Specifies the standard deviation for the normal prior on the test-relevant log odds ratio.
#' @param priorModelProbabilityEqual, Specifies that the 'success' probability is identical (there is no effect).
#' @param priorModelProbabilityGreater, Specifies that the 'success' probability in the experimental condition is higher than in the control condition.
#' @param priorModelProbabilityLess, Specifies that the 'success' probability in the experimental condition is lower than in the control condition.
#' @param priorModelProbabilityTwoSided, Specifies that the 'success' probability differs between the control and experimental condition, but does not specify which one is higher.
#' @param priorPlot, Plots parameter prior distributions.
#'    Defaults to \code{FALSE}.
#' @param priorPosteriorPlot, Displays the prior and posterior density for the quantity of interest.
#'    Defaults to \code{FALSE}.
#' @param samples, Specifies the number of importance samples for obtaining log marginal likelihood for (H+) and (H-) and the number of posterior samples.
#' @param y1, Number of successes in group 1 (control condition).
#' @param y2, Number of successes in group 2 (experimental condition).
ABTestBayesian <- function(
          data = NULL,
          version = "1",
          bayesFactorOrder = "bestModelTop",
          bayesFactorType = "BF10",
          bfRobustnessPlot = FALSE,
          bfRobustnessPlotLowerPriorMean = -0.5,
          bfRobustnessPlotLowerPriorSd = 0.1,
          bfRobustnessPlotStepsPriorMean = 5,
          bfRobustnessPlotStepsPriorSd = 5,
          bfRobustnessPlotType = "BF10",
          bfRobustnessPlotUpperPriorMean = 0.5,
          bfRobustnessPlotUpperPriorSd = 1,
          bfSequentialPlot = FALSE,
          descriptivesTable = FALSE,
          n1 = list(types = list(), value = ""),
          n2 = list(types = list(), value = ""),
          normalPriorMean = 0,
          normalPriorSd = 1,
          plotHeight = 320,
          plotWidth = 480,
          priorModelProbabilityEqual = 0.5,
          priorModelProbabilityGreater = 0.25,
          priorModelProbabilityLess = 0.25,
          priorModelProbabilityTwoSided = 0,
          priorPlot = FALSE,
          priorPlotType = "logOddsRatio",
          priorPosteriorPlot = FALSE,
          priorPosteriorPlotType = "logOddsRatio",
          samples = 10000,
          seed = 1,
          setSeed = FALSE,
          y1 = list(types = list(), value = ""),
          y2 = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspFrequencies::ABTestBayesian)
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

   optionsWithFormula <- c("bfRobustnessPlotType", "n1", "n2", "priorPlotType", "priorPosteriorPlotType", "y1", "y2")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "ABTestBayesian", "ABTestBayesian.qml", options, version, TRUE))
}