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

#' Informed Bayesian Multi-Binomial Test
#'
#' @param bridgeSamples, Max number of samples used to estimate marginal likelihoods for computing Bayes factors.
#' @param descriptivesPlot, Shows a plot of observed counts or proportions.
#'    Defaults to \code{FALSE}.
#' @param descriptivesTable, Displays a summary table of observed counts or proportions.
#'    Defaults to \code{FALSE}.
#' @param mcmcBurnin, Number of initial MCMC samples to discard before analysis begins, allowing the chain to stabilize.
#' @param mcmcSamples, Total number of MCMC samples used for estimating the posterior after burn-in.
#' @param posteriorPlot, Displays the posterior distribution with credible intervals.
#'    Defaults to \code{FALSE}.
#' @param sequentialAnalysisNumberOfSteps, Number of data points at which the Bayes factor is updated during sequential analysis.
#' @param sequentialAnalysisPlot, Displays the development of the Bayes factor or posterior probability as the data come in.
#'    Defaults to \code{FALSE}.
InformedBinomialTestBayesian <- function(
          data = NULL,
          version = "1",
          bayesFactorType = "BF10",
          bfComparison = "Encompassing",
          bfVsHypothesis = "Model 1",
          bridgeSamples = 1000,
          descriptivesDisplay = "counts",
          descriptivesPlot = FALSE,
          descriptivesTable = FALSE,
          factor = list(types = list(), value = ""),
          includeEncompassingModel = TRUE,
          includeNullModel = TRUE,
          mcmcBurnin = 500,
          mcmcSamples = 5000,
          models = list(list(modelName = "Model 1", syntax = "")),
          plotHeight = 320,
          plotWidth = 480,
          posteriorPlot = FALSE,
          posteriorPlotCiCoverage = 0.95,
          priorCounts = list(list(levels = list(), name = "data 1", values = list()), list(levels = list(), name = "data 2", values = list())),
          priorModelProbability = list(list(levels = list(), name = "data 1", values = list())),
          sampleSize = list(types = list(), value = ""),
          seed = 1,
          sequentialAnalysisNumberOfSteps = 10,
          sequentialAnalysisPlot = FALSE,
          sequentialAnalysisPlotType = "bayesFactor",
          setSeed = FALSE,
          successes = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspFrequencies::InformedBinomialTestBayesian)
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

   optionsWithFormula <- c("bfVsHypothesis", "factor", "models", "priorCounts", "priorModelProbability", "sampleSize", "successes")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "InformedBinomialTestBayesian", "InformedBinomialTestBayesian.qml", options, version, TRUE))
}