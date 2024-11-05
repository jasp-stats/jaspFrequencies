#
# Copyright (C) 2013-2024 University of Amsterdam
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

# This is a generated file. Don't change it

InformedMultinomialTestBayesian <- function(
          data = NULL,
          version = "0.19.2",
          bayesFactorType = "BF10",
          bfComparison = "null",
          bfVsHypothesis = "Model 1",
          bridgeSamples = 10000,
          count = list(types = list(), value = ""),
          descriptivesDisplay = "counts",
          descriptivesPlot = FALSE,
          descriptivesTable = FALSE,
          factor = list(types = list(), value = ""),
          includeEncompassingModel = TRUE,
          includeNullModel = TRUE,
          mcmcBurnin = 500,
          mcmcSamples = 10000,
          models = list(list(modelName = "Model 1", syntax = "")),
          plotHeight = 320,
          plotWidth = 480,
          posteriorPlot = FALSE,
          posteriorPlotCiCoverage = 0.95,
          priorCounts = list(list(levels = list(), name = "data 1", values = list())),
          priorModelProbability = list(list(levels = list("Null", "Encompassing", "Model 1"), name = "data 1", values = list("1", "1", "1"))),
          seed = 1,
          sequentialAnalysisNumberOfSteps = 10,
          sequentialAnalysisPlot = FALSE,
          sequentialAnalysisPlotType = "bayesFactor",
          setSeed = FALSE) {

   defaultArgCalls <- formals(jaspFrequencies::InformedMultinomialTestBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("bfVsHypothesis", "count", "factor", "models", "priorCounts", "priorModelProbability")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies::InformedMultinomialTestBayesian", data, options, version))
}
