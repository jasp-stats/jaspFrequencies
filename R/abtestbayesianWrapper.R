#
# Copyright (C) 2013-2022 University of Amsterdam
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

ABTestBayesian <- function(
          data = NULL,
          version = "0.17",
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
          n1 = "",
          n2 = "",
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
          y1 = "",
          y2 = "") {

   defaultArgCalls <- formals(jaspFrequencies::ABTestBayesian)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("bfRobustnessPlotType", "n1", "n2", "priorPlotType", "priorPosteriorPlotType", "y1", "y2")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies::ABTestBayesian", data, options, version))
}
