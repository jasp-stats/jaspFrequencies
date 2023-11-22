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

ContingencyTablesBayesian <- function(
          data = NULL,
          version = "0.18.2",
          formula = NULL,
          alternative = "twoSided",
          bayesFactorType = "BF10",
          columnOrder = "ascending",
          columns = list(),
          counts = "",
          countsExpected = FALSE,
          cramersV = FALSE,
          cramersVCiLevel = 0.95,
          cramersVPlot = FALSE,
          layers = list(),
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
          rows = list(),
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

   if (!is.null(formula)) {
      if (!inherits(formula, "formula")) {
         formula <- as.formula(formula)
      }
      options$formula <- jaspBase::jaspFormula(formula, data)
   }

   optionsWithFormula <- c("columns", "counts", "layers", "rows")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies::ContingencyTablesBayesian", data, options, version))
}