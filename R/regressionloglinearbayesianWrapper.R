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

RegressionLogLinearBayesian <- function(
          data = NULL,
          version = "0.18.2",
          formula = NULL,
          bayesFactorType = "BF10",
          count = "",
          factors = list(),
          modelCutOffBestDisplayed = 2,
          modelCutOffPosteriorProbability = 0.1,
          modelTerms = list(),
          plotHeight = 320,
          plotWidth = 480,
          priorScale = 0,
          priorShape = -1,
          regressionCoefficientsCi = FALSE,
          regressionCoefficientsCiLevel = 0.95,
          regressionCoefficientsEstimates = FALSE,
          regressionCoefficientsSubmodel = FALSE,
          regressionCoefficientsSubmodelCi = FALSE,
          regressionCoefficientsSubmodelCiLevel = 0.95,
          regressionCoefficientsSubmodelNo = 1,
          samplingMethod = "auto",
          samplingMethodManualSamples = 10000,
          seed = 1,
          setSeed = FALSE) {

   defaultArgCalls <- formals(jaspFrequencies::RegressionLogLinearBayesian)
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

   optionsWithFormula <- c("count", "factors", "modelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies::RegressionLogLinearBayesian", data, options, version))
}