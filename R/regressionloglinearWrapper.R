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

#' Log-Linear Regression
#'
#' Log-linear regression models the logarithm of the dependent variable as a linear combination of independent variables. It is useful for handling skewed data and multiplicative relationships.
#'
#' @param regressionCoefficientsCi, Coverage of the confidence intervals in percentages. The default value is 95.
#'    Defaults to \code{FALSE}.
#' @param regressionCoefficientsEstimates, Displays estimates of the regression coefficients along with their standard errors, Z-values, and associated p-values.
#'    Defaults to \code{FALSE}.
#' @param vovkSellke, An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.
#'    Defaults to \code{FALSE}.
RegressionLogLinear <- function(
          data = NULL,
          version = "1",
          formula = NULL,
          count = list(types = list(), value = ""),
          factors = list(types = list(), value = list()),
          modelTerms = list(optionKey = "components", types = list(), value = list()),
          plotHeight = 320,
          plotWidth = 480,
          regressionCoefficientsCi = FALSE,
          regressionCoefficientsCiLevel = 0.95,
          regressionCoefficientsEstimates = FALSE,
          vovkSellke = FALSE) {

   defaultArgCalls <- formals(jaspFrequencies::RegressionLogLinear)
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
   optionsWithFormula <- c("count", "factors", "modelTerms")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "RegressionLogLinear", "RegressionLogLinear.qml", options, version, TRUE))
}