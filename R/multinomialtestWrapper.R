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

MultinomialTest <- function(
          data = NULL,
          version = "0.19.2",
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
          testValues = "equal",
          testValuesCustom = list(list(levels = list(), name = "data 1", values = list())),
          vovkSellke = FALSE) {

   defaultArgCalls <- formals(jaspFrequencies::MultinomialTest)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL

   optionsWithFormula <- c("count", "expectedCount", "factor", "testValuesCustom")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspFrequencies::MultinomialTest", data, options, version))
}
