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

#' Multinomial Test
#'
#' The multinomial test allows the user to test whether an observed distribution of cell counts corresponds to an expected distribution.
#' ## Assumptions
#' - The variable of interest should be categorical.
#'
#' @param count, The variable that contains the count data.
#' @param descriptivesPlot, Plots the frequencies and the confidence intervals of the observed counts.
#'    Defaults to \code{FALSE}.
#' @param descriptivesPlotCiLevel, Coverage of the confidence intervals in percentages. The default value is 95.
#' @param descriptivesTable, Displays the descriptives of the observed and expected counts as well as the confidence intervals of the observed values.
#'    Defaults to \code{FALSE}.
#' @param descriptivesTableCi, Coverage of the confidence intervals in percentages. The default value is 95.
#'    Defaults to \code{FALSE}.
#' @param expectedCount, If the data includes a variable representing expected cell counts, enter it here; its values define the null hypothesis.
#' @param factor, The categorical variable we are interested in.
#' @param vovkSellke, An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.
#'    Defaults to \code{FALSE}.
#' @param ... options that are not an argument of this function: the options of an older version of the
#'   analysis, to be used together with `version`. They are upgraded to the options of this version.
MultinomialTest <- function(
          data = NULL,
          version = "1",
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
          vovkSellke = FALSE,
          ...) {

   options <- mget(setdiff(names(formals(sys.function())), "..."), envir = environment(), inherits = FALSE)
   options <- c(options, list(...))
   if (!all(nzchar(names(options))) || anyDuplicated(names(options)))
      stop("The extra options given to MultinomialTest must be named, each name once.")
   if (...length() > 0L && missing(version))
      stop("MultinomialTest has no option ", paste(names(list(...)), collapse = ", "), ". Options of an older version need the `version` they were written for.")
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("count", "expectedCount", "factor", "testValuesCustom")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   # Options not given in the call get their default from the form, which can depend on the other options.
   options <- options[names(options) %in% names(match.call())[-1L]]
   return(jaspBase::runWrappedAnalysis("jaspFrequencies", "MultinomialTest", "MultinomialTest.qml", options, version, TRUE))
}