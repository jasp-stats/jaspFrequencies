//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	info: qsTr("The Bayesian binomial test allows you to test whether a proportion of a dichotomous variable is equal to a test value (presumed population value). The analysis returns a binomial test for each level of the dependent variable against all other levels, so it will also work for variables with more than two levels.\n" + "## " + "Assumptions\n" + "- The variable should be a dichotomous scale.\n" + "- Observations should be independent.")

	Formula
	{
		rhs: "variables"
	}

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "variables"; allowedColumns: ["nominal"] }
	}
	
	FormulaField { label: qsTr("Test value: "); name: "testValue"; value: "0.5" ; min:0; max: 1; Layout.columnSpan: 2; info: qsTr("The proportion of the variable under the null hypothesis - the baseline for comparison.") }
	
	Group
	{
		title: qsTr("Alt. Hypothesis")

		RadioButtonGroup
		{
			id:		alternative
			title:	qsTr("Direction")
			name:	"alternative"
			RadioButton { value: "twoSided";	label: qsTr("≠ Test value"); checked: true; info: qsTr("Two-sided alternative hypothesis that the proportion is not equal to test value.")		}
			RadioButton { value: "greater";		label: qsTr("> Test value");				info: qsTr("One-sided alternative hypothesis that the proportion is larger than the test value.")	}
			RadioButton { value: "less";		label: qsTr("< Test value");				info: qsTr("One-sided alternative hypothesis that the proportion is smaller than the test value.")		}
		}

		Group
		{
			title: qsTr("Prior")
			FormulaField { name: "priorA"; label: qsTr("Beta prior: parameter a"); value: "1"; min:0; max: 10000; inclusive: JASP.None; info: qsTr("Sets how much prior belief you have in success. When a = b = 1, this corresponds to a uniform prior distribution.") }
			FormulaField { name: "priorB"; label: qsTr("Beta prior: parameter b"); value: "1"; min:0; max: 10000; inclusive: JASP.None; info: qsTr("Sets how much prior belief you have in failure. When a = b = 1, this corresponds to a uniform prior distribution.") }
		}
	}

	Group {
		title: qsTr("Plots")
		CheckBox
		{
			name:	"priorPosteriorPlot"
			label:	qsTr("Prior and posterior")
			info: 	qsTr("Displays the prior and posterior density of the population proportion under the alternative hypothesis.")
			CheckBox { name: "priorPosteriorPlotAdditionalInfo";	label: qsTr("Additional info");	checked: true; info: qsTr("Shows the Bayes factor using the chosen prior, a probability wheel showing evidence for each hypothesis, and the median with 95% credible interval of the effect size.") }
		}
		CheckBox { name: "bfSequentialPlot";	label: qsTr("Sequential analysis"); info: qsTr("Displays the development of the Bayes factor as the data come in using the user-defined prior.") }
		CheckBox
		{
			name:	"descriptivesPlot"
			label:	qsTr("Descriptive plots")
			info: 	qsTr("Display descriptives plots.")
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Credible interval"); info: qsTr("Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.") }
		}
	}

	BayesFactorType { correlated: alternative.value }
}
