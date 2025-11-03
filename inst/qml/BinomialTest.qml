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
	info: qsTr("The binomial test allows the user to test whether a proportion of a dichotomous variable is equal to a test value (=presumed population value). The analysis returns a binomial test for each level of the dependent variable against all other levels, so it will also work for variables with more than two levels.\n" + "## " + "Assumptions\n" + "- The variable should be a dichotomous scale.\n" + "- Observations should be independent.")

	Formula
	{
		rhs: "variables"
	}

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); allowedColumns: [ "nominal"] }
	}

	FormulaField { name: "testValue"; label: qsTr("Test value: "); value: "0.5" ; min: 0; max: 1; Layout.columnSpan: 2; info: qsTr("The proportion of the variable under the null hypothesis - the baseline for comparison.") }

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "alternative"
		RadioButton { value: "twoSided";	label: qsTr("≠ Test value"); checked: true; info: qsTr("Two-sided alternative hypothesis that the proportion is not equal to test value.")		}
		RadioButton { value: "greater";		label: qsTr("> Test value"); 				info: qsTr("One-sided alternative hypothesis that the proportion is larger than the test value.")	}
		RadioButton { value: "less";		label: qsTr("< Test value"); 				info: qsTr("One-sided alternative hypothesis that the proportion is smaller than the test value.")	}
	}

	Group
	{
		title: qsTr("Additional Statisics")
		CheckBox
		{
			name:	"ci"
			label:	qsTr("Confidence interval")
			info: qsTr("Coverage of the confidence intervals in percentages. The default value is 95.")
			CIField { name: "ciLevel";	label: qsTr("Interval") }
		}
		CheckBox { name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio"); info: qsTr("An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.") }
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "descriptivesPlot";					label: qsTr("Descriptive plots"); 	info: qsTr("Visualises the proportion and confidence interval of the two different values of your dichotomous variable.")
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Confidence interval"); info: qsTr("Coverage of the confidence intervals in percentages. The default value is 95.")}
		}
	}
}
