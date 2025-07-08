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
	info: qsTr("The Bayesian multinomial test allows the user to test whether an observed distribution of cell counts corresponds to an expected distribution.\n" + "## " + "Assumptions\n" + "- The variable of interest should be categorical.")

	VariablesForm
	{
		preferredHeight: 190 * preferencesModel.uiScale
		marginBetweenVariablesLists	: 15
		info: qsTr("**Input**")
		AvailableVariablesList {				name: "allVariablesList" }
		AssignedVariablesList {	id: factors;	name: "factor";			title: qsTr("Factor");			singleVariable: true; allowedColumns: ["nominal"]; 	info: qsTr("The categorical variable we are interested in.")	}
		AssignedVariablesList {					name: "count";			title: qsTr("Counts");			singleVariable: true; allowedColumns: ["scale"]; 	info: qsTr("The variable that contains the count data.")	}
		AssignedVariablesList {	id: exProbVar;	name: "expectedCount";	title: qsTr("Expected Counts"); singleVariable: true; allowedColumns: ["scale"]; 	info: qsTr("If the data includes a variable representing expected cell counts, enter it here; its values define the null hypothesis.") }
	}

	RadioButtonGroup
	{
		id		: hypothesisGroup
		name	: "testValues"
		title	: qsTr("Test Values")
		enabled	: exProbVar.count == 0

		Layout.columnSpan: 2

		RadioButton {	value: "equal";		label: qsTr("Equal proportions");			checked: true;		info: qsTr("Checks if observed counts across categories are uniformly distributed (the null hypothesis). A significant difference suggests the categories aren't equally likely.")	}
		RadioButton {	value: "custom";	label: qsTr("Custom expected proportions"); id: expectedProbs;	info: qsTr("Checks if observed counts match a specific expected distribution (the null hypothesis). By default, it tests for a uniform distribution, but you can set your own expectations. A significant difference suggests the actual distribution doesn’t fit the expected one.")	}

		Chi2TestTableView
		{
			name			: "testValuesCustom"
			preferredWidth	: form.availableWidth - hypothesisGroup.leftPadding
			visible			: expectedProbs.checked && factors.count > 0
			source			: "factor"
			maxNumHypotheses: 5
			decimals		: 3
		}
	}

	ColumnLayout
	{
		BayesFactorType { }

		Group
		{
			title	: qsTr("Additional Statistics")
			CheckBox
			{
				name	: "descriptivesTable"
				label	: qsTr("Descriptives")
				info	: qsTr("Displays a table containing the categories of interest, the observed values and the expected values under the specified hypotheses.")
				CheckBox
				{
					name				: "descriptivesTableCi"; label: qsTr("Credible interval"); info: qsTr("Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.")
					childrenOnSameRow	: true

					CIField { name: "descriptivesTableCiLevel" }
				}
			}
		}
	}

	ColumnLayout
	{
		RadioButtonGroup
		{
			name	: "descriptivesType"
			title	: qsTr("Display")

			RadioButton { value: "counts";		label: qsTr("Counts");	checked: true;  info: qsTr("Displays the descriptives as counts.")		}
			RadioButton { value: "proportions";	label: qsTr("Proportions");				info: qsTr("Displays the descriptives as a proportion.")	}
		}

		Group
		{
			title	: qsTr("Plots")
			CheckBox
			{
				name	: "descriptivesPlot"
				label	: qsTr("Descriptives plot")
				info	: qsTr("Displays the frequency of the reported counts and the corresponding credible intervals for every level of the variable of interest.")

				CIField { name: "descriptivesPlotCiLevel"; label: qsTr("Credible interval"); info: qsTr("A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.") }
			}
		}
	}

	Section
	{
		title	: qsTr("Prior")
		info	: qsTr("Option to adjust the prior distribution for the vector of cell probabilities.")

		Text
		{
			visible: factors.count == 0
			text: qsTr("No factor specified")
		}

		Chi2TestTableView
		{
			name			: "priorCounts"
			preferredWidth	: form.availableWidth - hypothesisGroup.leftPadding
			source			: "factor"
			visible			: factors.count > 0

			showAddButton		: false
			showDeleteButton	: false
			colHeader			: qsTr("Counts")
			cornerText			: qsTr("Level #")
			itemType			: JASP.Integer
		}
	}
}
