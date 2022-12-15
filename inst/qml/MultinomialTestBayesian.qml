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
	VariablesForm
	{
		preferredHeight: 190 * preferencesModel.uiScale
		marginBetweenVariablesLists	: 15
		AvailableVariablesList {				name: "allVariablesList" }
		AssignedVariablesList {	id: factors;	name: "factor";			title: qsTr("Factor");			singleVariable: true; suggestedColumns: ["ordinal", "nominal"]	}
		AssignedVariablesList {					name: "count";			title: qsTr("Counts");			singleVariable: true; suggestedColumns: ["scale", "ordinal"]	}
		AssignedVariablesList {	id: exProbVar;	name: "expectedCount";	title: qsTr("Expected Counts"); singleVariable: true; suggestedColumns: ["scale", "ordinal"]	}
	}

	RadioButtonGroup
	{
		id		: hypothesisGroup
		name	: "testValues"
		title	: qsTr("Test Values")
		enabled	: exProbVar.count == 0

		Layout.columnSpan: 2

		RadioButton {	value: "equal";		label: qsTr("Equal proportions");			checked: true				}
		RadioButton {	value: "custom";	label: qsTr("Custom expected proportions"); id: expectedProbs			}

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
				CheckBox
				{
					name				: "descriptivesTableCi"; label: qsTr("Credible interval")
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

			RadioButton { value: "counts";		label: qsTr("Counts");	checked: true	}
			RadioButton { value: "proportions";	label: qsTr("Proportions")				}
		}

		Group
		{
			title	: qsTr("Plots")
			CheckBox
			{
				name	: "descriptivesPlot"
				label	: qsTr("Descriptives plot")

				CIField { name: "descriptivesPlotCiLevel"; label: qsTr("Credible interval") }
			}
		}
	}

	ExpanderButton
	{
		title	: qsTr("Prior")

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
