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
		marginBetweenVariablesLists: 15
		AvailableVariablesList {				name: "allVariablesList" }
		AssignedVariablesList {	id: factors;	name: "factor";			title: qsTr("Factor");			singleVariable: true; allowedColumns: ["nominal"]	}
		AssignedVariablesList {					name: "count";			title: qsTr("Counts");			singleVariable: true; allowedColumns: ["scale"]	}
		AssignedVariablesList {	id: exProbVar;	name: "expectedCount";	title: qsTr("Expected Counts"); singleVariable: true; allowedColumns: ["scale"]	}
	}

	RadioButtonGroup
	{
		id		: hypothesisGroup
		name	: "testValues"
		title	: qsTr("Test Values")
		enabled	: exProbVar.count == 0

		Layout.columnSpan: 2

		RadioButton {						value: "equal";		label: qsTr("Equal proportions (multinomial test)");	 checked: true	}
		RadioButton {	id: expectedProbs;	value: "custom";	label: qsTr("Custom expected proportions (χ² test)"); 				  	}

		Chi2TestTableView
		{
			name			: "testValuesCustom"
			preferredWidth	: form.availableWidth - hypothesisGroup.leftPadding
			visible			: expectedProbs.checked && factors.count > 0
			source			: "factor"
			decimals		: 3
		}
	}

	Group
	{
		title	: qsTr("Additional Statistics")

		CheckBox
		{
			name	: "descriptivesTable"; label: qsTr("Descriptives")

			CheckBox
			{
				name:					"descriptivesTableCi"
				label:					qsTr("Confidence interval")
				childrenOnSameRow	:	true

				CIField {	name: "descriptivesTableCiLevel"	}
			}
		}
		CheckBox {	name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio")		}
	}

	ColumnLayout
	{
		RadioButtonGroup
		{
			name	: "descriptivesType"
			title	: qsTr("Display")

			RadioButton {	value: "counts";		label: qsTr("Counts");	checked: true	}
			RadioButton {	value: "proportions";	label: qsTr("Proportions")				}
		}

		Group
		{
			title	: qsTr("Plots")

			CheckBox
			{
				name	: "descriptivesPlot"
				label	: qsTr("Descriptives plot")

				CIField {	name: "descriptivesPlotCiLevel"; label: qsTr("Confidence interval")	}
			}
		}
	}
}
