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
	Formula
	{
		rhs: "variables"
	}

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); allowedColumns: [ "nominal"]; maxLevels:  50}
	}

	FormulaField { name: "testValue"; label: qsTr("Test value: "); value: "0.5" ; min: 0; max: 1; Layout.columnSpan: 2 }

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "alternative"
		RadioButton { value: "twoSided";	label: qsTr("≠ Test value"); checked: true	}
		RadioButton { value: "greater";		label: qsTr("> Test value")					}
		RadioButton { value: "less";		label: qsTr("< Test value")					}
	}

	Group
	{
		title: qsTr("Additional Statisics")
		CheckBox
		{
			name:	"ci"
			label:	qsTr("Confidence interval")
			CIField { name: "ciLevel";	label: qsTr("Interval") }
		}
		CheckBox { name: "vovkSellke";	label: qsTr("Vovk-Sellke maximum p-ratio") }
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "descriptivesPlot";					label: qsTr("Descriptives plots")
			CIField { name: "descriptivesPlotCiLevel";	label: qsTr("Confidence interval") }
		}
	}
}
