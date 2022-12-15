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
		rhs: "modelTerms"
	}

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "count";		title: qsTr("Counts (optional)"); singleVariable: true; suggestedColumns: ["scale", "ordinal"]			}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors"); itemType: "fixedFactors"; suggestedColumns: ["ordinal", "nominal"]	}
	}
	
	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList	{ name: "availableTerms";	title: qsTr("Components");	width: parent.width / 4;		source: ['factors'] }
			AssignedVariablesList	{ name: "modelTerms";		title: qsTr("Model Terms");	width: parent.width * 5 / 9;	listViewType: JASP.Interaction }
		}
	}
	
	Section
	{
		title: qsTr("Statistics")
		
		Group
		{
			title: qsTr("Regression Coefficient")
			CheckBox
			{
				name: "regressionCoefficientsEstimates";	label: qsTr("Estimates")
				CheckBox
				{
					name: "regressionCoefficientsCi";	label: qsTr("Confidence intervals")
					CIField { name: "regressionCoefficientsCiLevel"; label: qsTr("Interval") }
				}
			}
		}
		CheckBox { name: "vovkSellke"; label: qsTr("Vovk-Sellke maximum p-ratio") }
	}
}
