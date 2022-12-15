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
	plotWidth: 320
	plotHeight: 240

	Formula
	{
		lhs: "rows"
		rhs: "columns"
	}

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "rows";		title: qsTr("Rows");	suggestedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "columns";	title: qsTr("Columns");	suggestedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "counts";		title: qsTr("Counts");	suggestedColumns: ["scale", "ordinal"]; singleVariable: true }
		AssignedVariablesList { name: "layers";		title: qsTr("Layers");	suggestedColumns: ["ordinal", "nominal"]; listViewType: JASP.Layers; preferredHeight: 120 }
	}

	Section
	{
		title: qsTr("Statistics")

		RadioButtonGroup
		{
			name: "samplingModel"
			title: qsTr("Sample")
			RadioButton { value: "poisson";								label: qsTr("Poisson")											}
			RadioButton { value: "jointMultinomial";					label: qsTr("Joint multinomial")								}
			RadioButton { value: "independentMultinomialRowsFixed";		label: qsTr("Indep. multinomial, rows fixed"); checked: true	}
			RadioButton { value: "independentMultinomialColumnsFixed";	label: qsTr("Indep. multinomial, columns fixed")				}
			RadioButton { value: "hypergeometric";						label: qsTr("Hypergeometric (2x2 only)"); id: hypergeometric	}
		}

		Group
		{
			title: qsTr("Additional Statistics")
			CheckBox
			{
				name: "oddsRatio";	label: qsTr("Log odds ratio (2x2 only)")
				CIField { name: "oddsRatioCiLevel"; label: qsTr("Credible interval") }
			}
			CheckBox
			{
				name: "cramersV"; label: qsTr("Cramer's V"); debug: true
				CIField { name: "cramersVCiLevel"; label: qsTr("Credible interval"); debug: true }
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Alt. Hypothesis")
			name: "alternative"
			enabled: !hypergeometric.checked
			RadioButton { value: "twoSided";	label: qsTr("Group one ≠ Group two"); checked: true	}
			RadioButton { value: "greater";		label: qsTr("Group one > Group two")					}
			RadioButton { value: "less";		label: qsTr("Group one < Group two")					}
		}

		Group
		{
			title: qsTr("Plots")
			CheckBox
			{
				name: "posteriorOddsRatioPlot";			label: qsTr("Log odds ratio (2x2 only)")
				CheckBox { name: "posteriorOddsRatioPlotAdditionalInfo"; label: qsTr("Additional info"); checked: true }
			}
			CheckBox { name: "cramersVPlot";	label: qsTr("Cramer's V"); debug: true }
		}

		BayesFactorType {}

		Group
		{
			title: qsTr("Prior")
			DoubleField { name: "priorConcentration"; label: qsTr("Prior concentration"); defaultValue: 1; min: 1; decimals: 1 }
		}
	}

	Section
	{
		title: qsTr("Cells")

		Group
		{
			title:	qsTr("Counts")
			debug:	true
			CheckBox { name: "countsExpected";	label: qsTr("Expected") }
		}

		Group
		{
			title: qsTr("Residuals")
			debug:	true
			CheckBox { name: "residualsUnstandardized";	label: qsTr("Unstandardized")	}
			CheckBox { name: "residualsPearson";		label: qsTr("Pearson")			}
			CheckBox { name: "residualsStandardized";	label: qsTr("Standardized")		}
		}

		Group
		{
			title: qsTr("Percentages")
			CheckBox { name: "percentagesRow";		label: qsTr("Row")		}
			CheckBox { name: "percentagesColumn";	label: qsTr("Column")	}
			CheckBox { name: "percentagesTotal";	label: qsTr("Total")	}
		}
	}

	Section
	{
		title: qsTr("Options")

		RadioButtonGroup
		{
			name: "rowOrder"
			title: qsTr("Row Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true	}
			RadioButton { value: "descending";	label: qsTr("Descending")				}
		}
		RadioButtonGroup
		{
			name: "columnOrder"
			title: qsTr("Column Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true	}
			RadioButton { value: "descending";	label: qsTr("Descending")				}
		}

		SetSeed{}
	}
}
