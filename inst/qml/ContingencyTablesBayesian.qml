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
		AssignedVariablesList { name: "rows";		title: qsTr("Rows");	allowedColumns: ["nominal"] }
		AssignedVariablesList { name: "columns";	title: qsTr("Columns");	allowedColumns: ["nominal"] }
		AssignedVariablesList { name: "counts";		title: qsTr("Counts");	allowedColumns: ["scale"]; singleVariable: true }
		AssignedVariablesList { name: "layers";		title: qsTr("Layers");	allowedColumns: ["nominal"]; listViewType: JASP.Layers; preferredHeight: 120 }
	}

	Section
	{
		title: qsTr("Statistics")

		RadioButtonGroup
		{
			name: "samplingModel"
			title: qsTr("Sample")
			RadioButton { value: "poisson";								label: qsTr("Poisson");											info: qsTr("Assumes independent Poisson-distributed counts for each cell.")	}
			RadioButton { value: "jointMultinomial";					label: qsTr("Joint multinomial");								info: qsTr("Assumes all cell counts come from one multinomial distribution.")	}
			RadioButton { value: "independentMultinomialRowsFixed";		label: qsTr("Indep. multinomial, rows fixed"); checked: true;	info: qsTr("Each row is a separate multinomial distribution; row totals are fixed.")	}
			RadioButton { value: "independentMultinomialColumnsFixed";	label: qsTr("Indep. multinomial, columns fixed");				info: qsTr("Each column is a separate multinomial distribution; column totals are fixed.")	}
			RadioButton { value: "hypergeometric";						label: qsTr("Hypergeometric (2x2 only)"); id: hypergeometric;	info: qsTr("Assumes fixed row and column totals; suitable for exact tests.")	}
		}

		Group
		{
			title: qsTr("Additional Statistics")
			CheckBox
			{
				name: "oddsRatio";	label: qsTr("Log odds ratio (2x2 only)");	info: qsTr("Displays the odds ratio for 2×2 tables (ad/bc) with a credible interval.")
				CIField { name: "oddsRatioCiLevel"; label: qsTr("Credible interval"); info: qsTr("Range where the true log odds ratio likely falls, given the data.") }
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
			RadioButton { value: "twoSided";	label: qsTr("Group one ≠ Group two"); checked: true; 	info: qsTr("Two-sided alternative hypothesis that the proportion of group 1 is not equal to the proportion of group 2.")	}
			RadioButton { value: "greater";		label: qsTr("Group one > Group two");					info: qsTr("One-sided alternative hypothesis that the proportion of group 1 is greater than the proportion of group 2.")	}
			RadioButton { value: "less";		label: qsTr("Group one < Group two");					info: qsTr("One-sided alternative hypothesis that the proportion of group 1 is less than the proportion of group 2.")	}
		}

		Group
		{
			title: qsTr("Plots")
			CheckBox
			{
				name: "posteriorOddsRatioPlot";			label: qsTr("Log odds ratio (2x2 only)");		info: qsTr("Displays a graphical summary of the log odds ratio.")
				CheckBox { name: "posteriorOddsRatioPlotAdditionalInfo"; label: qsTr("Additional info"); checked: true }
			}
			CheckBox { name: "cramersVPlot";	label: qsTr("Cramer's V"); debug: true }
		}

		BayesFactorType {}

		Group
		{
			title: qsTr("Prior")
			DoubleField { name: "priorConcentration"; label: qsTr("Prior concentration"); defaultValue: 1; min: 1; decimals: 1; info: qsTr("Controls how strongly the prior favours equal proportions.") }
		}
	}

	Section
	{
		title: qsTr("Cells")

		Group
		{
			title:	qsTr("Counts")
			CheckBox { name: "countsObserved";	label: qsTr("Observed");	checked: true;	info: qsTr("Show actual counts from the data.") }
			CheckBox { name: "countsExpected";	label: qsTr("Expected");					info: qsTr("Show counts expected under the null hypothesis.") }
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
			CheckBox { name: "percentagesRow";		label: qsTr("Row");		info: qsTr("Shows row-wise percentages.")		}
			CheckBox { name: "percentagesColumn";	label: qsTr("Column");	info: qsTr("Shows column-wise percentages.")	}
			CheckBox { name: "percentagesTotal";	label: qsTr("Total");	info: qsTr("Shows percentages of total.")		}
		}

		Group
		{
			title: qsTr("Margin")
			CheckBox { name: "marginShowTotals";		label: qsTr("Show totals");		checked: true;		info: qsTr("Shows row and column totals.") }
		}
	}

	Section
	{
		title: qsTr("Options")

		RadioButtonGroup
		{
			name: "rowOrder"
			title: qsTr("Row Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true;	info: qsTr("Rows sorted in numerical order.")	}
			RadioButton { value: "descending";	label: qsTr("Descending");					info: qsTr("Opposite of above.")	}
		}
		RadioButtonGroup
		{
			name: "columnOrder"
			title: qsTr("Column Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true;	info: qsTr("Opposite of below.")	}
			RadioButton { value: "descending";	label: qsTr("Descending");					info: qsTr("Columns sorted in reverse numerical order.")	}
		}

		SetSeed{}
	}
}
