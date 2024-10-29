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

		Group
		{
			CheckBox { name: "chiSquared";						label: qsTr("χ²"); checked: true			}
			CheckBox { name: "chiSquaredContinuityCorrection";	label: qsTr("χ² continuity correction")	}
			CheckBox { name: "likelihoodRatio";					label: qsTr("Likelihood ratio")			}
			CheckBox { name: "vovkSellke";					label: qsTr("Vovk-Sellke maximum p-ratio") }
		}

		Group
		{
		    CheckBox
			{
				name: "oddsRatio"; label: qsTr("Odds ratio (2x2 only)")
				CheckBox
				{
					name: "oddsRatioAsLogOdds";     label: qsTr("Log Odds Ratio");	checked: true
				}
				CIField { name: "oddsRatioCiLevel"; label: qsTr("Confidence interval") }
				RadioButtonGroup
				{
					title: qsTr("Alt. Hypothesis (Fisher's exact test)")
					name: "oddsRatioAlternative"
					RadioButton { value: "twoSided";	label: qsTr("Group one ≠ Group two"); checked: true	}
					RadioButton { value: "greater";		label: qsTr("Group one > Group two")				}
					RadioButton { value: "less";		label: qsTr("Group one < Group two")				}
				}
			}
		}

		Group
		{
			title: qsTr("Nominal")
			CheckBox { name: "contingencyCoefficient" ; label: qsTr("Contingency coefficient")				}
			CheckBox { name: "phiAndCramersV";			label: qsTr("Phi and Cramer's V")					}
			CheckBox { name: "lambda";					label: qsTr("Lambda");								}
			CheckBox { name: "uncertaintyCoefficient";	label: qsTr("Uncertainty coefficient");	debug: true }
		}

		Group
		{
			title: qsTr("Ordinal")
			CheckBox { name: "gamma";			label: qsTr("Gamma")						}
			CheckBox { name: "somersD";			label: qsTr("Somers' d");		debug: true	}
			CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b")			}
			CheckBox { name: "kendallsTauC";	label: qsTr("Kendall's tau-c");	debug: true }
		}

		Group
		{
			debug: true
			title: qsTr("Nominal By Interval")
			CheckBox { name: "byIntervalEta"; label: qsTr("Eta") }
		}

		Group
		{
			debug: true
			Layout.columnSpan: 2
			CheckBox
			{
				name: "cochranAndMantel"; label: qsTr("Cochran's and Mantel-Haenszel statistics")
				DoubleField { name: "testOddsRatioEquals"; label: qsTr("Test common odds ratio equals"); defaultValue: 1 }
			}
		}
	}

	Section
	{
		title: qsTr("Cells")

		Group
		{
			title: qsTr("Counts")
			CheckBox { name: "countsObserved";	label: qsTr("Observed");	checked: true }
			CheckBox { name: "countsExpected";	label: qsTr("Expected") }
			CheckBox
			{
				name: "countsHiddenSmallCounts"; label: qsTr("Hide small counts"); debug: true
				IntegerField { name: "countsHiddenSmallCountsThreshold"; label: qsTr("Less than"); defaultValue: 5; debug: true }
			}
		}

		Group
		{
			title: qsTr("Z-Test")
			debug: true
			CheckBox
			{
				name: "zTestColumnComparison"; label: qsTr("Compare column proportions")
				CheckBox { name: "zTestAdjustedPValues";	label: qsTr("Adjust p-values") }
			}
		}

		Group
		{
			title: qsTr("Residuals")
			CheckBox { name: "residualsUnstandardized";	label: qsTr("Unstandardized")	}
			CheckBox { name: "residualsPearson";		label: qsTr("Pearson")			}
			CheckBox { name: "residualsStandardized";	label: qsTr("Standardized (adjusted Pearson)")		}
		}

		Group
		{
			title: qsTr("Percentages")
			CheckBox { name: "percentagesRow";		label: qsTr("Row")		}
			CheckBox { name: "percentagesColumn";	label: qsTr("Column")	}
			CheckBox { name: "percentagesTotal";	label: qsTr("Total")	}
		}

		
		CheckBox { name: "tableMargin";		label: qsTr("Table margin");		checked: true }
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
	}
}
