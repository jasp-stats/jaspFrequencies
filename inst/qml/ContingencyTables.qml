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
	info: qsTr("Contingency tables allow the user to identify how the frequencies of one categorical variable relate to another, helping to determine associations between variables.")
	
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
			CheckBox { name: "chiSquared";						label: qsTr("χ²"); checked: true;			info: qsTr("Pearson’s chi-squared test for independence.")	}
			CheckBox { name: "chiSquaredContinuityCorrection";	label: qsTr("χ² continuity correction");	info: qsTr("Applies Yates’ correction for continuity (for 2×2 tables).")	}
			CheckBox { name: "likelihoodRatio";					label: qsTr("Likelihood ratio");			info: qsTr("Calculates the likelihood of the data under the alternative hypothesis divided by the likelihood of the data under the null hypothesis.")	}
			CheckBox { name: "vovkSellke";						label: qsTr("Vovk-Sellke maximum p-ratio");	info: qsTr("An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.")	}
		}

		Group
		{
		    CheckBox
			{
				name: "oddsRatio"; label: qsTr("Odds ratio (2x2 only)"); info: qsTr("Displays the odds ratio for 2×2 tables (ad/bc).")	
				CheckBox
				{
					name: "oddsRatioAsLogOdds";     label: qsTr("Log Odds Ratio");	checked: true; info: qsTr("Shows the log-transformed odds ratio.")	
				}
				CIField { name: "oddsRatioCiLevel"; label: qsTr("Confidence interval"); info: qsTr("Coverage of the confidence intervals in percentages. The default value is 95.")	 }
				RadioButtonGroup
				{
					title: qsTr("Alt. Hypothesis (Fisher's exact test)")
					name: "oddsRatioAlternative"
					info: qsTr("Choose your fighter! Pick your alternative hypothesis")
					RadioButton { value: "twoSided";	label: qsTr("Group one ≠ Group two"); checked: true; 	info: qsTr("Two-sided alternative hypothesis that the proportion of group 1 is not equal to the proportion of group 2.")		}
					RadioButton { value: "greater";		label: qsTr("Group one > Group two"); 					info: qsTr("One-sided alternative hypothesis that the proportion of group 1 is greater than the proportion of group 2.")		}
					RadioButton { value: "less";		label: qsTr("Group one < Group two"); 					info: qsTr("One-sided alternative hypothesis that the proportion of group 1 is less than the proportion of group 2.")		}
				}
			}
		}

		Group
		{
			title: qsTr("Nominal")
			CheckBox { name: "contingencyCoefficient" ; label: qsTr("Contingency coefficient"); 				info: qsTr("Measure of association for nominal variables (based on χ²).")					}
			CheckBox { name: "phiAndCramersV";			label: qsTr("Phi and Cramer's V"); 						info: qsTr("Effect size for nominal association; Phi (2×2), Cramer’s V (larger tables).")	}
			CheckBox { name: "lambda";					label: qsTr("Lambda");									info: qsTr("Proportion reduction in error measure for nominal data.")						}
			CheckBox { name: "uncertaintyCoefficient";	label: qsTr("Uncertainty coefficient");	debug: true }
		}

		Group
		{
			title: qsTr("Ordinal")
			CheckBox { name: "gamma";			label: qsTr("Gamma");							info: qsTr("Measure of ordinal association based on concordant/discordant pairs.")	}
			CheckBox { name: "somersD";			label: qsTr("Somers' d");		debug: true	}
			CheckBox { name: "kendallsTauB";	label: qsTr("Kendall's tau-b"); 				info: qsTr("Ordinal correlation adjusting for ties.")	}
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
			CheckBox { name: "countsObserved";	label: qsTr("Observed");	checked: true;  info: qsTr("Show actual counts from the data.")	 }
			CheckBox { name: "countsExpected";	label: qsTr("Expected"); 					info: qsTr("Show counts expected under the null hypothesis.")	 }
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
			CheckBox { name: "residualsUnstandardized";	label: qsTr("Unstandardized"); 					info: qsTr("Difference between observed and expected counts.")			}
			CheckBox { name: "residualsPearson";		label: qsTr("Pearson"); 						info: qsTr("Standardized residuals based on Pearson’s χ².")				}
			CheckBox { name: "residualsStandardized";	label: qsTr("Standardized (adjusted Pearson)"); info: qsTr("Adjusted residuals accounting for row/column totals.")		}
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
			CheckBox { name: "marginShowTotals";		label: qsTr("Show totals");		checked: true; 	info: qsTr("Shows row and column totals.")	 }
		}

	}

	Section
	{
		title: qsTr("Options")

		RadioButtonGroup
		{
			name: "rowOrder"
			title: qsTr("Row Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true; 	info: qsTr("Rows sorted in numerical order.")		}
			RadioButton { value: "descending";	label: qsTr("Descending");					info: qsTr("Makes your data go topsy-turvy.")		}
		}
		RadioButtonGroup
		{
			name: "columnOrder"
			title: qsTr("Column Order")
			RadioButton { value: "ascending";	label: qsTr("Ascending"); checked: true;	info: qsTr("Columns sorted in numerical order.")				}
			RadioButton { value: "descending";	label: qsTr("Descending");					info: qsTr("What your data looks like in the mirror.")		}
		}
	}
}

