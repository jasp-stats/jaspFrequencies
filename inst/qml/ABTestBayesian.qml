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
		preferredHeight: 260 * preferencesModel.uiScale
		marginBetweenVariablesLists	: 15

		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "y1";	title: qsTr("Successes Group 1");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
		AssignedVariablesList	{ name: "n1";	title: qsTr("Sample Size Group 1");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
		AssignedVariablesList	{ name: "y2";	title: qsTr("Successes Group 2");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
		AssignedVariablesList	{ name: "n2";	title: qsTr("Sample Size Group 2");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
	}

	ColumnLayout
	{
		BayesFactorType { id: bayesFactorType }

		Group
		{
			title	: qsTr("Normal Prior on Log Odds Ratio")

			DoubleField { label: qsTr("\u03bc:"); name: "normalPriorMean";		defaultValue: 0;	negativeValues: true}
			DoubleField { label: qsTr("\u03c3:"); name: "normalPriorSd";		defaultValue: 1 }
		}

		CheckBox
		{
			name	: "descriptivesTable";
			label	: qsTr("Descriptives")
		}
	}

	ColumnLayout
	{
		Group
		{
			title	: qsTr("Plots")
			CheckBox
			{
				name	: "priorPosteriorPlot"
				label	: qsTr("Prior and posterior")
				childrenOnSameRow: true

				DropDown
				{
					id: plotPosteriorType
					name: "priorPosteriorPlotType"
					values:
					[
						{ value: "logOddsRatio",	label: qsTr("Log Odds Ratio")		},
						{ value: "oddsRatio",		label: qsTr("Odds Ratio")			},
						{ value: "relativeRisk",	label: qsTr("Relative Risk")		},
						{ value: "absoluteRisk",	label: qsTr("Absolute Risk")		},
						{ value: "p1P2",			label: qsTr("p1 & p2")				}
					]
				}
			}

			CheckBox
			{
				name	: "bfSequentialPlot"
				label	: qsTr("Sequential analysis")
			}

			CheckBox
			{
				name	: "priorPlot"
				label	: qsTr("Prior")
				childrenOnSameRow: true

				DropDown
				{
					id: plotPriorType
					name: "priorPlotType"
					values:
					[
						{ value: "logOddsRatio",	label: qsTr("Log Odds Ratio")		},
						{ value: "oddsRatio",		label: qsTr("Odds Ratio")			},
						{ value: "relativeRisk",	label: qsTr("Relative Risk")		},
						{ value: "absoluteRisk",	label: qsTr("Absolute Risk")		},
						{ value: "p1P2",			label: qsTr("p1 & p2")				},
						{ value: "p1",				label: qsTr("p1")					},
						{ value: "p1",				label: qsTr("p1")					},
					]
				}
			}

			CheckBox
			{
				name				: "bfRobustnessPlot"
				label				: qsTr("Bayes factor robustness check")
				childrenOnSameRow	: true

				DropDown
				{
					id		: plotRobustnessBFType
					name	: "bfRobustnessPlotType"
					values	: bayesFactorType.value == "BF01" ? ['BF01', 'BF0+', 'BF0-'] : ['BF10', 'BF+0', 'BF-0']
				}
			}
		}

		RadioButtonGroup
		{
			name	: "bayesFactorOrder"
			title	: qsTr("Order")
			RadioButton { value: "bestModelTop";	label: qsTr("Compare to best model");	checked: true	}
			RadioButton { value: "nullModelTop";	label: qsTr("Compare to null model")	}
		}
	}


	Section
	{
		title	: qsTr("Advanced Options")

		ColumnLayout
		{
			Group
			{
				title: qsTr("Prior Model Probability")
				DoubleField { name: "priorModelProbabilityEqual";		label: qsTr("Log odds ratio = 0");	defaultValue: 0.5;		max: 1;	min: 0;	decimals: 3	}
				DoubleField { name: "priorModelProbabilityGreater";		label: qsTr("Log odds ratio > 0");	defaultValue: 0.25;		max: 1;	min: 0;	decimals: 3	}
				DoubleField { name: "priorModelProbabilityLess";		label: qsTr("Log odds ratio < 0");		defaultValue: 0.25;	max: 1;	min: 0;	decimals: 3	}
				DoubleField { name: "priorModelProbabilityTwoSided";	label: qsTr("Log odds ratio \u2260 0");	defaultValue: 0;	max: 1;	min: 0;	decimals: 3	}
			}

			Group
			{
				title: qsTr("Sampling")
				IntegerField { name: "samples"; label: qsTr("No. samples"); defaultValue: 10000; min: 100; fieldWidth: 50; }
			}

			SetSeed {}
		}

		ColumnLayout
		{
			Group
			{
				title	: qsTr("Robustness Plot")

				Group
				{
					title	: qsTr("No. Steps")
					IntegerField { label: qsTr("\u03bc:"); name: "bfRobustnessPlotStepsPriorMean";	defaultValue: 5; min: 3 }
					IntegerField { label: qsTr("\u03c3:"); name: "bfRobustnessPlotStepsPriorSd";	defaultValue: 5; min: 3 }
				}

				Group
				{
					title	: qsTr("Step Range")
					columns : 3

					Label { text: "\u03bc:"; Layout.fillHeight: true; verticalAlignment: Text.AlignVCenter }
					DoubleField
					{
						id				: muLower
						label			: qsTr("lower:")
						name			: "bfRobustnessPlotLowerPriorMean"
						defaultValue	: plotRobustnessBFType.currentText == "BF+0" ? 0 : -0.5
						max				: muUpper.value
						negativeValues	: true
						inclusive		: JASP.None

					}
					DoubleField
					{
						id				: muUpper
						label			: qsTr("upper:")
						name			: "bfRobustnessPlotUpperPriorMean"
						defaultValue	: plotRobustnessBFType.currentText == "BF-0" ? 0 : 0.5
						min				: muLower.value
						negativeValues	: true
						inclusive		: JASP.None
					}

					Label { text: "\u03c3:"; Layout.fillHeight: true; verticalAlignment: Text.AlignVCenter }
					DoubleField
					{
						id				: sigmaLower
						label			: qsTr("lower:")
						name			: "bfRobustnessPlotLowerPriorSd"
						defaultValue	: 0.1
						max				: sigmaUpper.value
						negativeValues	: false
						inclusive		: JASP.None
					}
					DoubleField
					{
						id				: sigmaUpper
						label			: qsTr("upper:")
						name			: "bfRobustnessPlotUpperPriorSd"
						defaultValue	: 1.0
						min				: sigmaLower.value
						negativeValues	: false
						inclusive		: JASP.None
					}
				}
			}
		}
	}
}
