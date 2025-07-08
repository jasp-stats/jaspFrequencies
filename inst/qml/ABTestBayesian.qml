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
	info: qsTr("The Bayesian A/B test allows one to monitor the evidence for the hypotheses that an intervention or treatment has either a positive effect, a negative effect or no effect.")

	VariablesForm
	{
		preferredHeight: 260 * preferencesModel.uiScale
		marginBetweenVariablesLists	: 15
		info: qsTr("**Input**\n" + "1. Each of the below elements needs to be an integer.\n" + "2. A cumulative sequence of successes/trials can also be given as input.")
		AvailableVariablesList	{ name: "allVariablesList" }
		AssignedVariablesList	{ name: "y1";	title: qsTr("Successes Group 1");	singleVariable: true;	allowedColumns: ["scale"]; 	info: qsTr("Number of successes in group 1 (control condition).") 		}
		AssignedVariablesList	{ name: "n1";	title: qsTr("Sample Size Group 1");	singleVariable: true;	allowedColumns: ["scale"]; 	info: qsTr("Number of trials in group 1 (control condition).") 			}
		AssignedVariablesList	{ name: "y2";	title: qsTr("Successes Group 2");	singleVariable: true;	allowedColumns: ["scale"]; 	info: qsTr("Number of successes in group 2 (experimental condition).") 	}
		AssignedVariablesList	{ name: "n2";	title: qsTr("Sample Size Group 2");	singleVariable: true;	allowedColumns: ["scale"]; 	info: qsTr("Number of trials in group 2 (experimental condition).") 	}
	}

	ColumnLayout
	{
		BayesFactorType { id: bayesFactorType }

		Group
		{
			title	: qsTr("Normal Prior on Log Odds Ratio")

			DoubleField { label: qsTr("\u03bc:"); name: "normalPriorMean";		defaultValue: 0;	negativeValues: true; 	info: qsTr("Specifies the mean for the normal prior on the test-relevant log odds ratio.")					}
			DoubleField { label: qsTr("\u03c3:"); name: "normalPriorSd";		defaultValue: 1;							info: qsTr("Specifies the standard deviation for the normal prior on the test-relevant log odds ratio.")	}
		}

		CheckBox
		{
			name	: "descriptivesTable";
			label	: qsTr("Descriptives")
			info	: qsTr("Displays the counts and proportion for each group.")
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
				info	: qsTr("Displays the prior and posterior density for the quantity of interest.")
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
				info	: qsTr("Displays the development of posterior probabilities as the data come in. The probability wheels visualize prior and posterior probabilities of the hypotheses.")
			}

			CheckBox
			{
				name	: "priorPlot"
				label	: qsTr("Prior")
				info	: qsTr("Plots parameter prior distributions.")
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
				info				: qsTr("Displays the prior sensitivity analysis.")
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
			info	: qsTr("Compares each model against the model selected")
			RadioButton { value: "bestModelTop";	label: qsTr("Compare to best model.");	checked: true	}
			RadioButton { value: "nullModelTop";	label: qsTr("Compare to null model.")	}
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
				info:  qsTr("Specifies the prior probabilities for the four hypotheses.")
				DoubleField { name: "priorModelProbabilityEqual";		label: qsTr("Log odds ratio = 0");	defaultValue: 0.5;		max: 1;	min: 0;	decimals: 3;	info:  qsTr("Specifies that the 'success' probability is identical (there is no effect).")	}
				DoubleField { name: "priorModelProbabilityGreater";		label: qsTr("Log odds ratio > 0");	defaultValue: 0.25;		max: 1;	min: 0;	decimals: 3;	info:  qsTr("Specifies that the 'success' probability in the experimental condition is higher than in the control condition.")	}
				DoubleField { name: "priorModelProbabilityLess";		label: qsTr("Log odds ratio < 0");		defaultValue: 0.25;	max: 1;	min: 0;	decimals: 3;	info:  qsTr("Specifies that the 'success' probability in the experimental condition is lower than in the control condition.")	}
				DoubleField { name: "priorModelProbabilityTwoSided";	label: qsTr("Log odds ratio \u2260 0");	defaultValue: 0;	max: 1;	min: 0;	decimals: 3;	info:  qsTr("Specifies that the 'success' probability differs between the control and experimental condition, but does not specify which one is higher.")	}
			}

			Group
			{
				title: qsTr("Sampling")
				IntegerField { name: "samples"; label: qsTr("No. samples"); defaultValue: 10000; min: 100; fieldWidth: 50; info:  qsTr("Specifies the number of importance samples for obtaining log marginal likelihood for (H+) and (H-) and the number of posterior samples.")}
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
					IntegerField { label: qsTr("\u03bc:"); name: "bfRobustnessPlotStepsPriorMean";	defaultValue: 5; min: 3;	info:  qsTr("Specifies in how many discrete steps the \u03bc step range is partitioned.") }
					IntegerField { label: qsTr("\u03c3:"); name: "bfRobustnessPlotStepsPriorSd";	defaultValue: 5; min: 3;	info:  qsTr("Specifies in how many discrete steps the \u03c3 step range is partitioned.") }
				}

				Group
				{
					title	: qsTr("Step Range")
					info: qsTr("Specifies the range of \u03bc and \u03c3 values to consider.")
					columns : 3

					Label { text: "\u03bc:"; Layout.fillHeight: true; verticalAlignment: Text.AlignVCenter}
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

					Label { text: "\u03c3:"; Layout.fillHeight: true; verticalAlignment: Text.AlignVCenter}
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
