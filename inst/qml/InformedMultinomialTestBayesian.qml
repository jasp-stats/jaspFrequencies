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
import JASP.Controls
import JASP


Form
{
	VariablesForm
	{
		preferredHeight:				190 * preferencesModel.uiScale
		marginBetweenVariablesLists:	15

		AvailableVariablesList
		{
			name:				"allVariablesList"
		}

		AssignedVariablesList
		{
			id:					factors
			name:				"factor"
			title:				qsTr("Factor")
			singleVariable:		true
			allowedColumns:		["nominal"]
		}

		AssignedVariablesList
		{
			name:				"count"
			title:				qsTr("Counts")
			singleVariable:		true
			allowedColumns:		["scale"]
		}
	}

	ColumnLayout
	{

		RadioButtonGroup
		{
			title:		qsTr("Bayes Factor Comparison")
			name:		"bfComparison"

			RadioButton
			{
				id:			bfComparisonNull
				value:		"Null"
				label:		qsTr("Null")
				checked:	true
				enabled:	includeNullModel.checked
				onEnabledChanged:	
				{
					if (!enabled && checked) 
					{
						if (bfComparisonEncompassing.enabled)
							bfComparisonEncompassing.checked = true 
						else
							bfComparisonVs.checked = true
					}
				}
			}

			RadioButton
			{
				id:			bfComparisonEncompassing
				value:		"encompassing"
				label:		qsTr("Encompassing")
				enabled:	includeEncompassingModel.checked
				onEnabledChanged:	
				{
					if (!enabled && checked) 
						bfComparisonVs.checked = true
				}
			}

			RadioButton
			{
				id:           		bfComparisonVs
				name:				"vs"
				label:				qsTr("vs.")
				childrenOnSameRow:	true

				DropDown
				{
					source:				[ models ]
					name:				"bfVsHypothesis"
					id:					bfTypevsName
					indexDefaultValue:	0
				}
			}
		}

		BayesFactorType { }

	}

	ColumnLayout
	{

		Group
		{
			title:	"Descriptives"

			CheckBox
			{
				name:	"descriptivesTable"
				label:	qsTr("Table")
			}

			CheckBox
			{
				name:		"descriptivesPlot"
				id:			descriptivesPlot
				label:		qsTr("Plot")
			}

			RadioButtonGroup
			{
				name	: "descriptivesDisplay"
				title	: qsTr("Display")

				RadioButton
				{
					value:		"counts"
					label:		qsTr("Counts")
					checked:	true
				}

				RadioButton
				{
					value:		"proportions"
					label:		qsTr("Proportions")
				}
			}
		}
	

		CheckBox
		{
			name:		"posteriorPlot"
			id:			posteriorPlot
			label:		qsTr("Posterior plot")

			CIField
			{
				name:		"posteriorPlotCiCoverage"
				label:		qsTr("Credible interval")
			}
		}

		CheckBox
		{
			name:	"sequentialAnalysisPlot";
			label:	qsTr("Sequential analysis plot")

			RadioButtonGroup
			{
				name	: "sequentialAnalysisPlotType"
				title	: qsTr("Display")

				RadioButton
				{
					value:		"bayesFactor"
					label:		qsTr("Bayes factors")
					checked:	true
				}

				RadioButton
				{
					value:		"posteriorProbability"
					label:		qsTr("Posterior probability")
				}
			}
		}


	}

	Section
	{
		title	: qsTr("Prior Distribution")
		
		Text
		{
			visible: factors.count == 0
			text: qsTr("No factor specified")
		}

		Chi2TestTableView
		{
			name			: "priorCounts"

			source			: "factor"
			visible			: factors.count > 0

			minimum				: 1
			showAddButton		: false
			showDeleteButton	: false
			colHeader			: qsTr("Counts")
			cornerText			: qsTr("Level #")
			itemType			: JASP.Integer
		}
	}

	Section
	{
		title	: qsTr("Prior Model Probability")

		SimpleTableView
		{
			name:					"priorModelProbability"
			id:						priorModelProbability
			initialColumnCount: 	1
			property var alwaysAvailable:
			includeNullModel.checked && includeEncompassingModel.checked ?
			[
				{ label:	"Null",				value: "Null"},
				{ label:	"Encompassing",		value: "Encompassing"}
			]
			: !includeNullModel.checked && includeEncompassingModel.checked ?
			[
				{ label:	"Encompassing",		value: "Encompassing"}
			]
			: includeNullModel.checked && !includeEncompassingModel.checked ?
			[
				{ label:	"Null",		value: "Null"}
			]
			:
			[]

			source:	[{values: priorModelProbability.alwaysAvailable}, models]

			minimum				: 1
			showAddButton		: false
			showDeleteButton	: false
			buttonResetEnabled	: true
			//colHeader			: ""
			cornerText			: qsTr("Model")
			itemType			: JASP.String

			function getColHeaderText(headerText, colIndex) { return "Prior weight"}
		}
	}


	Section
	{
		
		title:		qsTr("Order Restricted Hypotheses")
		columns:	2
		expanded:	true
		
		Text
		{
			Layout.columnSpan:	1
			text:				qsTr("Enter each restriction of one hypothesis on a new line,\ne.g., Low <= Mid <= High\nwhere 'Low'/'Mid'/'High' are the factor level names.\nClick on the 'plus' icon to add more hypotheses. \nClick the information icon for more examples.")
		}

		HelpButton
		{
			toolTip:			qsTr("Click to learn more about the syntax for order restrictions.")
			helpPage:			"goric/restriktorSyntax"
			Layout.columnSpan:	1
		}

		TabView
		{
			id:					models
			name:				"models"
			maximumItems:		10
			newItemName:		qsTr("Model 1")
			optionKey:			"modelName"
			Layout.columnSpan:	2

			content: TextArea
			{
				name:				"syntax"
				textType:			JASP.TextTypeModel
				trim:				true
				applyScriptInfo:	qsTr("Ctrl + Enter to apply. Click on the blue button above for help on the restriction syntax")
			}
		}
	}

	Section
	{
		columns: 	2
		title: 		qsTr("Advanced")
		
		Group
		{
			title: 		qsTr("Estimation settings")
			columns: 	1

			IntegerField
			{
				label: 			qsTr("Burnin (MCMC)")
				name: 			"mcmcBurnin"
				defaultValue: 	500
				min:			50
				max: 			1000000
				fieldWidth: 	75
			}

			IntegerField
			{
				label: 			qsTr("Iterations (MCMC)")
				name: 			"mcmcSamples"
				defaultValue: 	10000
				min:			100
				max: 			1000000
				fieldWidth: 	75
			}

			IntegerField
			{
				label: 			qsTr("Maximum samples (bridgesampling)")
				name: 			"bridgeSamples"
				defaultValue: 	10000
				min:			5
				max: 			1000000
				fieldWidth: 	75
			}
		}

		Group
		{

			SetSeed { }

			Group
			{
				title:		qsTr("Sequential analysis")
				
				IntegerField
				{
					label: 			qsTr("Number of steps")
					name: 			"sequentialAnalysisNumberOfSteps"
					defaultValue: 	10
					min:			0
					fieldWidth: 	50
				}
			}

			Group
			{
				title:		qsTr("Include")

				CheckBox
				{
					name:		"includeNullModel"
					id:			includeNullModel
					label:		qsTr("Null model")
					checked:	true
				}
				
				CheckBox
				{
					name:		"includeEncompassingModel"
					id:			includeEncompassingModel
					label:		qsTr("Encompassing model")
					checked:	true
				}
			}
		}
	}
}
