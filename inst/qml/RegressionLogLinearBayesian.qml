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
		AssignedVariablesList { name: "count";	 title: qsTr("Counts (optional)"); singleVariable: true; allowedColumns:["scale"]}
		AssignedVariablesList { name: "factors"; title: qsTr("Factors"); itemType: "fixedFactors"; allowedColumns: ["nominal"] }
	}
	
	columns: 3
	BayesFactorType { }

	Group
	{
		title: qsTr("Prior")
		DoubleField { name: "priorShape"; label: qsTr("Shape"); defaultValue: -1 ; min: -1; info: qsTr("Specifies the shape of the prior distribution on the effect size.") }
		DoubleField { name: "priorScale"; label: qsTr("Scale"); defaultValue: 0;			info: qsTr("Controls how spread out the prior is; larger = more uncertainty.")  }
	}

	Group
	{
		title: qsTr("Model Cut-offs")
		IntegerField	{	name: "modelCutOffBestDisplayed";			label: qsTr("Display best") ;  defaultValue: 2; afterLabel: qsTr("models"); min: 2;	info: qsTr("Shows the top N models ranked by posterior probability.") }
		DoubleField		{	name: "modelCutOffPosteriorProbability";	label: qsTr("Posterior prob."); defaultValue: 0.1 ; max: 0.5;						info: qsTr("Only models with posterior probability above this threshold are shown.") }
	}
	
	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			
			AvailableVariablesList { name: "availableTerms"; title: qsTr("Components"); width: parent.width / 4; source: ['factors'] }
			AssignedVariablesList {  name: "modelTerms";	 title: qsTr("Model Terms"); width: parent.width * 5 / 9; listViewType: JASP.Interaction }
		}
	}
	
	Section
	{
		title: qsTr("Statistics")
		
		Group
		{
			title: qsTr("Regression Coefficients")
			CheckBox { name: "regressionCoefficientsEstimates";	label: qsTr("Estimates"); info: qsTr("Displays estimated regression coefficients for included terms.") }
			CheckBox
			{
				name: "regressionCoefficientsCi"; label: qsTr("Credible intervals"); info: qsTr("Adds a credible interval for each coefficient estimate.")
				CIField { name: "regressionCoefficientsCiLevel"; label: qsTr("Interval") }
			}
		}

		Group
		{
			CheckBox
			{
				name: "regressionCoefficientsSubmodel"; label: qsTr("Submodel"); id: regressionCoefficientsSubmodel; info: qsTr("Displays statistics for a specific model index.")
				childrenOnSameRow: true
				IntegerField { name: "regressionCoefficientsSubmodelNo"; defaultValue: 1 ; min: 1 }
			}

			Group
			{
				indent: true;
				enabled: regressionCoefficientsSubmodel.checked
				CheckBox
				{
					name: "regressionCoefficientsSubmodelCi"; label: qsTr("Credible intervals"); info: qsTr("Shows credible intervals for the selected submodel's coefficients.")
					CIField { name: "regressionCoefficientsSubmodelCiLevel"; label: qsTr("Interval") }
				}
			}
		}
	}
	
	Section
	{
		title: qsTr("Advanced")
		
		RadioButtonGroup
		{
			title: qsTr("Samples")
			name: "samplingMethod"
			RadioButton { value: "auto";	label: qsTr("Auto");  checked: true; 	info: qsTr("Automatically chooses the number of samples for estimation.")	}
			RadioButton
			{
				value: "manual";			label: qsTr("Manual");		info: qsTr("Allows you to set the number of posterior samples manually.")
				IntegerField { name: "samplingMethodManualSamples"; label: qsTr("No. samples"); defaultValue: 10000; fieldWidth: 60 }
			}
		}

		SetSeed{}

	}
}
