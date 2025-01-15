import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName: 		"BinomialTest"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "hypothesis";	to: "alternative"	}
		ChangeJS
		{
			name:		"alternative"
			jsFunction:	function(options)
			{
				switch(options["alternative"])
				{
					case "notEqualToTestValue":		return "twoSided";
					case "greaterThanTestValue":	return "greater";
					case "lessThanTestValue":		return "less";
				}
			}
		}
		ChangeRename{	from: "confidenceInterval";						to: "ci"						}
		ChangeRename{	from: "confidenceIntervalInterval";				to: "ciLevel"					}
		ChangeRename{	from: "VovkSellkeMPR";							to: "vovkSellke"				}
		ChangeRename{	from: "descriptivesPlots";						to: "descriptivesPlot"			}
		ChangeRename{	from: "descriptivesPlotsConfidenceInterval";	to: "descriptivesPlotCiLevel"	}
	}

	Upgrade
	{
		functionName: 		"BinomialTestBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "hypothesis";	to: "alternative"	}
		ChangeJS
		{
			name:		"alternative"
			jsFunction:	function(options)
			{
				switch(options["alternative"])
				{
					case "notEqualToTestValue":		return "twoSided";
					case "greaterThanTestValue":	return "greater";
					case "lessThanTestValue":		return "less";
				}
			}
		}
		ChangeRename{	from: "plotPriorAndPosterior";					to: "priorPosteriorPlot"					}
		ChangeRename{	from: "plotPriorAndPosteriorAdditionalInfo";	to: "priorPosteriorPlotAdditionalInfo"	}
		ChangeRename{	from: "plotSequentialAnalysis";					to: "bfSequentialPlot"				}
		ChangeRename{	from: "descriptivesPlots";						to: "descriptivesPlot"						}
		ChangeRename{	from: "descriptivesPlotsCredibleInterval";		to: "descriptivesPlotCiLevel"				}
	}

	Upgrade
	{
		functionName:		"MultinomialTest"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		// this option did not do anything and was always hidden so now it's removed
		ChangeRemove{	name: "simulatepval"	}
		ChangeRename{	from: "counts";					to: "count"				}
		ChangeRename{	from: "exProbVar";				to: "expectedCount"		}
		ChangeRename{	from: "hypothesis";				to: "testValues"		}
		ChangeJS
		{
			name:		"testValues"
			jsFunction:	function(options)
			{
				switch(options["testValues"])
				{
					case "multinomialTest":		return "equal";
					case "expectedProbs":		return "custom";
				}
			}
		}
		ChangeRename{	from: "tableWidget";					to: "testValuesCustom"			}
		ChangeRename{	from: "VovkSellkeMPR";					to: "vovkSellke"				}
		ChangeRename{	from: "descriptives";					to: "descriptivesTable"			}
		ChangeRename{	from: "confidenceInterval";				to: "descriptivesTableCi"		}
		ChangeRename{	from: "confidenceIntervalInterval";		to: "descriptivesTableCiLevel"	}
		ChangeRename{	from: "countProp";						to: "descriptivesType"			}
		ChangeJS
		{
			name: "descriptivesType"
			jsFunction:	function(options)
			{
				switch(options["descriptivesType"])
				{
					case "descCounts":		return "counts";
					case "descProps":		return "proportions";
				}
			}
		}
		ChangeRename{	from: "descriptivesPlotConfidenceInterval";		to: "descriptivesPlotCiLevel"	}
	}

	Upgrade
	{
		functionName:		"MultinomialTestBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "counts";					to: "count"				}
		ChangeRename{	from: "exProbVar";				to: "expectedCount"		}
		ChangeRename{	from: "hypothesis";				to: "testValues"		}
		ChangeJS
		{
			name:		"testValues"
			jsFunction:	function(options)
			{
				switch(options["testValues"])
				{
					case "multinomialTest":		return "equal";
					case "expectedProbs":		return "custom";
				}
			}
		}
		ChangeRename{	from: "tableWidget";					to: "testValuesCustom"			}
		ChangeRename{	from: "descriptives";					to: "descriptivesTable"			}
		ChangeRename{	from: "credibleInterval";				to: "descriptivesTableCi"		}
		ChangeRename{	from: "credibleIntervalInterval";		to: "descriptivesTableCiLevel"	}
		ChangeRename{	from: "countProp";						to: "descriptivesType"			}
		ChangeJS
		{
			name: "descriptivesType"
			jsFunction:	function(options)
			{
				switch(options["descriptivesType"])
				{
					case "descCounts":		return "counts";
					case "descProps":		return "proportions";
				}
			}
		}
		ChangeRename{	from: "descriptivesPlotCredibleInterval";		to: "descriptivesPlotCiLevel"	}
	}

	Upgrade
	{
		functionName:		"ContingencyTables"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "VovkSellkeMPR";							to: "vovkSellke"				}
		ChangeRename{	from: "LogOdds";								to: "oddsRatioAsLogOdds"		}
		ChangeRename{	from: "oddsRatioConfidenceIntervalInterval";	to: "oddsRatioCiLevel"			}
		ChangeRename{	from: "oddsRatioHypothesis";					to: "oddsRatioAlternative"		}
		ChangeSetValue
		{
			name:		"oddsRatioAlternative"
			condition:	function(options) { return options["oddsRatioAlternative"] === "two.sided" }
			jsonValue:	"twoSided"
		}
		ChangeRename{	from: "cochransAndMantel";						to: "cochranAndMantel"						}
		ChangeRename{	from: "testOddsRatioEquals";					to: "cochranAndMantelCommonOddsRatioTest"	}
		ChangeRename{	from: "hideSmallCounts";						to: "countsHiddenSmallCounts"				}
		ChangeRename{	from: "hideSmallCountsLessThan";				to: "countsHiddenSmallCountsThreshold"		}
		ChangeRename{	from: "zTestCompareColumns";					to: "zTestColumnComparison"					}
		ChangeRename{	from: "zTestAdjustPValues";						to: "zTestAdjustedPValues"					}
	}

	Upgrade
	{
		functionName:		"ContingencyTablesBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "oddsRatioCredibleIntervalInterval";			to: "oddsRatioCiLevel"	}
		ChangeRename{	from: "effectSize";									to: "cramersV"	}
		ChangeRename{	from: "effectSizeCredibleIntervalInterval";			to: "cramersVCiLevel"	}
		ChangeRename{	from: "hypothesis";									to: "alternative"		}
		ChangeJS
		{
			name:		"alternative"
			jsFunction:	function(options)
			{
				switch(options["alternative"])
				{
					case "groupsNotEqual":		return "twoSided";
					case "groupOneGreater":		return "greater";
					case "groupTwoGreater":		return "less";
				}
			}
		}
		ChangeRename{	from: "plotPosteriorOddsRatio";					to: "posteriorOddsRatioPlot"				}
		ChangeRename{	from: "plotPosteriorOddsRatioAdditionalInfo";	to: "posteriorOddsRatioPlotAdditionalInfo"	}
		ChangeRename{	from: "plotPosteriorEffectSize";				to: "cramersVPlot"							}
	}

	Upgrade
	{
		functionName:		"RegressionLogLinear"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "counts";													to: "count"								}
		ChangeRename{	from: "regressionCoefficientsConfidenceIntervals";				to: "regressionCoefficientsCi"			}
		ChangeRename{	from: "regressionCoefficientsConfidenceIntervalsInterval";		to: "regressionCoefficientsCiLevel"		}
		ChangeRename{	from: "VovkSellkeMPR";											to: "vovkSellke"						}
	}

	Upgrade
	{
		functionName:		"RegressionLogLinearBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "counts";															to: "count"										}
		ChangeRename{	from: "maxModels";														to: "modelCutOffBestDisplayed"					}
		ChangeRename{	from: "posteriorProbabilityCutOff";										to: "modelCutOffPosteriorProbability"			}
		ChangeRename{	from: "regressionCoefficientsCredibleIntervals";						to: "regressionCoefficientsCi"					}
		ChangeRename{	from: "regressionCoefficientsCredibleIntervalsInterval";				to: "regressionCoefficientsCiLevel"				}
		ChangeRename{	from: "regressionCoefficientsSubmodelCredibleIntervals";				to: "regressionCoefficientsSubmodelCi"			}
		ChangeRename{	from: "regressionCoefficientsSubmodelCredibleIntervalsInterval";		to: "regressionCoefficientsSubmodelCiLevel"		}
		ChangeRename{	from: "sampleMode";														to: "samplingMethod"							}
		ChangeRename{	from: "fixedSamplesNumber";												to: "samplingMethodManualSamples"				}
	}

	Upgrade
	{
		functionName:		"ABTestBayesian"
		fromVersion:		"0.16.4"
		toVersion:			"0.17.0"

		ChangeRename{	from: "normal_mu";					to: "normalPriorMean"				}
		ChangeRename{	from: "normal_sigma";				to: "normalPriorSd"					}
		ChangeRename{	from: "descriptives";				to: "descriptivesTable"				}
		ChangeRename{	from: "plotPriorAndPosterior";		to: "priorPosteriorPlot"			}
		ChangeRename{	from: "plotPosteriorType";			to: "priorPosteriorPlotType"		}
		ChangeJS
		{
			name:		"priorPosteriorPlotType"
			jsFunction:	function(options)
			{
				switch(options["priorPosteriorPlotType"])
				{
					case "LogOddsRatio":		return "logOddsRatio";
					case "OddsRatio":			return "oddsRatio";
					case "RelativeRisk":		return "relativeRisk";
					case "AbsoluteRisk":		return "absoluteRisk";
					case "p1&p2":				return "p1P2";
				}
			}
		}
		ChangeRename{	from: "plotSequentialAnalysis";		to: "bfSequentialPlot"		}
		ChangeRename{	from: "plotPriorOnly";				to: "priorPlot"						}
		ChangeRename{	from: "plotPriorType";				to: "priorPlotType"					}
		ChangeJS
		{
			name:		"priorPlotType"
			jsFunction:	function(options)
			{
				switch(options["priorPlotType"])
				{
					case "LogOddsRatio":		return "logOddsRatio";
					case "OddsRatio":			return "oddsRatio";
					case "RelativeRisk":		return "relativeRisk";
					case "AbsoluteRisk":		return "absoluteRisk";
					case "p1&p2":				return "p1P2";
					default:					return options["priorPlotType"];
				}
			}
		}
		ChangeRename{	from: "plotRobustness";						to: "bfRobustnessPlot"					}
		ChangeRename{	from: "plotRobustnessBFType";				to: "bfRobustnessPlotType"				}
		ChangeRename{	from: "orEqualTo1Prob";						to: "priorModelProbabilityEqual"		}
		ChangeRename{	from: "orGreaterThan1Prob";					to: "priorModelProbabilityGreater"		}
		ChangeRename{	from: "orLessThan1Prob";					to: "priorModelProbabilityLess"			}
		ChangeRename{	from: "orNotEqualTo1Prob";					to: "priorModelProbabilityTwoSided"		}
		ChangeRename{	from: "numSamples";							to: "samples"							}
		ChangeRename{	from: "mu_stepsize";						to: "bfRobustnessPlotStepsPriorMean"		}
		ChangeRename{	from: "sigma_stepsize";						to: "bfRobustnessPlotStepsPriorSd"		}
		ChangeRename{	from: "mu_stepsize_lower";					to: "bfRobustnessPlotLowerPriorMean"		}
		ChangeRename{	from: "mu_stepsize_upper";					to: "bfRobustnessPlotUpperPriorMean"		}
		ChangeRename{	from: "sigma_stepsize_lower";				to: "bfRobustnessPlotLowerPriorSd"		}
		ChangeRename{	from: "sigma_stepsize_upper";				to: "bfRobustnessPlotUpperPriorSd"		}
	}
}
