import QtQuick		2.12
import JASP.Module	1.0

Upgrades
{
	Upgrade
	{
		functionName: 		"BinomialTest"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

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
		ChangeRename{	from: "descriptivesPlots";						to: "descriptivePlot"			}
		ChangeRename{	from: "descriptivesPlotsConfidenceInterval";	to: "descriptivePlotCiLevel"	}
	}

	Upgrade
	{
		functionName: 		"BinomialTestBayesian"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

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
		ChangeRename{	from: "plotPriorAndPosterior";					to: "priorAndPosteriorPlot"					}
		ChangeRename{	from: "plotPriorAndPosteriorAdditionalInfo";	to: "priorAndPosteriorPlotAdditionalInfo"	}
		ChangeRename{	from: "plotSequentialAnalysis";					to: "sequentialAnalysisPlot"				}
		ChangeRename{	from: "descriptivesPlots";						to: "descriptivePlot"						}
		ChangeRename{	from: "descriptivesPlotsCredibleInterval";		to: "descriptivePlotCiLevel"				}
	}

	Upgrade
	{
		functionName:		"MultinomialTest"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

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
		ChangeRename{	from: "descriptives";					to: "descriptiveTable"			}
		ChangeRename{	from: "confidenceInterval";				to: "descriptiveTableCi"		}
		ChangeRename{	from: "confidenceIntervalInterval";		to: "descriptiveTableCiLevel"	}
		ChangeRename{	from: "countProp";						to: "descriptivesAs"			}
		ChangeJS
		{
			name: "descriptivesAs"
			jsFunction:	function(options)
			{
				switch(options["descriptivesAs"])
				{
					case "descCounts":		return "counts";
					case "descProps":		return "proportions";
				}
			}
		}
		ChangeRename{	from: "descriptivesPlot";						to: "descriptivePlot"			}
		ChangeRename{	from: "descriptivesPlotConfidenceInterval";		to: "descriptivePlotCiLevel"	}
	}

	Upgrade
	{
		functionName:		"MultinomialTestBayesian"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

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
		ChangeRename{	from: "descriptives";					to: "descriptiveTable"			}
		ChangeRename{	from: "credibleInterval";				to: "descriptiveTableCi"		}
		ChangeRename{	from: "credibleIntervalInterval";		to: "descriptiveTableCiLevel"	}
		ChangeRename{	from: "countProp";						to: "descriptivesAs"			}
		ChangeJS
		{
			name: "descriptivesAs"
			jsFunction:	function(options)
			{
				switch(options["descriptivesAs"])
				{
					case "descCounts":		return "counts";
					case "descProps":		return "proportions";
				}
			}
		}
		ChangeRename{	from: "descriptivesPlot";						to: "descriptivePlot"			}
		ChangeRename{	from: "descriptivesPlotCredibleInterval";		to: "descriptivePlotCiLevel"	}
	}

	Upgrade
	{
		functionName:		"ContingencyTables"
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

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
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

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
		fromVersion:		"0.15"
		toVersion:			"0.16.4"

		ChangeRename{	from: "counts";													to: "count"								}
		ChangeRename{	from: "regressionCoefficientsConfidenceIntervals";				to: "regressionCoefficientsCi"			}
		ChangeRename{	from: "regressionCoefficientsConfidenceIntervalsInterval";		to: "regressionCoefficientsCiLevel"		}
		ChangeRename{	from: "VovkSellkeMPR";											to: "vovkSellke"						}
	}
}
