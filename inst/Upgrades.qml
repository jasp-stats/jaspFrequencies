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
}
