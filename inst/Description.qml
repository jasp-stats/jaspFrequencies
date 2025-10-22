import QtQuick
import JASP.Module

Description
{
	title		: qsTr("Frequencies")
	description	: qsTr("Analyses for count data")
	icon		: "analysis-classical-crosstabs.svg"
	preloadData	: false
	hasWrappers	: true

	GroupTitle
	{
		title:	qsTr("Classical")
		icon:	"analysis-classical-crosstabs.svg"
	}

	Analysis
	{
		title:	qsTr("Binomial Test")
		func:	"BinomialTest"
	}

	Analysis
	{
		title:	qsTr("Multinomial Test")
		func:	"MultinomialTest"
	}

	Analysis
	{
		title:	qsTr("Contingency Tables")
		func:	"ContingencyTables"
	}

	Analysis
	{
		title:	qsTr("Log-Linear Regression")
		func:	"RegressionLogLinear"
	}

	Separator {}

	GroupTitle
	{
		title:	qsTr("Bayesian")
		icon:	"analysis-bayesian-crosstabs.svg"
	}

	Analysis
	{
		menu:	qsTr("Binomial Test")
		title:	qsTr("Bayesian Binomial Test")
		func:	"BinomialTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("A/B Test")
		title:	qsTr("Bayesian A/B Test")
		func:	"ABTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("Multinomial Test")
		title:	qsTr("Bayesian Multinomial Test")
		func:	"MultinomialTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("Informed Multinomial Test")
		title:	qsTr("Informed Bayesian Multinomial Test")
		func:	"InformedMultinomialTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("Informed Multi-Binomial Test")
		title:	qsTr("Informed Bayesian Multi-Binomial Test")
		func:	"InformedBinomialTestBayesian"
	}

	Analysis
	{
		menu:	qsTr("Contingency Tables")
		title:	qsTr("Bayesian Contingency Tables")
		func:	"ContingencyTablesBayesian"
	}

	Analysis
	{
		menu:	qsTr("Log-Linear Regression")
		title:	qsTr("Bayesian Log-Linear Regression")
		func:	"RegressionLogLinearBayesian"
	}
}
