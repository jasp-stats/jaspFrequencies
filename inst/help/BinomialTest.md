Binomial Test
====
The binomial test allows the user to test whether a proportion of a dichotomous variable is equal to a test value (=presumed population value). The analysis returns a binomial test for each level of the dependent variable against all other levels, so it will also work for variables with more than two levels.

### Assumptions
- The variable should be a dichotomous scale.
- Observations should be independent.

### Input 
----
- Test value: The proportion of the variable under the null hypothesis - the baseline for comparison.

#### Alt. Hypothesis
- *&ne; Test value*: Two-sided alternative hypothesis that the proportion is not equal to test value.
- *&gt; Test value*: One-sided alternative hypothesis that the proportion is larger than the test value.
- *&lt; Test value*: One-sided alternative hypothesis that the proportion is smaller than the test value.

#### Additional Statistics
- Confidence interval: Coverage of the confidence intervals in percentages. The default value is 95. The interval is the Clopper-Pearson interval from `stats::binom.test`. For two-sided tests, the p-value uses the minimum-likelihood method (summing the probabilities of all outcomes that are no more likely than the observed one), whereas the interval places equal probability in each tail. Because the interval does not invert the same test, the p-value and the interval can occasionally disagree about whether the test value is plausible. This can only happen for two-sided tests with a test value other than 0.5, where the binomial distribution is asymmetric.
- Vovk-Sellke Maximum *p*-Ratio: An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.

#### Plots
- Descriptives plots: Visualises the proportion and confidence interval of the two different values of your dichotomous variable.
  - Confidence interval: Coverage of the confidence intervals in percentages. The default value is 95.

### Output
-----------
#### Binomial Test
- Level: The two options of the dichotomous variable
- Counts: the count of the instances at the certain level of the dichotomous variable. The counts of all possible levels will result in the total.
- Total: the total number of observations.
- Proportion: calculated by counts/total.
- p: p-value.
- VS-MPR: Vovk-Sellke maximum p-ratio.
- CI for Proportion: Clopper-Pearson confidence interval for the proportion (see *Confidence interval* above for how it relates to the p-value).

#### Descriptive Plots
- Displays the proportion of the level of interest (black dot) and the width of the 95% credible interval. The horizontal dotted line represents the when the proportion of both levels would be equal (= 0.5).

### References
---
- Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*(4), 404-413.
- Fay, M. P. (2010). Two-sided exact tests and matching confidence intervals for discrete data. *The R Journal, 2*(1), 53-58.
- Sellke, T., Bayarri, M. J., & Berger, J. O. (2001). Calibration of p values for testing precise null hypotheses. *The American Statistician, 55*(1), 62-71.
- Moore, D. S., McCabe, G. P., & Craig, B. A. (2012). *Introduction to the practice of statistics (7th ed.)*. New York, NY: W. H. Freeman and Company.

### R Packages
---
- ggplot2
- stats
