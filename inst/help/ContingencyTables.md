Contingency Tables
==========================

Contingency tables allow the user to identify how the frequencies of one categorical variable relate to another, helping to determine associations between variables.

### Input
---

#### Statistics
- &chi;&sup2;: Pearson's chi-squared test for independence.
- &chi;&sup2; continuity correction: Applies Yates' correction for continuity (for 2×2 tables).
- McNemar's &chi;&sup2;: Tests equality of two marginal proportions in paired nominal data.
- McNemar's &chi;&sup2; continuity correction: Corrects for error introduced when approximating a discrete distribution with a continuous one in McNemar's test.
- Likelihood ratio: Calculates the likelihood of the data under the alternative hypothesis divided by the likelihood of the data under the null hypothesis.
- Vovk-Sellke maximum *p*-ratio: An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.

#### Odds Ratio (2x2 only)
- Odds ratio: Displays the odds ratio for 2×2 tables (ad/bc).
  - Log Odds Ratio: Shows the log-transformed odds ratio.
  - Confidence interval: Coverage of the confidence intervals in percentages. The default value is 95.
- Alt. Hypothesis (Fisher's exact test): Specify the direction of the alternative hypothesis for Fisher's exact test.
  - Group one &ne; Group two: Two-sided alternative hypothesis that the proportion of group 1 is not equal to the proportion of group 2.
  - Group one &gt; Group two: One-sided alternative hypothesis that the proportion of group 1 is greater than the proportion of group 2.
  - Group one &lt; Group two: One-sided alternative hypothesis that the proportion of group 1 is less than the proportion of group 2.

#### Nominal
- Contingency coefficient: Measure of association for nominal variables (based on &chi;&sup2;).
- Phi and Cramer's V: Effect size for nominal association; Phi (2×2), Cramer's V (larger tables).
- Lambda: Proportion reduction in error measure for nominal data.

#### Ordinal
- Gamma: Measure of ordinal association based on concordant/discordant pairs.
- Kendall's tau-b: Ordinal correlation adjusting for ties.

#### Cells
- Counts:
  - Observed: Show actual counts from the data.
  - Expected: Show counts expected under the null hypothesis.
- Residuals:
  - Unstandardized: Computed by (observed - expected).
  - Pearson: Standardized residuals; computed by (observed - expected) / &radic;(expected).
  - Adjusted Pearson: Accounts for row/column totals; computed by (observed - expected) / &radic;(expected × (1 - row marginal proportion) × (1 - column marginal proportion)).
- Percentages:
  - Row: Shows row-wise percentages.
  - Column: Shows column-wise percentages.
  - Total: Shows percentages of total.
- Show totals: Shows row and column totals.

#### Options
- Row Order:
  - Ascending: Rows sorted in numerical order.
  - Descending: Rows sorted in reverse numerical order.
- Column Order:
  - Ascending: Columns sorted in numerical order.
  - Descending: Columns sorted in reverse numerical order.

### R Packages
---
- ggplot2
- stats
- vcd
- vcdExtra
