Bayesian Contingency Tables
==========================

### Input
---

#### Sampling Model
- Poisson: Assumes independent Poisson-distributed counts for each cell.
- Joint multinomial: Assumes all cell counts come from one multinomial distribution.
- Indep. multinomial, rows fixed: Each row is a separate multinomial distribution; row totals are fixed.
- Indep. multinomial, columns fixed: Each column is a separate multinomial distribution; column totals are fixed.
- Hypergeometric (2x2 only): Assumes fixed row and column totals; suitable for exact tests.

#### Statistics
- Log odds ratio (2x2 only): Displays the odds ratio for 2×2 tables (ad/bc) with a credible interval.
  - Credible interval: Range where the true log odds ratio likely falls, given the data.
- Alt. Hypothesis:
  - Group one &ne; Group two: Two-sided alternative hypothesis that the proportion of group 1 is not equal to the proportion of group 2.
  - Group one &gt; Group two: One-sided alternative hypothesis that the proportion of group 1 is greater than the proportion of group 2.
  - Group one &lt; Group two: One-sided alternative hypothesis that the proportion of group 1 is less than the proportion of group 2.

#### Plots
- Log odds ratio (2x2 only): Displays a graphical summary of the log odds ratio.

#### Prior
- Prior concentration: Controls how strongly the prior favours equal proportions.

#### Cells
- Counts:
  - Observed: Show actual counts from the data.
  - Expected: Show counts expected under the null hypothesis.
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

### References
---
- Gunel, E., & Dickey, J. (1974). Bayes factors for independence in contingency tables. *Biometrika, 61*, 545-557.

### R Packages
---
- BayesFactor
- logspline
- plotrix
- stats
