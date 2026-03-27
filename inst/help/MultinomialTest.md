Multinomial Test
===

The multinomial test allows the user to test whether an observed distribution of cell counts corresponds to an expected distribution.

### Assumptions
- The variable of interest should be categorical.

### Input
---

#### Assignment Box
- Factor: The categorical variable we are interested in.
- Counts (optional): The variable that contains the count data.
- Expected Counts (optional): If the data includes a variable representing expected cell counts, enter it here; its values define the null hypothesis.

#### Alt. Hypothesis
- Equal proportions (multinomial test): Checks if observed counts across categories are uniformly distributed. It compares observed counts to what we'd expect by chance using a chi-squared test. A significant difference suggests the categories aren't equally likely.
- Custom expected proportions (&chi;&sup2; test): Checks if observed counts match a specific expected distribution. By default, it tests for a uniform distribution, but you can set your own expectations. A significant difference suggests the actual distribution doesn't fit the expected one.

#### Additional statistics
- Descriptives: Displays the descriptives of the observed and expected counts as well as the confidence intervals of the observed values.
  - Confidence interval: Coverage of the confidence intervals in percentages. The default value is 95.
- Vovk-Sellke maximum *p*-ratio: An upper bound on how much more likely a p-value is under the alternative hypothesis than under the null.

#### Display
- Counts: Displays the descriptives as counts.
- Proportions: Displays the descriptives as a proportion.

#### Plots
- Descriptives plot: Plots the frequencies and the confidence intervals of the observed counts.
  - Confidence interval: Coverage of the confidence intervals in percentages. The default value is 95.

### Output
---
#### Multinomial Test
- Multinomial Test:
  - Hypothesis: When the $$\chi^2$$ test is selected and there are multiple hypotheses entered these will all be displayed here.
  - $$\chi^2$$: The chi-square goodness-of-fit value.
  - p: the p-value of the multinomial test, or the $$\chi^2$$ goodness-of-fit test.
  - VS-MPR: Vovk-Sellke maximum p-ratio.
- Descriptives:
  - The descriptives table includes the categories of interest, the observed values, the expected values under the specified hypotheses, and confidence intervals based on independent binomial distributions. The descriptives are displayed either in counts or in proportions.

#### Descriptives plot
The descriptive plot displays the frequency of the reported counts and the corresponding confidence intervals for every level of the variable of interest.

### References
---
- Haberman, S. J. (1978). *Analysis of qualitative data: Introductory topics (Vol 1)*. Academic Press.
-  Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*, 404–413. doi: 10.2307/2331986.

### R Packages
---
- ggplot2
- stats
