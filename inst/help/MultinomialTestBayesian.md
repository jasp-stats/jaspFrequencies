Bayesian Multinomial Test
===
  
The Bayesian multinomial test allows the user to test whether an observed distribution of cell counts corresponds to an expected distribution.

### Assumptions
- The variable of interest should be categorical.

### Input
---

#### Assignment Box
- Factor: The categorical variable we are interested in.
- Counts (optional): The variable that contains the count data.
- Expected Counts (optional): If the data includes a variable representing expected cell counts, enter it here; its values define the null hypothesis.

#### Test Values
- Equal proportions: Checks if observed counts across categories are uniformly distributed (the null hypothesis). A significant difference suggests the categories aren't equally likely.
- Custom expected proportions: Checks if observed counts match a specific expected distribution (the null hypothesis). By default, it tests for a uniform distribution, but you can set your own expectations. A significant difference suggests the actual distribution doesn't fit the expected one.

#### Additional statistics
- Descriptives: Displays a table containing the categories of interest, the observed values and the expected values under the specified hypotheses.
  - Credible interval: Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.

#### Display
- Counts: Displays the descriptives as counts.
- Proportions: Displays the descriptives as a proportion.

#### Plots
- Descriptives plot: Displays the frequency of the reported counts and the corresponding credible intervals for every level of the variable of interest.
  - Credible interval: A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.

### Prior
Option to adjust the prior distribution for the vector of cell probabilities.

### Output
---

#### Bayesian Multinomial Test
- Hypothesis: Displays all specified hypotheses.
- Levels: Displays the number of categories of the variable of interest.
- *BF10*, *BF01*, or *Log(BF10)*: Displays the Bayes factor computed with the user-defined prior.

#### Descriptives
The descriptives table includes the categories of interest, the observed values, the expected values under the specified hypotheses. The descriptives are displayed either in counts or in proportions, dependent on what was selected under the option 'Display'. When specified, that table also displays the user-defined credible interval.

#### Descriptives plot
The descriptive plot displays the frequency of the reported counts and the corresponding credible intervals for every level of the variable of interest.

### References
---
- Clopper, C. J. & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. *Biometrika, 26*, 404–413. doi: 10.2307/2331986.
- Good, I. J. (1967). A Bayesian significance test for multinomial distributions. *Journal of the Royal Statistical Society: Series B (Methodological), 29*, 399-418.

### R Packages
---
- ggplot2
- stats
