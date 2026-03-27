Bayesian Binomial Test
===
The Bayesian binomial test allows you to test whether a proportion of a dichotomous variable is equal to a test value (presumed population value). The analysis returns a binomial test for each level of the dependent variable against all other levels, so it will also work for variables with more than two levels.

### Assumptions
- The variable should be a dichotomous scale.
- Observations should be independent.

### Input
---------

- Test value: The proportion of the variable under the null hypothesis - the baseline for comparison.

#### Alt. Hypothesis
- *&ne; Test value*: Two-sided alternative hypothesis that the proportion is not equal to test value.
- *&gt; Test value*: One-sided alternative hypothesis that the proportion is larger than the test value.
- *&lt; Test value*: One-sided alternative hypothesis that the proportion is smaller than the test value.

- Beta prior: parameter a: Sets how much prior belief you have in success. When a = b = 1, this corresponds to a uniform prior distribution.
- Beta prior: parameter b: Sets how much prior belief you have in failure. When a = b = 1, this corresponds to a uniform prior distribution.

#### Plots
- Prior and posterior: Displays the prior and posterior density of the population proportion under the alternative hypothesis.
    - Additional info: Shows the Bayes factor using the chosen prior, a probability wheel showing evidence for each hypothesis, and the median with 95% credible interval of the effect size.
- Sequential analysis: Displays the development of the Bayes factor as the data come in using the user-defined prior.
- Descriptives plots: Display descriptives plots.
  - Credible interval: Display central credible intervals. A credible interval shows the probability that the true effect size lies within certain values. The default credible interval is set at 95%.


### Output
---

#### Bayesian Binomial Test
- Level: The two options of the dichotomous variable.
- Counts: the count of the instances at the certain level of the dichotomous variable.
- Total: the total number of observations.
- Proportion: calculated by counts/total.
- BF10 (or BF01): Bayes factor. If a one-sided test is requested:
  - BF+0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis  that the population mean is larger than the test value, relative to the null hypothesis.
  - BF-0: Bayes factor that quantifies evidence for the one-sided alternative hypothesis that the population mean is smaller than the test value, relative to the null hypothesis.
  - BF0+: Bayes factor that quantifies evidence for the null hypothesis, relative to the one-sided alternative hypothesis that the population mean is larger
   than the test value.
  - BF0-: Bayes factor that quantifies evidence for the null hypothesis, relative to the one-sided alternative hypothesis that that the population mean is
  smaller than the test value.

#### Plots
- Prior and posterior: 
  - Displays the prior (dashed line) and posterior (solid line) density of the population proportion under the alternative hypothesis; the gray circles represent the height of the prior and the posterior density at the test value. The horizontal solid line represents the width of the 95% credible interval of the posterior.
  - Additional info: Displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density.
- Sequential analysis: 
  - Displays the development of the Bayes factor as a function of the number of observations (n) using the user-defined prior; displays the Bayes factor computed with the user-defined prior; displays a probability wheel depicting the odds of the data under the null vs. alternative hypothesis; displays the median and 95% credible interval of the posterior density; shows the decisiveness of the evidence in terms of Jeffreys' (1961) evidence categories.


### References
---
- Jeffreys, H. (1961). *Theory of Probability*. Oxford, Oxford University Press.
- O’Hagan, A., & Forster, J. (2004). *Kendall’s advanced theory of statistics vol. 2B: Bayesian inference (2nd ed.)*. London: Arnold.
- Haldane, J. B. S. (1932). A note on inverse probability. *Mathematical Proceedings of the Cambridge Philosophical Society, 28*, 55-61.

### R Packages
---
- ggplot2
- plotrix
- stats
