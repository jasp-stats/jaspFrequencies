Bayesian A/B test
=================

The Bayesian A/B test allows one to monitor the evidence for the hypotheses that an intervention or treatment has either a positive effect, a negative effect or no effect.


### Input
---------

#### Data
- Successes Group 1: Number of successes in group 1 (control condition).
- Sample Size Group 1: Number of trials in group 1 (control condition).
- Successes Group 2: Number of successes in group 2 (experimental condition).
- Sample Size Group 2: Number of trials in group 2 (experimental condition).

1. Each of the above elements needs to be an integer.
2. A cumulative sequence of successes/trials can also be given as input.

#### Normal prior on Log Odds Ratio
- &mu;: Specifies the mean for the normal prior on the test-relevant log odds ratio.
- &sigma;: Specifies the standard deviation for the normal prior on the test-relevant log odds ratio.

#### Descriptives
- Descriptives: Displays the counts and proportion for each group.

#### Plots
- Prior and posterior: Displays the prior and posterior density for the quantity of interest.
- Sequential analysis: Displays the development of posterior probabilities as the data come in. The probability wheels visualize prior and posterior probabilities of the hypotheses.
- Bayes factor robustness check: Displays the prior sensitivity analysis.
- Prior: Plots parameter prior distributions.

#### Order
- Compare to best model.
- Compare to null model.


### Advanced Options
--------------------

#### Prior Model probability
- Log odds ratio = 0: Specifies that the 'success' probability is identical (there is no effect).
- Log odds ratio > 0: Specifies that the 'success' probability in the experimental condition is higher than in the control condition.
- Log odds ratio < 0: Specifies that the 'success' probability in the experimental condition is lower than in the control condition.
- Log odds ratio &ne; 0: Specifies that the 'success' probability differs between the control and experimental condition, but does not specify which one is higher.

#### Sampling
- No. samples: Specifies the number of importance samples for obtaining log marginal likelihood for (H+) and (H-) and the number of posterior samples.

#### Robustness Plot, No. Steps
- &mu;: Specifies in how many discrete steps the &mu; step range is partitioned.
- &sigma;: Specifies in how many discrete steps the &sigma; step range is partitioned.

#### Robustness Plot, Step Range
- Specifies the range of &mu; and &sigma; values to consider.


### Output
----------

#### Model Comparison
  - Models: Hypotheses
  - P(M): Prior model probabilities
  - P(M | data): Posterior probabilities of the models considered.
  - BFM: Posterior model odds.
  - BF10 (or BF01): Bayes factor.

#### Descriptives
  - Groups
  - Counts: "Successes" in each group
  - Total: Sample size of the groups
  - Proportion


### References
--------------
  - Kass R. E. and Vaidyanathan S. K. (1992). *Approximate Bayes Factors and Orthogonal Parameters, with Application to Testing Equality of Two Binomial Proportions*. Journal of the Royal Statistical Society, Series B, 54, 129-144.
  - Gronau Q.F., Raj K.N. A., Wagenmakers E. J. (2019). *Informed Bayesian Inference for the A/B Test*. arXiv preprint arXiv:1905.02068.


### R packages
--------------
  - abtest
