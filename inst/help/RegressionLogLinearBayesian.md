Bayesian Log-linear Regression
==========================

### Input
---

#### Prior
- Shape: Specifies the shape of the prior distribution on the effect size.
- Scale: Controls how spread out the prior is; larger = more uncertainty.

#### Model Display
- Display best: Shows the top N models ranked by posterior probability.
- Posterior prob.: Only models with posterior probability above this threshold are shown.

#### Statistics
- Estimates: Displays estimated regression coefficients for included terms.
  - Credible intervals: Adds a credible interval for each coefficient estimate.
- Submodel: Displays statistics for a specific model index.
  - Credible intervals: Shows credible intervals for the selected submodel's coefficients.

#### Sampling
- Auto: Automatically chooses the number of samples for estimation.
- Manual: Allows you to set the number of posterior samples manually.

### References
---
- Overstall, A., & King, R. (2014). conting: an R package for Bayesian analysis of complete and incomplete contingency tables. *Journal of Statistical Software, 58*(7), 1-27.

### R Packages
---
- conting
- plyr
- stats
