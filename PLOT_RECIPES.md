# Plot recipe migration

Straightforward plot boundaries migrated:

- Binomial descriptives: precomputed proportions and confidence limits.
- Bayesian binomial prior/posterior: precomputed curves, points and interval summaries; the mathematical theta label is constructed during drawing. Sequential Bayes factors remain computed during analysis.
- Classical and Bayesian multinomial descriptives: count/proportion tables and confidence limits.
- Informed binomial and multinomial descriptives and unrestricted posterior summaries: plain summary data and only the plotting settings consumed by the helper.
- Informed sequential posterior probabilities and Bayes factors: precomputed tables/curves. Both analyses share `.createInformedMultPlotSequentialProb` and `jaspGraphs::PlotRobustnessSequential`.
- Bayesian contingency-table log odds: sampling, posterior summaries, Bayes-factor validation and curve calculation stay in the analysis; drawing and legend adjustment use a recipe.

Recipes contain named function references and plain data/settings, without ggplot objects, closures or result containers. Existing expensive computation, progress reporting and option dependencies remain in the analysis.

## Postponed plots

The Bayesian A/B test has four plot families requiring further separation:

- Prior and posterior return drawing closures; the posterior uses the abtest object and package-specific density functions, and some priors require simulation. Prepare a plain grid/density or contour table once, then draw it with a namespace helper. Preserve simulation draws and base-graphics appearance without retaining a closure or posterior model.
- Robustness computes a grid of A/B tests inside `plot_robustness_ggplot2`. Separate its grid evaluation/progress into analysis preparation, store the Bayes-factor surface plus axis settings, and build the contour layers during drawing.
- Sequential analysis calls `abtest:::compute_ab_seq` before constructing a ggplot. Split after the posterior-probability curves and model colors are prepared; retain numerical work and its errors in the analysis.

The unused `.createInformedMultBayesPosteriorPlots` still calls `.informedPlot`, which is absent from this module. Resolve the intended restricted-model plot producer before enabling/migrating that dormant code. Its active unrestricted counterpart is migrated.

## Validation

Use R-4.5.2 and the exact jaspBase/jaspGraphs/jaspTools revisions in `renv.lock`. Recipe tests verify roundtrip serialization, stable redraw/RNG, confidence limits, shared informed drawing helpers and the mathematical binomial label. Install the modified module before testing so package-qualified references resolve to its new namespace. Run existing analysis/plot tests without accepting or deleting reference snapshots.

Local validation used jaspBase `b187388a5e412c0d683c78ffd657908473859c2d`, jaspGraphs `288d2751131ee6ac17362bd89d2ed7d9c0c67c7c`, and jaspTools `1a109435bfdbedbae1e5fda5ceb39d227ca8e842`, matching the lockfile. The test runner used the newly locked testthat 3.3.2 rather than the older package physically present in the restored cache.

- Added recipe regression tests: 15 assertions passed without warnings.
- Existing full suite: 111 assertions passed, 3 tests skipped, 4 warnings, and one A/B robustness visual snapshot failure. The A/B analysis is unchanged in this migration; baseline verification is recorded below.
- Fourteen representative SVG comparisons were identical to the original builders: binomial summaries; informed binomial/multinomial counts, proportions and posterior summaries; shared sequential probabilities; Bayesian multinomial summaries; Bayesian binomial prior/posterior with and without additional information.
- Existing tests ran in an isolated source copy because jaspTools removes unused plot-object snapshots. The module's reference snapshots were preserved.

Baseline full-suite verification with the same interpreter and dependencies reproduced the identical A/B robustness failure (111 passes, 3 skips, 1 warning). Three additional warnings in the migrated suite arise when the unchanged multinomial drawing APIs are invoked during lazy materialization: ggplot's obsolete `size` parameter for bars (twice), and Bayesian multinomial `themeJasp` axis arguments. The corresponding SVG comparisons are exact matches; no reference snapshots or unrelated styling APIs were changed to silence these warnings.
