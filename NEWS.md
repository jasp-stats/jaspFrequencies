# jaspFrequencies Changelog

> **HOW TO READ AND UPDATE THIS CHANGELOG:**
> 
> This document follows a modified [Keep a Changelog](https://keepachangelog.com/) format adapted for the R/JASP ecosystem. Releases are listed in reverse chronological order (newest first).
> As an example see [jaspModuleTemplate](https://github.com/jasp-stats/jaspModuleTemplate/blob/master/NEWS.md)
> * **Adding New Changes (For Contributors):** All new commits should be logged at the very top of the file under the `# jaspFrequencies (development version)` header. Place your bullet point under the appropriate category (`## Added`, `## Fixed`, etc.). 
> * **Issue References:** Please reference the relevant GitHub Issue or Pull Request at the end of your line (e.g., `([Issue #19](https://github.com/jasp-stats/jaspFrequencies/issues/19))`). 
> * **Format Categories:**
>   * **Added:** New analyses, options, or QML controls.
>   * **Changed:** Updates to defaults, behaviour, or dependencies. 
>   * **Fixed:** Bug fixes in the R backend, QML layouts, or build pipeline.
>   * **Deprecated / Removed:** Outdated components or legacy code.


---

# jaspFrequencies (development version)

## Added
* Added a `Counts` variable box to the Binomial Test and the Bayesian Binomial Test, so binomial data can be entered as aggregated counts instead of raw rows ([PR #315](https://github.com/jasp-stats/jaspFrequencies/pull/315)).
* Added McNemar's test to Contingency Tables, for paired nominal data in a 2x2 table ([PR #317](https://github.com/jasp-stats/jaspFrequencies/pull/317)).
* Added info fields to all analyses, documenting every option in the interface and in the help files ([PR #268](https://github.com/jasp-stats/jaspFrequencies/pull/268)).
* Added this `NEWS.md`, a workflow reminding contributors to update it, and a workflow that auto-bumps the version.

## Changed
* Enabled `preloadData` for the module, so analyses reuse the data already loaded by JASP instead of re-reading it ([PR #315](https://github.com/jasp-stats/jaspFrequencies/pull/315)).
* Goodman and Kruskal's gamma is now computed in-module instead of via `vcdExtra`, dropping that dependency ([PR #305](https://github.com/jasp-stats/jaspFrequencies/pull/305)).
* The A/B test prior/posterior probability ("pizza") plots are now stacked vertically ([PR #323](https://github.com/jasp-stats/jaspFrequencies/pull/323)).
* The Binomial Test and Bayesian Binomial Test now accept at most 50 levels per variable ([PR #304](https://github.com/jasp-stats/jaspFrequencies/pull/304)).

## Fixed
* Fixed the Bayesian A/B test, which errored after the switch to preloaded data because the counts were passed to `abtest::ab_test()` in the wrong format; the robustness and sequential analysis plots now render again ([PR #323](https://github.com/jasp-stats/jaspFrequencies/pull/323)).
* Fixed the error checks in (Bayesian) Log-Linear Regression: infinite, missing, and negative counts, and factors with fewer than two levels, are now caught before the model is fitted ([PR #304](https://github.com/jasp-stats/jaspFrequencies/pull/304)).
* Fixed the Multinomial Test using the wrong counts column when rows were dropped for missing factor values, and added an upper bound on the counts ([PR #304](https://github.com/jasp-stats/jaspFrequencies/pull/304)).
