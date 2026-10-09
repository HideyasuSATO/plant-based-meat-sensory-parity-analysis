# Plant-based meat sensory parity reanalysis

This repository contains the R code and aggregate numerical results for:

**When is plant-based meat close enough? A multiplicity- and selection-aware non-inferiority reanalysis of sensory-parity claims**

Hideyasu Sato. *Food Quality and Preference*. DOI: [10.1016/j.foodqual.2026.106145](https://doi.org/10.1016/j.foodqual.2026.106145).

The analysis reconstructs the public NECTAR respondent-level exports, compares source-designated plant-based leaders with paired animal benchmarks, reports effect sizes and non-inferiority tests, applies Holm adjustments, and evaluates product selection and missingness. It also describes sample composition and the limited generic purchase-likelihood fields.

## Repository contents

```text
plant_based_meat_public_analysis.R   Entry point
R/01_prepare_data.R                 Export reconstruction and data audit
R/02_analyze.R                      Statistical analyses and cross-fitting
R/03_sample_audit.R                 Demographics and missingness sensitivity
R/04_export_results.R               Final manuscript tables and figures
data/raw/README.md                  Required input filenames
reference_results/                  Aggregate tables and numerical baselines
tests/validate_results.R            Comparison with the recorded original run
docs/source_scripts.csv             Original script names and SHA-256 hashes
docs/input_manifest.csv             Input snapshot filenames and SHA-256 hashes
docs/code_provenance.md             Source identification and release changes
docs/package_versions.csv           Direct package versions in the original run
docs/validation.md                  Release validation record
renv.lock                           R and package version snapshot
CITATION.cff                        Citation metadata
LICENSE                             MIT license
.gitignore                          Local data and intermediate-file exclusions
SHA256SUMS.csv                      Release file checksums
```

Raw exports, respondent-level derived data, reviewer correspondence, and manuscript files are not redistributed.

## Data source

Obtain the category-level respondent exports from the [NECTAR Taste of the Industry 2025 / Palate Insights dashboard](https://www.nectar.org/sensory-research/2025-taste-of-the-industry). Access and reuse are subject to the source's terms. Save the 14 CSV files listed in [data/raw/README.md](data/raw/README.md) in `data/raw/`.

The source study is:

van den Bedem, S. D., Kuhl, E., and Cotto, C. (2026). Open-source benchmarking of plant-based and animal meats. *Foods*, 15(12), 2112.

The released snapshot contains 2,682 category-specific respondent records. The source article reports 2,684 participants. The exports do not support cross-category linkage or an explanation of this difference. The script validates the specific export structure used for the article; it stops when locked counts, leader codes, mappings, or missingness patterns differ. See [docs/input_manifest.csv](docs/input_manifest.csv) for byte-level snapshot hashes. A changed download may require checking its structure against the published snapshot before using this pipeline.

## Software

The original analysis used R 4.6.1 and the package versions recorded in [docs/package_versions.csv](docs/package_versions.csv). The script uses the native pipe and requires R 4.1 or later. Use the recorded R and package versions when reproducing the numerical baseline.

Required packages:

```r
install.packages(c(
  "readr", "dplyr", "tidyr", "stringr", "purrr", "tibble",
  "ggplot2", "forcats", "scales"
))
```

The analysis does not install or update packages automatically. To restore the recorded package environment, install `renv` separately and run `renv::restore()` from the repository root. `renv.lock` records the original installed versions and their dependencies; availability of those versions depends on the configured package repository.

## Run the analysis

Start R in the repository root, then run:

```r
source("plant_based_meat_public_analysis.R")
```

The default full analysis uses 2,000 bootstrap resamples per paired source/sample-best group, 500 repetitions of five-fold cross-fitting, and 100,000 simulations for applicable Fisher-Freeman-Halton tests. The main analysis seed is `20260902`; the demographic audit seed is `20260903`.

A shorter pipeline check is available:

```r
options(sensory.analysis.mode = "quick")
options(sensory.analysis.output_dir = "outputs_quick")
source("plant_based_meat_public_analysis.R")
```

Quick mode uses 250 bootstrap resamples and 25 cross-fit repetitions. Its stochastic confidence intervals and cross-fit summaries are not the full-run manuscript results. Fisher simulations remain at 100,000.

For explicit paths or repeated runs:

```r
options(sensory.analysis.load_only = TRUE)
source("plant_based_meat_public_analysis.R")
run_analysis(
  project_dir = getwd(),
  data_dir = file.path(getwd(), "data", "raw"),
  output_dir = file.path(getwd(), "outputs_full"),
  mode = "full"
)
```

Use an empty output directory. The script does not delete a previous run. `verbose = FALSE` suppresses progress messages in the explicit function call; warnings and errors remain visible.

## Outputs

```text
outputs/audit/                       Reconstruction and data-structure checks
outputs/model_outputs/               Full-precision statistical results
outputs/additional_sample_audit/     Sample composition and missingness analyses
outputs/rds/                         Local intermediate objects
outputs/manuscript/tables/main/      Tables 1 and 2
outputs/manuscript/tables/supplement/ Tables S1-S16
outputs/manuscript/figures/main/      Figures 1 and 2, PNG and PDF
outputs/manuscript/figures/supplement/Figures S1-S5, PNG and PDF
outputs/run_configuration.csv
outputs/run_log.csv
outputs/package_versions.csv
outputs/sessionInfo.txt
```

The final table and figure identifiers follow the published manuscript and Supplement. Output CSVs use separate numerical columns for confidence bounds and correlation p-values; layout and typography differ from the Word tables. Full-precision values remain in the analysis output. The aggregate reference tables are in `reference_results/manuscript_tables/`.

Some local audit and model outputs contain respondent-level records. They are created for reproducibility and excluded from version control with the entire output directory. The distributed reference results contain aggregate records only.

## Numerical validation

After a full run:

```sh
Rscript tests/validate_results.R outputs
```

The validator compares selected aggregate numerical outputs with the recorded September 2026 analysis at a numerical tolerance of `1e-10`, preserving row order, column names, and nonnumeric values. It also checks the manuscript's nominal and adjusted discovery counts. Baselines are independently retained outputs from the original analysis, rather than values generated by the public code for its own test. Full-run numerical validation is required for the cross-fit and bootstrap comparisons.

The release passed a full rerun, comparison with 15 original aggregate outputs, and numerical checks against Tables 1-2 and S1-S16. See [docs/validation.md](docs/validation.md) for the recorded checks. The raw inputs must match the documented snapshot for those comparisons to be applicable.

## Interpretation

- Overall Liking is the focal endpoint. Flavor, Texture, Appearance, and Similarity are secondary endpoints.
- Similarity is rated relative to the respondent's usual product. Its plant-animal contrast is not a direct similarity judgment between the two served products.
- Source-designated leaders are held fixed for this reanalysis. Their designation may reflect outcome-driven selection in the source study.
- At the focal 0.5-point margin, two Overall Liking contrasts meet the nominal criterion; neither survives Holm adjustment across 14 categories. No candidate-product Overall Liking conclusion survives within-category Holm adjustment at that margin.
- Four secondary endpoint cases survive the 70-test Holm audit: Texture, Appearance, and Similarity for Chicken Nuggets, and Appearance for Meatballs.
- The minimum data-supported margin is derived from an unadjusted one-sided confidence bound. Decisions use a strict inequality. The value is not a behaviorally calibrated consumer-acceptance or market-substitution threshold.
- Cross-fit decision frequencies describe repeated partitions of the observed data. Held-out evaluation is conditional on observing the selected product; coverage is reported. These frequencies are not independent confirmation probabilities.
- The missingness sensitivity uses deterministic completions. The scale endpoints bound the completed mean difference, rather than the test statistic over every possible completion.
- The original pipeline also calculated 1.0-point and false-discovery-rate diagnostics. These are retained in the detailed outputs; the manuscript's focal inference uses Holm adjustment at 0.5 points, with 0.3 and 0.7 as margin sensitivity analyses.

## License and citation

The analysis code and its documentation are released under the [MIT License](LICENSE). The source data are not included and remain subject to the terms of their creators and distributor. The code license does not grant rights to the source data or publisher-formatted article.

Please cite the article above and the source study when reusing the analysis. Citation metadata are provided in [CITATION.cff](CITATION.cff). For reproducibility questions, use the repository's GitHub Issues page and identify the R version, package versions, input snapshot, and failing validation check. Do not attach respondent-level data.
