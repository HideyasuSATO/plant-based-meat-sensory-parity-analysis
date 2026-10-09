# Release validation

Version 1.0.0 was checked on October 9, 2026, using R 4.6.1 on Windows and the package versions in `package_versions.csv`.

The full analysis completed with seed 20260902, 2,000 bootstrap resamples, 500 five-fold cross-fit repetitions, a focal 0.5-point margin, and 100,000 Fisher simulations. The sample and missingness audit used seed 20260903. All four analysis modules completed successfully.

| Check | Result |
| --- | --- |
| Independent aggregate baselines from the original September analysis | 15 of 15 matched at tolerance `1e-10` |
| Numerical fields in proof Tables 1-2 | 158 of 158 matched |
| Numerical and non-inferiority fields in downloaded Supplement Tables S1-S16 | 1,885 of 1,885 matched |
| Parsed definitions of retained statistical and reconstruction functions | 54 of 54 identical to the source definitions |
| Manuscript tables exported | 2 main tables and 16 supplementary tables |
| Figure exports inspected | 2 main figures and 5 supplementary figures; PNG and PDF |
| Quick pipeline and caller RNG restoration | Passed |
| Original script hashes and input snapshot hashes | 5 scripts and 14 inputs matched the recorded manifests |

The focal conclusion checks passed: two nominal Overall Liking findings and zero after 14-category Holm adjustment; nine nominal findings across 70 endpoint tests and four after global Holm adjustment; zero candidate-product Overall Liking findings after within-category Holm adjustment at 0.5 points.

The table comparison checks the generated numerical values against the supplied proof text and downloaded Supplement, allowing separate confidence-limit columns and equivalent number formatting. Typography and explanatory text are not compared. Tables 1-2 retain the proof's row order. Table S16 uses one row per category and margin instead of the Supplement's wide layout.

The release includes the comparison records in `numerical_validation.csv`, `main_table_comparison.csv`, `supplement_table_comparison.csv`, and `function_comparison.csv`. The numerical validator can be rerun using `Rscript tests/validate_results.R outputs` after a full analysis. The proof and Supplement are not distributed with this code repository.

The exact full run preceded a final presentation-only correction to the Table 1 row order. The exporter was rerun successfully and the final tables were checked again. The final runner was also checked in quick mode, including restoration of the caller's random-number state. Statistical calculations were not changed by these final checks.

The validation uses the documented input snapshot. The pipeline has not been validated against changed exports, other datasets, or every R and package version permitted by its minimum version check. The lockfile records the installed environment; downloading and restoring that environment from CRAN was not tested as part of this release.
