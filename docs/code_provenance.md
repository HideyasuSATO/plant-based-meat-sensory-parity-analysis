# Analysis provenance

The public release is based on the successful full revision run recorded on September 2, 2026, followed by the additional sample and missingness audit on September 3, 2026. The revision run log identifies three executed analysis stages. The run configuration records 2,000 bootstrap resamples, 500 five-fold cross-fit repetitions, seed 20260902, and a focal 0.5-point margin. The additional audit records seed 20260903 and 100,000 Fisher simulations.

| Original file | Public role |
| --- | --- |
| `00_run_fqp_revision_pipeline.R` | Run configuration and entry-point behavior |
| `01_rebuild_fqp_revision_data.R` | `R/01_prepare_data.R` |
| `02_estimate_revised_fqp_models.R` | `R/02_analyze.R` |
| `03_make_fqp_revision_outputs.R` | Plot definitions reused in `R/04_export_results.R`; table presentation rewritten |
| `04_additional_sample_and_missingness_audit.R` | `R/03_sample_audit.R` |

The earlier `01_fqp_parity_full_v7_main_supp_numbered.R`, `02_least_noninferiority_margin_v8_supp_only.R`, and `03_make_manuscript_tables_and_text_v5_tableplan_selectionfix.R` belong to the pre-revision analysis. They are not the basis of the multiplicity- and selection-aware article and are not included.

The statistical calculations, direct rating mappings, source-leader definitions, exact-tie policy, inferred-panel rules, bootstrap draws, cross-fit selection and evaluation, seeds, Fisher simulation counts, and bounded missingness scenarios are retained. Machine-specific paths, automatic dependency installation, recursive output deletion, long explanatory comments, and draft prose generation are removed. The public runner uses an isolated analysis environment and requires an empty output directory.

The original presentation script used intermediate supplementary numbering and included a third main figure. The released exporter follows the final Supplement: the leader-definition plot becomes Figure S2, selection frequencies Figure S3, product-presentation counts Figure S4, and generic purchase linkage Figure S5. It exports Tables 1-2 and S1-S16. Table S6 adds the reported six-test Holm adjustment to the existing missingness diagnostics, and Table S16 assembles the existing margin-specific source-leader and candidate-product adjustments. Plot jitter is given a fixed seed; the underlying data and fitted lines are unchanged.

Original script hashes, calculated from the local source files used to prepare this release, are recorded in `source_scripts.csv`. The old working-directory manifests are not reused because they describe earlier file states.

The independent numerical baselines in `reference_results/numerical_baselines/` are selected aggregate CSV outputs from the original full run. No respondent-level audit tables or intermediate RDS objects are included.
