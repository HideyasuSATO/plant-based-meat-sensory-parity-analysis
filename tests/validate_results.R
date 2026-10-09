args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args)) args[[1L]] else "outputs"
reference_dir <- file.path("reference_results", "numerical_baselines")
manifest <- utils::read.csv(file.path(reference_dir, "manifest.csv"), stringsAsFactors = FALSE)
if (!dir.exists(output_dir)) stop("Output directory not found: ", output_dir)
configuration <- utils::read.csv(file.path(output_dir, "run_configuration.csv"), stringsAsFactors = FALSE)
mode <- configuration$value[configuration$setting == "FQP_RUN_MODE"]
if (!identical(mode, "full")) stop("Numerical baseline validation requires a full run.")
result <- data.frame(file = character(), passed = logical(), detail = character())
for (i in seq_len(nrow(manifest))) {
  baseline <- utils::read.csv(file.path(reference_dir, manifest$baseline[[i]]), stringsAsFactors = FALSE, check.names = FALSE)
  path <- file.path(output_dir, manifest$output[[i]])
  if (!file.exists(path)) stop("Required output not found: ", path)
  observed <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  comparison <- all.equal(baseline, observed, tolerance = 1e-10, check.attributes = TRUE)
  result <- rbind(result, data.frame(file = manifest$output[[i]], passed = isTRUE(comparison), detail = if (isTRUE(comparison)) "Matches original run" else paste(comparison, collapse = "; ")))
}
primary <- utils::read.csv(file.path(output_dir, "model_outputs", "04_primary_overall_liking_delta_0p5.csv"))
all_tests <- utils::read.csv(file.path(output_dir, "model_outputs", "03_source_leader_noninferiority_multiplicity.csv"))
all_tests <- all_tests[all_tests$delta == 0.5, ]
candidates <- utils::read.csv(file.path(output_dir, "model_outputs", "10_candidate_category_selection_summary_delta_0p5.csv"))
stopifnot(nrow(primary) == 14L, sum(primary$noninferior_nominal) == 2L,
  sum(primary$noninferior_holm_primary_14) == 0L, nrow(all_tests) == 70L,
  sum(all_tests$noninferior_nominal) == 9L, sum(all_tests$noninferior_holm_all70) == 4L,
  !any(candidates$any_Holm_NI))
utils::write.csv(result, file.path(output_dir, "numerical_validation.csv"), row.names = FALSE)
print(result[, c("file", "passed")], row.names = FALSE)
if (!all(result$passed)) stop("At least one output differs from the recorded original full run.")
cat("All aggregate baselines and manuscript conclusion checks passed.\n")
