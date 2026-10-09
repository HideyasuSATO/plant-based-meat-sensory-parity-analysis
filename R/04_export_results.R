FQP_MANUSCRIPT_DIR <- file.path(FQP_OUTPUT_DIR, "manuscript")
FQP_TABLE_MAIN_DIR <- file.path(FQP_MANUSCRIPT_DIR, "tables", "main")
FQP_TABLE_SUPP_DIR <- file.path(FQP_MANUSCRIPT_DIR, "tables", "supplement")
FQP_FIG_MAIN_DIR <- file.path(FQP_MANUSCRIPT_DIR, "figures", "main")
FQP_FIG_SUPP_DIR <- file.path(FQP_MANUSCRIPT_DIR, "figures", "supplement")
for (directory in c(FQP_TABLE_MAIN_DIR, FQP_TABLE_SUPP_DIR, FQP_FIG_MAIN_DIR, FQP_FIG_SUPP_DIR)) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
}

read_result <- function(directory, filename) {
  readr::read_csv(file.path(directory, filename), show_col_types = FALSE)
}
write_table <- function(x, directory, filename) {
  readr::write_csv(x, file.path(directory, filename), na = "")
}
round_numeric <- function(x, digits = 3L) {
  p_columns <- names(x)[grepl("_p($|_)", names(x))]
  numeric_columns <- setdiff(names(x)[vapply(x, is.double, logical(1))], p_columns)
  x <- dplyr::mutate(x, dplyr::across(dplyr::all_of(numeric_columns), function(z) round(z, digits)))
  dplyr::mutate(x, dplyr::across(dplyr::all_of(p_columns), format_public_p))
}
format_public_p <- function(x) {
  ifelse(is.na(x), NA_character_, ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)))
}
source_summary <- readRDS(file.path(FQP_RDS_DIR, "source_summary_revision.rds"))
source_ni <- readRDS(file.path(FQP_RDS_DIR, "source_ni_revision.rds"))
all_product_ni <- readRDS(file.path(FQP_RDS_DIR, "all_product_ni_revision.rds"))
integrated_overall <- readRDS(file.path(FQP_RDS_DIR, "integrated_overall_revision.rds"))
leader_audit <- readRDS(file.path(FQP_RDS_DIR, "leader_audit.rds"))
behavioral_link_data <- readRDS(file.path(FQP_RDS_DIR, "behavioral_link_data_revision.rds"))
presentation_by_category <- read_result(FQP_AUDIT_DIR, "17_product_presentation_by_category.csv")
selection_frequency <- read_result(FQP_MODEL_DIR, "12_crossfit_selection_frequency.csv")
coverage <- read_result(FQP_MODEL_DIR, "12b_crossfit_evaluation_coverage.csv")

primary <- source_ni |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking", .data$delta == FQP_PRIMARY_MARGIN) |>
  dplyr::arrange(dplyr::desc(.data$mean_diff), .data$category)
main_1 <- primary |>
  dplyr::transmute(
    Category = .data$category_label, Code = .data$plant_code, n = .data$n,
    Mean_difference = round(.data$mean_diff, 3), CI90_lower = round(.data$ci90_low, 3),
    CI90_upper = round(.data$ci90_high, 3), Cohen_dz = round(.data$cohen_dz, 3),
    Same_or_better_percent = round(100 * .data$same_or_better, 1),
    Minimum_data_supported_margin = round(.data$minimum_data_supported_margin, 3),
    NI_p = format_public_p(.data$p_ni), Holm_p_14_categories = format_public_p(.data$p_holm_primary_14)
  )
write_table(main_1, FQP_TABLE_MAIN_DIR, "Table_1_overall_liking.csv")

focal <- source_ni |>
  dplyr::filter(.data$delta == FQP_PRIMARY_MARGIN)
main_2 <- focal |>
  dplyr::group_by(.data$modality) |>
  dplyr::summarise(
    Nominal_NI = sum(.data$noninferior_nominal),
    Holm_NI_within_endpoint = sum(.data$noninferior_holm_endpoint_margin),
    Holm_NI_across_all_70 = sum(.data$noninferior_holm_all70),
    Cases_surviving_70_test_adjustment = paste(.data$category_label[.data$noninferior_holm_all70], collapse = "; "),
    .groups = "drop"
  ) |>
  dplyr::rename(Endpoint = modality) |>
  dplyr::mutate(Endpoint = as.character(.data$Endpoint))
main_2 <- dplyr::bind_rows(main_2, tibble::tibble(
  Endpoint = "All endpoints", Nominal_NI = sum(main_2$Nominal_NI),
  Holm_NI_within_endpoint = sum(main_2$Holm_NI_within_endpoint),
  Holm_NI_across_all_70 = sum(main_2$Holm_NI_across_all_70),
  Cases_surviving_70_test_adjustment = paste(paste(focal$category_label[focal$noninferior_holm_all70], focal$modality[focal$noninferior_holm_all70], sep = ": "), collapse = "; ")
))
write_table(main_2, FQP_TABLE_MAIN_DIR, "Table_2_multiplicity_audit.csv")

supplement <- list()
supplement[["Table_S1_sample_characteristics.csv"]] <- read_result(FQP_ADDITIONAL_AUDIT_DIR, "06_sample_characteristics_overall_long.csv") |>
  dplyr::transmute(
    Characteristic_group = .data$variable_label, Level = .data$characteristic_level,
    n = .data$n, Total_records = .data$n_total, Nonmissing_records = .data$n_nonmissing,
    Percent_all_records = round(.data$percent_total, 1), Percent_nonmissing = round(.data$percent_nonmissing, 1)
  )
supplement[["Table_S2_sample_by_category.csv"]] <- read_result(FQP_ADDITIONAL_AUDIT_DIR, "09_sample_characteristics_by_category_compact_numeric.csv") |>
  dplyr::transmute(
    Category = .data$category_label, N = .data$N,
    Age_18_35_n = .data$age_18_to_35_n,
    Age_18_35_percent = round(.data$age_18_to_35_pct, 1),
    Female_n = .data$female_n, Female_percent = round(.data$female_pct, 1),
    Male_n = .data$male_n, Male_percent = round(.data$male_pct, 1),
    Flexitarian_n = .data$flexitarian_n,
    Flexitarian_percent = round(.data$flexitarian_pct, 1),
    Bachelor_or_higher_n = .data$bachelor_or_higher_n,
    Bachelor_or_higher_percent = round(.data$bachelor_or_higher_pct, 1),
    Weekly_or_more_use_n = .data$weekly_or_more_consumption_n,
    Weekly_or_more_use_percent = round(.data$weekly_or_more_consumption_pct, 1),
    Any_core_characteristic_missing_n = .data$any_core_characteristic_missing_n,
    Any_core_characteristic_missing_percent = round(.data$any_core_characteristic_missing_pct, 1)
  )
supplement[["Table_S3_leader_definition.csv"]] <- leader_audit |>
  dplyr::transmute(
    Category = .data$category_label, Source_code = .data$source_leader_code,
    Source_mean = round(.data$source_mean, 3), Sample_best_set = .data$sample_best_set,
    Best_mean = round(.data$sample_best_mean, 3), Number_tied = .data$n_sample_best_tied,
    Source_minus_best = round(.data$source_minus_sample_best, 3), Relationship = .data$leader_status
  )
supplement[["Table_S4_product_presentation.csv"]] <- presentation_by_category |>
  dplyr::transmute(
    Category = .data$category_label, Records = .data$n_respondents,
    Plant_count_minimum = .data$min_plant_overall, Plant_count_maximum = .data$max_plant_overall,
    Plant_count_distribution = .data$plant_overall_distribution,
    Expected_blocks = .data$expected_plant_blocks_in_canonical_panels,
    Missing_blocks = .data$whole_product_missing_blocks,
    Missing_percent = round(100 * .data$whole_product_missing_rate, 2),
    Randomizer_missing_n = .data$randomizer_missing_n
  )
supplement[["Table_S5_source_leader_availability.csv"]] <- read_result(FQP_AUDIT_DIR, "24_source_leader_missingness_summary.csv") |>
  dplyr::transmute(
    Category = .data$category_label, Code = .data$source_leader_code,
    Panel_products = .data$source_panel_size, Panel_N = .data$source_canonical_panel_n,
    Observed = .data$source_observed_in_canonical_panel_n, Missing = .data$source_block_missing_n,
    Missing_percent = round(100 * .data$source_block_missing_rate, 1),
    Animal_mean_observed = round(.data$animal_mean_if_source_observed, 3),
    Animal_mean_missing = round(.data$animal_mean_if_source_missing, 3),
    Animal_balance_p = format_public_p(.data$animal_balance_p_value)
  )
supplement[["Table_S6_missingness_categorical.csv"]] <- read_result(FQP_ADDITIONAL_AUDIT_DIR, "14_bcf_missingness_global_tests.csv") |>
  dplyr::mutate(Holm_p = stats::p.adjust(.data$exact_or_monte_carlo_p_value, method = "holm")) |>
  dplyr::transmute(
    Variable = .data$variable_label, N = .data$n_analyzed, Levels = .data$number_of_levels,
    Exact_or_MC_p = .data$exact_or_monte_carlo_p_value, Holm_p = .data$Holm_p,
    Cramers_V = .data$cramer_v, Maximum_absolute_SMD = .data$maximum_absolute_level_standardized_difference
  ) |>
  round_numeric()
supplement[["Table_S7_missingness_numeric.csv"]] <- read_result(FQP_ADDITIONAL_AUDIT_DIR, "16_bcf_numeric_comparisons.csv") |>
  dplyr::transmute(
    Variable = .data$variable_label, n_observed = .data$n_observed, Mean_observed = .data$mean_observed,
    n_missing = .data$n_missing, Mean_missing = .data$mean_missing,
    Missing_minus_observed = .data$mean_difference_missing_minus_observed,
    SMD = .data$standardized_mean_difference_missing_minus_observed,
    Welch_p = .data$welch_t_p_value, Wilcoxon_p = .data$wilcoxon_rank_sum_p_value
  ) |>
  round_numeric()
supplement[["Table_S8_bounded_sensitivity.csv"]] <- read_result(FQP_ADDITIONAL_AUDIT_DIR, "17a_bcf_overall_liking_bounded_sensitivity.csv") |>
  dplyr::transmute(
    Scenario = .data$scenario, n = .data$n, Mean_difference = .data$mean_diff,
    One_sided_95_lower = .data$lower_one_sided_95,
    Minimum_data_supported_margin = .data$minimum_data_supported_margin,
    NI_p = .data$p_ni, NI_at_0p5 = .data$noninferior_nominal
  ) |>
  round_numeric()
supplement[["Table_S9_all_endpoints.csv"]] <- focal |>
  dplyr::arrange(.data$category, .data$modality) |>
  dplyr::transmute(
    Category = .data$category_label, Endpoint = as.character(.data$modality), n = .data$n,
    Difference = .data$mean_diff, CI90_lower = .data$ci90_low, CI90_upper = .data$ci90_high,
    Cohen_dz = .data$cohen_dz, Minimum_data_supported_margin = .data$minimum_data_supported_margin,
    NI_p = .data$p_ni, Endpoint_Holm_p = .data$p_holm_endpoint_margin, All_70_Holm_p = .data$p_holm_all70
  ) |>
  round_numeric()
supplement[["Table_S10_candidate_selection.csv"]] <- read_result(FQP_MODEL_DIR, "10_candidate_category_selection_summary_delta_0p5.csv") |>
  dplyr::transmute(
    Category = .data$category_label, Candidates = .data$n_candidate_products,
    Any_nominal_NI = .data$any_nominal_NI, Any_Holm_NI = .data$any_Holm_NI,
    Displayed_code = .data$best_adjusted_product_code, Difference = .data$best_adjusted_mean_diff,
    Raw_p = .data$best_raw_p_ni, Within_category_Holm_p = .data$best_Holm_p_ni
  ) |>
  round_numeric()
supplement[["Table_S11_selection_sensitivity.csv"]] <- integrated_overall |>
  dplyr::left_join(coverage |> dplyr::select(category, overall_coverage_rate), by = "category") |>
  dplyr::transmute(
    Category = .data$category_label, Relationship = .data$leader_status,
    Source_code = .data$source_code, Best_set = .data$sample_best_set,
    Source_minimum_margin = .data$source_minimum_margin, Best_minimum_margin = .data$sample_best_minimum_margin,
    Crossfit_median_minimum_margin = .data$crossfit_minimum_margin_median,
    Partitions_meeting_nominal_NI_percent = round(100 * .data$crossfit_probability_nominal_NI, 1),
    Evaluation_coverage_percent = round(100 * .data$overall_coverage_rate, 1)
  ) |>
  round_numeric()
supplement[["Table_S12_selection_frequency.csv"]] <- selection_frequency |>
  dplyr::transmute(
    Category = .data$category_label, Product_code = .data$selected_code,
    Source_designated_leader = .data$selected_is_source,
    Selected_folds = .data$selected_folds, Successful_folds = .data$total_successful_folds,
    Share_percent = round(100 * .data$selected_share, 2)
  )
supplement[["Table_S13_generic_purchase_linkage.csv"]] <- read_result(FQP_MODEL_DIR, "19_generic_purchase_linkage_by_category.csv") |>
  dplyr::transmute(
    Category = .data$category_label, n = .data$n, OL_difference = .data$source_overall_diff_mean,
    Plant_purchase = .data$plant_purchase_mean, Conventional_purchase = .data$conventional_purchase_mean,
    Purchase_difference = .data$purchase_diff_mean,
    Plant_at_least_conventional_percent = round(100 * .data$plant_purchase_at_least_conventional_share, 1),
    Pearson_r = .data$pearson_correlation, Pearson_p = .data$pearson_p_value,
    Spearman_rho = .data$spearman_correlation, Spearman_p = .data$spearman_p_value
  ) |>
  round_numeric()
supplement[["Table_S14_same_or_better.csv"]] <- read_result(FQP_AUDIT_DIR, "21_source_leader_same_or_better.csv") |>
  dplyr::transmute(
    Category = .data$category_label, n = .data$n,
    Plant_higher_percent = round(100 * .data$raw_plant_higher, 1),
    Equal_percent = round(100 * .data$raw_equal, 1), Animal_higher_percent = round(100 * .data$raw_animal_higher, 1),
    Raw_same_or_better_percent = round(100 * .data$raw_same_or_better, 1)
  )
supplement[["Table_S15_estimation_and_effect_sizes.csv"]] <- source_summary |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking") |>
  dplyr::transmute(
    Category = .data$category_label, n = .data$n, Mean_difference = .data$mean_diff,
    CI95_lower = .data$ci95_low, CI95_upper = .data$ci95_high, Cohen_dz = .data$cohen_dz,
    Bootstrap_dz_95_lower = .data$cohen_dz_boot_low, Bootstrap_dz_95_upper = .data$cohen_dz_boot_high,
    Hedges_gz = .data$hedges_gz, Matched_rank_biserial = .data$matched_rank_biserial
  ) |>
  round_numeric()
source_margin <- source_ni |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking", .data$delta %in% c(0.3, 0.5, 0.7)) |>
  dplyr::select(category, category_label, delta, Source_leader_Holm_p = p_holm_endpoint_margin)
candidate_margin <- all_product_ni |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking", .data$delta %in% c(0.3, 0.5, 0.7)) |>
  dplyr::group_by(.data$category, .data$delta) |>
  dplyr::summarise(Minimum_candidate_Holm_p = min(.data$p_holm_within_category), .groups = "drop")
supplement[["Table_S16_margin_sensitivity.csv"]] <- source_margin |>
  dplyr::left_join(candidate_margin, by = c("category", "delta")) |>
  dplyr::arrange(.data$category, .data$delta) |>
  dplyr::transmute(Category = .data$category_label, Margin = .data$delta,
    Source_leader_Holm_p = .data$Source_leader_Holm_p, Minimum_candidate_Holm_p = .data$Minimum_candidate_Holm_p) |>
  round_numeric()
for (filename in names(supplement)) write_table(supplement[[filename]], FQP_TABLE_SUPP_DIR, filename)

stopifnot(nrow(main_1) == 14L, nrow(main_2) == 6L, length(supplement) == 16L)
stopifnot(sum(primary$noninferior_nominal) == 2L, sum(primary$noninferior_holm_primary_14) == 0L)
stopifnot(sum(focal$noninferior_nominal) == 9L, sum(focal$noninferior_holm_all70) == 4L)
stopifnot(!any(supplement[["Table_S10_candidate_selection.csv"]]$Any_Holm_NI))

save_plot <- function(plot, directory, filename, width, height, dpi = 320) {
  ggplot2::ggsave(file.path(directory, paste0(filename, ".png")), plot = plot, width = width, height = height, dpi = dpi, bg = "white")
  device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  ggplot2::ggsave(file.path(directory, paste0(filename, ".pdf")), plot = plot, width = width, height = height, device = device, bg = "white")
}
overall_source_plot_data <- source_summary |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking") |>
  dplyr::mutate(
    category_ordered = forcats::fct_reorder(.data$category_label, .data$mean_diff)
  )

figure_1 <- ggplot2::ggplot(
  overall_source_plot_data,
  ggplot2::aes(y = .data$category_ordered, x = .data$mean_diff)
) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.45, linetype = "solid") +
  ggplot2::geom_vline(xintercept = -FQP_PRIMARY_MARGIN, linewidth = 0.45, linetype = "dashed") +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = .data$ci90_low, xmax = .data$ci90_high),
    orientation = "y",
    width = 0.18,
    linewidth = 0.55
  ) +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::labs(
    x = "Plant-based minus animal benchmark (Overall Liking)",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank()
  )
save_plot(
  figure_1, FQP_FIG_MAIN_DIR,
  "Figure_1_source_leader_overall_liking_forest",
  width = 7.4, height = 6.2
)

heatmap_data <- source_ni |>
  dplyr::filter(.data$delta == FQP_PRIMARY_MARGIN) |>
  dplyr::mutate(
    decision = dplyr::case_when(
      .data$noninferior_holm_endpoint_margin ~ "Holm-adjusted NI",
      .data$noninferior_nominal ~ "Nominal NI only",
      TRUE ~ "No NI"
    ),
    decision = factor(
      .data$decision,
      levels = c("Holm-adjusted NI", "Nominal NI only", "No NI")
    ),
    category_ordered = factor(
      .data$category_label,
      levels = rev(overall_source_plot_data$category_label[
        order(overall_source_plot_data$mean_diff)
      ])
    )
  )

figure_2 <- ggplot2::ggplot(
  heatmap_data,
  ggplot2::aes(x = .data$modality, y = .data$category_ordered, fill = .data$decision)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.4) +
  ggplot2::scale_fill_manual(
    values = c(
      "Holm-adjusted NI" = "#2B8CBE",
      "Nominal NI only" = "#A6BDDB",
      "No NI" = "#F0F0F0"
    ),
    drop = FALSE
  ) +
  ggplot2::labs(x = NULL, y = NULL, fill = NULL) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
    legend.position = "bottom"
  )
save_plot(
  figure_2, FQP_FIG_MAIN_DIR,
  "Figure_2_endpoint_NI_after_Holm_delta_0p5",
  width = 7.5, height = 6.3
)

selection_plot_data <- integrated_overall |>
  dplyr::select(
    category_label,
    `Source-designated leader` = source_minimum_margin,
    `Export-sample-best` = sample_best_minimum_margin,
    `Repeated cross-fit median` = crossfit_minimum_margin_median
  ) |>
  tidyr::pivot_longer(
    cols = -dplyr::all_of("category_label"),
    names_to = "estimand",
    values_to = "minimum_margin"
  ) |>
  dplyr::mutate(
    category_ordered = forcats::fct_reorder(
      .data$category_label,
      .data$minimum_margin,
      .fun = max,
      .desc = TRUE
    )
  )

figure_3 <- ggplot2::ggplot(
  selection_plot_data,
  ggplot2::aes(
    y = .data$category_ordered,
    x = .data$minimum_margin,
    shape = .data$estimand
  )
) +
  ggplot2::geom_vline(
    xintercept = FQP_PRIMARY_MARGIN,
    linetype = "dashed",
    linewidth = 0.45
  ) +
  ggplot2::geom_point(size = 2.2, position = ggplot2::position_dodge(width = 0.55)) +
  ggplot2::labs(
    x = "Minimum data-supported non-inferiority margin",
    y = NULL,
    shape = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    legend.position = "bottom"
  )
save_plot(
  figure_3, FQP_FIG_SUPP_DIR,
  "Figure_S2_leader_definition_and_selection_sensitivity",
  width = 7.6, height = 6.4
)

figure_s1 <- ggplot2::ggplot(
  overall_source_plot_data,
  ggplot2::aes(y = .data$category_ordered, x = .data$cohen_dz)
) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.45) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = .data$cohen_dz_boot_low,
      xmax = .data$cohen_dz_boot_high
    ),
    orientation = "y",
    width = 0.18,
    linewidth = 0.55
  ) +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::labs(x = "Paired Cohen's dz", y = NULL) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank()
  )
save_plot(
  figure_s1, FQP_FIG_SUPP_DIR,
  "Figure_S1_overall_liking_paired_effect_sizes",
  width = 7.2, height = 6.2
)

selection_top <- selection_frequency |>
  dplyr::group_by(.data$category, .data$category_label) |>
  dplyr::slice_max(.data$selected_share, n = 5L, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    product_label = paste0(.data$selected_code, ifelse(.data$selected_is_source, " (source)", "")),
    category_product = paste(.data$category_label, .data$product_label, sep = " — ")
  )

figure_s2 <- ggplot2::ggplot(
  selection_top,
  ggplot2::aes(
    y = forcats::fct_reorder(.data$category_product, .data$selected_share),
    x = .data$selected_share
  )
) +
  ggplot2::geom_col() +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(x = "Share of successful cross-validation folds", y = NULL) +
  ggplot2::theme_minimal(base_size = 9.5) +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
save_plot(
  figure_s2, FQP_FIG_SUPP_DIR,
  "Figure_S3_crossfit_product_selection_frequency",
  width = 8.2, height = 11.0
)

presentation_plot <- presentation_by_category |>
  dplyr::mutate(
    category_ordered = forcats::fct_reorder(
      .data$category_label,
      .data$median_plant_overall
    )
  )

figure_s3 <- ggplot2::ggplot(
  presentation_plot,
  ggplot2::aes(y = .data$category_ordered, x = .data$median_plant_overall)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      xmin = .data$min_plant_overall,
      xmax = .data$max_plant_overall
    ),
    orientation = "y",
    width = 0.18
  ) +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::scale_x_continuous(breaks = 0:10) +
  ggplot2::labs(
    x = "Plant-based products with observed Overall Liking per respondent\n(minimum, median, maximum)",
    y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 10.5) +
  ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
save_plot(
  figure_s3, FQP_FIG_SUPP_DIR,
  "Figure_S4_product_presentation_counts",
  width = 7.4, height = 6.2
)

if (nrow(behavioral_link_data) > 0L) {
  figure_s4 <- ggplot2::ggplot(
    behavioral_link_data,
    ggplot2::aes(x = .data$source_overall_diff, y = .data$purchase_diff)
  ) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.08, height = 0.08, seed = FQP_SEED), alpha = 0.35, size = 1.3) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, linewidth = 0.7) +
    ggplot2::facet_wrap(~category_label) +
    ggplot2::labs(
      x = "Source-leader minus animal Overall Liking",
      y = "Generic plant-based minus conventional purchase-likelihood score"
    ) +
    ggplot2::theme_minimal(base_size = 10.5)
  save_plot(
    figure_s4, FQP_FIG_SUPP_DIR,
    "Figure_S5_generic_purchase_likelihood_linkage",
    width = 7.4, height = 4.5
  )
}

