FQP_MODEL_DIR <- file.path(FQP_OUTPUT_DIR, "model_outputs")
FQP_RDS_DIR <- file.path(FQP_OUTPUT_DIR, "rds")
dir.create(FQP_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FQP_RDS_DIR, recursive = TRUE, showWarnings = FALSE)

MAIN_ENDPOINTS <- c(
  "Overall Liking", "Flavor", "Texture", "Appearance", "Similarity"
)

required_rds <- c(
  "ratings_revision_long.rds",
  "respondent_registry.rds",
  "panel_assignment.rds",
  "source_leaders.rds",
  "sample_best_ties.rds",
  "sample_best_representative.rds",
  "leader_audit.rds",
  "purchase_likelihood_generic.rds"
)
missing_rds <- required_rds[!file.exists(file.path(FQP_RDS_DIR, required_rds))]
if (length(missing_rds) > 0L) {
  stop(
    "Stage-1 RDS files were not found. Run plant_based_meat_public_analysis.R first.\n",
    paste(missing_rds, collapse = "\n")
  )
}

ratings <- readRDS(file.path(FQP_RDS_DIR, "ratings_revision_long.rds"))
respondent_registry <- readRDS(file.path(FQP_RDS_DIR, "respondent_registry.rds"))
panel_assignment <- readRDS(file.path(FQP_RDS_DIR, "panel_assignment.rds"))
source_leaders <- readRDS(file.path(FQP_RDS_DIR, "source_leaders.rds"))
sample_best_ties <- readRDS(file.path(FQP_RDS_DIR, "sample_best_ties.rds"))
sample_best_representative <- readRDS(
  file.path(FQP_RDS_DIR, "sample_best_representative.rds")
)
leader_audit <- readRDS(file.path(FQP_RDS_DIR, "leader_audit.rds"))
purchase_likelihood <- readRDS(
  file.path(FQP_RDS_DIR, "purchase_likelihood_generic.rds")
)

set.seed(FQP_SEED)

write_model_csv <- function(x, filename) {
  readr::write_csv(x, file.path(FQP_MODEL_DIR, filename), na = "")
}

safe_wilcox_p <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L || all(x == 0)) return(NA_real_)
  tryCatch(
    stats::wilcox.test(x, mu = 0, exact = FALSE)$p.value,
    error = function(e) NA_real_
  )
}

matched_rank_biserial <- function(x) {
  x <- x[!is.na(x) & x != 0]
  if (length(x) == 0L) return(NA_real_)
  ranks <- rank(abs(x), ties.method = "average")
  denominator <- sum(ranks)
  if (denominator == 0) return(NA_real_)
  (sum(ranks[x > 0]) - sum(ranks[x < 0])) / denominator
}

hedges_correction <- function(df) {
  if (is.na(df) || df <= 1) return(NA_real_)
  1 - 3 / (4 * df - 1)
}

paired_statistics <- function(dat) {
  diff <- dat$diff
  n <- sum(!is.na(diff))
  diff <- diff[!is.na(diff)]
  if (n == 0L) return(tibble::tibble())

  mean_diff <- mean(diff)
  sd_diff <- if (n >= 2L) stats::sd(diff) else NA_real_
  se_diff <- if (n >= 2L) sd_diff / sqrt(n) else NA_real_
  dfree <- max(n - 1L, 1L)
  t95 <- stats::qt(0.975, dfree)
  t90 <- stats::qt(0.95, dfree)
  cohen_dz <- if (!is.na(sd_diff) && sd_diff > 0) mean_diff / sd_diff else NA_real_
  hedges_gz <- cohen_dz * hedges_correction(n - 1L)

  tibble::tibble(
    n = n,
    plant_mean = mean(dat$plant_score, na.rm = TRUE),
    animal_mean_paired = mean(dat$animal_score, na.rm = TRUE),
    mean_diff = mean_diff,
    sd_diff = sd_diff,
    se_diff = se_diff,
    ci90_low = mean_diff - t90 * se_diff,
    ci90_high = mean_diff + t90 * se_diff,
    ci95_low = mean_diff - t95 * se_diff,
    ci95_high = mean_diff + t95 * se_diff,
    lower_one_sided_95 = mean_diff - t90 * se_diff,
    minimum_data_supported_margin = pmax(0, -(mean_diff - t90 * se_diff)),
    cohen_dz = cohen_dz,
    hedges_gz = hedges_gz,
    matched_rank_biserial = matched_rank_biserial(diff),
    same_or_better = mean(diff >= 0),
    same_or_better_n = sum(diff >= 0),
    plant_higher = mean(diff > 0),
    equal = mean(diff == 0),
    animal_higher = mean(diff < 0),
    paired_t_p_two_sided = if (!is.na(se_diff) && se_diff > 0) {
      2 * stats::pt(-abs(mean_diff / se_diff), df = n - 1L)
    } else {
      NA_real_
    },
    wilcox_p_two_sided = safe_wilcox_p(diff)
  )
}

bootstrap_paired_effects <- function(dat, reps = FQP_EFFECT_BOOT_REPS) {
  diff <- dat$diff
  diff <- diff[!is.na(diff)]
  n <- length(diff)
  if (n < 2L || reps < 2L) {
    return(tibble::tibble(
      boot_reps = reps,
      mean_diff_boot_low = NA_real_, mean_diff_boot_high = NA_real_,
      cohen_dz_boot_low = NA_real_, cohen_dz_boot_high = NA_real_
    ))
  }

  draws <- replicate(reps, {
    boot <- sample(diff, size = n, replace = TRUE)
    boot_sd <- stats::sd(boot)
    c(
      mean_diff = mean(boot),
      cohen_dz = if (is.finite(boot_sd) && boot_sd > 0) mean(boot) / boot_sd else NA_real_
    )
  })

  tibble::tibble(
    boot_reps = reps,
    mean_diff_boot_low = stats::quantile(
      draws["mean_diff", ], 0.025, na.rm = TRUE, names = FALSE
    ),
    mean_diff_boot_high = stats::quantile(
      draws["mean_diff", ], 0.975, na.rm = TRUE, names = FALSE
    ),
    cohen_dz_boot_low = stats::quantile(
      draws["cohen_dz", ], 0.025, na.rm = TRUE, names = FALSE
    ),
    cohen_dz_boot_high = stats::quantile(
      draws["cohen_dz", ], 0.975, na.rm = TRUE, names = FALSE
    )
  )
}

summarise_paired_groups <- function(dat, group_vars, bootstrap = FALSE) {
  grouped <- dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)))

  basic <- grouped |>
    dplyr::group_modify(function(.x, .y) paired_statistics(.x)) |>
    dplyr::ungroup()

  if (!isTRUE(bootstrap)) return(basic)

  message(
    "Bootstrapping paired mean differences and Cohen's dz (",
    FQP_EFFECT_BOOT_REPS, " replications per group)..."
  )
  boot <- grouped |>
    dplyr::group_modify(function(.x, .y) {
      bootstrap_paired_effects(.x, reps = FQP_EFFECT_BOOT_REPS)
    }) |>
    dplyr::ungroup()

  dplyr::left_join(basic, boot, by = group_vars)
}

ni_p_value <- function(mean_diff, se_diff, n, delta) {
  if (is.na(mean_diff) || is.na(se_diff) || n < 2L) return(NA_real_)
  if (se_diff == 0) return(ifelse(mean_diff > -delta, 0, 1))
  t_stat <- (mean_diff + delta) / se_diff
  stats::pt(t_stat, df = n - 1L, lower.tail = FALSE)
}

add_noninferiority <- function(summary_table, margins = FQP_MARGINS) {
  summary_table |>
    tidyr::crossing(delta = margins) |>
    dplyr::mutate(
      p_ni = purrr::pmap_dbl(
        list(.data$mean_diff, .data$se_diff, .data$n, .data$delta),
        ni_p_value
      ),
      noninferior_nominal = !is.na(.data$p_ni) & .data$p_ni < FQP_ALPHA,
      ni_decision_nominal = dplyr::if_else(
        .data$noninferior_nominal,
        "Meets NI criterion",
        "Does not meet NI criterion"
      )
    )
}

message("Stage 2: rebuilding paired product-versus-animal data")

animal_scores <- ratings |>
  dplyr::filter(.data$product_type == "animal") |>
  dplyr::select(
    category, category_label, category_format,
    Respondent, respondent_uid, Randomizer,
    panel_id, modality,
    animal_product = related_product,
    animal_code = product_code,
    animal_display = product_display,
    animal_score = score
  )

animal_duplicate_check <- animal_scores |>
  dplyr::count(
    .data$category, .data$respondent_uid, .data$modality,
    name = "n_animal_cells"
  ) |>
  dplyr::filter(.data$n_animal_cells > 1L)
if (nrow(animal_duplicate_check) > 0L) {
  stop("More than one animal benchmark rating was found for a respondent/endpoint.")
}

plant_scores <- ratings |>
  dplyr::filter(.data$product_type == "plant_based") |>
  dplyr::select(
    category, category_label, category_format,
    Respondent, respondent_uid, Randomizer,
    panel_id, modality,
    plant_product = related_product,
    plant_code = product_code,
    plant_display = product_display,
    is_source_leader = is_source_leader,
    plant_score = score
  )

paired_all_products <- plant_scores |>
  dplyr::inner_join(
    animal_scores |>
      dplyr::select(
        category, respondent_uid, modality,
        animal_product, animal_code,
        animal_display, animal_score
      ),
    by = c("category", "respondent_uid", "modality")
  ) |>
  dplyr::mutate(
    diff = .data$plant_score - .data$animal_score,
    same_or_better = .data$diff >= 0,
    plant_higher = .data$diff > 0,
    equal = .data$diff == 0,
    animal_higher = .data$diff < 0
  )

saveRDS(
  paired_all_products,
  file.path(FQP_RDS_DIR, "paired_all_products_revision.rds")
)

all_product_group_vars <- c(
  "category", "category_label", "category_format",
  "plant_product", "plant_code", "plant_display", "is_source_leader",
  "animal_product", "animal_code", "animal_display", "modality"
)

all_product_summary <- summarise_paired_groups(
  paired_all_products,
  group_vars = all_product_group_vars,
  bootstrap = FALSE
) |>
  dplyr::mutate(modality = factor(.data$modality, levels = MAIN_ENDPOINTS)) |>
  dplyr::arrange(.data$category, .data$modality, dplyr::desc(.data$plant_mean))
write_model_csv(all_product_summary, "01_all_product_paired_summary.csv")

source_paired <- paired_all_products |>
  dplyr::inner_join(
    source_leaders |>
      dplyr::select(category, source_leader_code),
    by = c("category", "plant_code" = "source_leader_code")
  )

source_summary <- summarise_paired_groups(
  source_paired,
  group_vars = all_product_group_vars,
  bootstrap = TRUE
) |>
  dplyr::mutate(
    leader_estimand = "source-designated leader",
    modality = factor(.data$modality, levels = MAIN_ENDPOINTS)
  ) |>
  dplyr::arrange(.data$category, .data$modality)
write_model_csv(source_summary, "02_source_leader_paired_effects.csv")

source_ni <- add_noninferiority(source_summary) |>
  dplyr::group_by(.data$modality, .data$delta) |>
  dplyr::mutate(
    p_holm_endpoint_margin = stats::p.adjust(.data$p_ni, method = "holm"),
    p_fdr_endpoint_margin = stats::p.adjust(.data$p_ni, method = "BH")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    noninferior_holm_endpoint_margin =
      !is.na(.data$p_holm_endpoint_margin) & .data$p_holm_endpoint_margin < FQP_ALPHA,
    noninferior_fdr_endpoint_margin =
      !is.na(.data$p_fdr_endpoint_margin) & .data$p_fdr_endpoint_margin < FQP_ALPHA
  )

primary_family <- source_ni |>
  dplyr::filter(
    as.character(.data$modality) == "Overall Liking",
    .data$delta == FQP_PRIMARY_MARGIN
  ) |>
  dplyr::mutate(
    p_holm_primary_14 = stats::p.adjust(.data$p_ni, method = "holm"),
    noninferior_holm_primary_14 = .data$p_holm_primary_14 < FQP_ALPHA
  ) |>
  dplyr::select(
    category, p_holm_primary_14,
    noninferior_holm_primary_14
  )

all70_family <- source_ni |>
  dplyr::filter(.data$delta == FQP_PRIMARY_MARGIN) |>
  dplyr::mutate(
    p_holm_all70 = stats::p.adjust(.data$p_ni, method = "holm"),
    noninferior_holm_all70 = .data$p_holm_all70 < FQP_ALPHA
  ) |>
  dplyr::select(
    category, modality,
    p_holm_all70, noninferior_holm_all70
  )

source_ni <- source_ni |>
  dplyr::left_join(primary_family, by = "category") |>
  dplyr::mutate(
    p_holm_primary_14 = dplyr::if_else(
      as.character(.data$modality) == "Overall Liking" &
        .data$delta == FQP_PRIMARY_MARGIN,
      .data$p_holm_primary_14,
      NA_real_
    ),
    noninferior_holm_primary_14 = dplyr::if_else(
      as.character(.data$modality) == "Overall Liking" &
        .data$delta == FQP_PRIMARY_MARGIN,
      .data$noninferior_holm_primary_14,
      NA
    )
  ) |>
  dplyr::left_join(
    all70_family,
    by = c("category", "modality")
  ) |>
  dplyr::mutate(
    p_holm_all70 = dplyr::if_else(
      .data$delta == FQP_PRIMARY_MARGIN,
      .data$p_holm_all70,
      NA_real_
    ),
    noninferior_holm_all70 = dplyr::if_else(
      .data$delta == FQP_PRIMARY_MARGIN,
      .data$noninferior_holm_all70,
      NA
    )
  ) |>
  dplyr::arrange(.data$modality, .data$delta, .data$category)

write_model_csv(source_ni, "03_source_leader_noninferiority_multiplicity.csv")
write_model_csv(
  source_ni |>
    dplyr::filter(
      as.character(.data$modality) == "Overall Liking",
      .data$delta == FQP_PRIMARY_MARGIN
    ),
  "04_primary_overall_liking_delta_0p5.csv"
)

sample_best_paired_representative <- paired_all_products |>
  dplyr::inner_join(
    sample_best_representative |>
      dplyr::select(category, sample_best_code),
    by = c("category", "plant_code" = "sample_best_code")
  )

sample_best_summary <- summarise_paired_groups(
  sample_best_paired_representative,
  group_vars = all_product_group_vars,
  bootstrap = TRUE
) |>
  dplyr::mutate(
    leader_estimand = "export-sample-best representative",
    modality = factor(.data$modality, levels = MAIN_ENDPOINTS)
  ) |>
  dplyr::arrange(.data$category, .data$modality)

sample_best_ni <- add_noninferiority(sample_best_summary) |>
  dplyr::group_by(.data$modality, .data$delta) |>
  dplyr::mutate(
    p_holm_endpoint_margin = stats::p.adjust(.data$p_ni, method = "holm"),
    noninferior_holm_endpoint_margin = .data$p_holm_endpoint_margin < FQP_ALPHA
  ) |>
  dplyr::ungroup()

sample_best_all_ties_summary <- all_product_summary |>
  dplyr::inner_join(
    sample_best_ties |>
      dplyr::select(category, sample_best_code = product_code),
    by = c("category", "plant_code" = "sample_best_code")
  ) |>
  dplyr::mutate(
    tie_aware_role = dplyr::if_else(
      .data$is_source_leader,
      "source member of sample-best tie set",
      "non-source member of sample-best tie set"
    )
  )

write_model_csv(sample_best_summary, "05_sample_best_representative_effects.csv")
write_model_csv(sample_best_ni, "06_sample_best_representative_noninferiority.csv")
write_model_csv(sample_best_all_ties_summary, "07_sample_best_all_ties_summary.csv")

leader_estimand_comparison <- source_summary |>
  dplyr::select(
    category, category_label, category_format,
    modality,
    source_code = plant_code,
    source_display = plant_display,
    source_n = n,
    source_mean_diff = mean_diff,
    source_ci90_low = ci90_low,
    source_ci90_high = ci90_high,
    source_ci95_low = ci95_low,
    source_ci95_high = ci95_high,
    source_lower_one_sided_95 = lower_one_sided_95,
    source_minimum_data_supported_margin = minimum_data_supported_margin,
    source_cohen_dz = cohen_dz,
    source_same_or_better = same_or_better
  ) |>
  dplyr::left_join(
    sample_best_summary |>
      dplyr::select(
        category, modality,
        sample_best_code = plant_code,
        sample_best_display = plant_display,
        sample_best_n = n,
        sample_best_mean_diff = mean_diff,
        sample_best_ci95_low = ci95_low,
        sample_best_ci95_high = ci95_high,
        sample_best_minimum_data_supported_margin = minimum_data_supported_margin,
        sample_best_cohen_dz = cohen_dz,
        sample_best_same_or_better = same_or_better
      ),
    by = c("category", "modality")
  ) |>
  dplyr::left_join(
    leader_audit |>
      dplyr::select(
        category, sample_best_set,
        n_sample_best_tied, leader_status
      ),
    by = "category"
  ) |>
  dplyr::mutate(
    mean_diff_sample_best_minus_source =
      .data$sample_best_mean_diff - .data$source_mean_diff,
    margin_sample_best_minus_source =
      .data$sample_best_minimum_data_supported_margin -
      .data$source_minimum_data_supported_margin
  ) |>
  dplyr::arrange(.data$category, .data$modality)
write_model_csv(leader_estimand_comparison, "08_source_vs_sample_best_comparison.csv")

all_product_ni <- add_noninferiority(all_product_summary) |>
  dplyr::group_by(.data$category, .data$modality, .data$delta) |>
  dplyr::mutate(
    n_candidate_products = dplyr::n(),
    p_holm_within_category = stats::p.adjust(.data$p_ni, method = "holm"),
    p_fdr_within_category = stats::p.adjust(.data$p_ni, method = "BH"),
    noninferior_holm_within_category =
      .data$p_holm_within_category < FQP_ALPHA,
    noninferior_fdr_within_category =
      .data$p_fdr_within_category < FQP_ALPHA
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    .data$category, .data$modality, .data$delta,
    .data$p_holm_within_category, dplyr::desc(.data$mean_diff)
  )
write_model_csv(all_product_ni, "09_all_candidate_products_selection_adjusted_NI.csv")

candidate_category_summary <- all_product_ni |>
  dplyr::filter(
    as.character(.data$modality) == "Overall Liking",
    .data$delta == FQP_PRIMARY_MARGIN
  ) |>
  dplyr::group_by(.data$category, .data$category_label, .data$category_format) |>
  dplyr::arrange(.data$p_holm_within_category, dplyr::desc(.data$mean_diff), .by_group = TRUE) |>
  dplyr::summarise(
    n_candidate_products = dplyr::first(.data$n_candidate_products),
    any_nominal_NI = any(.data$noninferior_nominal),
    any_Holm_NI = any(.data$noninferior_holm_within_category),
    best_adjusted_product_code = dplyr::first(.data$plant_code),
    best_adjusted_product_display = dplyr::first(.data$plant_display),
    best_adjusted_mean_diff = dplyr::first(.data$mean_diff),
    best_raw_p_ni = dplyr::first(.data$p_ni),
    best_Holm_p_ni = dplyr::first(.data$p_holm_within_category),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category)
write_model_csv(candidate_category_summary, "10_candidate_category_selection_summary_delta_0p5.csv")

assign_stratified_folds <- function(meta, k) {
  meta |>
    dplyr::group_by(.data$panel_id) |>
    dplyr::group_modify(function(.x, .y) {
      n <- nrow(.x)
      fold_vector <- rep(seq_len(k), length.out = n)
      .x$fold <- sample(fold_vector, size = n, replace = FALSE)
      .x
    }) |>
    dplyr::ungroup()
}

select_training_leader <- function(training_overall, min_training_n = 10L) {
  candidates <- training_overall |>
    dplyr::group_by(
      .data$plant_code, .data$plant_display, .data$is_source_leader
    ) |>
    dplyr::summarise(
      training_n = dplyr::n(),
      training_mean = mean(.data$plant_score),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$training_n >= min_training_n)

  if (nrow(candidates) == 0L) return(tibble::tibble())

  best_mean <- max(candidates$training_mean)
  candidates |>
    dplyr::filter(abs(.data$training_mean - best_mean) <= 1e-12) |>
    dplyr::arrange(
      dplyr::desc(.data$is_source_leader),
      dplyr::desc(.data$training_n),
      .data$plant_code
    ) |>
    dplyr::slice(1L) |>
    dplyr::mutate(n_training_tied = sum(abs(candidates$training_mean - best_mean) <= 1e-12))
}

crossfit_one_repeat <- function(repeat_id, paired_data, respondent_meta, k) {
  category_list <- split(paired_data, paired_data$category)
  meta_list <- split(respondent_meta, respondent_meta$category)

  selections <- list()
  evaluations <- list()
  counter <- 0L

  for (category_name in names(category_list)) {
    dat_cat <- category_list[[category_name]]
    meta_cat <- meta_list[[category_name]]
    folds <- assign_stratified_folds(meta_cat, k = k)

    for (fold_id in seq_len(k)) {
      train_ids <- folds$respondent_uid[folds$fold != fold_id]
      eval_ids <- folds$respondent_uid[folds$fold == fold_id]

      training_overall <- dat_cat |>
        dplyr::filter(
          .data$respondent_uid %in% train_ids,
          as.character(.data$modality) == "Overall Liking"
        )
      selected <- select_training_leader(training_overall)
      if (nrow(selected) == 0L) next

      selected_code <- selected$plant_code[[1L]]
      eval_dat <- dat_cat |>
        dplyr::filter(
          .data$respondent_uid %in% eval_ids,
          .data$plant_code == selected_code
        ) |>
        dplyr::mutate(
          repeat_id = repeat_id,
          fold = fold_id,
          selected_code = selected_code,
          selected_display = selected$plant_display[[1L]],
          selected_is_source = selected$is_source_leader[[1L]],
          training_n = selected$training_n[[1L]],
          training_mean = selected$training_mean[[1L]],
          n_training_tied = selected$n_training_tied[[1L]]
        )

      counter <- counter + 1L
      selections[[counter]] <- tibble::tibble(
        repeat_id = repeat_id,
        category = category_name,
        category_label = dplyr::first(dat_cat$category_label),
        fold = fold_id,
        selected_code = selected_code,
        selected_display = selected$plant_display[[1L]],
        selected_is_source = selected$is_source_leader[[1L]],
        training_n = selected$training_n[[1L]],
        training_mean = selected$training_mean[[1L]],
        n_training_tied = selected$n_training_tied[[1L]],
        evaluation_respondents_assigned = length(eval_ids),
        evaluation_respondents_with_selected_product =
          dplyr::n_distinct(eval_dat$respondent_uid),
        evaluation_coverage_rate = dplyr::if_else(
          length(eval_ids) > 0L,
          dplyr::n_distinct(eval_dat$respondent_uid) / length(eval_ids),
          NA_real_
        ),
        evaluation_endpoint_rows_observed = nrow(eval_dat)
      )
      evaluations[[counter]] <- eval_dat
    }
  }

  selection_table <- dplyr::bind_rows(selections)
  evaluation_table <- dplyr::bind_rows(evaluations)

  if (nrow(evaluation_table) == 0L) {
    repeat_summary <- tibble::tibble()
  } else {
    repeat_summary <- evaluation_table |>
      dplyr::group_by(
        .data$repeat_id, .data$category, .data$category_label,
        .data$category_format, .data$modality
      ) |>
      dplyr::group_modify(function(.x, .y) paired_statistics(.x)) |>
      dplyr::ungroup()
  }

  list(selection = selection_table, summary = repeat_summary)
}

message(
  "Running repeated ", FQP_CROSSFIT_K, "-fold cross-fitting (",
  FQP_CROSSFIT_REPEATS, " repeats)..."
)

crossfit_meta <- panel_assignment |>
  dplyr::filter(.data$n_observed > 0L) |>
  dplyr::select(
    category, respondent_uid, panel_id
  ) |>
  dplyr::distinct()

crossfit_runs <- vector("list", FQP_CROSSFIT_REPEATS)
for (repeat_id in seq_len(FQP_CROSSFIT_REPEATS)) {
  if (repeat_id == 1L || repeat_id %% 25L == 0L ||
      repeat_id == FQP_CROSSFIT_REPEATS) {
    message("  cross-fitting repeat ", repeat_id, "/", FQP_CROSSFIT_REPEATS)
  }
  set.seed(FQP_SEED + repeat_id)
  crossfit_runs[[repeat_id]] <- crossfit_one_repeat(
    repeat_id = repeat_id,
    paired_data = paired_all_products,
    respondent_meta = crossfit_meta,
    k = FQP_CROSSFIT_K
  )
}

crossfit_selection_by_fold <- purrr::map_dfr(crossfit_runs, "selection")
crossfit_repeat_summary <- purrr::map_dfr(crossfit_runs, "summary") |>
  dplyr::mutate(modality = factor(.data$modality, levels = MAIN_ENDPOINTS))

crossfit_selection_frequency <- crossfit_selection_by_fold |>
  dplyr::count(
    .data$category, .data$category_label,
    .data$selected_code, .data$selected_display,
    .data$selected_is_source,
    name = "selected_folds"
  ) |>
  dplyr::group_by(.data$category, .data$category_label) |>
  dplyr::mutate(
    total_successful_folds = sum(.data$selected_folds),
    selected_share = .data$selected_folds / .data$total_successful_folds
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(.data$category, dplyr::desc(.data$selected_share))

crossfit_evaluation_coverage <- crossfit_selection_by_fold |>
  dplyr::group_by(.data$category, .data$category_label) |>
  dplyr::summarise(
    successful_folds = dplyr::n(),
    assigned_respondents_total = sum(.data$evaluation_respondents_assigned),
    respondents_with_selected_product_total = sum(
      .data$evaluation_respondents_with_selected_product
    ),
    fold_coverage_mean = mean(.data$evaluation_coverage_rate),
    fold_coverage_min = min(.data$evaluation_coverage_rate),
    fold_coverage_max = max(.data$evaluation_coverage_rate),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    overall_coverage_rate = dplyr::if_else(
      .data$assigned_respondents_total > 0L,
      .data$respondents_with_selected_product_total /
        .data$assigned_respondents_total,
      NA_real_
    )
  ) |>
  dplyr::arrange(.data$category)

crossfit_distribution <- crossfit_repeat_summary |>
  dplyr::group_by(
    .data$category, .data$category_label,
    .data$category_format, .data$modality
  ) |>
  dplyr::summarise(
    successful_repeats = dplyr::n(),
    n_evaluation_mean = mean(.data$n),
    n_evaluation_min = min(.data$n),
    n_evaluation_max = max(.data$n),
    mean_diff_mean = mean(.data$mean_diff),
    mean_diff_median = stats::median(.data$mean_diff),
    mean_diff_q025 = stats::quantile(.data$mean_diff, 0.025, names = FALSE),
    mean_diff_q975 = stats::quantile(.data$mean_diff, 0.975, names = FALSE),
    minimum_margin_mean = mean(.data$minimum_data_supported_margin),
    minimum_margin_median = stats::median(.data$minimum_data_supported_margin),
    minimum_margin_q025 = stats::quantile(
      .data$minimum_data_supported_margin, 0.025, names = FALSE
    ),
    minimum_margin_q975 = stats::quantile(
      .data$minimum_data_supported_margin, 0.975, names = FALSE
    ),
    same_or_better_mean = mean(.data$same_or_better),
    cohen_dz_mean = mean(.data$cohen_dz, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category, .data$modality)

crossfit_ni_distribution <- crossfit_repeat_summary |>
  tidyr::crossing(delta = FQP_MARGINS) |>
  dplyr::mutate(
    p_ni = purrr::pmap_dbl(
      list(.data$mean_diff, .data$se_diff, .data$n, .data$delta),
      ni_p_value
    ),
    noninferior = .data$p_ni < FQP_ALPHA
  ) |>
  dplyr::group_by(
    .data$category, .data$category_label,
    .data$category_format, .data$modality, .data$delta
  ) |>
  dplyr::summarise(
    successful_repeats = dplyr::n(),
    probability_nominal_NI = mean(.data$noninferior, na.rm = TRUE),
    median_p_ni = stats::median(.data$p_ni, na.rm = TRUE),
    mean_diff_mean = mean(.data$mean_diff),
    minimum_margin_median = stats::median(.data$minimum_data_supported_margin),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category, .data$modality, .data$delta)

write_model_csv(crossfit_selection_by_fold, "11_crossfit_selection_by_fold.csv")
write_model_csv(crossfit_selection_frequency, "12_crossfit_selection_frequency.csv")
write_model_csv(crossfit_evaluation_coverage, "12b_crossfit_evaluation_coverage.csv")
write_model_csv(crossfit_repeat_summary, "13_crossfit_repeat_summary.csv")
write_model_csv(crossfit_distribution, "14_crossfit_distribution_by_endpoint.csv")
write_model_csv(crossfit_ni_distribution, "15_crossfit_NI_probability_by_margin.csv")

integrated_overall <- source_ni |>
  dplyr::filter(
    as.character(.data$modality) == "Overall Liking",
    .data$delta == FQP_PRIMARY_MARGIN
  ) |>
  dplyr::select(
    category, category_label, category_format,
    source_code = plant_code,
    source_display = plant_display,
    source_n = n,
    source_mean_diff = mean_diff,
    source_ci90_low = ci90_low,
    source_ci90_high = ci90_high,
    source_ci95_low = ci95_low,
    source_ci95_high = ci95_high,
    source_lower_one_sided_95 = lower_one_sided_95,
    source_minimum_margin = minimum_data_supported_margin,
    source_cohen_dz = cohen_dz,
    source_same_or_better = same_or_better,
    source_p_ni = p_ni,
    source_p_holm_primary14 = p_holm_primary_14,
    source_NI_nominal = noninferior_nominal,
    source_NI_Holm_primary14 = noninferior_holm_primary_14
  ) |>
  dplyr::left_join(
    sample_best_ni |>
      dplyr::filter(
        as.character(.data$modality) == "Overall Liking",
        .data$delta == FQP_PRIMARY_MARGIN
      ) |>
      dplyr::select(
        category,
        sample_best_code = plant_code,
        sample_best_display = plant_display,
        sample_best_n = n,
        sample_best_mean_diff = mean_diff,
        sample_best_minimum_margin = minimum_data_supported_margin,
        sample_best_cohen_dz = cohen_dz,
        sample_best_p_ni = p_ni,
        sample_best_p_holm14 = p_holm_endpoint_margin,
        sample_best_NI_nominal = noninferior_nominal,
        sample_best_NI_Holm14 = noninferior_holm_endpoint_margin
      ),
    by = "category"
  ) |>
  dplyr::left_join(
    candidate_category_summary |>
      dplyr::select(
        category, n_candidate_products,
        any_nominal_NI, any_Holm_NI,
        best_adjusted_product_code, best_Holm_p_ni
      ),
    by = "category"
  ) |>
  dplyr::left_join(
    crossfit_distribution |>
      dplyr::filter(as.character(.data$modality) == "Overall Liking") |>
      dplyr::select(
        category,
        crossfit_successful_repeats = successful_repeats,
        crossfit_mean_diff_mean = mean_diff_mean,
        crossfit_mean_diff_q025 = mean_diff_q025,
        crossfit_mean_diff_q975 = mean_diff_q975,
        crossfit_minimum_margin_median = minimum_margin_median,
        crossfit_minimum_margin_q025 = minimum_margin_q025,
        crossfit_minimum_margin_q975 = minimum_margin_q975
      ),
    by = "category"
  ) |>
  dplyr::left_join(
    crossfit_ni_distribution |>
      dplyr::filter(
        as.character(.data$modality) == "Overall Liking",
        .data$delta == FQP_PRIMARY_MARGIN
      ) |>
      dplyr::select(
        category,
        crossfit_probability_nominal_NI = probability_nominal_NI
      ),
    by = "category"
  ) |>
  dplyr::left_join(
    leader_audit |>
      dplyr::select(
        category, sample_best_set,
        n_sample_best_tied, leader_status
      ),
    by = "category"
  ) |>
  dplyr::arrange(dplyr::desc(.data$source_mean_diff))

write_model_csv(integrated_overall, "16_integrated_overall_liking_revision_summary.csv")

multiplicity_summary <- tibble::tibble(
  family = c(
    "Overall Liking, delta 0.5, 14 source leaders",
    "All five endpoints, delta 0.5, 70 source-leader tests",
    "Endpoint-specific family, delta 0.5, 14 categories per endpoint"
  ),
  number_of_tests = c(14L, 70L, 14L),
  nominal_discoveries = c(
    sum(
      source_ni$noninferior_nominal[
        as.character(source_ni$modality) == "Overall Liking" &
          source_ni$delta == FQP_PRIMARY_MARGIN
      ]
    ),
    sum(source_ni$noninferior_nominal[source_ni$delta == FQP_PRIMARY_MARGIN]),
    NA_integer_
  ),
  Holm_discoveries = c(
    sum(
      source_ni$noninferior_holm_primary_14[
        as.character(source_ni$modality) == "Overall Liking" &
          source_ni$delta == FQP_PRIMARY_MARGIN
      ],
      na.rm = TRUE
    ),
    sum(
      source_ni$noninferior_holm_all70[source_ni$delta == FQP_PRIMARY_MARGIN],
      na.rm = TRUE
    ),
    NA_integer_
  ),
  note = c(
    "Primary inferential family",
    "Conservative global audit requested by Reviewer 1",
    "See endpoint-specific rows in 03_source_leader_noninferiority_multiplicity.csv"
  )
)
write_model_csv(multiplicity_summary, "17_multiplicity_family_summary.csv")

purchase_wide <- purchase_likelihood |>
  dplyr::filter(.data$purchase_target %in% c("plant_based", "conventional")) |>
  dplyr::select(
    category, category_label,
    respondent_uid, purchase_target, purchase_score
  ) |>
  tidyr::pivot_wider(
    names_from = purchase_target,
    values_from = purchase_score,
    names_prefix = "purchase_"
  )

source_overall_individual <- source_paired |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking") |>
  dplyr::select(
    category, category_label, category_format,
    respondent_uid, plant_code, plant_display,
    plant_score, animal_score,
    source_overall_diff = diff
  )

behavioral_link_data <- source_overall_individual |>
  dplyr::inner_join(
    purchase_wide,
    by = c("category", "category_label", "respondent_uid")
  ) |>
  dplyr::filter(
    !is.na(.data$purchase_plant_based),
    !is.na(.data$purchase_conventional)
  ) |>
  dplyr::mutate(
    purchase_diff = .data$purchase_plant_based - .data$purchase_conventional,
    plant_purchase_at_least_conventional =
      .data$purchase_plant_based >= .data$purchase_conventional,
    behavioral_measure_scope =
      "generic category-level purchase likelihood; not product-specific"
  )

safe_cor_test <- function(x, y, method) {
  keep <- stats::complete.cases(x, y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 4L || stats::sd(x) == 0 || stats::sd(y) == 0) {
    return(c(estimate = NA_real_, p_value = NA_real_))
  }
  result <- suppressWarnings(stats::cor.test(x, y, method = method, exact = FALSE))
  c(estimate = unname(result$estimate), p_value = result$p.value)
}

behavioral_link_summary <- behavioral_link_data |>
  dplyr::group_by(.data$category, .data$category_label) |>
  dplyr::group_modify(function(.x, .y) {
    pearson <- safe_cor_test(
      .x$source_overall_diff, .x$purchase_diff, method = "pearson"
    )
    spearman <- safe_cor_test(
      .x$source_overall_diff, .x$purchase_diff, method = "spearman"
    )
    tibble::tibble(
      n = nrow(.x),
      source_overall_diff_mean = mean(.x$source_overall_diff),
      plant_purchase_mean = mean(.x$purchase_plant_based),
      conventional_purchase_mean = mean(.x$purchase_conventional),
      purchase_diff_mean = mean(.x$purchase_diff),
      plant_purchase_at_least_conventional_share =
        mean(.x$plant_purchase_at_least_conventional),
      pearson_correlation = pearson[["estimate"]],
      pearson_p_value = pearson[["p_value"]],
      spearman_correlation = spearman[["estimate"]],
      spearman_p_value = spearman[["p_value"]]
    )
  }) |>
  dplyr::ungroup()

model_coefficients <- function(model, exponentiate = FALSE) {
  coef_matrix <- summary(model)$coefficients
  conf <- stats::confint.default(model)
  result <- tibble::tibble(
    term = rownames(coef_matrix),
    estimate = coef_matrix[, 1],
    std_error = coef_matrix[, 2],
    statistic = coef_matrix[, 3],
    p_value = coef_matrix[, 4],
    conf_low = conf[, 1],
    conf_high = conf[, 2]
  )
  if (isTRUE(exponentiate)) {
    result <- result |>
      dplyr::mutate(
        odds_ratio = exp(.data$estimate),
        odds_ratio_low = exp(.data$conf_low),
        odds_ratio_high = exp(.data$conf_high)
      )
  }
  result
}

if (nrow(behavioral_link_data) >= 10L &&
    dplyr::n_distinct(behavioral_link_data$category_label) >= 2L) {
  behavioral_link_data <- behavioral_link_data |>
    dplyr::mutate(
      category_label = factor(.data$category_label)
    )

  purchase_difference_model <- stats::lm(
    purchase_diff ~ source_overall_diff + category_label,
    data = behavioral_link_data
  )
  purchase_binary_model <- stats::glm(
    plant_purchase_at_least_conventional ~ source_overall_diff + category_label,
    data = behavioral_link_data,
    family = stats::binomial()
  )

  purchase_difference_coefficients <- model_coefficients(
    purchase_difference_model,
    exponentiate = FALSE
  ) |>
    dplyr::mutate(model = "Linear model: purchase-score difference", .before = 1L)
  purchase_binary_coefficients <- model_coefficients(
    purchase_binary_model,
    exponentiate = TRUE
  ) |>
    dplyr::mutate(model = "Logit: plant purchase likelihood >= conventional", .before = 1L)

  behavioral_prediction_grid <- tidyr::crossing(
    source_overall_diff = c(-1.0, -0.7, -0.5, -0.3, 0.0),
    category_label = levels(behavioral_link_data$category_label)
  ) |>
    dplyr::mutate(
      category_label = factor(
        .data$category_label,
        levels = levels(behavioral_link_data$category_label)
      )
    )
  behavioral_prediction_grid$predicted_probability_plant_at_least_conventional <-
    stats::predict(
      purchase_binary_model,
      newdata = behavioral_prediction_grid,
      type = "response"
    )
  behavioral_prediction_grid$predicted_purchase_score_difference <-
    stats::predict(
      purchase_difference_model,
      newdata = behavioral_prediction_grid
    )
} else {
  purchase_difference_coefficients <- tibble::tibble()
  purchase_binary_coefficients <- tibble::tibble()
  behavioral_prediction_grid <- tibble::tibble()
}

write_model_csv(behavioral_link_data, "18_generic_purchase_linkage_individual.csv")
write_model_csv(behavioral_link_summary, "19_generic_purchase_linkage_by_category.csv")
write_model_csv(
  dplyr::bind_rows(purchase_difference_coefficients, purchase_binary_coefficients),
  "20_generic_purchase_linkage_model_coefficients.csv"
)
write_model_csv(behavioral_prediction_grid, "21_generic_purchase_linkage_predictions.csv")

saveRDS(source_summary, file.path(FQP_RDS_DIR, "source_summary_revision.rds"))
saveRDS(source_ni, file.path(FQP_RDS_DIR, "source_ni_revision.rds"))
saveRDS(sample_best_summary, file.path(FQP_RDS_DIR, "sample_best_summary_revision.rds"))
saveRDS(all_product_ni, file.path(FQP_RDS_DIR, "all_product_ni_revision.rds"))
saveRDS(crossfit_distribution, file.path(FQP_RDS_DIR, "crossfit_distribution_revision.rds"))
saveRDS(crossfit_ni_distribution, file.path(FQP_RDS_DIR, "crossfit_ni_distribution_revision.rds"))
saveRDS(integrated_overall, file.path(FQP_RDS_DIR, "integrated_overall_revision.rds"))
saveRDS(behavioral_link_data, file.path(FQP_RDS_DIR, "behavioral_link_data_revision.rds"))

stage2_validation <- tibble::tribble(
  ~check, ~passed, ~observed, ~expected,
  "Source-leader effects contain 14 x 5 rows",
  nrow(source_summary) == 70L,
  as.character(nrow(source_summary)),
  "70",
  "Primary family contains 14 rows",
  nrow(
    source_ni |>
      dplyr::filter(
        as.character(.data$modality) == "Overall Liking",
        .data$delta == FQP_PRIMARY_MARGIN
      )
  ) == 14L,
  as.character(nrow(
    source_ni |>
      dplyr::filter(
        as.character(.data$modality) == "Overall Liking",
        .data$delta == FQP_PRIMARY_MARGIN
      )
  )),
  "14",
  "All-70 family contains 70 rows",
  nrow(source_ni |> dplyr::filter(.data$delta == FQP_PRIMARY_MARGIN)) == 70L,
  as.character(nrow(source_ni |> dplyr::filter(.data$delta == FQP_PRIMARY_MARGIN))),
  "70",
  "Integrated overall table contains 14 rows",
  nrow(integrated_overall) == 14L,
  as.character(nrow(integrated_overall)),
  "14",
  "Generic purchase-likelihood linkage contains both available categories",
  setequal(unique(behavioral_link_data$category), c("Deli_Ham", "Meatballs")),
  paste(sort(unique(behavioral_link_data$category)), collapse = ";"),
  "Deli_Ham;Meatballs",
  "Cross-fitting produced at least one successful repeat for every category",
  setequal(crossfit_distribution$category, source_leaders$category),
  paste(sort(unique(crossfit_distribution$category)), collapse = ";"),
  paste(sort(source_leaders$category), collapse = ";")
)
stage2_validation <- dplyr::bind_rows(
  stage2_validation,
  tibble::tibble(
    check = c(
      "Cross-fit coverage summary contains all 14 categories",
      "All cross-fit fold coverage rates lie between zero and one"
    ),
    passed = c(
      nrow(crossfit_evaluation_coverage) == 14L &&
        setequal(crossfit_evaluation_coverage$category, source_leaders$category),
      all(
        is.na(crossfit_selection_by_fold$evaluation_coverage_rate) |
          dplyr::between(
            crossfit_selection_by_fold$evaluation_coverage_rate,
            0, 1
          )
      )
    ),
    observed = c(
      as.character(nrow(crossfit_evaluation_coverage)),
      paste0(
        format(min(crossfit_selection_by_fold$evaluation_coverage_rate, na.rm = TRUE), digits = 4),
        " to ",
        format(max(crossfit_selection_by_fold$evaluation_coverage_rate, na.rm = TRUE), digits = 4)
      )
    ),
    expected = c("14", "0 to 1")
  )
)

write_model_csv(stage2_validation, "99_stage2_validation.csv")

if (any(!stage2_validation$passed)) {
  failed <- stage2_validation |>
    dplyr::filter(!.data$passed)
  stop(
    "Stage-2 validation failed:\n",
    paste0("- ", failed$check, " [observed: ", failed$observed,
           "; expected: ", failed$expected, "]", collapse = "\n")
  )
}

message("Stage 2 completed: revised primary, multiplicity, effect-size, and selection analyses were written.")
