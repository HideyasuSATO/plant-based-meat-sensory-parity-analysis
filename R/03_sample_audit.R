FQP_ADDITIONAL_TABLE_DIR <- file.path(
  FQP_ADDITIONAL_AUDIT_DIR,
  "manuscript_tables"
)
FQP_ADDITIONAL_RDS_DIR <- file.path(FQP_ADDITIONAL_AUDIT_DIR, "rds")
dir.create(FQP_ADDITIONAL_TABLE_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FQP_ADDITIONAL_RDS_DIR, recursive = TRUE, showWarnings = FALSE)

set.seed(FQP_ADDITIONAL_AUDIT_SEED)

EXPECTED_FILES <- c(
  "Bacon.csv",
  "Bratwurst.csv",
  "Breaded_Chicken_Filets.csv",
  "Breakfast_Sausages.csv",
  "Burgers.csv",
  "Chicken_Nuggets.csv",
  "Deli_Ham.csv",
  "Deli_Turkey.csv",
  "Hot_Dogs.csv",
  "Meatballs.csv",
  "Pulled_Pork.csv",
  "Steak.csv",
  "Unbreaded_Chicken_Filets.csv",
  "Unbreaded_Chicken_StripsandChunks.csv"
)
EXPECTED_CATEGORIES <- tools::file_path_sans_ext(EXPECTED_FILES)
INPUT_FILES <- file.path(FQP_DATA_DIR, EXPECTED_FILES)

CATEGORY_LABELS <- c(
  "Bacon" = "Bacon",
  "Bratwurst" = "Bratwurst",
  "Breaded_Chicken_Filets" = "Breaded Chicken Filets",
  "Breakfast_Sausages" = "Breakfast Sausages",
  "Burgers" = "Burgers",
  "Chicken_Nuggets" = "Chicken Nuggets",
  "Deli_Ham" = "Deli Ham",
  "Deli_Turkey" = "Deli Turkey",
  "Hot_Dogs" = "Hot Dogs",
  "Meatballs" = "Meatballs",
  "Pulled_Pork" = "Pulled Pork",
  "Steak" = "Steak",
  "Unbreaded_Chicken_Filets" = "Unbreaded Chicken Filets",
  "Unbreaded_Chicken_StripsandChunks" =
    "Unbreaded Chicken Strips and Chunks"
)

REQUIRED_EXISTING_OUTPUTS <- c(
  file.path(FQP_REVISION_DIR, "audit", "03_respondent_registry.csv"),
  file.path(FQP_REVISION_DIR, "audit", "23_source_leader_missingness_status.csv"),
  file.path(FQP_REVISION_DIR, "audit", "24_source_leader_missingness_summary.csv"),
  file.path(FQP_REVISION_DIR, "audit", "25_locked_audit_validation.csv"),
  file.path(FQP_REVISION_DIR, "audit", "12b_panel_summary_consistency.csv")
)

missing_input_files <- INPUT_FILES[!file.exists(INPUT_FILES)]
missing_existing_outputs <- REQUIRED_EXISTING_OUTPUTS[
  !file.exists(REQUIRED_EXISTING_OUTPUTS)
]
if (length(missing_input_files) > 0L) {
  stop(
    "Required raw CSV files were not found:\n",
    paste(missing_input_files, collapse = "\n")
  )
}
if (length(missing_existing_outputs) > 0L) {
  stop(
    "Required v1.2 audit outputs were not found:\n",
    paste(missing_existing_outputs, collapse = "\n")
  )
}

clean_category <- function(x) {
  x <- as.character(x)
  out <- unname(CATEGORY_LABELS[x])
  missing <- is.na(out)
  out[missing] <- stringr::str_replace_all(x[missing], "_", " ")
  out
}

extract_tag <- function(x, tag) {
  pattern <- paste0("\\[", tag, ":\\s*([^\\]]+)\\]")
  stringr::str_match(x, pattern)[, 2]
}

normalize_modality <- function(x) {
  x <- stringr::str_squish(as.character(x))
  dplyr::case_when(
    x == "Dietary Restriciton" ~ "Dietary Restriction",
    x == "Dietary Restriciton " ~ "Dietary Restriction",
    TRUE ~ x
  )
}

clean_response <- function(x) {
  x <- stringr::str_squish(as.character(x))
  missing_token <- is.na(x) |
    x == "" |
    stringr::str_to_lower(x) %in% c(
      "no response", "not answered", "n/a", "na"
    )
  x[missing_token] <- NA_character_
  x
}

map_exact <- function(x, mapping) {
  x <- clean_response(x)
  out <- unname(mapping[x])
  out[is.na(x)] <- NA_character_
  out
}

normalize_age <- function(x) {
  map_exact(
    x,
    c(
      "18-25" = "18-25",
      "26-35" = "26-35",
      "36-45" = "36-45",
      "46-55" = "46-55",
      "Greater than 55" = "Greater than 55"
    )
  )
}

normalize_gender <- function(x) {
  map_exact(
    x,
    c(
      "Female" = "Female",
      "Male" = "Male",
      "Non-binary" = "Non-binary",
      "Prefer not to say" = "Prefer not to say"
    )
  )
}

normalize_dietary <- function(x) {
  x <- clean_response(x)
  dplyr::case_when(
    stringr::str_starts(x, "Omnivore") ~ "Omnivore",
    stringr::str_starts(x, "Flexitarian") ~ "Flexitarian",
    is.na(x) ~ NA_character_,
    TRUE ~ NA_character_
  )
}

normalize_education <- function(x) {
  map_exact(
    x,
    c(
      "Some High School" = "Some High School",
      "High School" = "High School",
      "Trade School" = "Trade School",
      "Some College" = "Some College",
      "Bachelor's Degree" = "Bachelor's Degree",
      "Master's Degree" = "Master's Degree",
      "Ph.D. or higher" = "Ph.D. or higher"
    )
  )
}

normalize_consumption <- function(x) {
  map_exact(
    x,
    c(
      "Never or rarely" = "Never or rarely",
      "2-3 times a year" = "2-3 times a year",
      "4-5 times a year" = "4-5 times a year",
      "Once every 1-2 months" = "Once every 1-2 months",
      "2-3 times a month" = "2-3 times a month",
      "Once a week" = "Once a week",
      "2-3 times a week" = "2-3 times a week",
      "Everyday" = "Everyday"
    )
  )
}

AGE_LEVELS <- c(
  "18-25", "26-35", "36-45", "46-55", "Greater than 55"
)
GENDER_LEVELS <- c(
  "Female", "Male", "Non-binary", "Prefer not to say"
)
DIETARY_LEVELS <- c("Omnivore", "Flexitarian")
EDUCATION_LEVELS <- c(
  "Some High School", "High School", "Trade School", "Some College",
  "Bachelor's Degree", "Master's Degree", "Ph.D. or higher"
)
CONSUMPTION_LEVELS <- c(
  "Never or rarely", "2-3 times a year", "4-5 times a year",
  "Once every 1-2 months", "2-3 times a month", "Once a week",
  "2-3 times a week", "Everyday"
)

CHARACTERISTIC_VARIABLES <- c(
  "age_group", "gender", "dietary_preference", "education",
  "consumption_frequency"
)
CHARACTERISTIC_LABELS <- c(
  "age_group" = "Age",
  "gender" = "Gender",
  "dietary_preference" = "Dietary preference",
  "education" = "Education",
  "consumption_frequency" = "Category-specific consumption frequency"
)
CHARACTERISTIC_LEVELS <- list(
  age_group = AGE_LEVELS,
  gender = GENDER_LEVELS,
  dietary_preference = DIETARY_LEVELS,
  education = EDUCATION_LEVELS,
  consumption_frequency = CONSUMPTION_LEVELS
)

write_audit_csv <- function(x, filename) {
  readr::write_csv(
    x,
    file.path(FQP_ADDITIONAL_AUDIT_DIR, filename),
    na = ""
  )
}

write_table_csv <- function(x, filename) {
  readr::write_csv(
    x,
    file.path(FQP_ADDITIONAL_TABLE_DIR, filename),
    na = ""
  )
}

format_n_pct <- function(n, denominator, digits = 1L) {
  output_length <- max(length(n), length(denominator))
  n <- rep_len(n, output_length)
  denominator <- rep_len(denominator, output_length)
  output <- rep(NA_character_, output_length)
  valid <- !is.na(n) & !is.na(denominator) & denominator > 0
  output[valid] <- paste0(
    n[valid], " (",
    formatC(
      100 * n[valid] / denominator[valid],
      format = "f",
      digits = digits
    ),
    "%)"
  )
  output
}

format_p <- function(x) {
  dplyr::case_when(
    is.na(x) ~ "NA",
    x < 0.001 ~ "<0.001",
    TRUE ~ formatC(x, format = "f", digits = 3)
  )
}

safe_test_p <- function(expr) {
  tryCatch(expr, error = function(e) NA_real_)
}

safe_smd_numeric <- function(x_missing, x_observed) {
  x_missing <- x_missing[!is.na(x_missing)]
  x_observed <- x_observed[!is.na(x_observed)]
  if (length(x_missing) < 2L || length(x_observed) < 2L) return(NA_real_)
  denominator <- sqrt(
    (stats::var(x_missing) + stats::var(x_observed)) / 2
  )
  if (!is.finite(denominator) || denominator == 0) return(NA_real_)
  (mean(x_missing) - mean(x_observed)) / denominator
}

safe_smd_binary <- function(p_missing, p_observed) {
  denominator <- sqrt(
    (p_missing * (1 - p_missing) +
       p_observed * (1 - p_observed)) / 2
  )
  if (!is.finite(denominator) || denominator == 0) return(NA_real_)
  (p_missing - p_observed) / denominator
}

find_one_demographic_column <- function(meta, predicate, variable_label) {
  demographic_meta <- meta |>
    dplyr::filter(.data$question_type == "Demographic")
  keep <- predicate(demographic_meta$modality)
  keep[is.na(keep)] <- FALSE
  candidates <- demographic_meta[keep, , drop = FALSE]
  if (nrow(candidates) != 1L) {
    stop(
      "Expected exactly one demographic column for ", variable_label,
      " in ", unique(meta$category), "; found ", nrow(candidates), ".\n",
      paste(candidates$raw_col, collapse = "\n")
    )
  }
  candidates
}

as_logical_safe <- function(x) {
  if (is.logical(x)) return(x)
  x <- stringr::str_to_lower(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    x %in% c("true", "1") ~ TRUE,
    x %in% c("false", "0") ~ FALSE,
    TRUE ~ NA
  )
}

read_demographics_one <- function(path) {
  category <- tools::file_path_sans_ext(basename(path))
  raw <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal",
    show_col_types = FALSE,
    progress = FALSE
  )

  if (!all(c("Respondent", "Randomizer") %in% names(raw))) {
    stop("Respondent or Randomizer was not found in ", basename(path))
  }

  meta <- tibble::tibble(
    raw_col = names(raw)
  ) |>
    dplyr::mutate(
      category = .env$category,
      question_type = extract_tag(.data$raw_col, "QUESTION_TYPE"),
      modality = normalize_modality(
        extract_tag(.data$raw_col, "MODALITY")
      )
    )

  age_meta <- find_one_demographic_column(
    meta,
    function(x) x == "Age",
    "Age"
  )
  gender_meta <- find_one_demographic_column(
    meta,
    function(x) x == "Gender",
    "Gender"
  )
  dietary_meta <- find_one_demographic_column(
    meta,
    function(x) x == "Dietary Restriction",
    "Dietary Restriction"
  )
  education_meta <- find_one_demographic_column(
    meta,
    function(x) x == "Education",
    "Education"
  )
  consumption_meta <- find_one_demographic_column(
    meta,
    function(x) stringr::str_detect(
      stringr::str_to_lower(x),
      "consumption"
    ),
    "category-specific consumption frequency"
  )

  city_site_meta <- meta |>
    dplyr::filter(
      stringr::str_detect(
        stringr::str_to_lower(
          paste(.data$modality, .data$raw_col)
        ),
        "\\b(city|site|location|restaurant|metropolitan area)\\b"
      )
    )

  age_col <- age_meta$raw_col[[1]]
  gender_col <- gender_meta$raw_col[[1]]
  dietary_col <- dietary_meta$raw_col[[1]]
  education_col <- education_meta$raw_col[[1]]
  consumption_col <- consumption_meta$raw_col[[1]]

  respondent <- suppressWarnings(readr::parse_integer(raw$Respondent))
  randomizer <- suppressWarnings(readr::parse_integer(raw$Randomizer))

  data <- tibble::tibble(
    category = category,
    category_label = clean_category(category),
    Respondent = respondent,
    respondent_uid = paste0(category, "__", respondent),
    Randomizer = randomizer,
    age_raw = raw[[age_col]],
    gender_raw = raw[[gender_col]],
    dietary_raw = raw[[dietary_col]],
    education_raw = raw[[education_col]],
    consumption_raw = raw[[consumption_col]],
    consumption_modality = consumption_meta$modality[[1]]
  ) |>
    dplyr::mutate(
      age_group = normalize_age(.data$age_raw),
      gender = normalize_gender(.data$gender_raw),
      dietary_preference = normalize_dietary(.data$dietary_raw),
      education = normalize_education(.data$education_raw),
      consumption_frequency = normalize_consumption(.data$consumption_raw)
    )

  column_map <- dplyr::bind_rows(
    age_meta |> dplyr::mutate(variable = "age_group"),
    gender_meta |> dplyr::mutate(variable = "gender"),
    dietary_meta |> dplyr::mutate(variable = "dietary_preference"),
    education_meta |> dplyr::mutate(variable = "education"),
    consumption_meta |> dplyr::mutate(variable = "consumption_frequency")
  ) |>
    dplyr::select(
      category, variable, question_type, modality, raw_col
    )

  city_site_candidates <- if (nrow(city_site_meta) == 0L) {
    tibble::tibble(
      category = category,
      raw_col = NA_character_,
      question_type = NA_character_,
      modality = NA_character_
    )
  } else {
    city_site_meta |>
      dplyr::select(
        category, raw_col, question_type, modality
      )
  }

  list(
    data = data,
    column_map = column_map,
    city_site_candidates = city_site_candidates
  )
}

message("Reading demographic fields from 14 public CSV exports")
parsed <- purrr::map(INPUT_FILES, read_demographics_one)
demographics <- purrr::map_dfr(parsed, "data") |>
  dplyr::arrange(.data$category, .data$Respondent)
demographic_column_map <- purrr::map_dfr(parsed, "column_map") |>
  dplyr::arrange(.data$category, .data$variable)
city_site_candidates <- purrr::map_dfr(parsed, "city_site_candidates") |>
  dplyr::filter(!is.na(.data$raw_col)) |>
  dplyr::arrange(.data$category, .data$raw_col)

raw_normalized_pairs <- list(
  age_group = c("age_raw", "age_group"),
  gender = c("gender_raw", "gender"),
  dietary_preference = c("dietary_raw", "dietary_preference"),
  education = c("education_raw", "education"),
  consumption_frequency = c("consumption_raw", "consumption_frequency")
)

normalization_audit <- purrr::imap_dfr(
  raw_normalized_pairs,
  function(cols, variable) {
    raw_col <- cols[[1]]
    normalized_col <- cols[[2]]
    demographics |>
      dplyr::transmute(
        category = .data$category,
        variable = variable,
        raw_value = clean_response(.data[[raw_col]]),
        normalized_value = .data[[normalized_col]],
        expected_missing = is.na(.data$raw_value),
        unmapped_nonmissing = !.data$expected_missing &
          is.na(.data$normalized_value)
      ) |>
      dplyr::count(
        .data$category, .data$variable, .data$raw_value,
        .data$normalized_value, .data$expected_missing,
        .data$unmapped_nonmissing,
        name = "n"
      )
  }
) |>
  dplyr::arrange(
    .data$category, .data$variable,
    dplyr::desc(.data$n), .data$raw_value
  )

availability_audit <- tibble::tibble(
  item = c(
    "Age",
    "Gender",
    "Dietary preference",
    "Education",
    "Category-specific consumption frequency",
    "City/site/location identifier"
  ),
  available_in_all_14_exports = c(
    all(table(demographic_column_map$variable)["age_group"] == 14L),
    all(table(demographic_column_map$variable)["gender"] == 14L),
    all(table(demographic_column_map$variable)["dietary_preference"] == 14L),
    all(table(demographic_column_map$variable)["education"] == 14L),
    all(table(demographic_column_map$variable)["consumption_frequency"] == 14L),
    nrow(city_site_candidates) >= 14L &&
      dplyr::n_distinct(city_site_candidates$category) == 14L
  ),
  number_of_exports_with_field = c(
    sum(demographic_column_map$variable == "age_group"),
    sum(demographic_column_map$variable == "gender"),
    sum(demographic_column_map$variable == "dietary_preference"),
    sum(demographic_column_map$variable == "education"),
    sum(demographic_column_map$variable == "consumption_frequency"),
    dplyr::n_distinct(city_site_candidates$category)
  ),
  interpretation = c(
    rep("Available for sample-description audit", 5L),
    if (nrow(city_site_candidates) == 0L) {
      paste(
        "No demographic city/site field was identified;",
        "metropolitan-area heterogeneity cannot be estimated from these exports"
      )
    } else {
      "At least one possible city/site field was detected and requires manual review"
    }
  )
)

summarize_characteristic <- function(data, variable, group_vars = character()) {
  level_order <- c(
    CHARACTERISTIC_LEVELS[[variable]],
    "Missing/No response"
  )

  working <- data |>
    dplyr::mutate(
      characteristic_level = dplyr::if_else(
        is.na(.data[[variable]]),
        "Missing/No response",
        as.character(.data[[variable]])
      )
    )

  if (length(group_vars) == 0L) {
    counts_observed <- working |>
      dplyr::count(.data$characteristic_level, name = "n")
    counts <- tidyr::crossing(
      characteristic_level = level_order
    ) |>
      dplyr::left_join(counts_observed, by = "characteristic_level") |>
      dplyr::mutate(
        n = tidyr::replace_na(.data$n, 0L),
        n_total = nrow(working),
        n_nonmissing = sum(!is.na(working[[variable]]))
      )
  } else {
    denominators <- working |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        n_total = dplyr::n(),
        n_nonmissing = sum(!is.na(.data[[variable]])),
        .groups = "drop"
      )
    counts <- working |>
      dplyr::count(
        dplyr::across(dplyr::all_of(group_vars)),
        .data$characteristic_level,
        name = "n"
      ) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      tidyr::complete(
        characteristic_level = level_order,
        fill = list(n = 0L)
      ) |>
      dplyr::ungroup() |>
      dplyr::left_join(denominators, by = group_vars)
  }

  counts |>
    dplyr::mutate(
      variable = variable,
      variable_label = unname(CHARACTERISTIC_LABELS[variable]),
      level_order = match(.data$characteristic_level, level_order),
      percent_total = 100 * .data$n / .data$n_total,
      percent_nonmissing = dplyr::if_else(
        .data$characteristic_level == "Missing/No response" |
          .data$n_nonmissing == 0L,
        NA_real_,
        100 * .data$n / .data$n_nonmissing
      ),
      n_percent_total = format_n_pct(.data$n, .data$n_total),
      n_percent_nonmissing = dplyr::if_else(
        .data$characteristic_level == "Missing/No response",
        NA_character_,
        format_n_pct(.data$n, .data$n_nonmissing)
      )
    )
}

overall_characteristics_long <- purrr::map_dfr(
  CHARACTERISTIC_VARIABLES,
  function(variable) summarize_characteristic(demographics, variable)
) |>
  dplyr::select(
    variable, variable_label,
    characteristic_level,
    level_order, n, n_total, n_nonmissing,
    percent_total, percent_nonmissing,
    n_percent_total, n_percent_nonmissing
  ) |>
  dplyr::arrange(.data$variable, .data$level_order)

category_characteristics_long <- purrr::map_dfr(
  CHARACTERISTIC_VARIABLES,
  function(variable) {
    summarize_characteristic(
      demographics,
      variable,
      group_vars = c("category", "category_label")
    )
  }
) |>
  dplyr::select(
    category, category_label,
    variable, variable_label,
    characteristic_level,
    level_order, n, n_total, n_nonmissing,
    percent_total, percent_nonmissing,
    n_percent_total, n_percent_nonmissing
  ) |>
  dplyr::arrange(
    .data$category, .data$variable, .data$level_order
  )

sample_completeness <- purrr::map_dfr(
  CHARACTERISTIC_VARIABLES,
  function(variable) {
    demographics |>
      dplyr::group_by(.data$category, .data$category_label) |>
      dplyr::summarise(
        variable = variable,
        n_total = dplyr::n(),
        n_nonmissing = sum(!is.na(.data[[variable]])),
        n_missing_or_no_response = sum(is.na(.data[[variable]])),
        missing_or_no_response_rate =
          .data$n_missing_or_no_response / .data$n_total,
        .groups = "drop"
      )
  }
) |>
  dplyr::arrange(.data$category, .data$variable)

make_compact_metrics <- function(data, group_vars = character()) {
  working <- data |>
    dplyr::mutate(
      age_18_to_35 = .data$age_group %in% c("18-25", "26-35"),
      female = .data$gender == "Female",
      male = .data$gender == "Male",
      flexitarian = .data$dietary_preference == "Flexitarian",
      bachelor_or_higher = .data$education %in% c(
        "Bachelor's Degree", "Master's Degree", "Ph.D. or higher"
      ),
      weekly_or_more_consumption = .data$consumption_frequency %in% c(
        "Once a week", "2-3 times a week", "Everyday"
      ),
      any_core_characteristic_missing =
        is.na(.data$age_group) |
        is.na(.data$gender) |
        is.na(.data$dietary_preference) |
        is.na(.data$education) |
        is.na(.data$consumption_frequency)
    )

  if (length(group_vars) == 0L) {
    working <- working |> dplyr::mutate(sample = "Overall")
    group_vars <- "sample"
  }

  working |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      N = dplyr::n(),
      age_18_to_35_n = sum(.data$age_18_to_35, na.rm = TRUE),
      female_n = sum(.data$female, na.rm = TRUE),
      male_n = sum(.data$male, na.rm = TRUE),
      flexitarian_n = sum(.data$flexitarian, na.rm = TRUE),
      bachelor_or_higher_n = sum(.data$bachelor_or_higher, na.rm = TRUE),
      weekly_or_more_consumption_n =
        sum(.data$weekly_or_more_consumption, na.rm = TRUE),
      any_core_characteristic_missing_n =
        sum(.data$any_core_characteristic_missing),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      age_18_to_35_pct = 100 * .data$age_18_to_35_n / .data$N,
      female_pct = 100 * .data$female_n / .data$N,
      male_pct = 100 * .data$male_n / .data$N,
      flexitarian_pct = 100 * .data$flexitarian_n / .data$N,
      bachelor_or_higher_pct =
        100 * .data$bachelor_or_higher_n / .data$N,
      weekly_or_more_consumption_pct =
        100 * .data$weekly_or_more_consumption_n / .data$N,
      any_core_characteristic_missing_pct =
        100 * .data$any_core_characteristic_missing_n / .data$N
    )
}

overall_compact_numeric <- make_compact_metrics(demographics)
category_compact_numeric <- make_compact_metrics(
  demographics,
  group_vars = c("category", "category_label")
) |>
  dplyr::arrange(.data$category)

format_compact_table <- function(x) {
  x |>
    dplyr::transmute(
      dplyr::across(
        dplyr::any_of(c("sample", "category", "category_label", "N"))
      ),
      `Age 18-35, n (%)` = format_n_pct(.data$age_18_to_35_n, .data$N),
      `Female, n (%)` = format_n_pct(.data$female_n, .data$N),
      `Male, n (%)` = format_n_pct(.data$male_n, .data$N),
      `Flexitarian, n (%)` =
        format_n_pct(.data$flexitarian_n, .data$N),
      `Bachelor's degree or higher, n (%)` =
        format_n_pct(.data$bachelor_or_higher_n, .data$N),
      `Category product weekly or more, n (%)` =
        format_n_pct(.data$weekly_or_more_consumption_n, .data$N),
      `Any core characteristic missing/no response, n (%)` =
        format_n_pct(.data$any_core_characteristic_missing_n, .data$N)
    )
}

overall_compact_formatted <- format_compact_table(overall_compact_numeric)
category_compact_formatted <- format_compact_table(category_compact_numeric)

metric_range_by_category <- category_compact_numeric |>
  dplyr::select(
    category, category_label,
    dplyr::ends_with("_pct")
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::ends_with("_pct"),
    names_to = "metric",
    values_to = "percent"
  ) |>
  dplyr::group_by(.data$metric) |>
  dplyr::mutate(
    minimum_percent = min(.data$percent, na.rm = TRUE),
    maximum_percent = max(.data$percent, na.rm = TRUE)
  ) |>
  dplyr::summarise(
    minimum_percent = dplyr::first(.data$minimum_percent),
    minimum_category = paste(
      .data$category_label[
        abs(.data$percent - dplyr::first(.data$minimum_percent)) < 1e-12
      ],
      collapse = "; "
    ),
    maximum_percent = dplyr::first(.data$maximum_percent),
    maximum_category = paste(
      .data$category_label[
        abs(.data$percent - dplyr::first(.data$maximum_percent)) < 1e-12
      ],
      collapse = "; "
    ),
    .groups = "drop"
  )

message("Auditing Breaded Chicken Filets source-leader missingness")
source_missing_status <- readr::read_csv(
  file.path(
    FQP_REVISION_DIR,
    "audit",
    "23_source_leader_missingness_status.csv"
  ),
  show_col_types = FALSE
)
source_missing_summary_existing <- readr::read_csv(
  file.path(
    FQP_REVISION_DIR,
    "audit",
    "24_source_leader_missingness_summary.csv"
  ),
  show_col_types = FALSE
)

bcf_status <- source_missing_status |>
  dplyr::filter(.data$category == "Breaded_Chicken_Filets") |>
  dplyr::mutate(
    source_observed = as_logical_safe(.data$source_observed),
    Randomizer_status = suppressWarnings(as.integer(.data$Randomizer))
  ) |>
  dplyr::select(
    category, category_label,
    source_leader_code, source_leader_display,
    source_panel_id, source_panel_size,
    respondent_uid, Respondent,
    Randomizer_status, source_observed,
    source_leader_score, animal_overall_score
  )

bcf_demographics <- demographics |>
  dplyr::filter(.data$category == "Breaded_Chicken_Filets")

bcf_analysis <- bcf_status |>
  dplyr::left_join(
    bcf_demographics |>
      dplyr::select(
        respondent_uid, Randomizer_demographic = Randomizer,
        age_group, gender,
        dietary_preference, education,
        consumption_frequency
      ),
    by = "respondent_uid"
  ) |>
  dplyr::mutate(
    Randomizer = .data$Randomizer_demographic,
    randomizer_matches_existing = dplyr::coalesce(
      .data$Randomizer_status == .data$Randomizer_demographic,
      is.na(.data$Randomizer_status) & is.na(.data$Randomizer_demographic)
    ),
    source_group = dplyr::if_else(
      .data$source_observed,
      "Observed",
      "Missing"
    ),
    source_group = factor(
      .data$source_group,
      levels = c("Observed", "Missing")
    ),
    randomizer_factor = dplyr::if_else(
      is.na(.data$Randomizer),
      NA_character_,
      as.character(.data$Randomizer)
    ),
    age_ordinal = match(.data$age_group, AGE_LEVELS),
    consumption_ordinal = match(
      .data$consumption_frequency,
      CONSUMPTION_LEVELS
    ) - 1L
  ) |>
  dplyr::arrange(.data$source_group, .data$Respondent)

bcf_group_counts <- bcf_analysis |>
  dplyr::count(.data$source_group, name = "n") |>
  dplyr::mutate(
    N = sum(.data$n),
    percent = 100 * .data$n / .data$N,
    n_percent = format_n_pct(.data$n, .data$N)
  )

bcf_observed_n <- sum(bcf_analysis$source_observed)
bcf_missing_n <- sum(!bcf_analysis$source_observed)
bcf_total_n <- nrow(bcf_analysis)

BCF_BALANCE_VARIABLES <- c(
  "age_group", "gender", "dietary_preference", "education",
  "consumption_frequency", "randomizer_factor"
)
BCF_BALANCE_LABELS <- c(
  "age_group" = "Age",
  "gender" = "Gender",
  "dietary_preference" = "Dietary preference",
  "education" = "Education",
  "consumption_frequency" = "Category-specific consumption frequency",
  "randomizer_factor" = "Randomizer"
)
BCF_BALANCE_LEVELS <- c(
  CHARACTERISTIC_LEVELS,
  list(randomizer_factor = as.character(1:6))
)

bcf_distribution_long <- purrr::map_dfr(
  BCF_BALANCE_VARIABLES,
  function(variable) {
    level_order <- c(
      BCF_BALANCE_LEVELS[[variable]],
      "Missing/No response"
    )
    bcf_analysis |>
      dplyr::mutate(
        level = dplyr::if_else(
          is.na(.data[[variable]]),
          "Missing/No response",
          as.character(.data[[variable]])
        )
      ) |>
      dplyr::count(.data$source_group, .data$level, name = "n") |>
      tidyr::complete(
        source_group = factor(
          c("Observed", "Missing"),
          levels = c("Observed", "Missing")
        ),
        level = level_order,
        fill = list(n = 0L)
      ) |>
      dplyr::group_by(.data$source_group) |>
      dplyr::mutate(
        group_n = sum(.data$n),
        percent = 100 * .data$n / .data$group_n,
        n_percent = format_n_pct(.data$n, .data$group_n)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        variable = variable,
        variable_label = unname(BCF_BALANCE_LABELS[variable]),
        level_order = match(.data$level, level_order)
      )
  }
) |>
  dplyr::select(
    variable, variable_label,
    level, level_order,
    source_group, n, group_n,
    percent, n_percent
  ) |>
  dplyr::arrange(
    .data$variable, .data$level_order, .data$source_group
  )

bcf_level_balance <- bcf_distribution_long |>
  dplyr::select(
    variable, variable_label,
    level, level_order,
    source_group, n, group_n, percent
  ) |>
  tidyr::pivot_wider(
    names_from = source_group,
    values_from = c(n, group_n, percent),
    names_glue = "{.value}_{source_group}"
  ) |>
  dplyr::mutate(
    proportion_Observed = .data$percent_Observed / 100,
    proportion_Missing = .data$percent_Missing / 100,
    difference_percentage_points_missing_minus_observed =
      .data$percent_Missing - .data$percent_Observed,
    standardized_difference_missing_minus_observed = purrr::map2_dbl(
      .data$proportion_Missing,
      .data$proportion_Observed,
      safe_smd_binary
    )
  ) |>
  dplyr::select(
    variable, variable_label,
    level, level_order,
    n_observed = n_Observed,
    percent_observed = percent_Observed,
    n_missing = n_Missing,
    percent_missing = percent_Missing,
    difference_percentage_points_missing_minus_observed,
    standardized_difference_missing_minus_observed
  ) |>
  dplyr::arrange(.data$variable, .data$level_order)

categorical_test_one <- function(variable, variable_index) {
  values <- bcf_analysis[[variable]]
  values <- dplyr::if_else(
    is.na(values),
    "Missing/No response",
    as.character(values)
  )
  table_input <- table(
    source_group = bcf_analysis$source_group,
    level = values,
    useNA = "no"
  )
  table_input <- table_input[, colSums(table_input) > 0L, drop = FALSE]

  chi_square <- suppressWarnings(
    stats::chisq.test(table_input, correct = FALSE)
  )
  chi_square_statistic <- unname(chi_square$statistic)
  chi_square_p <- unname(chi_square$p.value)
  minimum_expected_count <- min(chi_square$expected)
  cramer_v <- sqrt(
    chi_square_statistic /
      (sum(table_input) * min(nrow(table_input) - 1L, ncol(table_input) - 1L))
  )

  set.seed(FQP_ADDITIONAL_AUDIT_SEED + variable_index)
  if (ncol(table_input) == 2L) {
    fisher_p <- safe_test_p(stats::fisher.test(table_input)$p.value)
    fisher_method <- "Fisher exact test"
  } else {
    fisher_p <- safe_test_p(
      stats::fisher.test(
        table_input,
        simulate.p.value = TRUE,
        B = FQP_FISHER_MONTE_CARLO_B
      )$p.value
    )
    fisher_method <- paste0(
      "Fisher-Freeman-Halton Monte Carlo test (B=",
      format(FQP_FISHER_MONTE_CARLO_B, scientific = FALSE),
      ")"
    )
  }

  variable_balance <- bcf_level_balance |>
    dplyr::filter(.data$variable == .env$variable)

  tibble::tibble(
    variable = variable,
    variable_label = unname(BCF_BALANCE_LABELS[variable]),
    n_analyzed = sum(table_input),
    number_of_levels = ncol(table_input),
    exact_or_monte_carlo_test = fisher_method,
    exact_or_monte_carlo_p_value = fisher_p,
    pearson_chi_square = chi_square_statistic,
    pearson_chi_square_df = unname(chi_square$parameter),
    pearson_chi_square_p_value = chi_square_p,
    minimum_expected_count = minimum_expected_count,
    cramer_v = cramer_v,
    maximum_absolute_level_standardized_difference = max(
      abs(variable_balance$standardized_difference_missing_minus_observed),
      na.rm = TRUE
    )
  )
}

bcf_global_tests <- purrr::map2_dfr(
  BCF_BALANCE_VARIABLES,
  seq_along(BCF_BALANCE_VARIABLES),
  categorical_test_one
)

bcf_randomizer_cross_tab <- bcf_distribution_long |>
  dplyr::filter(
    .data$variable == "randomizer_factor",
    .data$level != "Missing/No response"
  ) |>
  dplyr::select(
    Randomizer = level,
    source_group, n, percent
  ) |>
  tidyr::pivot_wider(
    names_from = source_group,
    values_from = c(n, percent),
    names_glue = "{.value}_{source_group}"
  ) |>
  dplyr::arrange(as.integer(.data$Randomizer))

numeric_comparison_one <- function(variable, variable_label) {
  x_missing <- bcf_analysis[[variable]][
    bcf_analysis$source_group == "Missing"
  ]
  x_observed <- bcf_analysis[[variable]][
    bcf_analysis$source_group == "Observed"
  ]
  x_missing <- x_missing[!is.na(x_missing)]
  x_observed <- x_observed[!is.na(x_observed)]

  t_test <- tryCatch(
    stats::t.test(x_missing, x_observed, conf.level = 0.95),
    error = function(e) NULL
  )
  wilcoxon_p <- safe_test_p(
    suppressWarnings(
      stats::wilcox.test(
        x_missing,
        x_observed,
        exact = FALSE,
        correct = FALSE
      )$p.value
    )
  )

  tibble::tibble(
    variable = variable,
    variable_label = variable_label,
    n_missing = length(x_missing),
    mean_missing = if (length(x_missing) > 0L) mean(x_missing) else NA_real_,
    sd_missing = if (length(x_missing) > 1L) stats::sd(x_missing) else NA_real_,
    median_missing = if (length(x_missing) > 0L) stats::median(x_missing) else NA_real_,
    n_observed = length(x_observed),
    mean_observed = if (length(x_observed) > 0L) mean(x_observed) else NA_real_,
    sd_observed = if (length(x_observed) > 1L) stats::sd(x_observed) else NA_real_,
    median_observed = if (length(x_observed) > 0L) stats::median(x_observed) else NA_real_,
    mean_difference_missing_minus_observed =
      if (length(x_missing) > 0L && length(x_observed) > 0L) {
        mean(x_missing) - mean(x_observed)
      } else {
        NA_real_
      },
    standardized_mean_difference_missing_minus_observed =
      safe_smd_numeric(x_missing, x_observed),
    welch_t_p_value = if (is.null(t_test)) NA_real_ else t_test$p.value,
    welch_difference_ci95_low =
      if (is.null(t_test)) NA_real_ else unname(t_test$conf.int[[1]]),
    welch_difference_ci95_high =
      if (is.null(t_test)) NA_real_ else unname(t_test$conf.int[[2]]),
    wilcoxon_rank_sum_p_value = wilcoxon_p
  )
}

bcf_numeric_comparisons <- dplyr::bind_rows(
  numeric_comparison_one(
    "age_ordinal",
    "Age category ordinal score (1=18-25; 5=>55)"
  ),
  numeric_comparison_one(
    "consumption_ordinal",
    "Consumption-frequency ordinal score (0=never/rarely; 7=everyday)"
  ),
  numeric_comparison_one(
    "animal_overall_score",
    "Animal-benchmark Overall Liking (1-7)"
  )
)

summarize_ni_differences <- function(
    differences,
    scenario,
    missing_rating_assumption,
    delta = 0.5,
    alpha = 0.05) {
  differences <- differences[!is.na(differences)]
  n <- length(differences)
  mean_diff <- mean(differences)
  sd_diff <- stats::sd(differences)
  se_diff <- sd_diff / sqrt(n)
  critical_value <- stats::qt(1 - alpha, df = n - 1L)
  lower_one_sided_95 <- mean_diff - critical_value * se_diff
  t_ni <- (mean_diff + delta) / se_diff
  p_ni <- stats::pt(t_ni, df = n - 1L, lower.tail = FALSE)
  tibble::tibble(
    scenario = scenario,
    missing_rating_assumption = missing_rating_assumption,
    n = n,
    mean_diff = mean_diff,
    sd_diff = sd_diff,
    se_diff = se_diff,
    lower_one_sided_95 = lower_one_sided_95,
    minimum_data_supported_margin = max(0, -lower_one_sided_95),
    delta = delta,
    p_ni = p_ni,
    noninferior_nominal = lower_one_sided_95 > -delta
  )
}

bcf_observed_differences <- bcf_analysis |>
  dplyr::filter(.data$source_observed) |>
  dplyr::transmute(
    difference = .data$source_leader_score - .data$animal_overall_score
  ) |>
  dplyr::pull("difference")

bcf_missing_animal_scores <- bcf_analysis |>
  dplyr::filter(!.data$source_observed) |>
  dplyr::pull("animal_overall_score")

bcf_bounded_ni_sensitivity <- dplyr::bind_rows(
  summarize_ni_differences(
    bcf_observed_differences,
    "Complete-case observed ratings",
    "No imputation; 78 respondents with the leader block"
  ),
  summarize_ni_differences(
    c(bcf_observed_differences, 1 - bcf_missing_animal_scores),
    "Lower rating bound",
    "Assign plant leader score 1 to all 13 unobserved ratings"
  ),
  summarize_ni_differences(
    c(bcf_observed_differences, rep(0, length(bcf_missing_animal_scores))),
    "Benchmark-equal assumption",
    "Assign plant leader score equal to each respondent's animal score"
  ),
  summarize_ni_differences(
    c(bcf_observed_differences, 7 - bcf_missing_animal_scores),
    "Upper rating bound",
    "Assign plant leader score 7 to all 13 unobserved ratings"
  )
)

bcf_existing_summary_row <- source_missing_summary_existing |>
  dplyr::filter(.data$category == "Breaded_Chicken_Filets")

write_audit_csv(demographic_column_map, "01_demographic_column_map.csv")
write_audit_csv(normalization_audit, "02_value_normalization_audit.csv")
write_audit_csv(availability_audit, "03_variable_availability.csv")
write_audit_csv(city_site_candidates, "04_city_site_candidate_columns.csv")
write_audit_csv(sample_completeness, "05_sample_completeness_by_category.csv")
write_audit_csv(overall_characteristics_long, "06_sample_characteristics_overall_long.csv")
write_audit_csv(category_characteristics_long, "07_sample_characteristics_by_category_long.csv")
write_audit_csv(overall_compact_numeric, "08_sample_characteristics_overall_compact_numeric.csv")
write_audit_csv(category_compact_numeric, "09_sample_characteristics_by_category_compact_numeric.csv")
write_audit_csv(metric_range_by_category, "10_category_range_summary.csv")
write_audit_csv(bcf_group_counts, "11_bcf_source_leader_missingness_group_counts.csv")
write_audit_csv(bcf_distribution_long, "12_bcf_missingness_distributions.csv")
write_audit_csv(bcf_level_balance, "13_bcf_missingness_level_balance.csv")
write_audit_csv(bcf_global_tests, "14_bcf_missingness_global_tests.csv")
write_audit_csv(bcf_randomizer_cross_tab, "15_bcf_randomizer_cross_tab.csv")
write_audit_csv(bcf_numeric_comparisons, "16_bcf_numeric_comparisons.csv")
write_audit_csv(bcf_analysis, "17_bcf_joined_analysis_data.csv")
write_audit_csv(bcf_bounded_ni_sensitivity, "17a_bcf_overall_liking_bounded_sensitivity.csv")

write_table_csv(
  overall_characteristics_long |>
    dplyr::select(
      variable_label,
      Characteristic = characteristic_level,
      `n (%) of all records` = n_percent_total,
      `n (%) among nonmissing` = n_percent_nonmissing
    ),
  "Table_sample_characteristics_overall.csv"
)
write_table_csv(
  category_compact_formatted |>
    dplyr::select(-category),
  "Table_sample_characteristics_by_category_compact.csv"
)
bcf_balance_manuscript <- bcf_level_balance |>
  dplyr::filter(
    .data$level != "Missing/No response",
    .data$n_observed + .data$n_missing > 0L
  ) |>
  dplyr::transmute(
    Characteristic = .data$variable_label,
    Level = .data$level,
    `Leader observed, n (%)` =
      format_n_pct(.data$n_observed, bcf_observed_n),
    `Leader missing, n (%)` =
      format_n_pct(.data$n_missing, bcf_missing_n),
    `Difference, percentage points` = round(
      .data$difference_percentage_points_missing_minus_observed,
      1
    ),
    `Standardized difference` = round(
      .data$standardized_difference_missing_minus_observed,
      3
    )
  )
write_table_csv(
  bcf_balance_manuscript,
  "Table_bcf_source_leader_missingness_balance.csv"
)

bcf_global_tests_manuscript <- bcf_global_tests |>
  dplyr::transmute(
    Characteristic = .data$variable_label,
    `Global test` = .data$exact_or_monte_carlo_test,
    `Global p-value` = .data$exact_or_monte_carlo_p_value,
    `Cramer's V` = .data$cramer_v,
    `Maximum absolute level standardized difference` =
      .data$maximum_absolute_level_standardized_difference
  )
write_table_csv(
  bcf_global_tests_manuscript,
  "Table_bcf_missingness_global_tests.csv"
)

bcf_bounded_sensitivity_manuscript <- bcf_bounded_ni_sensitivity |>
  dplyr::transmute(
    Scenario = .data$scenario,
    `Missing-rating assumption` = .data$missing_rating_assumption,
    n = .data$n,
    `Mean difference` = round(.data$mean_diff, 3),
    `One-sided 95% lower bound` = round(.data$lower_one_sided_95, 3),
    `NI p-value at 0.5 margin` = round(.data$p_ni, 3),
    `Meets nominal NI` = .data$noninferior_nominal
  )
write_table_csv(
  bcf_bounded_sensitivity_manuscript,
  "Table_bcf_overall_liking_bounded_sensitivity.csv"
)

saveRDS(
  demographics,
  file.path(FQP_ADDITIONAL_RDS_DIR, "demographics_clean.rds")
)
saveRDS(
  bcf_analysis,
  file.path(FQP_ADDITIONAL_RDS_DIR, "bcf_missingness_analysis.rds")
)

randomizer_p <- bcf_global_tests$exact_or_monte_carlo_p_value[
  bcf_global_tests$variable == "randomizer_factor"
]
animal_row <- bcf_numeric_comparisons |>
  dplyr::filter(.data$variable == "animal_overall_score")
upper_bound_row <- bcf_bounded_ni_sensitivity |>
  dplyr::filter(.data$scenario == "Upper rating bound")

respondent_registry <- readr::read_csv(
  file.path(FQP_REVISION_DIR, "audit", "03_respondent_registry.csv"),
  show_col_types = FALSE
)
locked_audit_validation <- readr::read_csv(
  file.path(FQP_REVISION_DIR, "audit", "25_locked_audit_validation.csv"),
  show_col_types = FALSE
)
panel_summary_consistency <- readr::read_csv(
  file.path(FQP_REVISION_DIR, "audit", "12b_panel_summary_consistency.csv"),
  show_col_types = FALSE
)

count_check <- demographics |>
  dplyr::count(.data$category, name = "new_n") |>
  dplyr::full_join(
    respondent_registry |>
      dplyr::count(.data$category, name = "registry_n"),
    by = "category"
  ) |>
  dplyr::mutate(matches = .data$new_n == .data$registry_n)

bcf_existing <- source_missing_summary_existing |>
  dplyr::filter(.data$category == "Breaded_Chicken_Filets")

validation <- tibble::tibble(
  check = c(
    "All 14 required raw CSV files exist",
    "All required v1.2 audit outputs exist",
    "Demographic audit contains 2,682 respondent records",
    "Demographic audit contains all 14 expected categories",
    "Respondent UID is unique within the pooled audit",
    "Category counts match the v1.2 respondent registry",
    "Exactly five required demographic fields were found per category",
    "All nonmissing demographic values mapped to locked levels",
    "The preceding locked data audit passed",
    "The corrected panel summary passed its consistency audit",
    "Breaded Chicken Filets source panel contains 91 respondents",
    "Breaded Chicken Filets leader-observed group contains 78 respondents",
    "Breaded Chicken Filets leader-missing group contains 13 respondents",
    "Breaded Chicken Filets demographic join is complete",
    "Breaded Chicken Filets Randomizer agrees with the existing audit",
    "Existing missingness summary agrees with 13 of 91 missing",
    "City/site availability is reported rather than imputed"
  ),
  required = c(rep(TRUE, 16L), FALSE),
  passed = c(
    all(file.exists(INPUT_FILES)),
    all(file.exists(REQUIRED_EXISTING_OUTPUTS)),
    nrow(demographics) == 2682L,
    setequal(unique(demographics$category), EXPECTED_CATEGORIES),
    dplyr::n_distinct(demographics$respondent_uid) == nrow(demographics),
    nrow(count_check) == 14L && all(count_check$matches),
    nrow(demographic_column_map) == 14L * 5L &&
      all(table(demographic_column_map$variable) == 14L),
    !any(normalization_audit$unmapped_nonmissing),
    all(locked_audit_validation$passed),
    nrow(panel_summary_consistency) > 0L &&
      all(panel_summary_consistency$counts_match),
    nrow(bcf_analysis) == 91L,
    sum(bcf_analysis$source_observed) == 78L,
    sum(!bcf_analysis$source_observed) == 13L,
    !any(is.na(bcf_analysis$age_group)) &&
      !any(is.na(bcf_analysis$gender)) &&
      !any(is.na(bcf_analysis$dietary_preference)) &&
      !any(is.na(bcf_analysis$education)) &&
      !any(is.na(bcf_analysis$consumption_frequency)),
    all(bcf_analysis$randomizer_matches_existing),
    nrow(bcf_existing) == 1L &&
      bcf_existing$source_block_missing_n[[1]] == 13L &&
      bcf_existing$source_canonical_panel_n[[1]] == 91L,
    TRUE
  ),
  observed = c(
    paste(sum(file.exists(INPUT_FILES)), "of", length(INPUT_FILES)),
    paste(
      sum(file.exists(REQUIRED_EXISTING_OUTPUTS)),
      "of", length(REQUIRED_EXISTING_OUTPUTS)
    ),
    as.character(nrow(demographics)),
    paste(sort(unique(demographics$category)), collapse = ";"),
    as.character(dplyr::n_distinct(demographics$respondent_uid)),
    paste0(sum(count_check$matches), "/", nrow(count_check)),
    paste0(nrow(demographic_column_map), " mapped fields"),
    as.character(sum(normalization_audit$unmapped_nonmissing)),
    paste0(sum(locked_audit_validation$passed), "/", nrow(locked_audit_validation)),
    paste0(
      sum(panel_summary_consistency$counts_match),
      "/", nrow(panel_summary_consistency)
    ),
    as.character(nrow(bcf_analysis)),
    as.character(sum(bcf_analysis$source_observed)),
    as.character(sum(!bcf_analysis$source_observed)),
    as.character(sum(stats::complete.cases(
      bcf_analysis[, c(
        "age_group", "gender", "dietary_preference",
        "education", "consumption_frequency"
      )]
    ))),
    as.character(sum(bcf_analysis$randomizer_matches_existing)),
    paste0(
      bcf_existing$source_block_missing_n[[1]], "/",
      bcf_existing$source_canonical_panel_n[[1]]
    ),
    if (nrow(city_site_candidates) == 0L) {
      "No city/site field found"
    } else {
      paste0(nrow(city_site_candidates), " candidate fields found")
    }
  ),
  expected = c(
    "14 of 14",
    paste(length(REQUIRED_EXISTING_OUTPUTS), "of", length(REQUIRED_EXISTING_OUTPUTS)),
    "2682",
    paste(sort(EXPECTED_CATEGORIES), collapse = ";"),
    "2682",
    "14/14",
    "70 mapped fields",
    "0",
    "all",
    "all",
    "91",
    "78",
    "13",
    "91 complete joins",
    "91",
    "13/91",
    "Informational only"
  )
)
expected_max_smd <- bcf_level_balance |>
  dplyr::group_by(.data$variable) |>
  dplyr::summarise(
    expected_maximum = max(
      abs(.data$standardized_difference_missing_minus_observed),
      na.rm = TRUE
    ),
    .groups = "drop"
  )
max_smd_check <- bcf_global_tests |>
  dplyr::select(
    variable,
    reported_maximum = maximum_absolute_level_standardized_difference
  ) |>
  dplyr::left_join(expected_max_smd, by = "variable") |>
  dplyr::mutate(matches = abs(.data$reported_maximum - .data$expected_maximum) < 1e-12)

age_2635_table_value <- bcf_balance_manuscript |>
  dplyr::filter(
    .data$Characteristic == "Age",
    .data$Level == "26-35"
  ) |>
  dplyr::pull(`Leader observed, n (%)`)

reporting_validation <- tibble::tibble(
  check = c(
    "BCF manuscript balance table preserves row-specific counts",
    "Variable-specific maximum standardized differences are correct",
    "Global p-value lookup returns the Randomizer row",
    "Numeric row lookup returns exactly one animal-benchmark row",
    "Upper-bound missing-rating scenario still does not meet 0.5-point NI",
    "Pooled compact sample counts match locked totals"
  ),
  required = TRUE,
  passed = c(
    length(age_2635_table_value) == 1L &&
      identical(age_2635_table_value[[1]], "27 (34.6%)"),
    nrow(max_smd_check) == length(BCF_BALANCE_VARIABLES) &&
      all(max_smd_check$matches),
    length(randomizer_p) == 1L &&
      abs(
        randomizer_p - bcf_global_tests$exact_or_monte_carlo_p_value[
          bcf_global_tests$variable == "randomizer_factor"
        ]
      ) < 1e-12 && randomizer_p < 0.05,
    nrow(animal_row) == 1L && animal_row$variable[[1]] == "animal_overall_score",
    nrow(upper_bound_row) == 1L &&
      !upper_bound_row$noninferior_nominal[[1]] &&
      upper_bound_row$lower_one_sided_95[[1]] <= -0.5,
    nrow(overall_compact_numeric) == 1L &&
      overall_compact_numeric$N[[1]] == 2682L &&
      overall_compact_numeric$age_18_to_35_n[[1]] == 1353L &&
      overall_compact_numeric$female_n[[1]] == 1318L &&
      overall_compact_numeric$flexitarian_n[[1]] == 662L
  ),
  observed = c(
    if (length(age_2635_table_value) == 1L) age_2635_table_value[[1]] else "missing",
    paste0(sum(max_smd_check$matches), "/", nrow(max_smd_check)),
    format_p(randomizer_p),
    as.character(nrow(animal_row)),
    paste0(
      "lower=", sprintf("%.3f", upper_bound_row$lower_one_sided_95),
      "; p=", format_p(upper_bound_row$p_ni)
    ),
    paste0(
      "N=", overall_compact_numeric$N[[1]],
      "; age18-35=", overall_compact_numeric$age_18_to_35_n[[1]],
      "; female=", overall_compact_numeric$female_n[[1]],
      "; flexitarian=", overall_compact_numeric$flexitarian_n[[1]]
    )
  ),
  expected = c(
    "27 (34.6%)",
    paste0(length(BCF_BALANCE_VARIABLES), "/", length(BCF_BALANCE_VARIABLES)),
    "<0.05 and exact row match",
    "1",
    "lower <= -0.5 and NI=FALSE",
    "N=2682; age18-35=1353; female=1318; flexitarian=662"
  )
)
validation <- dplyr::bind_rows(validation, reporting_validation)
write_audit_csv(validation, "99_additional_audit_validation.csv")

required_failures <- validation |>
  dplyr::filter(.data$required, !.data$passed)
if (nrow(required_failures) > 0L) {
  stop(
    "Additional audit validation failed:\n",
    paste(
      required_failures$check,
      "[observed:", required_failures$observed,
      "; expected:", required_failures$expected, "]",
      collapse = "\n"
    )
  )
}

output_index <- tibble::tibble(
  file = list.files(FQP_ADDITIONAL_AUDIT_DIR, pattern = "\\.csv$", recursive = TRUE)
)
write_audit_csv(output_index, "00_output_index.csv")
message("Sample and missingness audit completed.")
