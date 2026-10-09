FQP_AUDIT_DIR <- file.path(FQP_OUTPUT_DIR, "audit")
FQP_RDS_DIR <- file.path(FQP_OUTPUT_DIR, "rds")
dir.create(FQP_AUDIT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(FQP_RDS_DIR, recursive = TRUE, showWarnings = FALSE)

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
  "Unbreaded_Chicken_StripsandChunks" = "Unbreaded Chicken Strips and Chunks"
)

CATEGORY_FORMAT <- c(
  "Bacon" = "processed/strip",
  "Bratwurst" = "processed/sausage",
  "Breaded_Chicken_Filets" = "breaded/coated",
  "Breakfast_Sausages" = "processed/sausage",
  "Burgers" = "comminuted/patty",
  "Chicken_Nuggets" = "breaded/coated",
  "Deli_Ham" = "processed/deli",
  "Deli_Turkey" = "processed/deli",
  "Hot_Dogs" = "processed/sausage",
  "Meatballs" = "comminuted/meatball",
  "Pulled_Pork" = "shredded/pulled",
  "Steak" = "whole-cut-like",
  "Unbreaded_Chicken_Filets" = "whole-muscle-like filet",
  "Unbreaded_Chicken_StripsandChunks" = "whole-muscle-like chunks"
)

MAIN_ENDPOINTS <- c(
  "Overall Liking", "Flavor", "Texture", "Appearance", "Similarity"
)

missing_files <- INPUT_FILES[!file.exists(INPUT_FILES)]
if (length(missing_files) > 0L) {
  stop(
    "The following required CSV files were not found:\n",
    paste(missing_files, collapse = "\n")
  )
}

clean_category <- function(x) {
  x <- as.character(x)
  out <- unname(CATEGORY_LABELS[x])
  missing <- is.na(out)
  out[missing] <- stringr::str_replace_all(x[missing], "_", " ")
  out
}

category_format <- function(x) {
  x <- as.character(x)
  unname(CATEGORY_FORMAT[x])
}

category_from_file <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

clean_product_label <- function(related_product, product_code) {
  related_product <- as.character(related_product)
  product_code <- as.character(product_code)
  ifelse(
    is.na(product_code) | product_code == "",
    related_product,
    paste0(related_product, " / ", product_code)
  )
}

parse_headers <- function(nms, category) {
  tibble::tibble(raw_col = nms) |>
    dplyr::mutate(
      category = category,
      related_product = stringr::str_match(
        .data$raw_col,
        "\\[RELATED_PRODUCT:\\s*([^\\]]+)\\]"
      )[, 2],
      question_type = stringr::str_match(
        .data$raw_col,
        "\\[QUESTION_TYPE:\\s*([^\\]]+)\\]"
      )[, 2],
      modality = stringr::str_match(
        .data$raw_col,
        "\\[MODALITY:\\s*([^\\]]+)\\]"
      )[, 2],
      product_code_list = stringr::str_extract_all(.data$raw_col, "\\b\\d{3}\\b"),
      product_code = purrr::map_chr(
        .data$product_code_list,
        function(z) if (length(z) == 0L) NA_character_ else tail(z, 1L)
      ),
      modality = dplyr::case_when(
        .data$modality == "Apperance" ~ "Appearance",
        .data$modality == "Dietary Restriciton" ~ "Dietary Restriction",
        TRUE ~ .data$modality
      ),
      related_product_norm = stringr::str_to_lower(
        stringr::str_squish(.data$related_product)
      ),
      product_type = dplyr::case_when(
        stringr::str_detect(.data$related_product_norm, "^animal") ~ "animal",
        stringr::str_detect(.data$related_product_norm, "^conventional") ~ "animal",
        stringr::str_detect(.data$related_product_norm, "^plant") ~ "plant_based",
        TRUE ~ NA_character_
      ),
      is_source_leader = stringr::str_detect(
        .data$related_product_norm,
        "plant-based leader"
      ),
      product_display = clean_product_label(
        .data$related_product,
        .data$product_code
      )
    ) |>
    dplyr::select(-product_code_list)
}

LIKERT_SCORE_MAP <- c(
  "Dislike very much" = 1,
  "Dislike" = 2,
  "Dislike somewhat" = 3,
  "Somewhat dislike" = 3,
  "Neither like nor dislike" = 4,
  "Neutral" = 4,
  "Like somewhat" = 5,
  "Somewhat like" = 5,
  "Like" = 6,
  "Like very much" = 7,
  "Very dissimilar" = 1,
  "Dissimilar" = 2,
  "Somewhat dissimilar" = 3,
  "Neither similar nor dissimilar" = 4,
  "Somewhat similar" = 5,
  "Similar" = 6,
  "Very similar" = 7,
  "Not at all similar" = 1,
  "Slightly similar" = 2,
  "Moderately similar" = 5,
  "Extremely similar" = 7
)

score_likert <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x[x == ""] <- NA_character_
  unname(LIKERT_SCORE_MAP[x])
}

collapse_codes <- function(x) {
  x <- sort(unique(as.character(x[!is.na(x) & x != ""])))
  paste(x, collapse = ",")
}

count_distribution <- function(x) {
  tab <- table(x, useNA = "no")
  if (length(tab) == 0L) return(NA_character_)
  paste0(
    names(tab), ":", as.integer(tab),
    " (", sprintf("%.1f", 100 * as.integer(tab) / sum(tab)), "%)",
    collapse = "; "
  )
}

near_exact <- function(x, target, tolerance = 1e-12) {
  abs(x - target) <= tolerance
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) NA_real_ else stats::sd(x)
}

write_csv_utf8 <- function(x, filename) {
  readr::write_csv(x, file.path(FQP_AUDIT_DIR, filename), na = "")
}

connected_components <- function(products, edge_table) {
  products <- sort(unique(as.character(products)))
  if (length(products) == 0L) return(list())

  adjacency <- stats::setNames(vector("list", length(products)), products)
  if (nrow(edge_table) > 0L) {
    for (i in seq_len(nrow(edge_table))) {
      a <- as.character(edge_table$product_a[[i]])
      b <- as.character(edge_table$product_b[[i]])
      adjacency[[a]] <- unique(c(adjacency[[a]], b))
      adjacency[[b]] <- unique(c(adjacency[[b]], a))
    }
  }

  unseen <- products
  components <- list()
  while (length(unseen) > 0L) {
    start <- unseen[[1L]]
    queue <- start
    component <- character()
    while (length(queue) > 0L) {
      current <- queue[[1L]]
      queue <- queue[-1L]
      if (current %in% component) next
      component <- c(component, current)
      neighbours <- adjacency[[current]]
      queue <- unique(c(queue, neighbours[!neighbours %in% component]))
    }
    component <- sort(unique(component))
    components[[length(components) + 1L]] <- component
    unseen <- setdiff(unseen, component)
  }

  component_keys <- vapply(components, paste, collapse = ",", FUN.VALUE = character(1))
  component_sizes <- lengths(components)
  components[order(-component_sizes, component_keys)]
}

infer_panels_one_category <- function(category_sets, edge_support_min = 2L) {
  category_name <- unique(category_sets$category)
  if (length(category_name) != 1L) {
    stop("infer_panels_one_category received more than one category.")
  }

  product_lists <- category_sets$product_codes
  products <- sort(unique(unlist(product_lists, use.names = FALSE)))
  products <- products[!is.na(products) & products != ""]

  pair_rows <- purrr::map_dfr(product_lists, function(codes) {
    codes <- sort(unique(as.character(codes)))
    if (length(codes) < 2L) {
      return(tibble::tibble(product_a = character(), product_b = character()))
    }
    pairs <- utils::combn(codes, 2L)
    tibble::tibble(product_a = pairs[1, ], product_b = pairs[2, ])
  })

  edge_table <- pair_rows |>
    dplyr::count(.data$product_a, .data$product_b, name = "edge_support") |>
    dplyr::filter(.data$edge_support >= edge_support_min)

  components <- connected_components(products, edge_table)
  panel_ids <- paste0("P", seq_along(components))

  assignment <- purrr::pmap_dfr(
    list(
      category_sets$category,
      category_sets$Respondent,
      category_sets$respondent_uid,
      category_sets$Randomizer,
      category_sets$product_codes,
      category_sets$plant_set,
      category_sets$n_plant_overall
    ),
    function(category, Respondent, respondent_uid, Randomizer,
             product_codes, plant_set, n_plant_overall) {
      observed <- sort(unique(as.character(product_codes)))
      observed <- observed[!is.na(observed) & observed != ""]

      candidate_index <- which(vapply(
        components,
        function(component) length(observed) > 0L && all(observed %in% component),
        logical(1)
      ))

      if (length(observed) == 0L) {
        panel_id <- "EMPTY"
        canonical <- character()
      } else if (length(candidate_index) == 1L) {
        panel_id <- panel_ids[[candidate_index]]
        canonical <- components[[candidate_index]]
      } else if (length(candidate_index) > 1L) {
        panel_id <- "AMBIGUOUS"
        canonical <- character()
      } else {
        panel_id <- "CROSS_PANEL"
        canonical <- character()
      }

      expected_panel_size <- if (length(canonical) > 0L) length(canonical) else NA_integer_
      complete_panel <- !is.na(expected_panel_size) && length(observed) == expected_panel_size

      tibble::tibble(
        category = category,
        Respondent = Respondent,
        respondent_uid = respondent_uid,
        Randomizer = Randomizer,
        panel_id = panel_id,
        observed_product_set = plant_set,
        n_observed = as.integer(n_plant_overall),
        panel_products = if (length(canonical) > 0L) paste(canonical, collapse = ",") else "",
        expected_panel_size = expected_panel_size,
        complete_panel = complete_panel,
        missing_products = list(setdiff(canonical, observed))
      )
    }
  )

  panel_summary <- purrr::map_dfr(seq_along(components), function(i) {
    panel_id <- panel_ids[[i]]
    component <- components[[i]]
    dat <- assignment |>
      dplyr::filter(.data$panel_id == .env$panel_id)
    tibble::tibble(
      category = category_name,
      panel_id = panel_id,
      panel_products = paste(component, collapse = ","),
      panel_size = length(component),
      n_respondents = nrow(dat),
      n_complete = sum(dat$complete_panel),
      n_incomplete = sum(!dat$complete_panel),
      randomizer_missing = sum(is.na(dat$Randomizer) | dat$Randomizer == ""),
      observed_n_distribution = paste(
        names(table(dat$n_observed)),
        as.integer(table(dat$n_observed)),
        sep = ":",
        collapse = "; "
      )
    )
  })

  special_ids <- c("EMPTY", "CROSS_PANEL", "AMBIGUOUS")
  special_summary <- purrr::map_dfr(special_ids, function(panel_id) {
    dat <- assignment |>
      dplyr::filter(.data$panel_id == .env$panel_id)
    if (nrow(dat) == 0L) return(tibble::tibble())
    tibble::tibble(
      category = category_name,
      panel_id = panel_id,
      panel_products = "",
      panel_size = NA_integer_,
      n_respondents = nrow(dat),
      n_complete = 0L,
      n_incomplete = nrow(dat),
      randomizer_missing = sum(is.na(dat$Randomizer) | dat$Randomizer == ""),
      observed_n_distribution = paste(
        names(table(dat$n_observed)),
        as.integer(table(dat$n_observed)),
        sep = ":",
        collapse = "; "
      )
    )
  })

  missing_detail <- assignment |>
    dplyr::select(
      category, Respondent, respondent_uid,
      Randomizer, panel_id, missing_products
    ) |>
    tidyr::unnest_longer(missing_products, values_to = "missing_product_code") |>
    dplyr::filter(!is.na(.data$missing_product_code), .data$missing_product_code != "")

  list(
    assignment = assignment |>
      dplyr::select(-missing_products),
    panel_summary = dplyr::bind_rows(panel_summary, special_summary),
    missing_detail = missing_detail,
    edge_table = edge_table |>
      dplyr::mutate(category = category_name, .before = 1L)
  )
}

message("Stage 1: reading 14 public CSV exports from ", FQP_DATA_DIR)

raw_list <- stats::setNames(
  lapply(INPUT_FILES, function(path) {
    readr::read_csv(
      path,
      show_col_types = FALSE,
      name_repair = "minimal",
      na = c("", "NA", "N/A", "No response"),
      col_types = readr::cols(.default = readr::col_character())
    )
  }),
  category_from_file(INPUT_FILES)
)

file_qc <- purrr::imap_dfr(raw_list, function(dat, category) {
  if (!"Respondent" %in% names(dat)) {
    stop("Respondent column not found in ", category)
  }
  tibble::tibble(
    category = category,
    category_label = clean_category(category),
    n_rows = nrow(dat),
    n_cols = ncol(dat),
    n_respondents = dplyr::n_distinct(dat$Respondent)
  )
})
write_csv_utf8(file_qc, "00_file_qc.csv")

meta_all <- purrr::imap_dfr(
  raw_list,
  function(dat, category) parse_headers(names(dat), category)
)
write_csv_utf8(meta_all, "01_column_metadata.csv")

product_inventory <- meta_all |>
  dplyr::filter(
    .data$question_type %in% c("Likert", "CATA", "Qualitative"),
    !is.na(.data$product_type),
    !is.na(.data$product_code)
  ) |>
  dplyr::group_by(
    .data$category, .data$product_type, .data$product_code
  ) |>
  dplyr::summarise(
    category_label = clean_category(dplyr::first(.data$category)),
    related_labels = paste(sort(unique(stats::na.omit(.data$related_product))), collapse = "; "),
    product_display = dplyr::first(stats::na.omit(.data$product_display)),
    is_source_leader = any(.data$is_source_leader, na.rm = TRUE),
    modalities = paste(sort(unique(stats::na.omit(.data$modality))), collapse = "; "),
    n_columns = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category, .data$product_type, .data$product_code)
write_csv_utf8(product_inventory, "02_product_inventory.csv")

respondent_registry <- purrr::imap_dfr(raw_list, function(dat, category) {
  if (!"Randomizer" %in% names(dat)) dat$Randomizer <- NA_character_
  tibble::tibble(
    category = category,
    category_label = clean_category(category),
    Respondent = as.character(dat$Respondent),
    respondent_uid = paste(category, as.character(dat$Respondent), sep = "__"),
    Randomizer = stringr::str_squish(as.character(dat$Randomizer))
  ) |>
    dplyr::mutate(
      Randomizer = dplyr::na_if(.data$Randomizer, "")
    )
})
write_csv_utf8(respondent_registry, "03_respondent_registry.csv")

likert_mapping_table <- tibble::tibble(
  response_label = names(LIKERT_SCORE_MAP),
  numeric_score = as.numeric(LIKERT_SCORE_MAP),
  mapping_type = "direct ordered mapping to 1--7"
)
write_csv_utf8(likert_mapping_table, "04_direct_1_to_7_likert_mapping.csv")

PURCHASE_SCORE_MAP <- c(
  "Definitely would NOT buy" = 1,
  "Would NOT buy" = 2,
  "Probably would NOT buy" = 3,
  "Might or might not buy" = 4,
  "Probably would buy" = 5,
  "Would buy" = 6,
  "Definitely would buy" = 7
)

purchase_likelihood <- purrr::imap_dfr(raw_list, function(dat, category) {
  purchase_cols <- names(dat)[stringr::str_detect(
    stringr::str_to_lower(names(dat)),
    "how likely would you be to purchase"
  )]
  if (length(purchase_cols) == 0L) return(tibble::tibble())
  if (!"Randomizer" %in% names(dat)) dat$Randomizer <- NA_character_

  dat |>
    dplyr::mutate(
      Respondent = as.character(.data$Respondent),
      respondent_uid = paste(category, .data$Respondent, sep = "__"),
      Randomizer = stringr::str_squish(as.character(.data$Randomizer)),
      Randomizer = dplyr::na_if(.data$Randomizer, "")
    ) |>
    dplyr::select(
      Respondent, respondent_uid, Randomizer,
      dplyr::all_of(purchase_cols)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(purchase_cols),
      names_to = "purchase_question",
      values_to = "purchase_response"
    ) |>
    dplyr::mutate(
      category = category,
      category_label = clean_category(category),
      purchase_target = dplyr::case_when(
        stringr::str_detect(
          stringr::str_to_lower(.data$purchase_question),
          "conventional"
        ) ~ "conventional",
        stringr::str_detect(
          stringr::str_to_lower(.data$purchase_question),
          "plantbased|plant-based"
        ) ~ "plant_based",
        TRUE ~ "other"
      ),
      purchase_response = stringr::str_squish(as.character(.data$purchase_response)),
      purchase_score = unname(PURCHASE_SCORE_MAP[.data$purchase_response]),
      purchase_measure_scope = "generic category-level likelihood; not product-specific"
    ) |>
    dplyr::select(
      category, category_label,
      Respondent, respondent_uid, Randomizer,
      purchase_target, purchase_response,
      purchase_score, purchase_measure_scope,
      purchase_question
    )
})

purchase_unmapped <- purchase_likelihood |>
  dplyr::filter(!is.na(.data$purchase_response), is.na(.data$purchase_score)) |>
  dplyr::distinct(.data$category, .data$purchase_target, .data$purchase_response)
write_csv_utf8(purchase_likelihood, "04c_generic_purchase_likelihood_two_categories.csv")
write_csv_utf8(purchase_unmapped, "04d_unmapped_purchase_likelihood_values.csv")

message("Stage 1: converting Likert fields to long format")

ratings_raw <- purrr::imap_dfr(raw_list, function(dat, category) {
  dat <- dat |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  if (!"Randomizer" %in% names(dat)) dat$Randomizer <- NA_character_

  dat <- dat |>
    dplyr::mutate(
      category = category,
      Respondent = as.character(.data$Respondent),
      respondent_uid = paste(category, .data$Respondent, sep = "__"),
      Randomizer = stringr::str_squish(as.character(.data$Randomizer)),
      Randomizer = dplyr::na_if(.data$Randomizer, "")
    )

  id_cols <- c("category", "Respondent", "respondent_uid", "Randomizer")

  dat |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "raw_col",
      values_to = "response",
      values_transform = list(response = as.character)
    )
}) |>
  dplyr::left_join(
    meta_all |>
      dplyr::select(
        category, raw_col, related_product,
        question_type, modality, product_code,
        product_type, is_source_leader, product_display
      ),
    by = c("category", "raw_col")
  ) |>
  dplyr::mutate(
    category_label = clean_category(.data$category),
    category_format = category_format(.data$category),
    response_chr = stringr::str_squish(as.character(.data$response)),
    score = score_likert(.data$response_chr)
  )

unmapped_likert <- ratings_raw |>
  dplyr::filter(
    .data$question_type == "Likert",
    !is.na(.data$response_chr),
    .data$response_chr != "",
    is.na(.data$score)
  ) |>
  dplyr::distinct(
    .data$category, .data$modality, .data$response_chr
  ) |>
  dplyr::arrange(.data$category, .data$modality, .data$response_chr)
write_csv_utf8(unmapped_likert, "04b_unmapped_likert_values.csv")

ratings <- ratings_raw |>
  dplyr::filter(
    .data$question_type == "Likert",
    .data$modality %in% MAIN_ENDPOINTS,
    .data$product_type %in% c("animal", "plant_based"),
    !is.na(.data$product_code),
    !is.na(.data$score)
  ) |>
  dplyr::transmute(
    category = .data$category,
    category_label = .data$category_label,
    category_format = .data$category_format,
    Respondent = .data$Respondent,
    respondent_uid = .data$respondent_uid,
    Randomizer = .data$Randomizer,
    related_product = .data$related_product,
    product_code = .data$product_code,
    product_display = .data$product_display,
    product_type = .data$product_type,
    is_source_leader = .data$is_source_leader,
    modality = factor(.data$modality, levels = MAIN_ENDPOINTS),
    response_chr = .data$response_chr,
    score = as.numeric(.data$score)
  )

rating_duplicates <- ratings |>
  dplyr::count(
    .data$category, .data$respondent_uid, .data$product_type,
    .data$product_code, .data$modality,
    name = "n_cells"
  ) |>
  dplyr::filter(.data$n_cells > 1L)
write_csv_utf8(rating_duplicates, "05_duplicate_rating_cells.csv")

main_endpoint_blocks <- ratings |>
  dplyr::group_by(
    .data$category, .data$category_label, .data$Respondent,
    .data$respondent_uid, .data$Randomizer, .data$product_type,
    .data$product_code, .data$product_display, .data$is_source_leader
  ) |>
  dplyr::summarise(
    n_endpoints_observed = dplyr::n_distinct(.data$modality),
    missing_endpoints = paste(
      setdiff(MAIN_ENDPOINTS, as.character(unique(.data$modality))),
      collapse = ";"
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    item_block_complete = .data$n_endpoints_observed == length(MAIN_ENDPOINTS)
  )
write_csv_utf8(main_endpoint_blocks, "06_main_endpoint_block_completeness.csv")

product_score_summary <- ratings |>
  dplyr::group_by(
    .data$category, .data$category_label, .data$category_format,
    .data$product_type, .data$product_code, .data$product_display,
    .data$is_source_leader, .data$modality
  ) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean_score = mean(.data$score),
    sd_score = safe_sd(.data$score),
    se_score = .data$sd_score / sqrt(.data$n),
    ci95_low = .data$mean_score - stats::qt(0.975, pmax(.data$n - 1L, 1L)) * .data$se_score,
    ci95_high = .data$mean_score + stats::qt(0.975, pmax(.data$n - 1L, 1L)) * .data$se_score,
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category, .data$modality, .data$product_type, dplyr::desc(.data$mean_score))
write_csv_utf8(product_score_summary, "07_product_score_summary.csv")

source_leader_codes <- product_inventory |>
  dplyr::filter(
    .data$product_type == "plant_based",
    .data$is_source_leader
  ) |>
  dplyr::distinct(
    .data$category, .data$category_label,
    source_leader_code = .data$product_code
  )

source_count_check <- source_leader_codes |>
  dplyr::count(.data$category, name = "n_source_leader_codes")
if (nrow(source_count_check) != length(EXPECTED_CATEGORIES) ||
    any(source_count_check$n_source_leader_codes != 1L)) {
  stop("Exactly one source-designated leader code was not found in every category.")
}

plant_overall_summary <- product_score_summary |>
  dplyr::filter(
    .data$product_type == "plant_based",
    as.character(.data$modality) == "Overall Liking"
  ) |>
  dplyr::select(
    category, category_label, category_format,
    product_code, product_display, is_source_leader,
    n, mean_score, sd_score
  )

source_leaders <- source_leader_codes |>
  dplyr::left_join(
    plant_overall_summary,
    by = c(
      "category", "category_label",
      "source_leader_code" = "product_code"
    )
  ) |>
  dplyr::transmute(
    category = .data$category,
    category_label = .data$category_label,
    category_format = .data$category_format,
    source_leader_code = .data$source_leader_code,
    source_leader_display = .data$product_display,
    source_n = .data$n,
    source_mean = .data$mean_score,
    source_sd = .data$sd_score
  ) |>
  dplyr::arrange(.data$category)

sample_best_ties <- plant_overall_summary |>
  dplyr::group_by(.data$category) |>
  dplyr::mutate(
    sample_best_mean = max(.data$mean_score),
    is_sample_best = near_exact(.data$mean_score, .data$sample_best_mean)
  ) |>
  dplyr::filter(.data$is_sample_best) |>
  dplyr::arrange(.data$category, .data$product_code) |>
  dplyr::ungroup()

sample_best_sets <- sample_best_ties |>
  dplyr::group_by(.data$category, .data$category_label, .data$category_format) |>
  dplyr::summarise(
    sample_best_set = paste(.data$product_code, collapse = "; "),
    sample_best_products = paste(.data$product_display, collapse = "; "),
    sample_best_mean = dplyr::first(.data$sample_best_mean),
    n_sample_best_tied = dplyr::n(),
    source_in_tie_set = any(.data$is_source_leader),
    .groups = "drop"
  )

sample_best_representative <- sample_best_ties |>
  dplyr::arrange(
    .data$category,
    dplyr::desc(.data$is_source_leader),
    .data$product_code
  ) |>
  dplyr::group_by(.data$category) |>
  dplyr::slice(1L) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    category = .data$category,
    sample_best_code = .data$product_code,
    sample_best_display = .data$product_display,
    sample_best_n = .data$n,
    sample_best_representative_mean = .data$mean_score,
    sample_best_is_source = .data$is_source_leader
  )

leader_audit <- source_leaders |>
  dplyr::left_join(sample_best_sets, by = c("category", "category_label", "category_format")) |>
  dplyr::left_join(sample_best_representative, by = "category") |>
  dplyr::mutate(
    source_in_sample_best_set = purrr::map2_lgl(
      .data$source_leader_code,
      .data$sample_best_set,
      function(source_code, best_set) {
        source_code %in% stringr::str_split(best_set, ";\\s*")[[1L]]
      }
    ),
    source_minus_sample_best = .data$source_mean - .data$sample_best_mean,
    leader_status = dplyr::case_when(
      .data$source_in_sample_best_set & .data$n_sample_best_tied == 1L ~
        "Source leader is unique sample-best",
      .data$source_in_sample_best_set & .data$n_sample_best_tied > 1L ~
        "Source leader is tied sample-best",
      TRUE ~ "Source leader differs from sample-best"
    )
  ) |>
  dplyr::arrange(.data$category)

write_csv_utf8(source_leaders, "08_source_designated_leaders.csv")
write_csv_utf8(sample_best_ties, "09_sample_best_tie_set.csv")
write_csv_utf8(leader_audit, "10_leader_audit.csv")

overall_ratings <- ratings |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking")

overall_plant <- overall_ratings |>
  dplyr::filter(.data$product_type == "plant_based")

overall_animal <- overall_ratings |>
  dplyr::filter(.data$product_type == "animal")

respondent_product_sets_observed <- overall_plant |>
  dplyr::group_by(
    .data$category, .data$Respondent, .data$respondent_uid
  ) |>
  dplyr::summarise(
    product_codes = list(sort(unique(.data$product_code))),
    plant_set = collapse_codes(.data$product_code),
    n_plant_overall = dplyr::n_distinct(.data$product_code),
    .groups = "drop"
  )

respondent_product_sets <- respondent_registry |>
  dplyr::left_join(
    respondent_product_sets_observed,
    by = c("category", "Respondent", "respondent_uid")
  ) |>
  dplyr::mutate(
    product_codes = purrr::map(
      .data$product_codes,
      function(z) if (is.null(z) || length(z) == 0L || all(is.na(z))) character() else z
    ),
    plant_set = tidyr::replace_na(.data$plant_set, ""),
    n_plant_overall = tidyr::replace_na(.data$n_plant_overall, 0L)
  )

panel_results <- split(respondent_product_sets, respondent_product_sets$category) |>
  purrr::map(infer_panels_one_category)

panel_assignment <- purrr::map_dfr(panel_results, "assignment") |>
  dplyr::arrange(.data$category, .data$Respondent)
panel_summary <- purrr::map_dfr(panel_results, "panel_summary") |>
  dplyr::arrange(.data$category, .data$panel_id)
panel_edges <- purrr::map_dfr(panel_results, "edge_table") |>
  dplyr::arrange(.data$category, .data$product_a, .data$product_b)
missing_product_detail <- purrr::map_dfr(panel_results, "missing_detail") |>
  dplyr::arrange(.data$category, .data$panel_id, .data$missing_product_code, .data$Respondent)

panel_assignment_counts <- panel_assignment |>
  dplyr::group_by(.data$category, .data$panel_id) |>
  dplyr::summarise(
    assignment_n_respondents = dplyr::n(),
    assignment_n_complete = sum(.data$complete_panel),
    assignment_n_incomplete = sum(!.data$complete_panel),
    assignment_randomizer_missing = sum(
      is.na(.data$Randomizer) | .data$Randomizer == ""
    ),
    .groups = "drop"
  )

panel_summary_consistency <- dplyr::full_join(
  panel_assignment_counts,
  panel_summary |>
    dplyr::transmute(
      category = .data$category,
      panel_id = .data$panel_id,
      summary_n_respondents = .data$n_respondents,
      summary_n_complete = .data$n_complete,
      summary_n_incomplete = .data$n_incomplete,
      summary_randomizer_missing = .data$randomizer_missing
    ),
  by = c("category", "panel_id")
) |>
  dplyr::mutate(
    counts_match =
      !is.na(.data$assignment_n_respondents) &
      !is.na(.data$summary_n_respondents) &
      .data$assignment_n_respondents == .data$summary_n_respondents &
      .data$assignment_n_complete == .data$summary_n_complete &
      .data$assignment_n_incomplete == .data$summary_n_incomplete &
      .data$assignment_randomizer_missing == .data$summary_randomizer_missing
  ) |>
  dplyr::arrange(.data$category, .data$panel_id)

if (nrow(missing_product_detail) > 0L) {
  missing_product_blocks <- missing_product_detail |>
    dplyr::group_by(
      .data$category, .data$panel_id, .data$missing_product_code
    ) |>
    dplyr::summarise(
      n_missing_blocks = dplyr::n(),
      randomizers = paste(
        sort(unique(stats::na.omit(.data$Randomizer))),
        collapse = ";"
      ),
      .groups = "drop"
    )
} else {
  missing_product_blocks <- tibble::tibble(
    category = character(), panel_id = character(),
    missing_product_code = character(), n_missing_blocks = integer(),
    randomizers = character()
  )
}

write_csv_utf8(panel_assignment, "11_respondent_panel_assignment.csv")
write_csv_utf8(panel_summary, "12_inferred_panel_summary.csv")
write_csv_utf8(panel_summary_consistency, "12b_panel_summary_consistency.csv")
write_csv_utf8(panel_edges, "13_panel_cooccurrence_edges.csv")
write_csv_utf8(missing_product_blocks, "14_whole_product_missing_blocks.csv")
write_csv_utf8(missing_product_detail, "15_whole_product_missing_detail.csv")

ratings <- ratings |>
  dplyr::left_join(
    panel_assignment |>
      dplyr::select(
        category, respondent_uid, panel_id,
        observed_product_set, n_observed,
        expected_panel_size, complete_panel
      ),
    by = c("category", "respondent_uid")
  )

overall_ratings <- ratings |>
  dplyr::filter(as.character(.data$modality) == "Overall Liking")
overall_plant <- overall_ratings |>
  dplyr::filter(.data$product_type == "plant_based")
overall_animal <- overall_ratings |>
  dplyr::filter(.data$product_type == "animal")

animal_counts <- overall_animal |>
  dplyr::group_by(.data$category, .data$respondent_uid) |>
  dplyr::summarise(
    n_animal_overall = dplyr::n_distinct(.data$product_code),
    .groups = "drop"
  )

respondent_counts <- panel_assignment |>
  dplyr::transmute(
    category = .data$category,
    Respondent = .data$Respondent,
    respondent_uid = .data$respondent_uid,
    Randomizer = .data$Randomizer,
    panel_id = .data$panel_id,
    n_plant_overall = .data$n_observed,
    expected_panel_size = .data$expected_panel_size,
    complete_panel = .data$complete_panel
  ) |>
  dplyr::left_join(animal_counts, by = c("category", "respondent_uid")) |>
  dplyr::mutate(
    n_animal_overall = tidyr::replace_na(.data$n_animal_overall, 0L),
    is_canonical_panel = stringr::str_detect(.data$panel_id, "^P[0-9]+$")
  )
write_csv_utf8(respondent_counts, "16_respondent_product_counts.csv")

presentation_by_category <- respondent_counts |>
  dplyr::group_by(.data$category) |>
  dplyr::summarise(
    category_label = clean_category(dplyr::first(.data$category)),
    n_respondents = dplyr::n(),
    randomizer_missing_n = sum(is.na(.data$Randomizer) | .data$Randomizer == ""),
    randomizer_missing_rate = .data$randomizer_missing_n / .data$n_respondents,
    min_plant_overall = min(.data$n_plant_overall),
    median_plant_overall = stats::median(.data$n_plant_overall),
    max_plant_overall = max(.data$n_plant_overall),
    min_animal_overall = min(.data$n_animal_overall),
    median_animal_overall = stats::median(.data$n_animal_overall),
    max_animal_overall = max(.data$n_animal_overall),
    respondents_with_5plant_1animal = sum(
      .data$n_plant_overall == 5L & .data$n_animal_overall == 1L
    ),
    pct_with_5plant_1animal = mean(
      .data$n_plant_overall == 5L & .data$n_animal_overall == 1L
    ),
    canonical_panel_respondents = sum(.data$is_canonical_panel),
    special_panel_assignment_respondents = sum(!.data$is_canonical_panel),
    expected_plant_blocks_in_canonical_panels = sum(
      dplyr::if_else(
        .data$is_canonical_panel,
        as.integer(.data$expected_panel_size),
        0L
      ),
      na.rm = TRUE
    ),
    observed_plant_blocks_in_canonical_panels = sum(
      dplyr::if_else(
        .data$is_canonical_panel,
        as.integer(.data$n_plant_overall),
        0L
      ),
      na.rm = TRUE
    ),
    plant_overall_distribution = count_distribution(.data$n_plant_overall),
    animal_overall_distribution = count_distribution(.data$n_animal_overall),
    panel_distribution = paste(
      names(table(.data$panel_id)),
      as.integer(table(.data$panel_id)),
      sep = ":",
      collapse = "; "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    whole_product_missing_blocks =
      .data$expected_plant_blocks_in_canonical_panels -
      .data$observed_plant_blocks_in_canonical_panels,
    whole_product_missing_rate = dplyr::if_else(
      .data$expected_plant_blocks_in_canonical_panels > 0L,
      .data$whole_product_missing_blocks /
        .data$expected_plant_blocks_in_canonical_panels,
      NA_real_
    )
  ) |>
  dplyr::arrange(.data$category)
write_csv_utf8(presentation_by_category, "17_product_presentation_by_category.csv")

randomizer_set_patterns <- respondent_product_sets |>
  dplyr::count(
    .data$category, .data$Randomizer, .data$plant_set,
    name = "n_respondents"
  ) |>
  dplyr::arrange(.data$category, .data$Randomizer, dplyr::desc(.data$n_respondents))

randomizer_summary <- randomizer_set_patterns |>
  dplyr::group_by(.data$category, .data$Randomizer) |>
  dplyr::arrange(dplyr::desc(.data$n_respondents), .data$plant_set, .by_group = TRUE) |>
  dplyr::summarise(
    n_respondents = sum(.data$n_respondents),
    n_unique_product_sets = dplyr::n_distinct(.data$plant_set),
    modal_plant_set = dplyr::first(.data$plant_set),
    modal_set_n = dplyr::first(.data$n_respondents),
    modal_set_share = .data$modal_set_n / .data$n_respondents,
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category, .data$Randomizer)

randomizer_product_incidence <- overall_ratings |>
  dplyr::count(
    .data$category, .data$Randomizer, .data$product_type,
    .data$product_code, .data$product_display, .data$is_source_leader,
    name = "n_rated"
  ) |>
  dplyr::left_join(
    respondent_registry |>
      dplyr::count(.data$category, .data$Randomizer, name = "n_respondents_randomizer"),
    by = c("category", "Randomizer")
  ) |>
  dplyr::mutate(
    rated_share_within_randomizer = .data$n_rated / .data$n_respondents_randomizer
  ) |>
  dplyr::arrange(.data$category, .data$Randomizer, .data$product_type, .data$product_code)

write_csv_utf8(randomizer_summary, "18_randomizer_summary.csv")
write_csv_utf8(randomizer_set_patterns, "19_randomizer_product_set_patterns.csv")
write_csv_utf8(randomizer_product_incidence, "20_randomizer_product_incidence.csv")

animal_overall_pair <- overall_animal |>
  dplyr::select(
    category, respondent_uid,
    animal_code = product_code,
    animal_display = product_display,
    animal_score = score
  )

paired_overall_all_products <- overall_plant |>
  dplyr::select(
    category, category_label, category_format,
    Respondent, respondent_uid, Randomizer,
    panel_id, plant_code = product_code,
    plant_display = product_display,
    is_source_leader = is_source_leader,
    plant_score = score
  ) |>
  dplyr::inner_join(
    animal_overall_pair,
    by = c("category", "respondent_uid")
  ) |>
  dplyr::mutate(
    diff = .data$plant_score - .data$animal_score,
    same_or_better = .data$diff >= 0,
    plant_higher = .data$diff > 0,
    equal = .data$diff == 0,
    animal_higher = .data$diff < 0
  )

source_same_or_better <- paired_overall_all_products |>
  dplyr::filter(.data$is_source_leader) |>
  dplyr::group_by(
    .data$category, .data$category_label, .data$category_format,
    .data$plant_code, .data$plant_display,
    .data$animal_code, .data$animal_display
  ) |>
  dplyr::summarise(
    n = dplyr::n(),
    plant_mean = mean(.data$plant_score),
    animal_mean_paired = mean(.data$animal_score),
    mean_diff = mean(.data$diff),
    raw_plant_higher = mean(.data$plant_higher),
    raw_equal = mean(.data$equal),
    raw_animal_higher = mean(.data$animal_higher),
    raw_same_or_better = mean(.data$same_or_better),
    same_or_better_n = sum(.data$same_or_better),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category)

paper_figure8 <- tibble::tribble(
  ~category, ~paper_prefer_plant, ~paper_no_preference, ~paper_prefer_animal, ~paper_same_or_better,
  "Unbreaded_Chicken_Filets", 0.24, 0.24, 0.52, 0.48,
  "Burgers", 0.23, 0.24, 0.53, 0.47,
  "Breaded_Chicken_Filets", 0.20, 0.22, 0.58, 0.42,
  "Chicken_Nuggets", 0.23, 0.18, 0.59, 0.41,
  "Breakfast_Sausages", 0.20, 0.20, 0.60, 0.40
)

same_or_better_discrepancy <- source_same_or_better |>
  dplyr::left_join(paper_figure8, by = "category") |>
  dplyr::mutate(
    same_or_better_gap_pp = 100 * (.data$raw_same_or_better - .data$paper_same_or_better),
    plant_higher_gap_pp = 100 * (.data$raw_plant_higher - .data$paper_prefer_plant),
    equal_gap_pp = 100 * (.data$raw_equal - .data$paper_no_preference),
    animal_higher_gap_pp = 100 * (.data$raw_animal_higher - .data$paper_prefer_animal)
  )
write_csv_utf8(source_same_or_better, "21_source_leader_same_or_better.csv")
write_csv_utf8(same_or_better_discrepancy, "22_same_or_better_figure8_discrepancy.csv")

source_panel_lookup <- purrr::map_dfr(seq_len(nrow(source_leaders)), function(i) {
  category_i <- source_leaders$category[[i]]
  source_code_i <- source_leaders$source_leader_code[[i]]
  candidates <- panel_summary |>
    dplyr::filter(
      .data$category == category_i,
      stringr::str_detect(.data$panel_id, "^P[0-9]+$")
    ) |>
    dplyr::mutate(
      contains_source = purrr::map_lgl(
        stringr::str_split(.data$panel_products, ","),
        function(z) source_code_i %in% z
      )
    ) |>
    dplyr::filter(.data$contains_source)

  if (nrow(candidates) != 1L) {
    return(tibble::tibble(
      category = category_i,
      source_leader_code = source_code_i,
      source_panel_id = NA_character_,
      source_panel_n = NA_integer_,
      source_panel_size = NA_integer_
    ))
  }

  tibble::tibble(
    category = category_i,
    source_leader_code = source_code_i,
    source_panel_id = candidates$panel_id[[1L]],
    source_panel_n = candidates$n_respondents[[1L]],
    source_panel_size = candidates$panel_size[[1L]]
  )
})

source_missing_status <- source_panel_lookup |>
  dplyr::left_join(source_leaders, by = c("category", "source_leader_code")) |>
  dplyr::left_join(
    panel_assignment |>
      dplyr::select(
        category, respondent_uid, Respondent,
        Randomizer, panel_id
      ),
    by = "category"
  ) |>
  dplyr::filter(.data$panel_id == .data$source_panel_id) |>
  dplyr::left_join(
    overall_plant |>
      dplyr::select(
        category, respondent_uid,
        product_code_observed = product_code,
        source_leader_score = score
      ),
    by = c("category", "respondent_uid")
  ) |>
  dplyr::mutate(
    source_leader_score = dplyr::if_else(
      .data$product_code_observed == .data$source_leader_code,
      .data$source_leader_score,
      NA_real_
    )
  ) |>
  dplyr::group_by(
    .data$category, .data$category_label, .data$source_leader_code,
    .data$source_leader_display, .data$source_n, .data$source_panel_id,
    .data$source_panel_n, .data$source_panel_size,
    .data$respondent_uid, .data$Respondent, .data$Randomizer
  ) |>
  dplyr::summarise(
    source_leader_score = {
      z <- stats::na.omit(.data$source_leader_score)
      if (length(z) == 0L) NA_real_ else z[[1L]]
    },
    .groups = "drop"
  ) |>
  dplyr::left_join(
    overall_animal |>
      dplyr::select(
        category, respondent_uid,
        animal_overall_score = score
      ),
    by = c("category", "respondent_uid")
  ) |>
  dplyr::mutate(
    source_observed = !is.na(.data$source_leader_score)
  )

source_missingness_summary <- source_missing_status |>
  dplyr::group_by(
    .data$category, .data$category_label,
    .data$source_leader_code, .data$source_leader_display,
    .data$source_n, .data$source_panel_id, .data$source_panel_size
  ) |>
  dplyr::summarise(
    source_total_observed_n = dplyr::first(.data$source_n),
    source_canonical_panel_n = dplyr::n(),
    source_observed_in_canonical_panel_n = sum(.data$source_observed),
    source_block_missing_n = sum(!.data$source_observed),
    source_block_missing_rate = mean(!.data$source_observed),
    source_noncanonical_extra_n = pmax(
      0L,
      .data$source_total_observed_n - .data$source_observed_in_canonical_panel_n
    ),
    animal_mean_if_source_observed = safe_mean(
      .data$animal_overall_score[.data$source_observed]
    ),
    animal_mean_if_source_missing = safe_mean(
      .data$animal_overall_score[!.data$source_observed]
    ),
    animal_mean_missing_minus_observed =
      .data$animal_mean_if_source_missing - .data$animal_mean_if_source_observed,
    animal_balance_p_value = {
      observed_values <- .data$animal_overall_score[.data$source_observed]
      missing_values <- .data$animal_overall_score[!.data$source_observed]
      if (sum(!is.na(observed_values)) >= 2L && sum(!is.na(missing_values)) >= 2L) {
        stats::t.test(missing_values, observed_values)$p.value
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$category)

write_csv_utf8(source_missing_status, "23_source_leader_missingness_status.csv")
write_csv_utf8(source_missingness_summary, "24_source_leader_missingness_summary.csv")

message("Stage 1: applying locked audit checks")

EXPECTED_ROW_COUNTS <- c(
  "Bacon" = 192L,
  "Bratwurst" = 213L,
  "Breaded_Chicken_Filets" = 91L,
  "Breakfast_Sausages" = 204L,
  "Burgers" = 201L,
  "Chicken_Nuggets" = 305L,
  "Deli_Ham" = 203L,
  "Deli_Turkey" = 203L,
  "Hot_Dogs" = 186L,
  "Meatballs" = 99L,
  "Pulled_Pork" = 204L,
  "Steak" = 195L,
  "Unbreaded_Chicken_Filets" = 197L,
  "Unbreaded_Chicken_StripsandChunks" = 189L
)

EXPECTED_SOURCE_CODES <- c(
  "Bacon" = "334",
  "Bratwurst" = "702",
  "Breaded_Chicken_Filets" = "491",
  "Breakfast_Sausages" = "413",
  "Burgers" = "922",
  "Chicken_Nuggets" = "206",
  "Deli_Ham" = "923",
  "Deli_Turkey" = "491",
  "Hot_Dogs" = "164",
  "Meatballs" = "859",
  "Pulled_Pork" = "651",
  "Steak" = "923",
  "Unbreaded_Chicken_Filets" = "413",
  "Unbreaded_Chicken_StripsandChunks" = "702"
)

EXPECTED_MISSING_BLOCKS <- tibble::tribble(
  ~category, ~panel_id, ~missing_product_code, ~n_missing_blocks,
  "Bratwurst", "P1", "923", 8L,
  "Breaded_Chicken_Filets", "P1", "491", 13L,
  "Chicken_Nuggets", "P1", "164", 9L,
  "Hot_Dogs", "P2", "651", 20L,
  "Steak", "P2", "396", 7L,
  "Steak", "P2", "859", 4L
) |>
  dplyr::arrange(.data$category, .data$panel_id, .data$missing_product_code)

validation_checks <- tibble::tribble(
  ~check, ~passed, ~observed, ~expected,
  "Fourteen categories present",
  setequal(file_qc$category, EXPECTED_CATEGORIES),
  paste(sort(file_qc$category), collapse = ";"),
  paste(sort(EXPECTED_CATEGORIES), collapse = ";"),
  "CSV respondent row counts match locked audit",
  identical(
    as.integer(file_qc$n_rows[match(names(EXPECTED_ROW_COUNTS), file_qc$category)]),
    as.integer(EXPECTED_ROW_COUNTS)
  ),
  paste(file_qc$n_rows[match(names(EXPECTED_ROW_COUNTS), file_qc$category)], collapse = ";"),
  paste(EXPECTED_ROW_COUNTS, collapse = ";"),
  "Total public-export respondent rows equal 2682",
  sum(file_qc$n_rows) == 2682L,
  as.character(sum(file_qc$n_rows)),
  "2682",
  "Source leader codes match locked audit",
  identical(
    source_leaders$source_leader_code[match(names(EXPECTED_SOURCE_CODES), source_leaders$category)],
    unname(EXPECTED_SOURCE_CODES)
  ),
  paste(source_leaders$source_leader_code[match(names(EXPECTED_SOURCE_CODES), source_leaders$category)], collapse = ";"),
  paste(EXPECTED_SOURCE_CODES, collapse = ";"),
  "Source differs from tie-aware sample-best only in two locked categories",
  setequal(
    leader_audit$category[leader_audit$leader_status == "Source leader differs from sample-best"],
    c("Breakfast_Sausages", "Chicken_Nuggets")
  ),
  paste(
    sort(leader_audit$category[leader_audit$leader_status == "Source leader differs from sample-best"]),
    collapse = ";"
  ),
  "Breakfast_Sausages;Chicken_Nuggets",
  "Bacon exact sample-best tie retained",
  {
    row <- leader_audit[leader_audit$category == "Bacon", ]
    identical(row$sample_best_set, "257; 334") && row$n_sample_best_tied == 2L
  },
  {
    row <- leader_audit[leader_audit$category == "Bacon", ]
    paste(row$sample_best_set, row$n_sample_best_tied)
  },
  "257; 334 and two tied products",
  "No duplicate rating cells",
  nrow(rating_duplicates) == 0L,
  as.character(nrow(rating_duplicates)),
  "0",
  "No unmapped nonmissing Likert responses",
  nrow(unmapped_likert) == 0L,
  as.character(nrow(unmapped_likert)),
  "0",
  "Observed five-endpoint product blocks equal 14603",
  nrow(main_endpoint_blocks) == 14603L,
  as.character(nrow(main_endpoint_blocks)),
  "14603",
  "No within-block item missingness",
  sum(!main_endpoint_blocks$item_block_complete) == 0L,
  as.character(sum(!main_endpoint_blocks$item_block_complete)),
  "0",
  "Randomizer missing total equals 201",
  sum(is.na(respondent_registry$Randomizer) | respondent_registry$Randomizer == "") == 201L,
  as.character(sum(is.na(respondent_registry$Randomizer) | respondent_registry$Randomizer == "")),
  "201",
  "Generic purchase-likelihood fields contain 604 mapped responses",
  nrow(purchase_likelihood) == 604L && nrow(purchase_unmapped) == 0L,
  paste0(nrow(purchase_likelihood), " responses; ", nrow(purchase_unmapped), " unmapped"),
  "604 responses; 0 unmapped",
  "Whole-product missing-block pattern matches locked audit",
  identical(
    missing_product_blocks |>
      dplyr::select(
        category, panel_id,
        missing_product_code, n_missing_blocks
      ) |>
      dplyr::arrange(.data$category, .data$panel_id, .data$missing_product_code),
    EXPECTED_MISSING_BLOCKS
  ),
  paste(
    missing_product_blocks$category,
    missing_product_blocks$panel_id,
    missing_product_blocks$missing_product_code,
    missing_product_blocks$n_missing_blocks,
    sep = ":",
    collapse = ";"
  ),
  paste(
    EXPECTED_MISSING_BLOCKS$category,
    EXPECTED_MISSING_BLOCKS$panel_id,
    EXPECTED_MISSING_BLOCKS$missing_product_code,
    EXPECTED_MISSING_BLOCKS$n_missing_blocks,
    sep = ":",
    collapse = ";"
  ),
  "UCF source-leader same-or-better equals 62/103",
  {
    row <- source_same_or_better |>
      dplyr::filter(.data$category == "Unbreaded_Chicken_Filets")
    nrow(row) == 1L && row$same_or_better_n == 62L && row$n == 103L
  },
  {
    row <- source_same_or_better |>
      dplyr::filter(.data$category == "Unbreaded_Chicken_Filets")
    paste0(row$same_or_better_n, "/", row$n)
  },
  "62/103"
)

validation_checks <- dplyr::bind_rows(
  validation_checks,
  tibble::tibble(
    check = c(
      "Panel summary counts match respondent-level panel assignment",
      "Whole-product missing blocks total 61 across canonical panels",
      "Only two noncanonical respondent assignments, both in Deli Turkey"
    ),
    passed = c(
      nrow(panel_summary_consistency) > 0L &&
        all(panel_summary_consistency$counts_match),
      sum(presentation_by_category$whole_product_missing_blocks) == 61L,
      {
        special <- panel_assignment |>
          dplyr::filter(!stringr::str_detect(.data$panel_id, "^P[0-9]+$")) |>
          dplyr::count(.data$category, .data$panel_id, name = "n") |>
          dplyr::arrange(.data$category, .data$panel_id)
        keys <- paste(special$category, special$panel_id, special$n, sep = ":")
        nrow(special) == 2L &&
          setequal(
            keys,
            c("Deli_Turkey:CROSS_PANEL:1", "Deli_Turkey:EMPTY:1")
          )
      }
    ),
    observed = c(
      paste0(
        sum(panel_summary_consistency$counts_match), "/",
        nrow(panel_summary_consistency), " panel/category rows matched"
      ),
      as.character(sum(presentation_by_category$whole_product_missing_blocks)),
      {
        special <- panel_assignment |>
          dplyr::filter(!stringr::str_detect(.data$panel_id, "^P[0-9]+$")) |>
          dplyr::count(.data$category, .data$panel_id, name = "n") |>
          dplyr::arrange(.data$category, .data$panel_id)
        paste(special$category, special$panel_id, special$n, sep = ":", collapse = ";")
      }
    ),
    expected = c(
      "all panel/category rows matched",
      "61",
      "Deli_Turkey:CROSS_PANEL:1;Deli_Turkey:EMPTY:1"
    )
  )
)

write_csv_utf8(validation_checks, "25_locked_audit_validation.csv")

if (any(!validation_checks$passed)) {
  failed <- validation_checks |>
    dplyr::filter(!.data$passed)
  stop(
    "Locked audit validation failed:\n",
    paste0("- ", failed$check, " [observed: ", failed$observed,
           "; expected: ", failed$expected, "]", collapse = "\n")
  )
}

saveRDS(ratings, file.path(FQP_RDS_DIR, "ratings_revision_long.rds"))
saveRDS(respondent_registry, file.path(FQP_RDS_DIR, "respondent_registry.rds"))
saveRDS(panel_assignment, file.path(FQP_RDS_DIR, "panel_assignment.rds"))
saveRDS(panel_summary, file.path(FQP_RDS_DIR, "panel_summary.rds"))
saveRDS(source_leaders, file.path(FQP_RDS_DIR, "source_leaders.rds"))
saveRDS(sample_best_ties, file.path(FQP_RDS_DIR, "sample_best_ties.rds"))
saveRDS(sample_best_representative, file.path(FQP_RDS_DIR, "sample_best_representative.rds"))
saveRDS(leader_audit, file.path(FQP_RDS_DIR, "leader_audit.rds"))
saveRDS(paired_overall_all_products, file.path(FQP_RDS_DIR, "paired_overall_all_products.rds"))
saveRDS(purchase_likelihood, file.path(FQP_RDS_DIR, "purchase_likelihood_generic.rds"))

stage1_manifest <- tibble::tibble(
  object = c(
    "ratings", "respondent_registry", "panel_assignment", "panel_summary",
    "source_leaders", "sample_best_ties", "sample_best_representative",
    "leader_audit", "paired_overall_all_products", "purchase_likelihood_generic"
  ),
  file = c(
    "ratings_revision_long.rds", "respondent_registry.rds",
    "panel_assignment.rds", "panel_summary.rds", "source_leaders.rds",
    "sample_best_ties.rds", "sample_best_representative.rds",
    "leader_audit.rds", "paired_overall_all_products.rds",
    "purchase_likelihood_generic.rds"
  ),
  rows = c(
    nrow(ratings), nrow(respondent_registry), nrow(panel_assignment),
    nrow(panel_summary), nrow(source_leaders), nrow(sample_best_ties),
    nrow(sample_best_representative), nrow(leader_audit),
    nrow(paired_overall_all_products), nrow(purchase_likelihood)
  )
)
write_csv_utf8(stage1_manifest, "26_stage1_object_manifest.csv")

message("Stage 1 completed: data audit is locked and reusable RDS files were written.")
