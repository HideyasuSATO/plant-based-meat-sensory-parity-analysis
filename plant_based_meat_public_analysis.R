run_analysis <- function(
  project_dir = getwd(),
  data_dir = file.path(project_dir, "data", "raw"),
  output_dir = file.path(project_dir, "outputs"),
  mode = c("full", "quick"),
  verbose = TRUE
) {
  mode <- match.arg(mode)
  if (getRversion() < "4.1.0") stop("R 4.1.0 or later is required.")
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)
  if (file.exists(output_dir) && !dir.exists(output_dir)) {
    stop("The output path is a file: ", output_dir)
  }
  if (dir.exists(output_dir) && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("Choose an empty output directory: ", output_dir)
  }
  packages <- c("readr", "dplyr", "tidyr", "stringr", "purrr", "tibble", "ggplot2", "forcats", "scales")
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install the required R packages first: ", paste(missing, collapse = ", "))
  suppressPackageStartupMessages(invisible(lapply(packages, library, character.only = TRUE)))
  scripts <- file.path(project_dir, "R", c("01_prepare_data.R", "02_analyze.R", "03_sample_audit.R", "04_export_results.R"))
  if (!all(file.exists(scripts))) stop("One or more analysis modules are missing.")
  invisible(lapply(scripts, function(path) parse(path, encoding = "UTF-8")))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  analysis <- new.env(parent = globalenv())
  settings <- list(
    FQP_ROOT_DIR = project_dir,
    FQP_DATA_DIR = data_dir,
    FQP_OUTPUT_DIR = output_dir,
    FQP_REVISION_DIR = output_dir,
    FQP_ADDITIONAL_AUDIT_DIR = file.path(output_dir, "additional_sample_audit"),
    FQP_CODE_VERSION = "1.0.0",
    FQP_RUN_MODE = mode,
    FQP_SEED = 20260902L,
    FQP_ADDITIONAL_AUDIT_SEED = 20260903L,
    FQP_ALPHA = 0.05,
    FQP_MARGINS = c(0.3, 0.5, 0.7, 1.0),
    FQP_PRIMARY_MARGIN = 0.5,
    FQP_EFFECT_BOOT_REPS = if (mode == "full") 2000L else 250L,
    FQP_CROSSFIT_K = 5L,
    FQP_CROSSFIT_REPEATS = if (mode == "full") 500L else 25L,
    FQP_FISHER_MONTE_CARLO_B = 100000L
  )
  list2env(settings, envir = analysis)
  configuration <- data.frame(
    setting = names(settings),
    value = vapply(settings, function(x) paste(x, collapse = ";"), character(1))
  )
  utils::write.csv(configuration, file.path(output_dir, "run_configuration.csv"), row.names = FALSE)
  seed_existed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_rng <- RNGkind()
  if (seed_existed) old_seed <- get(".Random.seed", envir = globalenv())
  on.exit({
    do.call(RNGkind, as.list(old_rng))
    if (seed_existed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(settings$FQP_SEED)
  log <- data.frame(stage = character(), started = character(), finished = character(), status = character())
  for (script in scripts) {
    started <- Sys.time()
    error <- NULL
    tryCatch({
      if (verbose) {
        message(basename(script))
        sys.source(script, envir = analysis, keep.source = FALSE)
      } else {
        suppressMessages(sys.source(script, envir = analysis, keep.source = FALSE))
      }
    }, error = function(e) error <<- e)
    log <- rbind(log, data.frame(
      stage = basename(script), started = format(started, "%Y-%m-%d %H:%M:%S"),
      finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      status = if (is.null(error)) "success" else conditionMessage(error)
    ))
    utils::write.csv(log, file.path(output_dir, "run_log.csv"), row.names = FALSE)
    if (!is.null(error)) stop(error)
  }
  capture.output(sessionInfo(), file = file.path(output_dir, "sessionInfo.txt"))
  versions <- data.frame(package = packages, version = vapply(packages, function(x) as.character(packageVersion(x)), character(1)))
  utils::write.csv(versions, file.path(output_dir, "package_versions.csv"), row.names = FALSE)
  if (verbose) message("Completed: ", output_dir)
  invisible(output_dir)
}

if (!isTRUE(getOption("sensory.analysis.load_only", FALSE))) {
  run_analysis(
    project_dir = getwd(),
    data_dir = getOption("sensory.analysis.data_dir", file.path(getwd(), "data", "raw")),
    output_dir = getOption("sensory.analysis.output_dir", file.path(getwd(), "outputs")),
    mode = getOption("sensory.analysis.mode", "full"),
    verbose = getOption("sensory.analysis.verbose", TRUE)
  )
}
