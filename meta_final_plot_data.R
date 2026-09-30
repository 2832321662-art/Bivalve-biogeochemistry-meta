# Shared data and model layer for the revised ridgeline and forest plots.
# The input is sheet 1 (primary analysis) of meta_analysis_final_workbook.xlsx.
# Other sheets are audit/archive records and must not enter the primary model.

plot_script_dir <- function() {
  arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(arg)) {
    return(dirname(normalizePath(sub("^--file=", "", arg[[1]]), winslash = "/")))
  }
  getwd()
}

plot_set_windows_locale <- function() {
  if (.Platform$OS.type == "windows") {
    # Some Windows R builds start in the C locale and cannot expand Chinese paths.
    current <- Sys.getlocale("LC_CTYPE")
    if (identical(current, "C")) {
      suppressWarnings(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.936"))
    }
  }
}

plot_require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         ". Install them before rerunning; this script does not install packages.")
  }
}

plot_load_primary <- function(input_file) {
  plot_set_windows_locale()
  plot_require_packages(c("readxl", "metafor", "dplyr"))
  if (!file.exists(input_file)) stop("Final workbook not found: ", input_file)
  x <- readxl::read_excel(input_file, sheet = 1L, .name_repair = "minimal")
  required <- c("comparison_id", "source_row", "publication_key", "response",
                "shellfish_group", "setting", "article_title", "authors",
                "publication_year_verified", "n_c", "mean_c", "sd_c",
                "n_t", "mean_t", "sd_t", "yi", "vi",
                "author_final_SD_confirmation")
  absent <- setdiff(required, names(x))
  if (length(absent)) stop("Primary sheet is missing: ", paste(absent, collapse = ", "))
  if (nrow(x) != 669L) stop("Expected 669 author-adjudicated primary rows; found ", nrow(x))
  if (anyDuplicated(x$comparison_id)) stop("Duplicate comparison_id in primary sheet")
  if (length(unique(x$publication_key)) != 40L) stop("Expected 40 canonical primary publications")
  if (length(unique(x$response)) != 20L) stop("Expected 20 primary outcomes")
  numeric <- c("n_c", "mean_c", "sd_c", "n_t", "mean_t", "sd_t", "yi", "vi")
  if (!all(vapply(x[numeric], is.numeric, logical(1)))) {
    stop("Primary model columns must be numeric; check the workbook cell types")
  }
  if (any(!is.finite(as.matrix(x[numeric]))) ||
      any(x$n_c <= 1 | x$n_t <= 1 | x$sd_c <= 0 | x$sd_t <= 0 | x$vi <= 0)) {
    stop("Invalid n, SD, mean, yi or vi in the final primary input")
  }
  if (any(is.na(x$publication_key) | is.na(x$response) |
          is.na(x$shellfish_group) | is.na(x$setting))) {
    stop("Missing analytical-unit key field")
  }
  if (!all(as.character(x$author_final_SD_confirmation) %in% c("TRUE", "YES", "1"))) {
    stop("Final SD author confirmation is missing for one or more primary rows")
  }
  calc <- metafor::escalc(measure = "SMD", m1i = x$mean_t, sd1i = x$sd_t,
                          n1i = x$n_t, m2i = x$mean_c, sd2i = x$sd_c,
                          n2i = x$n_c, vtype = "LS", correct = TRUE)
  if (max(abs(x$yi - calc$yi)) > 1e-8 || max(abs(x$vi - calc$vi)) > 1e-7) {
    stop("Workbook yi/vi do not match recomputed Hedges' g and variance")
  }
  x
}

plot_aggregate_units <- function(x) {
  d <- x |>
    dplyr::group_by(publication_key, response, shellfish_group, setting) |>
    dplyr::summarise(
      n_comparisons = dplyr::n(),
      source_rows = paste(source_row, collapse = ";"),
      comparison_ids = paste(comparison_id, collapse = ";"),
      article_title = dplyr::first(article_title),
      authors = dplyr::first(authors),
      publication_year = dplyr::first(publication_year_verified),
      yi = sum(yi / vi) / sum(1 / vi),
      vi = 1 / sum(1 / vi),
      .groups = "drop"
    ) |>
    dplyr::arrange(response, publication_key, shellfish_group, setting)
  if (nrow(d) != 146L || sum(d$n_comparisons) != 669L) {
    stop("Aggregation differs from the adjudicated 146 units / 669 comparisons")
  }
  first_author <- vapply(strsplit(d$authors, ";", fixed = TRUE),
                         function(a) trimws(a[[1]]), character(1))
  d$Analytical_Unit_ID <- paste(d$publication_key, d$response,
                                d$shellfish_group, d$setting, sep = "::")
  d$Response_Variable <- d$response
  d$Publication_ID <- d$publication_key
  d$Bivalve_Group <- d$shellfish_group
  d$Study_Type <- d$setting
  d$Hedges_g <- d$yi
  d$SE <- sqrt(d$vi)
  d$CI_lower <- d$yi - 1.96 * d$SE
  d$CI_upper <- d$yi + 1.96 * d$SE
  d$Weight <- 1 / d$vi
  d$study_label <- paste0(first_author, " (", d$publication_year, ")")
  d
}

plot_fit_outcome <- function(d, minimum_k = 3L) {
  k <- nrow(d)
  n_pub <- length(unique(d$publication_key))
  if (k < minimum_k) {
    return(data.frame(k = k, n_pub = n_pub, Total_Effect = NA_real_,
                      CI_lower = NA_real_, CI_upper = NA_real_,
                      p_value = NA_real_, tau2 = NA_real_, I2 = NA_real_,
                      status = "descriptive"))
  }
  fit <- metafor::rma.uni(yi = d$yi, vi = d$vi, method = "SJ", test = "knha")
  data.frame(k = k, n_pub = n_pub, Total_Effect = as.numeric(fit$b),
             CI_lower = as.numeric(fit$ci.lb), CI_upper = as.numeric(fit$ci.ub),
             p_value = as.numeric(fit$pval), tau2 = as.numeric(fit$tau2),
             I2 = as.numeric(fit$I2), status = "OK")
}

plot_make_summary <- function(units, expected_summary_file = NULL) {
  summary <- units |>
    dplyr::group_by(Response_Variable) |>
    dplyr::group_modify(~ plot_fit_outcome(.x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(Response_Variable)
  summary$p_fdr <- NA_real_
  pooled <- which(is.finite(summary$p_value))
  summary$p_fdr[pooled] <- stats::p.adjust(summary$p_value[pooled], method = "BH")
  summary$result_label <- ifelse(
    summary$status == "OK",
    paste0("k = ", summary$k, "; n_pub = ", summary$n_pub,
           "; g = ", sprintf("%.2f", summary$Total_Effect),
           "; P_FDR ", ifelse(summary$p_fdr < 0.001,
                              "< 0.001", paste0("= ", sprintf("%.3f", summary$p_fdr)))),
    paste0("k = ", summary$k, "; n_pub = ", summary$n_pub,
           "; descriptive only")
  )
  if (nrow(summary) != 20L || sum(summary$status == "OK") != 14L) {
    stop("Expected 20 outcomes, of which 14 are pooled")
  }
  if (!is.null(expected_summary_file) && file.exists(expected_summary_file)) {
    expected <- utils::read.csv(expected_summary_file, stringsAsFactors = FALSE)
    m <- merge(summary, expected, by.x = "Response_Variable", by.y = "response")
    if (nrow(m) != 20L || any(m$k.x != m$k.y) ||
        max(abs(m$Total_Effect - m$g), na.rm = TRUE) > 1e-6 ||
        max(abs(m$p_fdr.x - m$p_fdr.y), na.rm = TRUE) > 1e-6) {
      stop("Recomputed model summary differs from the archived final analysis")
    }
  }
  summary
}

plot_prepare_data <- function(input_file, expected_summary_file = NULL) {
  x <- plot_load_primary(input_file)
  units <- plot_aggregate_units(x)
  summary <- plot_make_summary(units, expected_summary_file)
  list(comparisons = x, units = units, summary = summary)
}
