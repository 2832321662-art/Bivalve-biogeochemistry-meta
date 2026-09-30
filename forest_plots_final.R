# Revised from the original per-indicator forest plot.
# Source: sheet 1 of meta_analysis_final_workbook.xlsx, via the shared helper.
# Exports 20 primary outcomes: 14 pooled and 6 descriptive (k < 3).

if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.936"))
}
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else ""
script_dir <- if (file.exists("meta_final_plot_data.R")) "." else if (nzchar(script_file)) dirname(script_file) else getwd()
source(file.path(script_dir, "meta_final_plot_data.R"), encoding = "UTF-8")
plot_require_packages(c("ggplot2", "dplyr"))

input_file <- Sys.getenv("META_FINAL_WORKBOOK", file.path(script_dir, "meta_analysis_final_workbook.xlsx"))
output_dir <- file.path(script_dir, "forest_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
prepared <- plot_prepare_data(input_file, file.path(script_dir, "meta_summary_final.csv"))
study_es <- prepared$units
meta_summary <- prepared$summary
utils::write.csv(study_es, file.path(output_dir, "aggregated_units_for_plot.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(meta_summary, file.path(output_dir, "outcome_models_for_plot.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

# Original forest-plot encodings retained: shape = shellfish, colour = setting.
shape_values <- c(Mussels = 17, Scallop = 15, Clam = 18, Oyster = 16)
study_type_colors <- c(Aquaculture = "#EFA68A", Reef = "#6E9FD1", Mesocosm = "#63BFAE")
response_labels <- c(
  NH4_concentration = "NH4+ concentration", NO3_concentration = "NO3- concentration",
  NO2_concentration = "NO2- concentration", NOx_concentration = "NOx concentration",
  PO4_concentration = "Reactive P concentration", N2_flux = "Net N2 exchange",
  NH4_flux = "NH4+ flux", NO2_flux = "NO2- flux", NO3_flux = "NO3- flux",
  NOx_flux = "NOx flux", PO4_flux = "Reactive P flux",
  CH4_flux = "CH4 flux", CO2_flux = "CO2 flux", N2O_flux = "N2O flux",
  POC_content = "Sediment/particulate POC", PON_content = "Sediment/particulate PON",
  O2_concentration = "O2 concentration", POC_concentration = "POC concentration",
  PON_concentration = "PON concentration", O2_flux = "O2 flux"
)
stopifnot(setequal(names(response_labels), unique(study_es$response)))
valid_indicators <- names(response_labels)

format_p <- function(p) ifelse(is.na(p), "not pooled", ifelse(p < 0.001, "< 0.001", sprintf("= %.3f", p)))
make_indicator_plot <- function(indicator_name) {
  d <- study_es |>
    dplyr::filter(Response_Variable == indicator_name) |>
    dplyr::mutate(
      shellfish_group = factor(Bivalve_Group, levels = names(shape_values)),
      study_type = factor(Study_Type, levels = names(study_type_colors))
    ) |>
    dplyr::arrange(study_type, shellfish_group, Hedges_g) |>
    dplyr::mutate(
      display_label = paste0(sprintf("%03d", dplyr::row_number()), "___", study_label),
      display_label = factor(display_label, levels = rev(display_label))
    )
  s <- meta_summary[meta_summary$Response_Variable == indicator_name, , drop = FALSE]
  stopifnot(nrow(s) == 1L, nrow(d) == s$k)
  shellfish_counts <- d |>
    dplyr::count(shellfish_group, .drop = TRUE) |>
    dplyr::mutate(label = paste0(shellfish_group, " = ", n)) |>
    dplyr::pull(label) |>
    paste(collapse = "; ")
  setting_counts <- d |>
    dplyr::count(study_type, .drop = TRUE) |>
    dplyr::mutate(label = paste0(study_type, " = ", n)) |>
    dplyr::pull(label) |>
    paste(collapse = "; ")
  summary_label <- if (s$status == "OK") {
    paste0("k = ", s$k, "; n_pub = ", s$n_pub,
           "\ng = ", sprintf("%.2f", s$Total_Effect),
           " [", sprintf("%.2f", s$CI_lower), ", ", sprintf("%.2f", s$CI_upper), "]",
           "\nBH-FDR P ", format_p(s$p_fdr))
  } else {
    paste0("k = ", s$k, "; n_pub = ", s$n_pub,
           "\nDescriptive only; no pooled g")
  }
  p <- ggplot2::ggplot(d) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.55, colour = "black") +
    ggplot2::geom_segment(
      ggplot2::aes(x = CI_lower, xend = CI_upper,
                   y = display_label, yend = display_label),
      linewidth = 0.55, colour = "black"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = Hedges_g, y = display_label,
                   shape = shellfish_group, colour = study_type),
      size = 4.3, stroke = 0.8
    ) +
    ggplot2::annotate("text", x = Inf, y = levels(d$display_label)[length(levels(d$display_label))],
                      label = summary_label, hjust = 1.05, vjust = 1.05,
                      size = 3.2, lineheight = 1.05, fontface = "bold") +
    ggplot2::scale_shape_manual(values = shape_values,
                                breaks = names(shape_values)[names(shape_values) %in% as.character(d$shellfish_group)]) +
    ggplot2::scale_colour_manual(values = study_type_colors,
                                 breaks = names(study_type_colors)[names(study_type_colors) %in% as.character(d$study_type)]) +
    ggplot2::scale_y_discrete(labels = function(x) sub("^[0-9]+___", "", x)) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.25))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = unname(response_labels[indicator_name]),
      subtitle = paste0("Study type: ", setting_counts, " | Shellfish group: ", shellfish_counts,
                        ". Solid line = 0", if (s$status == "OK") "; dashed line = pooled g." else "."),
      x = "Effect size (Hedges' g)", y = NULL,
      shape = "Shellfish group", colour = "Study type",
      caption = paste0("Source: final primary workbook; unit = publication x outcome x shellfish x setting.",
                       if (indicator_name == "CO2_flux")
                         " CO2 cluster-robust CR2 BH-FDR P = 0.106." else "")
    ) +
    ggplot2::theme_classic(base_size = 11, base_family = "Microsoft YaHei") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_text(size = 8.5, colour = "black"),
      axis.text.x = ggplot2::element_text(colour = "black"),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(8, 24, 8, 8)
    )
  if (s$status == "OK") {
    p <- p + ggplot2::geom_vline(xintercept = s$Total_Effect,
                                linetype = "dashed", linewidth = 0.55, colour = "black")
  }
  list(plot = p, k = nrow(d))
}

pdf_device <- function(filename, ...) grDevices::cairo_pdf(filename = filename, ...)
save_bundle <- function(plot, stem, width, height, dpi = 600) {
  target <- file.path(output_dir, stem)
  ggplot2::ggsave(paste0(target, ".pdf"), plot = plot, width = width,
                  height = height, units = "in", device = pdf_device,
                  bg = "white", limitsize = FALSE)
  ggplot2::ggsave(paste0(target, ".png"), plot = plot, width = width,
                  height = height, units = "in", dpi = dpi,
                  device = grDevices::png, type = "cairo",
                  bg = "white", limitsize = FALSE)
  ggplot2::ggsave(paste0(target, ".tiff"), plot = plot, width = width,
                  height = height, units = "in", dpi = dpi,
                  device = grDevices::tiff, compression = "lzw", type = "cairo",
                  bg = "white", limitsize = FALSE)
}

for (i in seq_along(valid_indicators)) {
  indicator_name <- valid_indicators[[i]]
  result <- make_indicator_plot(indicator_name)
  stem <- paste0(sprintf("%03d", i), "_", indicator_name)
  save_bundle(result$plot, stem, 10.5, max(4.8, min(22, 2.8 + 0.34 * result$k)))
}

# The overview retains the original faceted composition and includes all 20
# outcomes. The six sparse facets have individual CIs but no pooled line.
overview_dat <- study_es |>
  dplyr::mutate(
    shellfish_group = factor(Bivalve_Group, levels = names(shape_values)),
    study_type = factor(Study_Type, levels = names(study_type_colors)),
    indicator = factor(Response_Variable, levels = valid_indicators,
                       labels = unname(response_labels[valid_indicators]))
  ) |>
  dplyr::arrange(indicator, study_type, shellfish_group, Hedges_g) |>
  dplyr::group_by(indicator) |>
  dplyr::mutate(local_order = dplyr::row_number(),
                y_key = paste(indicator, sprintf("%04d", local_order), study_label, sep = "___")) |>
  dplyr::ungroup()
overview_dat$y_key <- factor(overview_dat$y_key, levels = rev(unique(overview_dat$y_key)))
overview_summary <- meta_summary |>
  dplyr::filter(status == "OK") |>
  dplyr::mutate(indicator = factor(Response_Variable, levels = valid_indicators,
                                   labels = unname(response_labels[valid_indicators])))
overview_plot <- ggplot2::ggplot(overview_dat) +
  ggplot2::geom_vline(xintercept = 0, linewidth = 0.4, colour = "black") +
  ggplot2::geom_vline(data = overview_summary,
                      ggplot2::aes(xintercept = Total_Effect),
                      inherit.aes = FALSE, linetype = "dashed",
                      linewidth = 0.45, colour = "black") +
  ggplot2::geom_segment(ggplot2::aes(x = CI_lower, xend = CI_upper,
                                     y = y_key, yend = y_key),
                        linewidth = 0.42, colour = "black") +
  ggplot2::geom_point(ggplot2::aes(x = Hedges_g, y = y_key,
                                   shape = shellfish_group, colour = study_type),
                      size = 2.5, stroke = 0.6) +
  ggplot2::facet_wrap(~ indicator, scales = "free", ncol = 3) +
  ggplot2::scale_y_discrete(labels = function(x) sub("^.*___[0-9]+___", "", x)) +
  ggplot2::scale_shape_manual(values = shape_values, limits = names(shape_values), drop = FALSE) +
  ggplot2::scale_colour_manual(values = study_type_colors, limits = names(study_type_colors), drop = FALSE) +
  ggplot2::labs(
    title = "Coastal shellfish: primary outcomes",
    subtitle = "Twenty outcomes; 14 pooled by Sidik-Jonkman/Knapp-Hartung; six with k < 3 shown descriptively.",
    x = "Effect size (Hedges' g)", y = NULL,
    shape = "Shellfish group", colour = "Study type",
    caption = "Dashed line = pooled g where k >= 3. Each point represents an aggregated analytical unit."
  ) +
  ggplot2::theme_classic(base_size = 9, base_family = "Microsoft YaHei") +
  ggplot2::theme(
    strip.text = ggplot2::element_text(face = "bold", size = 10),
    axis.text.y = ggplot2::element_text(size = 6.8, colour = "black"),
    axis.text.x = ggplot2::element_text(size = 7.5, colour = "black"),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold", size = 15),
    panel.spacing = grid::unit(1.0, "lines")
  )
save_bundle(overview_plot, "ALL_INDICATORS_faceted_overview", 16,
            max(7, 4.4 * ceiling(length(valid_indicators) / 3)), dpi = 300)
capture.output(sessionInfo(), file = file.path(output_dir, "analysis_sessionInfo.txt"))
message("Forest plots complete: 20 outcomes, 14 pooled, 6 descriptive.")
