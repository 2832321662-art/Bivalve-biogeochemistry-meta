# Revised from the original three-panel ridgeline forest plot.
# Run with Rscript from any directory; workbook and helper live beside this file.
# Figure data: 669 author-adjudicated comparisons -> 146 analytical units.
# The 20 outcomes are the current coastal primary model; C:N is not included.

if (.Platform$OS.type == "windows" && identical(Sys.getlocale("LC_CTYPE"), "C")) {
  suppressWarnings(Sys.setlocale("LC_CTYPE", "Chinese (Simplified)_China.936"))
}
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_file <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else ""
script_dir <- if (file.exists("meta_final_plot_data.R")) {
  "."
} else if (nzchar(script_file)) {
  dirname(script_file)
} else getwd()
source(file.path(script_dir, "meta_final_plot_data.R"), encoding = "UTF-8")
plot_require_packages(c("ggplot2", "ggridges", "patchwork", "scales", "jsonlite"))

input_file <- Sys.getenv("META_FINAL_WORKBOOK",
                         file.path(script_dir, "meta_analysis_final_workbook.xlsx"))
output_dir <- file.path(script_dir, "ridgeline_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
minimum_k <- 3L
set.seed(20260911)

prepared <- plot_prepare_data(input_file,
                              file.path(script_dir, "meta_summary_final.csv"))
meta_df <- prepared$units
meta_summary <- prepared$summary
utils::write.csv(meta_df, file.path(output_dir, "aggregated_units_for_plot.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
utils::write.csv(meta_summary, file.path(output_dir, "outcome_models_for_plot.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

# Preserve the original figure's group palette and setting symbols.
bivalve_colors <- c(Oyster = "#3E78B2", Mussels = "#4F9D69",
                    Clam = "#D9A928", Scallop = "#8C6BB1")
study_shapes <- c(Aquaculture = 16, Reef = 17, Mesocosm = 15)
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
panel_order <- list(
  A = c("NH4_concentration", "NO3_concentration", "NO2_concentration",
        "NOx_concentration", "PO4_concentration", "N2_flux", "NH4_flux",
        "NO2_flux", "NO3_flux", "NOx_flux", "PO4_flux"),
  B = c("CH4_flux", "CO2_flux", "N2O_flux"),
  C = c("POC_content", "PON_content", "O2_concentration",
        "POC_concentration", "PON_concentration", "O2_flux")
)
stopifnot(setequal(unlist(panel_order, use.names = FALSE), unique(meta_df$response)))

plot_ridgeline_panel <- function(panel, title) {
  order <- panel_order[[panel]]
  d <- meta_df[meta_df$Response_Variable %in% order, , drop = FALSE]
  s <- meta_summary[meta_summary$Response_Variable %in% order, , drop = FALSE]
  d$Response_Variable <- factor(d$Response_Variable, levels = rev(order))
  s$Response_Variable <- factor(s$Response_Variable, levels = rev(order))
  d$Bivalve_Group <- factor(d$Bivalve_Group, levels = names(bivalve_colors))
  d$Study_Type <- factor(d$Study_Type, levels = names(study_shapes))

  density_data <- d |>
    dplyr::add_count(Response_Variable, Bivalve_Group, name = "k_group") |>
    dplyr::filter(k_group >= minimum_k)
  small_k <- s |>
    dplyr::filter(k < minimum_k) |>
    dplyr::left_join(
      d |>
        dplyr::group_by(Response_Variable) |>
        dplyr::summarise(x_min = min(CI_lower), x_max = max(CI_upper), .groups = "drop"),
      by = "Response_Variable"
    )
  total_effects <- s |>
    dplyr::filter(status == "OK", is.finite(Total_Effect))
  s$plot_label <- ifelse(
    s$status == "OK",
    paste0("k=", s$k, ", pubs=", s$n_pub,
           ", g=", sprintf("%.2f", s$Total_Effect),
           ", q", ifelse(s$p_fdr < 0.001, "<0.001",
                          paste0("=", sprintf("%.3f", s$p_fdr)))),
    paste0("k=", s$k, ", pubs=", s$n_pub, "; descriptive")
  )
  jitter_position <- ggplot2::position_jitter(width = 0, height = 0.065,
                                               seed = 20260911)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = Hedges_g, y = Response_Variable)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45,
                        colour = "#555555")
  if (nrow(density_data)) {
    p <- p + ggridges::geom_density_ridges(
      data = density_data,
      ggplot2::aes(fill = Bivalve_Group,
                   group = interaction(Response_Variable, Bivalve_Group)),
      alpha = 0.34, colour = scales::alpha("#333333", 0.45),
      linewidth = 0.30, scale = 0.78, rel_min_height = 0.02,
      from = min(d$CI_lower), to = max(d$CI_upper), show.legend = TRUE
    )
  }
  if (nrow(small_k)) {
    p <- p + ggplot2::geom_segment(
      data = small_k,
      ggplot2::aes(x = x_min, xend = x_max,
                   y = Response_Variable, yend = Response_Variable),
      inherit.aes = FALSE, colour = "#B8B8B8", linewidth = 2.0,
      alpha = 0.65, lineend = "round"
    )
  }
  p <- p +
    ggplot2::geom_segment(
      ggplot2::aes(x = CI_lower, xend = CI_upper,
                   yend = Response_Variable, colour = Bivalve_Group,
                   group = interaction(Analytical_Unit_ID, Response_Variable)),
      linewidth = 0.38, alpha = 0.75, position = jitter_position
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = Bivalve_Group, shape = Study_Type, size = Weight),
      alpha = 0.88, stroke = 0.40, position = jitter_position
    )
  if (nrow(total_effects)) {
    p <- p +
      ggplot2::geom_segment(
        data = total_effects,
        ggplot2::aes(x = CI_lower, xend = CI_upper,
                     y = Response_Variable, yend = Response_Variable),
        inherit.aes = FALSE, linewidth = 1.05, colour = "black"
      ) +
      ggplot2::geom_point(
        data = total_effects,
        ggplot2::aes(x = Total_Effect, y = Response_Variable),
        inherit.aes = FALSE, shape = 23, size = 3.6, stroke = 0.75,
        fill = "white", colour = "black"
      )
  }
  caveat <- if (identical(panel, "B")) {
    "CO2 primary-model FDR P = 0.023; publication-cluster CR2 FDR P = 0.106."
  } else {
    "Points are aggregated analytical units; k < 3 is descriptive only."
  }
  p +
    ggplot2::geom_text(
      data = s,
      ggplot2::aes(x = Inf, y = Response_Variable, label = plot_label),
      inherit.aes = FALSE, hjust = -0.08, vjust = -0.55,
      size = 2.5, colour = "#333333"
    ) +
    ggplot2::scale_fill_manual(name = "Bivalve group", values = bivalve_colors,
                               drop = FALSE, guide = "none") +
    ggplot2::scale_colour_manual(name = "Bivalve group", values = bivalve_colors,
                                 drop = FALSE) +
    ggplot2::scale_shape_manual(name = "Study type", values = study_shapes,
                                drop = FALSE) +
    ggplot2::scale_size_continuous(name = "Inverse-variance weight",
                                   range = c(1.5, 3.6),
                                   breaks = scales::breaks_pretty(n = 3),
                                   guide = "none") +
    ggplot2::scale_y_discrete(labels = function(x) unname(response_labels[x])) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.06, 0.24))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title,
      subtitle = paste0("Aggregated analytical-unit Hedges' g; diamond = ",
                        "outcome-specific random-effects estimate when k >= 3"),
      x = "Effect size (Hedges' g)", y = NULL,
      caption = paste0("k = analytical units; n_pub = canonical publications. ", caveat),
      tag = panel
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 2)
    ) +
    ggplot2::theme_classic(base_size = 9, base_family = "sans") +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(colour = "black", size = 8.2,
                                          margin = ggplot2::margin(r = 5)),
      axis.text.x = ggplot2::element_text(colour = "black", size = 8),
      axis.title.x = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 7.8, colour = "#444444"),
      plot.caption = ggplot2::element_text(size = 7, colour = "#555555", hjust = 0),
      legend.position = "bottom", legend.box = "vertical",
      legend.title = ggplot2::element_text(face = "bold", size = 8),
      legend.text = ggplot2::element_text(size = 7.5),
      panel.grid.major.x = ggplot2::element_line(colour = "#E7E7E7",
                                                 linewidth = 0.25),
      plot.tag = ggplot2::element_text(face = "bold", size = 12),
      plot.tag.position = c(0.01, 0.99),
      plot.margin = ggplot2::margin(8, 148, 8, 8)
    )
}

pdf_device <- function(filename, ...) grDevices::cairo_pdf(filename = filename, ...)
save_plot_bundle <- function(plot, stem, width, height, dpi = 600) {
  target <- file.path(output_dir, stem)
  ggplot2::ggsave(paste0(target, ".pdf"), plot = plot,
                  width = width, height = height, units = "in",
                  device = pdf_device, bg = "white", limitsize = FALSE)
  ggplot2::ggsave(paste0(target, ".png"), plot = plot,
                  width = width, height = height, units = "in",
                  dpi = dpi, device = grDevices::png, type = "cairo",
                  bg = "white", limitsize = FALSE)
  ggplot2::ggsave(paste0(target, ".tiff"), plot = plot,
                  width = width, height = height, units = "in",
                  dpi = dpi, device = grDevices::tiff,
                  compression = "lzw", type = "cairo",
                  bg = "white", limitsize = FALSE)
}

pA <- plot_ridgeline_panel("A", "Nutrients and nitrogen exchange")
pB <- plot_ridgeline_panel("B", "Greenhouse gases")
pC <- plot_ridgeline_panel("C", "Particulate matter and oxygen")
pABC <- (pA / pB / pC) +
  patchwork::plot_layout(heights = c(1.35, 0.85, 1.0)) +
  patchwork::plot_annotation(
    caption = paste0("Colors: oyster blue, mussels green, clam yellow, scallop purple. ",
                     "Shapes: aquaculture circle, reef triangle, mesocosm square.")
  ) &
  ggplot2::theme(legend.position = "none")

# Mandatory final-geometry alignment check for the three vertically stacked panels.
source(file.path(script_dir, "panel_alignment.R"), encoding = "UTF-8")
require_patchwork_panel_alignment(
  plot = pABC,
  manifest_path = file.path(output_dir, "Figure_ABC_panel_manifest.json"),
  report_path = file.path(output_dir, "Figure_ABC_panel_alignment.json"),
  overlay_svg = NULL,
  width_in = 11.2, height_in = 17.2,
  panel_ids = c("A", "B", "C"),
  column_groups = list(c("A", "B", "C")),
  audit_script = file.path(script_dir, "audit_panel_alignment.py"),
  python = Sys.getenv("META_PYTHON", "D:/APP/Python/python.exe"),
  strict = TRUE
)

save_plot_bundle(pA, "Figure_A_nutrients_N_exchange", 10.5, 7.4)
save_plot_bundle(pB, "Figure_B_greenhouse_gases", 10.5, 5.2)
save_plot_bundle(pC, "Figure_C_particles_oxygen", 10.5, 6.0)
save_plot_bundle(pABC, "Figure_ABC_ridgeline_forest", 11.2, 17.2)
capture.output(sessionInfo(), file = file.path(output_dir, "analysis_sessionInfo.txt"))
message("Ridgeline complete: 669 comparisons, 146 units, 20 outcomes, 14 pooled.")
