# ============================================================
# 双壳贝类生态效应：山脊森林图（Ridgeline Forest Plot）
#
# 输出：
#   Figure_A_nutrients_N_removal.{png,pdf,svg,tiff}
#   Figure_B_greenhouse_gases.{png,pdf,svg,tiff}
#   Figure_C_particles_oxygen.{png,pdf,svg,tiff}
#   Figure_ABC_ridgeline_forest.{png,pdf,svg,tiff}
#   meta_df_study_level.csv
#   meta_summary_total_effects.csv
#   data_exclusion_audit.csv
#   analysis_sessionInfo.txt
#
# 统计流程：
#   原始实验/对照均值、SD、n -> Hedges' g -> 研究内固定效应合并
#   -> 指标级 Sidik-Jonkman + Hartung-Knapp 随机效应模型
#   -> Benjamini-Hochberg FDR 校正
# ============================================================

# -------------------- 0. 用户设置 --------------------
input_file <- "D:/2026/AA综述图片汇总/meta分析提取数据/New-Stata extraction-LCH03_原文核查修订版_20260911.xlsx"
input_sheet <- "Merge"
output_dir <- "D:/2026/AA综述图片汇总/meta山脊图/山脊图03"

# TRUE：自动安装缺失包；正式复现时建议使用 renv 锁定版本。
install_missing_packages <- TRUE

# 至少3个研究层面效应量才估计密度和总体随机效应。
minimum_k <- 3L

# 是否在控制台额外运行模拟数据示例。
run_mock_example <- FALSE

# 随机种子只用于模拟数据和点的纵向抖动，不改变真实分析结果。
set.seed(20260911)

# -------------------- 1. 安装并加载包 --------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr", "readr",
  "tibble", "ggplot2", "ggridges", "patchwork", "metafor", "scales",
  "svglite", "ragg", "jsonlite"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  if (!isTRUE(install_missing_packages)) {
    stop(
      "缺少R包：", paste(missing_packages, collapse = ", "),
      "\n请先运行 install.packages(c(",
      paste(sprintf("\"%s\"", missing_packages), collapse = ", "), "))."
    )
  }
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------- 2. 固定颜色、形状和标签 --------------------
bivalve_colors <- c(
  "Oyster"  = "#3E78B2",
  "Mussels" = "#4F9D69",
  "Clam"    = "#D9A928",
  "Scallop" = "#8C6BB1"
)

study_shapes <- c(
  "Aquaculture" = 16,
  "Reef"        = 17,
  "Mesocosm"    = 15
)

response_labels <- c(
  "NH4_flux" = "NH₄⁺ flux",
  "NH4_concentration" = "NH₄⁺ concentration",
  "NH4_unspecified" = "NH₄⁺ response (unspecified)",
  "PO4_flux" = "Reactive P flux",
  "PO4_concentration" = "Reactive P concentration",
  "PO4_unspecified" = "Reactive P response (unspecified)",
  "NOx_flux" = "NOₓ flux",
  "NOx_concentration" = "NOₓ concentration",
  "NOx_unspecified" = "NOₓ response (unspecified)",
  "NO2_flux" = "NO₂⁻ flux",
  "NO2_concentration" = "NO₂⁻ concentration",
  "NO2_unspecified" = "NO₂⁻ response (unspecified)",
  "NO3_flux" = "NO₃⁻ flux",
  "NO3_concentration" = "NO₃⁻ concentration",
  "NO3_unspecified" = "NO₃⁻ response (unspecified)",
  "N2_flux" = "N₂ flux / production",
  "CO2_flux" = "CO₂ flux",
  "CO2_concentration" = "CO₂ concentration",
  "CH4_flux" = "CH₄ flux",
  "CH4_concentration" = "CH₄ concentration",
  "N2O_flux" = "N₂O flux",
  "N2O_concentration" = "N₂O concentration",
  "POC_concentration" = "POC concentration",
  "POC_content" = "Sediment/particulate POC",
  "PON_concentration" = "PON concentration",
  "PON_content" = "Sediment/particulate PON",
  "O2_flux" = "O₂ flux",
  "O2_concentration" = "O₂ concentration",
  "O2_unspecified" = "O₂ response (unspecified)"
)

# -------------------- 3. 文本和类别标准化 --------------------
normalize_text <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_na("") |>
    stringr::str_replace_all(c(
      "₂" = "2", "₃" = "3", "₄" = "4",
      "²" = "2", "³" = "3", "⁺" = "+", "⁻" = "-",
      "μ" = "U", "µ" = "U",
      "−" = "-", "–" = "-", "—" = "-",
      "（" = "(", "）" = ")"
    )) |>
    stringr::str_squish() |>
    stringr::str_to_upper()
}

normalize_bivalve <- function(x) {
  z <- normalize_text(x)
  dplyr::case_when(
    stringr::str_detect(z, "OYSTER|牡蛎") ~ "Oyster",
    stringr::str_detect(z, "MUSSEL|贻贝") ~ "Mussels",
    stringr::str_detect(z, "CLAM|蛤") ~ "Clam",
    stringr::str_detect(z, "SCALLOP|扇贝") ~ "Scallop",
    TRUE ~ NA_character_
  )
}

normalize_study_type <- function(x) {
  z <- normalize_text(x)
  dplyr::case_when(
    stringr::str_detect(z, "^AQUACULTURE$|养殖") ~ "Aquaculture",
    stringr::str_detect(z, "^REEF$|礁") ~ "Reef",
    stringr::str_detect(z, "^MESOCOSM$|中尺度|围隔") ~ "Mesocosm",
    TRUE ~ NA_character_
  )
}

canonical_response <- function(standardized, original) {
  zs <- normalize_text(standardized)
  zr <- normalize_text(original)
  z <- paste(zs, zr)

  raw_flux <- stringr::str_detect(
    zr,
    "FLUX|通量|/M2|M-2|M\\^?-?2|M2/H|M2/D|M2/DAY"
  )
  raw_conc <- stringr::str_detect(
    zr,
    "CONCENTRATION|浓度|MG/L|UG/L|UMOL/L|MMOL/L|NMOL/L|\\bUM\\b|\\bMM\\b"
  )
  std_flux <- stringr::str_detect(zs, "FLUX|通量")
  std_conc <- stringr::str_detect(zs, "CONCENTRATION|浓度")

  is_flux <- raw_flux | (!raw_conc & std_flux & !std_conc)
  is_conc <- raw_conc | (!raw_flux & std_conc & !std_flux)

  is_n2o <- stringr::str_detect(z, "N2O|NITROUS OXIDE")
  is_ch4 <- stringr::str_detect(z, "CH4|METHANE")
  is_co2 <- stringr::str_detect(z, "CO2|CARBON DIOXIDE")
  is_nh4 <- stringr::str_detect(z, "NH4|AMMONIUM")
  is_nox <- stringr::str_detect(z, "NOX|NO2\\s*\\+\\s*NO3|NO3\\s*\\+\\s*NO2")
  is_no2 <- !is_nox & stringr::str_detect(z, "NO2|NITRITE")
  is_no3 <- !is_nox & stringr::str_detect(z, "NO3|NITRATE")
  is_n2 <- !is_n2o & stringr::str_detect(
    z,
    "DENITRIFICATION|N2 PRODUCTION|(^|[^A-Z0-9])N2([^A-Z0-9]|$)"
  )
  is_po4 <- stringr::str_detect(z, "PO4|PHOSPHATE|DIP|SRP|HPO4")
  is_poc <- stringr::str_detect(z, "POC|PARTICULATE.*ORGANIC.*CARBON")
  is_pon <- stringr::str_detect(z, "PON|PARTICULATE.*ORGANIC.*NITROGEN")
  is_o2 <- !is_co2 & !is_n2o & stringr::str_detect(
    z,
    "(^|[^A-Z0-9])O2([^A-Z0-9]|$)|OXYGEN"
  )

  dplyr::case_when(
    is_n2o & is_flux ~ "N2O_flux",
    is_n2o & is_conc ~ "N2O_concentration",
    is_n2o ~ "N2O_flux",
    is_ch4 & is_flux ~ "CH4_flux",
    is_ch4 & is_conc ~ "CH4_concentration",
    is_ch4 ~ "CH4_flux",
    is_co2 & is_flux ~ "CO2_flux",
    is_co2 & is_conc ~ "CO2_concentration",
    is_co2 ~ "CO2_flux",
    is_nh4 & is_flux ~ "NH4_flux",
    is_nh4 & is_conc ~ "NH4_concentration",
    is_nh4 ~ "NH4_unspecified",
    is_nox & is_flux ~ "NOx_flux",
    is_nox & is_conc ~ "NOx_concentration",
    is_nox ~ "NOx_unspecified",
    is_no2 & is_flux ~ "NO2_flux",
    is_no2 & is_conc ~ "NO2_concentration",
    is_no2 ~ "NO2_unspecified",
    is_no3 & is_flux ~ "NO3_flux",
    is_no3 & is_conc ~ "NO3_concentration",
    is_no3 ~ "NO3_unspecified",
    is_n2 ~ "N2_flux",
    is_po4 & is_flux ~ "PO4_flux",
    is_po4 & is_conc ~ "PO4_concentration",
    is_po4 ~ "PO4_unspecified",
    is_poc & is_conc ~ "POC_concentration",
    is_poc ~ "POC_content",
    is_pon & is_conc ~ "PON_concentration",
    is_pon ~ "PON_content",
    is_o2 & is_flux ~ "O2_flux",
    is_o2 & is_conc ~ "O2_concentration",
    is_o2 ~ "O2_unspecified",
    TRUE ~ NA_character_
  )
}

# -------------------- 4. 工作簿读取和质量筛选 --------------------
prepare_workbook_data <- function(path = input_file, sheet = input_sheet) {
  if (!file.exists(path)) stop("找不到Excel文件：", path)

  raw <- readxl::read_excel(path, sheet = sheet, .name_repair = "unique") |>
    dplyr::select(dplyr::where(~ !all(is.na(.x))))

  required_columns <- c(
    "Study", "author", "year", "Shellfish group", "Study type",
    "Measurement index", "Standardized indicator", "study_id",
    "n.c", "m.c", "sd.c", "n.e", "m.e", "sd.e"
  )
  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0L) {
    stop("Merge工作表缺少必要列：", paste(missing_columns, collapse = ", "))
  }

  optional_columns <- c(
    "Publication ID", "Meta-use recommendation", "Audit status",
    "Audit note", "Compartment", "record_id"
  )
  for (nm in optional_columns) {
    if (!nm %in% names(raw)) raw[[nm]] <- NA_character_
  }

  numeric_columns <- c("n.c", "m.c", "sd.c", "n.e", "m.e", "sd.e")

  cleaned <- raw |>
    dplyr::mutate(
      source_row = dplyr::row_number() + 1L,
      dplyr::across(
        dplyr::all_of(numeric_columns),
        ~ readr::parse_number(
          as.character(.x),
          na = c("", "NA", "N/A", "/", "-", "未报告")
        )
      ),
      Bivalve_Group = normalize_bivalve(.data[["Shellfish group"]]),
      Study_Type = normalize_study_type(.data[["Study type"]]),
      Response_Variable = canonical_response(
        .data[["Standardized indicator"]],
        .data[["Measurement index"]]
      ),
      Study_ID = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(study_id)), ""),
        dplyr::na_if(stringr::str_squish(as.character(.data[["Publication ID"]])), ""),
        dplyr::na_if(stringr::str_squish(as.character(Study)), ""),
        paste0(
          dplyr::coalesce(as.character(author), "Unknown"), "_",
          dplyr::coalesce(as.character(year), "n.d."), "_row", source_row
        )
      ),
      meta_use_text = stringr::str_squish(
        dplyr::coalesce(as.character(.data[["Meta-use recommendation"]]), "")
      ),
      excluded_by_audit = stringr::str_detect(
        meta_use_text,
        "缺SD|暂不纳入|敏感性分析中剔除|淡水体系|从净N2主分析中排除"
      ),
      exclusion_reason = dplyr::case_when(
        is.na(Bivalve_Group) ~ "无法识别双壳贝类类群",
        is.na(Study_Type) ~ "无法识别研究类型",
        is.na(Response_Variable) ~ "不属于本山脊图预设指标",
        is.na(n.c) | is.na(m.c) | is.na(sd.c) |
          is.na(n.e) | is.na(m.e) | is.na(sd.e) ~ "缺少n、均值或SD",
        n.c <= 1 | n.e <= 1 ~ "样本量小于或等于1",
        sd.c <= 0 | sd.e <= 0 ~ "SD小于或等于0",
        excluded_by_audit ~ paste0("审核建议排除：", meta_use_text),
        TRUE ~ NA_character_
      ),
      include_main = is.na(exclusion_reason)
    )

  readr::write_csv(
    cleaned |>
      dplyr::filter(!include_main) |>
      dplyr::select(
        source_row, record_id, Study_ID, Study, author, year,
        Bivalve_Group, Study_Type, `Measurement index`,
        `Standardized indicator`, Response_Variable,
        dplyr::all_of(numeric_columns),
        `Audit status`, `Meta-use recommendation`, exclusion_reason
      ),
    file.path(output_dir, "data_exclusion_audit.csv")
  )

  analysis_rows <- cleaned |>
    dplyr::filter(include_main) |>
    dplyr::mutate(
      Bivalve_Group = factor(
        Bivalve_Group,
        levels = c("Oyster", "Mussels", "Clam", "Scallop")
      ),
      Study_Type = factor(
        Study_Type,
        levels = c("Aquaculture", "Reef", "Mesocosm")
      )
    )

  if (nrow(analysis_rows) == 0L) {
    stop("质量筛选后没有可用于效应量计算的数据。")
  }

  list(raw = raw, cleaned = cleaned, analysis_rows = analysis_rows)
}

# -------------------- 5. Hedges' g和研究内合并 --------------------
pool_within_study <- function(d) {
  if (nrow(d) == 1L) {
    return(tibble::tibble(
      Hedges_g = d$yi[[1]],
      SE = sqrt(d$vi[[1]]),
      CI_lower = d$yi[[1]] - 1.96 * sqrt(d$vi[[1]]),
      CI_upper = d$yi[[1]] + 1.96 * sqrt(d$vi[[1]]),
      Weight = 1 / d$vi[[1]],
      n_source_rows = 1L
    ))
  }

  fit <- tryCatch(
    metafor::rma.uni(yi = d$yi, vi = d$vi, method = "FE"),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    w <- 1 / d$vi
    pooled_g <- sum(w * d$yi) / sum(w)
    pooled_vi <- 1 / sum(w)
    return(tibble::tibble(
      Hedges_g = pooled_g,
      SE = sqrt(pooled_vi),
      CI_lower = pooled_g - 1.96 * sqrt(pooled_vi),
      CI_upper = pooled_g + 1.96 * sqrt(pooled_vi),
      Weight = 1 / pooled_vi,
      n_source_rows = nrow(d)
    ))
  }

  tibble::tibble(
    Hedges_g = as.numeric(fit$b),
    SE = as.numeric(fit$se),
    CI_lower = as.numeric(fit$ci.lb),
    CI_upper = as.numeric(fit$ci.ub),
    Weight = 1 / as.numeric(fit$se)^2,
    n_source_rows = nrow(d)
  )
}

make_study_level_meta_df <- function(analysis_rows) {
  row_effects <- metafor::escalc(
    measure = "SMD",
    m1i = m.e,
    sd1i = sd.e,
    n1i = n.e,
    m2i = m.c,
    sd2i = sd.c,
    n2i = n.c,
    data = analysis_rows,
    append = TRUE
  ) |>
    dplyr::filter(is.finite(yi), is.finite(vi), vi > 0)

  meta_df <- row_effects |>
    dplyr::group_by(
      Response_Variable, Study_ID, Bivalve_Group, Study_Type
    ) |>
    dplyr::group_modify(~ pool_within_study(.x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(Response_Variable, Study_ID)

  readr::write_csv(
    meta_df,
    file.path(output_dir, "meta_df_study_level.csv")
  )

  meta_df
}

# -------------------- 6. 指标级总体Meta分析和FDR --------------------
fit_total_effect <- function(d, minimum_k = minimum_k) {
  k <- nrow(d)
  if (k < minimum_k) {
    return(tibble::tibble(
      k = k,
      Total_Effect = NA_real_,
      CI_lower = NA_real_,
      CI_upper = NA_real_,
      p_value = NA_real_,
      tau2 = NA_real_,
      I2 = NA_real_,
      status = paste0("Descriptive only: k < ", minimum_k)
    ))
  }

  fit <- tryCatch(
    metafor::rma.uni(
      yi = d$Hedges_g,
      sei = d$SE,
      method = "SJ",
      test = "knha"
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble::tibble(
      k = k,
      Total_Effect = NA_real_,
      CI_lower = NA_real_,
      CI_upper = NA_real_,
      p_value = NA_real_,
      tau2 = NA_real_,
      I2 = NA_real_,
      status = "Model failed"
    ))
  }

  tibble::tibble(
    k = k,
    Total_Effect = as.numeric(fit$b),
    CI_lower = as.numeric(fit$ci.lb),
    CI_upper = as.numeric(fit$ci.ub),
    p_value = as.numeric(fit$pval),
    tau2 = as.numeric(fit$tau2),
    I2 = as.numeric(fit$I2),
    status = "OK"
  )
}

make_meta_summary <- function(meta_df) {
  meta_summary <- meta_df |>
    dplyr::group_by(Response_Variable) |>
    dplyr::group_modify(~ fit_total_effect(.x, minimum_k = minimum_k)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      p_fdr = stats::p.adjust(p_value, method = "BH"),
      result_label = dplyr::case_when(
        status == "OK" & !is.na(p_fdr) ~ paste0(
          "k=", k,
          "; g=", sprintf("%.2f", Total_Effect),
          "; FDR P=", dplyr::if_else(
            p_fdr < 0.001,
            "<0.001",
            sprintf("%.3f", p_fdr)
          )
        ),
        TRUE ~ paste0("k=", k, "; descriptive")
      )
    )

  readr::write_csv(
    meta_summary,
    file.path(output_dir, "meta_summary_total_effects.csv")
  )

  meta_summary
}

# -------------------- 7. 模拟数据 --------------------
make_mock_meta_df <- function(seed = 20260911) {
  set.seed(seed)

  response_pool <- c(
    "NH4_flux", "PO4_flux", "NOx_flux", "NO2_flux", "NO3_flux", "N2_flux",
    "CO2_flux", "CH4_flux", "N2O_flux",
    "POC_content", "PON_content", "O2_flux"
  )

  mock <- tidyr::expand_grid(
    Response_Variable = response_pool,
    replicate_id = seq_len(7)
  ) |>
    dplyr::filter(
      !(Response_Variable == "CH4_flux" & replicate_id > 2),
      !(Response_Variable == "POC_content" & replicate_id > 1)
    ) |>
    dplyr::mutate(
      Study_ID = paste0("Mock_", sprintf("%03d", dplyr::row_number())),
      Bivalve_Group = sample(
        c("Oyster", "Mussels", "Clam", "Scallop"),
        dplyr::n(), replace = TRUE
      ),
      Study_Type = sample(
        c("Aquaculture", "Reef", "Mesocosm"),
        dplyr::n(), replace = TRUE,
        prob = c(0.60, 0.25, 0.15)
      ),
      response_shift = dplyr::case_when(
        Response_Variable == "N2_flux" ~ 0.65,
        Response_Variable == "NH4_flux" ~ 0.45,
        Response_Variable == "CO2_flux" ~ 0.30,
        Response_Variable == "O2_flux" ~ -0.35,
        TRUE ~ 0
      ),
      Hedges_g = stats::rnorm(dplyr::n(), response_shift, 0.55),
      SE = stats::runif(dplyr::n(), 0.12, 0.35),
      CI_lower = Hedges_g - 1.96 * SE,
      CI_upper = Hedges_g + 1.96 * SE,
      Weight = 1 / SE^2
    ) |>
    dplyr::select(
      Study_ID, Response_Variable, Bivalve_Group, Study_Type,
      Hedges_g, CI_lower, CI_upper, SE, Weight
    ) |>
    dplyr::mutate(
      Bivalve_Group = factor(
        Bivalve_Group,
        levels = c("Oyster", "Mussels", "Clam", "Scallop")
      ),
      Study_Type = factor(
        Study_Type,
        levels = c("Aquaculture", "Reef", "Mesocosm")
      )
    )

  mock
}

# -------------------- 8. 山脊森林图函数 --------------------
panel_filter <- function(meta_df, panel = c("A", "B", "C")) {
  panel <- match.arg(panel)

  keep <- switch(
    panel,
    A = stringr::str_detect(
      meta_df$Response_Variable,
      "^(NH4|PO4|NOx|NO2|NO3|N2)_"
    ) & !stringr::str_detect(meta_df$Response_Variable, "^N2O_"),
    B = stringr::str_detect(
      meta_df$Response_Variable,
      "^(CO2|CH4|N2O)_"
    ),
    C = stringr::str_detect(
      meta_df$Response_Variable,
      "^(POC|PON|O2)_"
    )
  )

  meta_df[keep, , drop = FALSE]
}

plot_ridgeline_panel <- function(
    meta_df,
    meta_summary,
    panel = c("A", "B", "C"),
    title,
    panel_letter
) {
  panel <- match.arg(panel)
  d <- panel_filter(meta_df, panel)
  s <- panel_filter(meta_summary, panel)

  if (nrow(d) == 0L) {
    stop("图", panel, "没有符合筛选条件的数据。")
  }

  response_order <- d |>
    dplyr::distinct(Response_Variable) |>
    dplyr::pull(Response_Variable)

  d <- d |>
    dplyr::mutate(
      Response_Variable = factor(
        Response_Variable,
        levels = rev(response_order)
      )
    )
  s <- s |>
    dplyr::mutate(
      Response_Variable = factor(
        Response_Variable,
        levels = levels(d$Response_Variable)
      )
    )

  density_data <- d |>
    dplyr::add_count(Response_Variable, Bivalve_Group, name = "k_group") |>
    dplyr::filter(k_group >= minimum_k)

  small_k <- s |>
    dplyr::filter(k < minimum_k) |>
    dplyr::left_join(
      d |>
        dplyr::group_by(Response_Variable) |>
        dplyr::summarise(
          x_min = min(CI_lower, na.rm = TRUE),
          x_max = max(CI_upper, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "Response_Variable"
    ) |>
    dplyr::mutate(same_limit = x_min == x_max) |>
    dplyr::mutate(
      x_min = dplyr::if_else(same_limit, x_min - 0.20, x_min),
      x_max = dplyr::if_else(same_limit, x_max + 0.20, x_max)
    ) |>
    dplyr::select(-same_limit)

  total_effects <- s |>
    dplyr::filter(status == "OK", is.finite(Total_Effect))

  jitter_position <- ggplot2::position_jitter(
    width = 0,
    height = 0.075,
    seed = 20260911
  )

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(x = Hedges_g, y = Response_Variable)
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "#555555"
    )

  if (nrow(density_data) > 0L) {
    p <- p +
      ggridges::geom_density_ridges(
        data = density_data,
        ggplot2::aes(
          fill = Bivalve_Group,
          group = interaction(Response_Variable, Bivalve_Group)
        ),
        alpha = 0.34,
        colour = scales::alpha("#333333", 0.45),
        linewidth = 0.30,
        scale = 0.78,
        rel_min_height = 0.02,
        from = min(d$CI_lower, na.rm = TRUE),
        to = max(d$CI_upper, na.rm = TRUE),
        show.legend = TRUE
      )
  }

  if (nrow(small_k) > 0L) {
    p <- p +
      ggplot2::geom_segment(
        data = small_k,
        ggplot2::aes(
          x = x_min, xend = x_max,
          y = Response_Variable, yend = Response_Variable
        ),
        inherit.aes = FALSE,
        colour = "#B8B8B8",
        linewidth = 2.2,
        alpha = 0.65,
        lineend = "round"
      )
  }

  p <- p +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = CI_lower,
        xmax = CI_upper,
        colour = Bivalve_Group,
        group = interaction(Study_ID, Response_Variable)
      ),
      height = 0,
      linewidth = 0.38,
      alpha = 0.75,
      position = jitter_position
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        colour = Bivalve_Group,
        shape = Study_Type,
        size = Weight
      ),
      alpha = 0.88,
      stroke = 0.40,
      position = jitter_position
    )

  if (nrow(total_effects) > 0L) {
    p <- p +
      ggplot2::geom_errorbarh(
        data = total_effects,
        ggplot2::aes(
          xmin = CI_lower,
          xmax = CI_upper,
          y = Response_Variable
        ),
        inherit.aes = FALSE,
        height = 0,
        linewidth = 1.05,
        colour = "black"
      ) +
      ggplot2::geom_point(
        data = total_effects,
        ggplot2::aes(
          x = Total_Effect,
          y = Response_Variable
        ),
        inherit.aes = FALSE,
        shape = 23,
        size = 3.8,
        stroke = 0.75,
        fill = "white",
        colour = "black"
      )
  }

  p +
    ggplot2::geom_text(
      data = s,
      ggplot2::aes(
        x = Inf,
        y = Response_Variable,
        label = result_label
      ),
      inherit.aes = FALSE,
      hjust = 1.03,
      vjust = -0.62,
      size = 2.5,
      colour = "#333333"
    ) +
    ggplot2::scale_fill_manual(
      name = "Bivalve group",
      values = bivalve_colors,
      drop = FALSE
    ) +
    ggplot2::scale_colour_manual(
      name = "Bivalve group",
      values = bivalve_colors,
      drop = FALSE
    ) +
    ggplot2::scale_shape_manual(
      name = "Study type",
      values = study_shapes,
      drop = FALSE
    ) +
    ggplot2::scale_size_continuous(
      name = "Inverse-variance weight",
      range = c(1.5, 3.6),
      breaks = scales::breaks_pretty(n = 3)
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) {
        out <- unname(response_labels[x])
        out[is.na(out)] <- x[is.na(out)]
        out
      }
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(0.06, 0.24))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = title,
      subtitle = paste0(
        "Study-level Hedges' g; diamond = total effect when k ≥ ", minimum_k,
        "; grey ridge = descriptive only"
      ),
      x = "Effect size (Hedges' g)",
      y = NULL,
      caption = "Ridges show subgroup density only when at least 3 study-level effects are available."
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, override.aes = list(alpha = 0.55)),
      colour = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 2),
      size = ggplot2::guide_legend(order = 3)
    ) +
    ggplot2::theme_classic(base_size = 9, base_family = "sans") +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        colour = "black",
        size = 8.2,
        margin = ggplot2::margin(r = 5)
      ),
      axis.text.x = ggplot2::element_text(colour = "black", size = 8),
      axis.title.x = ggplot2::element_text(size = 9),
      plot.title = ggplot2::element_text(face = "bold", size = 11),
      plot.subtitle = ggplot2::element_text(size = 7.8, colour = "#444444"),
      plot.caption = ggplot2::element_text(size = 7, colour = "#555555", hjust = 0),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = ggplot2::element_text(face = "bold", size = 8),
      legend.text = ggplot2::element_text(size = 7.5),
      panel.grid.major.x = ggplot2::element_line(
        colour = "#E7E7E7",
        linewidth = 0.25
      ),
      plot.tag = ggplot2::element_text(face = "bold", size = 12),
      plot.tag.position = c(0.01, 0.99),
      plot.margin = ggplot2::margin(8, 44, 8, 8)
    ) +
    ggplot2::labs(tag = panel_letter)
}

# -------------------- 9. 导出函数 --------------------
svg_device <- function(filename, ...) {
  svglite::svglite(file = filename, ...)
}

pdf_device <- function(filename, ...) {
  grDevices::cairo_pdf(filename = filename, ...)
}

save_plot_bundle <- function(plot, stem, width, height, dpi = 600) {
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(stem, ".pdf")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = pdf_device,
    bg = "white",
    limitsize = FALSE
  )
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(stem, ".png")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(stem, ".svg")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = svg_device,
    bg = "white",
    limitsize = FALSE
  )
  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(stem, ".tiff")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    device = ragg::agg_tiff,
    compression = "lzw",
    bg = "white",
    limitsize = FALSE
  )
}

# -------------------- 10. 真实数据分析和绘图 --------------------
prepared <- prepare_workbook_data()
meta_df <- make_study_level_meta_df(prepared$analysis_rows)
meta_summary <- make_meta_summary(meta_df)

pA <- plot_ridgeline_panel(
  meta_df, meta_summary,
  panel = "A",
  title = "Nutrients and nitrogen removal",
  panel_letter = "A"
)

pB <- plot_ridgeline_panel(
  meta_df, meta_summary,
  panel = "B",
  title = "Greenhouse gases",
  panel_letter = "B"
)

pC <- plot_ridgeline_panel(
  meta_df, meta_summary,
  panel = "C",
  title = "Particulate matter and oxygen",
  panel_letter = "C"
)

pABC <- (pA / pB / pC) +
  patchwork::plot_layout(
    heights = c(1.35, 0.85, 1.0),
    guides = "collect"
  ) &
  ggplot2::theme(legend.position = "bottom")

# 组合图导出前执行面板对齐审计；辅助文件与本脚本放在同一输出目录。
alignment_helper <- file.path(output_dir, "panel_alignment.R")
alignment_auditor <- file.path(output_dir, "audit_panel_alignment.py")
python_exe <- Sys.which("python")
if (!nzchar(python_exe)) python_exe <- Sys.which("python3")

if (
  file.exists(alignment_helper) &&
  file.exists(alignment_auditor) &&
  nzchar(python_exe)
) {
  source(alignment_helper, local = TRUE)
  require_patchwork_panel_alignment(
    plot = pABC,
    manifest_path = file.path(output_dir, "Figure_ABC_panel_manifest.json"),
    report_path = file.path(output_dir, "Figure_ABC_panel_alignment_report.md"),
    overlay_svg = file.path(output_dir, "Figure_ABC_panel_alignment_overlay.svg"),
    width_in = 11.2,
    height_in = 17.2,
    panel_ids = c("A", "B", "C"),
    column_groups = list(c("A", "B", "C")),
    audit_script = alignment_auditor,
    python = python_exe,
    strict = TRUE
  )
} else {
  warning(
    "未执行自动面板对齐审计：缺少 panel_alignment.R、",
    "audit_panel_alignment.py 或 Python；不影响单图生成。"
  )
}

save_plot_bundle(pA, "Figure_A_nutrients_N_removal", width = 10.5, height = 7.4)
save_plot_bundle(pB, "Figure_B_greenhouse_gases", width = 10.5, height = 5.2)
save_plot_bundle(pC, "Figure_C_particles_oxygen", width = 10.5, height = 6.0)
save_plot_bundle(pABC, "Figure_ABC_ridgeline_forest", width = 11.2, height = 17.2)

capture.output(
  sessionInfo(),
  file = file.path(output_dir, "analysis_sessionInfo.txt")
)

message("分析完成。输出目录：", normalizePath(output_dir, winslash = "/"))
message("研究层面效应量：", nrow(meta_df))
message("达到 k ≥ ", minimum_k, " 的指标：", sum(meta_summary$status == "OK"))
message("k < ", minimum_k, " 的描述性指标：", sum(meta_summary$k < minimum_k))

# -------------------- 11. 可选模拟数据测试 --------------------
if (isTRUE(run_mock_example)) {
  mock_meta_df <- make_mock_meta_df()
  readr::write_csv(
    mock_meta_df,
    file.path(output_dir, "mock_meta_df.csv")
  )

  mock_summary <- make_meta_summary(mock_meta_df)
  mock_A <- plot_ridgeline_panel(
    mock_meta_df, mock_summary,
    panel = "A",
    title = "Mock data: nutrients and nitrogen removal",
    panel_letter = "A"
  )
  print(mock_A)
}
