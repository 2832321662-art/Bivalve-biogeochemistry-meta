# ============================================================
# Merge 工作表：四类贝类 × 各指标森林图（基于原文核查修订版）
#
# 功能：
# 1. 直接读取 Excel 中的 Merge 工作表；
# 2. 使用 Shellfish group 列区分四类贝类，并用形状表示；
# 3. 使用 Study type 列区分 Aquaculture、Reef、Mesocosm，并用三种颜色表示；
# 4. 每个指标单独生成一张森林图，风格和配色参考 009_NH4_flux；
# 5. 使用原文核查修订版中的审查列，排除待核实/暂不纳入主分析行；
# 6. 计算 Hedges' g，先在研究内合并重复观测，再进行
#    Sidik-Jonkman + Hartung-Knapp 随机效应 Meta 分析。
# ============================================================
setwd("D:/2026")
# -------------------- 0. 安装并加载包 --------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "purrr",
  "readr", "ggplot2", "metafor", "tibble", "svglite", "ragg"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# -------------------- 1. 用户设置 --------------------
# 修改为你的 Excel 完整路径；找不到时会弹窗选择。
file_path <- "D:/2026/AA综述图片汇总/meta分析提取数据/New-Stata extraction-LCH03_原文核查修订版_20260911.xlsx"

sheet_name <- "Merge"

# 每个指标至少有多少个“研究层面的效应值”才进行随机效应分析和作图。
# 与方法稿保持一致：正式总体随机效应 Meta 分析要求 k >= 3。
min_studies <- 3

# TRUE：同一论文中同一指标的月份、季节、重复处理先固定效应合并，
#       森林图尽量做到“一篇论文一个点”。
# FALSE：每一行原始观测都作为一个点，仅建议用于探索性检查。
combine_repeated_within_study <- TRUE

# TRUE：不同 Station 分别作为研究单元；
# FALSE：同一论文的不同 Station 也在论文内部合并。
separate_stations <- FALSE

# NULL 表示绘制所有达到随机效应作图条件的指标。
# 009_NH4_flux 只作为图形风格和配色参考，不作为指标筛选条件。
indicators_to_plot <- NULL

# 全部指标从 001 开始编号；若排序与旧版一致，NH4 flux 仍会落在对应序号。
output_index_start <- 1

# 是否生成“全部指标放在一张分面图中”的总图。
make_all_indicator_overview <- TRUE
overview_ncol <- 3

# 与方法稿保持一致：缺失、0 或负 SD 不做数值替换，直接排除。
zero_sd_policy <- "exclude"

# TRUE 时仅保留“已按原文修正/已标准化”等更严格核查状态；
# FALSE 时保留修订表中未被标为“待核实/暂不纳入”的原始提取行。
strict_source_checked_only <- FALSE

# 四类贝类形状：三角形、正方形、菱形、圆形。
shape_values <- c(
  "Mussels" = 17,
  "Scallop" = 15,
  "Clam"    = 18,
  "Oyster"  = 16
)

# 三类 Study type 的颜色（色盲友好配色）。
study_type_colors <- c(
  "Aquaculture" = "#EFA68A",
  "Reef"        = "#6E9FD1",
  "Mesocosm"    = "#63BFAE"
)

# 若自动标准化仍未合并某些同义指标，可在这里手工指定：
# 左边必须与 Excel 原始指标文字完全一致，右边是统一后的名称。
indicator_manual_map <- c(
  # "CO₂ flux (µmol/m²/h)" = "CO2 flux",
  # "Carbon dioxide flux"  = "CO2 flux"
)

# -------------------- 2. 路径与工作表检查 --------------------
if (!file.exists(file_path)) {
  message("找不到设置的 Excel 文件，请在弹窗中选择文件。")
  file_path <- file.choose()
}

available_sheets <- readxl::excel_sheets(file_path)
if (!sheet_name %in% available_sheets) {
  stop(
    "Excel 中没有工作表：", sheet_name,
    "\n当前工作表为：", paste(available_sheets, collapse = ", ")
  )
}

output_dir <- "D:/2026/AA综述图片汇总/meta森林图分析绘图/Codex05"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------- 3. 辅助函数 --------------------
pick_column <- function(data_names, candidates, required = TRUE, label = "列") {
  hit <- candidates[candidates %in% data_names]
  if (length(hit) > 0) return(hit[[1]])

  if (required) {
    stop(
      "找不到", label, "。可接受的列名包括：\n",
      paste(candidates, collapse = ", "),
      "\n\nExcel 当前列名为：\n",
      paste(data_names, collapse = ", ")
    )
  }

  NA_character_
}

normalize_shellfish_group <- function(x) {
  z <- x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_to_upper()

  dplyr::case_when(
    stringr::str_detect(z, "^MUSSEL(S)?$|贻贝") ~ "Mussels",
    stringr::str_detect(z, "^SCALLOP(S)?$|扇贝") ~ "Scallop",
    stringr::str_detect(z, "^CLAM(S)?$|蛤|蚶") ~ "Clam",
    stringr::str_detect(z, "^OYSTER(S)?$|牡蛎") ~ "Oyster",
    TRUE ~ NA_character_
  )
}

normalize_study_type <- function(x) {
  z <- x |>
    as.character() |>
    stringr::str_squish() |>
    stringr::str_to_upper()

  dplyr::case_when(
    stringr::str_detect(z, "^AQUACULTURE$|养殖") ~ "Aquaculture",
    stringr::str_detect(z, "^REEF$|礁") ~ "Reef",
    stringr::str_detect(z, "^MESOCOSM$|中尺度|围隔") ~ "Mesocosm",
    TRUE ~ NA_character_
  )
}

normalize_indicator_text <- function(x) {
  x |>
    as.character() |>
    stringr::str_replace_all(c(
      "₂" = "2", "₃" = "3", "₄" = "4",
      "²" = "2", "³" = "3",
      "⁺" = "+", "⁻" = "-",
      "−" = "-", "–" = "-", "—" = "-",
      "μ" = "µ",
      "（" = "(", "）" = ")"
    )) |>
    stringr::str_squish()
}

standardize_indicator <- function(x) {
  raw_text <- as.character(x)
  z <- normalize_indicator_text(raw_text)

  key <- z |>
    stringr::str_replace_all("µ", "U") |>
    stringr::str_to_upper()

  # “面积 + 时间”或明确写有 flux/通量时，判断为通量。
  is_flux <- stringr::str_detect(
    key,
    "FLUX|通量|/M2/(H|HR|D|DAY)|M-2\\s*(H-1|HR-1|D-1|DAY-1)|M2\\s*(H-1|D-1)"
  )

  is_concentration <- stringr::str_detect(
    key,
    "MG/L|UG/L|UMOL/L|MMOL/L|NMOL/L|MOL/L|\\bUM\\b|\\bMM\\b"
  )

  auto_name <- dplyr::case_when(
    # ---------- 气体和碳通量 ----------
    is_flux & stringr::str_detect(key, "N2O|NITROUS OXIDE") ~ "N2O flux",
    is_flux & stringr::str_detect(key, "CH4|METHANE") ~ "CH4 flux",
    is_flux & stringr::str_detect(key, "CO2|CARBON DIOXIDE") ~ "CO2 flux",
    is_flux & stringr::str_detect(key, "\\bDIC\\b|DISSOLVED INORGANIC CARBON") ~ "DIC flux",
    is_flux & stringr::str_detect(key, "\\bDOC\\b|DISSOLVED ORGANIC CARBON") ~ "DOC flux",
    is_flux & stringr::str_detect(key, "(^|[^A-Z0-9])O2([^A-Z0-9]|$)|OXYGEN") ~ "O2 flux",

    # ---------- 氮、磷、硅通量 ----------
    is_flux & stringr::str_detect(key, "DENITRIFICATION|(^|[^A-Z0-9])N2([^A-Z0-9]|$)") ~ "N2 flux",
    is_flux & stringr::str_detect(key, "NH4|AMMONIUM") ~ "NH4 flux",
    is_flux & stringr::str_detect(key, "NOX|NO2\\s*\\+\\s*NO3|NO3\\s*\\+\\s*NO2") ~ "NOx flux",
    is_flux & stringr::str_detect(key, "NO2|NITRITE") ~ "NO2 flux",
    is_flux & stringr::str_detect(key, "NO3|NITRATE") ~ "NO3 flux",
    is_flux & stringr::str_detect(key, "PO4|PHOSPHATE|\\bSRP\\b") ~ "PO4/SRP flux",
    is_flux & stringr::str_detect(key, "SILICATE|\\bSI\\b") ~ "Si flux",

    # ---------- 水体浓度 ----------
    !is_flux & stringr::str_detect(key, "\\bDOC\\b|DISSOLVED ORGANIC CARBON") ~ "DOC concentration",
    !is_flux & stringr::str_detect(key, "\\bPOC\\b|PARTICULATE ORGANIC CARBON") ~ "POC concentration",
    !is_flux & stringr::str_detect(key, "\\bDON\\b|DISSOLVED ORGANIC NITROGEN") ~ "DON concentration",
    !is_flux & stringr::str_detect(key, "\\bDOP\\b|DISSOLVED ORGANIC PHOSPHORUS") ~ "DOP concentration",
    !is_flux & stringr::str_detect(key, "\\bPON\\b|PARTICULATE ORGANIC NITROGEN") ~ "PON concentration",
    !is_flux & stringr::str_detect(key, "PARTICULATE ORGANIC PHOSPHORUS|(^|[^A-Z0-9])PP([^A-Z0-9]|$)") ~ "PP concentration",
    !is_flux & stringr::str_detect(key, "\\bDIN\\b|DISSOLVED INORGANIC NITROGEN") ~ "DIN concentration",
    !is_flux & stringr::str_detect(key, "NH4|AMMONIUM") ~ "NH4 concentration",
    !is_flux & stringr::str_detect(key, "NOX") ~ "NOx concentration",
    !is_flux & stringr::str_detect(key, "NO2|NITRITE") ~ "NO2 concentration",
    !is_flux & stringr::str_detect(key, "NO3|NITRATE") ~ "NO3 concentration",
    !is_flux & stringr::str_detect(key, "PO4|PHOSPHATE|\\bSRP\\b") ~ "PO4/SRP concentration",
    !is_flux & stringr::str_detect(key, "HCO3|BICARBONATE") ~ "HCO3 concentration",
    !is_flux & stringr::str_detect(key, "CO3|CARBONATE") ~ "CO3 concentration",
    !is_flux & is_concentration & stringr::str_detect(key, "CO2|CARBON DIOXIDE") ~ "CO2 concentration",
    !is_flux & is_concentration & stringr::str_detect(key, "(^|[^A-Z0-9])O2([^A-Z0-9]|$)|OXYGEN") ~ "O2 concentration",

    # ---------- 比值、百分比及其他常见指标 ----------
    stringr::str_detect(key, "C:N|C/N|CARBON.*NITROGEN.*RATIO") ~ "C:N ratio",
    stringr::str_detect(key, "N:P|N/P|NITROGEN.*PHOSPHORUS.*RATIO") ~ "N:P ratio",
    stringr::str_detect(key, "NH4.*NO3|NH4.*NOX") ~ "NH4:NOx ratio",
    stringr::str_detect(key, "ORGANIC NITROGEN.*MASS|\\bON\\b.*MASS") ~ "Organic nitrogen mass percentage",
    stringr::str_detect(key, "ORGANIC CARBON.*MASS|\\bOC\\b.*MASS") ~ "Organic carbon mass percentage",

    TRUE ~ z
  )

  if (length(indicator_manual_map) > 0) {
    mapped <- unname(indicator_manual_map[raw_text])
    auto_name[!is.na(mapped)] <- mapped[!is.na(mapped)]
  }

  auto_name
}

is_flux_like_indicator <- function(x) {
  key <- normalize_indicator_text(x) |>
    stringr::str_replace_all("µ", "U") |>
    stringr::str_to_upper()

  stringr::str_detect(
    key,
    "FLUX|通量|/M2/(H|HR|D|DAY)|M-2\\s*(H-1|HR-1|D-1|DAY-1)|M2\\s*(H-1|D-1)"
  )
}

refine_revised_indicator <- function(indicator_raw, indicator_manual) {
  manual <- stringr::str_squish(as.character(indicator_manual))
  auto <- standardize_indicator(indicator_raw)
  raw_is_flux <- is_flux_like_indicator(indicator_raw)

  dplyr::case_when(
    is.na(manual) | manual == "" ~ auto,
    manual == "NH4 flux/concentration" & raw_is_flux ~ "NH4 flux",
    manual == "NH4 flux/concentration" & !raw_is_flux ~ "NH4 concentration",
    manual == "NH4 flux" & !raw_is_flux ~ auto,
    TRUE ~ manual
  )
}

format_p <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "P = NA",
    p < 0.001 ~ "P < 0.001",
    TRUE ~ paste0("P = ", sprintf("%.3f", p))
  )
}

format_fdr_p <- function(p) {
  stringr::str_replace(format_p(p), "^P", "FDR-adjusted P")
}

safe_file_name <- function(x) {
  y <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  y[is.na(y)] <- x[is.na(y)]
  y <- gsub("[^A-Za-z0-9_-]+", "_", y)
  y <- gsub("_+", "_", y)
  y <- gsub("^_|_$", "", y)
  ifelse(nchar(y) == 0, "indicator", y)
}

fit_random_effects <- function(d, minimum_k = min_studies) {
  k <- nrow(d)

  if (k < minimum_k) {
    return(tibble::tibble(
      k = k,
      pooled_g = NA_real_, ci_lb = NA_real_, ci_ub = NA_real_,
      p_value = NA_real_, tau2 = NA_real_, I2 = NA_real_,
      method = NA_character_, status = paste0("Skipped: k < ", minimum_k)
    ))
  }

  fit <- tryCatch(
    metafor::rma.uni(
      yi = d$yi,
      vi = d$vi,
      method = "SJ",
      test = "knha"
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(tibble::tibble(
      k = k,
      pooled_g = NA_real_, ci_lb = NA_real_, ci_ub = NA_real_,
      p_value = NA_real_, tau2 = NA_real_, I2 = NA_real_,
      method = "SJ + knha", status = "Model failed"
    ))
  }

  tibble::tibble(
    k = k,
    pooled_g = as.numeric(fit$b),
    ci_lb = as.numeric(fit$ci.lb),
    ci_ub = as.numeric(fit$ci.ub),
    p_value = as.numeric(fit$pval),
    tau2 = as.numeric(fit$tau2),
    I2 = as.numeric(fit$I2),
    method = "Sidik-Jonkman + Hartung-Knapp",
    status = "OK"
  )
}

pool_within_study <- function(d) {
  if (nrow(d) == 1) {
    return(tibble::tibble(
      yi = d$yi[[1]],
      vi = d$vi[[1]],
      ci_lb = d$yi[[1]] - 1.96 * sqrt(d$vi[[1]]),
      ci_ub = d$yi[[1]] + 1.96 * sqrt(d$vi[[1]]),
      n_raw_rows = 1L
    ))
  }

  fit <- tryCatch(
    metafor::rma.uni(yi = d$yi, vi = d$vi, method = "FE"),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    w <- 1 / d$vi
    pooled_yi <- sum(w * d$yi) / sum(w)
    pooled_vi <- 1 / sum(w)

    return(tibble::tibble(
      yi = pooled_yi,
      vi = pooled_vi,
      ci_lb = pooled_yi - 1.96 * sqrt(pooled_vi),
      ci_ub = pooled_yi + 1.96 * sqrt(pooled_vi),
      n_raw_rows = nrow(d)
    ))
  }

  tibble::tibble(
    yi = as.numeric(fit$b),
    vi = as.numeric(fit$se)^2,
    ci_lb = as.numeric(fit$ci.lb),
    ci_ub = as.numeric(fit$ci.ub),
    n_raw_rows = nrow(d)
  )
}

# -------------------- 4. 读取 Merge 工作表 --------------------
raw <- readxl::read_excel(
  file_path,
  sheet = sheet_name,
  .name_repair = "unique"
) |>
  dplyr::select(dplyr::where(~ !all(is.na(.x))))

names(raw) <- stringr::str_squish(names(raw))

indicator_col <- pick_column(
  names(raw),
  c("Measurement index", "对照组数据", "Indicator", "indicator", "Measurement", "指标"),
  required = TRUE,
  label = "指标列"
)

group_col <- pick_column(
  names(raw),
  c("Shellfish group", "Shellfish Group", "shellfish_group", "贝类类别", "贝类组"),
  required = TRUE,
  label = "Shellfish group 列"
)

study_type_col <- pick_column(
  names(raw),
  c("Study type", "Study Type", "study_type", "Study_type", "研究类型"),
  required = TRUE,
  label = "Study type 列"
)

standardized_indicator_col <- pick_column(
  names(raw),
  c("Standardized indicator", "Indicator standardized", "标准化指标"),
  required = FALSE
)

audit_status_col <- pick_column(
  names(raw),
  c("Audit status", "审查状态"),
  required = FALSE
)

meta_use_col <- pick_column(
  names(raw),
  c("Meta-use recommendation", "Meta use recommendation", "Meta使用建议"),
  required = FALSE
)

compartment_col <- pick_column(
  names(raw),
  c("Compartment", "Medium", "介质"),
  required = FALSE
)

original_row_col <- pick_column(
  names(raw),
  c("Original Excel row", "原始Excel行"),
  required = FALSE
)

required_numeric <- c("n.c", "m.c", "sd.c", "n.e", "m.e", "sd.e")
missing_numeric <- setdiff(required_numeric, names(raw))
if (length(missing_numeric) > 0) {
  stop("缺少计算 Hedges' g 所需列：", paste(missing_numeric, collapse = ", "))
}

raw <- raw |>
  dplyr::rename(
    indicator_raw = dplyr::all_of(indicator_col),
    shellfish_group_raw = dplyr::all_of(group_col),
    study_type_raw = dplyr::all_of(study_type_col)
  )

if (!is.na(standardized_indicator_col)) {
  raw$indicator_manual <- raw[[standardized_indicator_col]]
} else {
  raw$indicator_manual <- NA_character_
}

if (!is.na(audit_status_col)) {
  raw$audit_status <- raw[[audit_status_col]]
} else {
  raw$audit_status <- NA_character_
}

if (!is.na(meta_use_col)) {
  raw$meta_use_recommendation <- raw[[meta_use_col]]
} else {
  raw$meta_use_recommendation <- NA_character_
}

if (!is.na(compartment_col)) {
  raw$compartment <- raw[[compartment_col]]
} else {
  raw$compartment <- NA_character_
}

if (!is.na(original_row_col)) {
  raw$original_excel_row <- raw[[original_row_col]]
} else {
  raw$original_excel_row <- NA
}

# 补齐可能省略填写的研究信息与贝类组别。
fill_columns <- intersect(
  c(
    "Study", "author", "city", "year", "发表期刊", "Species",
    "Station", "shellfish_group_raw", "study_type_raw"
  ),
  names(raw)
)

raw <- raw |>
  tidyr::fill(dplyr::all_of(fill_columns), .direction = "down")

# 确保后续需要的可选列存在。
for (nm in c("Study", "author", "year", "发表期刊", "Species", "Station", "Time", "Time group")) {
  if (!nm %in% names(raw)) raw[[nm]] <- NA
}

# -------------------- 5. 清洗与标准化 --------------------
dat <- raw |>
  dplyr::mutate(
    row_id = dplyr::row_number(),
    shellfish_group = normalize_shellfish_group(shellfish_group_raw),
    study_type = normalize_study_type(study_type_raw),
    indicator = refine_revised_indicator(indicator_raw, indicator_manual),
    audit_status = stringr::str_squish(as.character(audit_status)),
    meta_use_recommendation = stringr::str_squish(as.character(meta_use_recommendation)),
    compartment = stringr::str_squish(as.character(compartment)),
    excluded_by_source_review = stringr::str_detect(
      dplyr::coalesce(audit_status, ""),
      "^待核实"
    ) |
      stringr::str_detect(
        dplyr::coalesce(meta_use_recommendation, ""),
        "暂不纳入|不纳入SMD主分析|不并入海水贝类主分析|建议敏感性分析中剔除"
      ),
    source_checked_flag = stringr::str_detect(
      dplyr::coalesce(audit_status, ""),
      "已按原文修正|已标准化"
    ),
    dplyr::across(
      dplyr::all_of(required_numeric),
      ~ readr::parse_number(as.character(.x), na = c("", "NA", "N/A", "/", "-"))
    ),
    year = readr::parse_number(as.character(year), na = c("", "NA", "N/A", "/"))
  )

# 保存原始名称与标准化名称的对应表，务必检查此文件。
indicator_audit <- dat |>
  dplyr::count(
    shellfish_group,
    indicator,
    indicator_raw,
    sort = TRUE,
    name = "n_rows"
  )

readr::write_csv(
  indicator_audit,
  file.path(output_dir, "indicator_name_audit.csv")
)

# 贝类类别检查。
group_audit <- dat |>
  dplyr::count(shellfish_group_raw, shellfish_group, sort = TRUE, name = "n_rows")

readr::write_csv(
  group_audit,
  file.path(output_dir, "shellfish_group_audit01.csv")
)

# Study type 检查：最终只能保留 Aquaculture / Reef / Mesocosm 三类。
study_type_audit <- dat |>
  dplyr::count(study_type_raw, study_type, sort = TRUE, name = "n_rows")

readr::write_csv(
  study_type_audit,
  file.path(output_dir, "study_type_audit.csv")
)

analysis_configuration <- tibble::tibble(
  item = c(
    "input_file",
    "sheet",
    "indicator",
    "min_studies",
    "combine_repeated_within_study",
    "separate_stations",
    "zero_sd_policy",
    "strict_source_checked_only",
    "audit_filter"
  ),
  value = c(
    normalizePath(file_path, winslash = "/", mustWork = FALSE),
    sheet_name,
    paste(indicators_to_plot, collapse = "; "),
    as.character(min_studies),
    as.character(combine_repeated_within_study),
    as.character(separate_stations),
    zero_sd_policy,
    as.character(strict_source_checked_only),
    "Exclude rows with Audit status starting 待核实 or Meta-use recommendation indicating 暂不纳入/不纳入/剔除"
  )
)

readr::write_csv(
  analysis_configuration,
  file.path(output_dir, "analysis_configuration.csv")
)

source_review_audit <- dat |>
  dplyr::count(
    excluded_by_source_review,
    source_checked_flag,
    audit_status,
    meta_use_recommendation,
    sort = TRUE,
    name = "n_rows"
  )

readr::write_csv(
  source_review_audit,
  file.path(output_dir, "source_review_filter_audit.csv")
)

source_review_excluded_rows <- dat |>
  dplyr::filter(excluded_by_source_review) |>
  dplyr::select(
    row_id,
    original_excel_row,
    Study,
    author,
    year,
    indicator_raw,
    indicator,
    audit_status,
    meta_use_recommendation,
    compartment,
    dplyr::all_of(required_numeric)
  )

readr::write_csv(
  source_review_excluded_rows,
  file.path(output_dir, "excluded_by_source_review_rows.csv")
)

unknown_study_types <- study_type_audit |>
  dplyr::filter(is.na(study_type))

if (nrow(unknown_study_types) > 0) {
  stop(
    "Study type 中发现无法识别的值。请先在 Excel 中改成 Aquaculture、Reef 或 Mesocosm：\n",
    paste(
      paste0(unknown_study_types$study_type_raw, " (n=", unknown_study_types$n_rows, ")"),
      collapse = "; "
    )
  )
}

# 记录 SD = 0 的行。
zero_sd_rows <- dat |>
  dplyr::filter(
    (!is.na(sd.c) & sd.c <= 0) |
      (!is.na(sd.e) & sd.e <= 0)
  )

readr::write_csv(
  zero_sd_rows,
  file.path(output_dir, "zero_or_negative_sd_rows.csv")
)

if (zero_sd_policy == "replace_with_1") {
  warning(
    "当前方法稿要求 SD <= 0 或缺失时排除，不建议使用 zero_sd_policy = 'replace_with_1'。"
  )
  dat <- dat |>
    dplyr::mutate(
      sd.c = dplyr::if_else(!is.na(sd.c) & sd.c <= 0, 1, sd.c),
      sd.e = dplyr::if_else(!is.na(sd.e) & sd.e <= 0, 1, sd.e)
    )
} else if (zero_sd_policy != "exclude") {
  stop("zero_sd_policy 只能是 'replace_with_1' 或 'exclude'。")
}

# 只有实验组、对照组的 n、均值和 SD 都存在，才能计算 Hedges' g。
invalid_rows <- dat |>
  dplyr::filter(
    is.na(shellfish_group) |
      is.na(study_type) |
      is.na(indicator) | indicator == "" |
      excluded_by_source_review |
      (strict_source_checked_only & !source_checked_flag) |
      is.na(n.c) | is.na(m.c) | is.na(sd.c) |
      is.na(n.e) | is.na(m.e) | is.na(sd.e) |
      n.c <= 1 | n.e <= 1 |
      sd.c <= 0 | sd.e <= 0
  )

readr::write_csv(
  invalid_rows,
  file.path(output_dir, "excluded_invalid_rows.csv")
)

analysis_rows <- dat |>
  dplyr::filter(
    !is.na(shellfish_group),
    !is.na(study_type),
    !is.na(indicator), indicator != "",
    !excluded_by_source_review,
    !(strict_source_checked_only & !source_checked_flag),
    !is.na(n.c), !is.na(m.c), !is.na(sd.c),
    !is.na(n.e), !is.na(m.e), !is.na(sd.e),
    n.c > 1, n.e > 1,
    sd.c > 0, sd.e > 0
  )

if (nrow(analysis_rows) == 0) {
  stop(
    "清洗后没有可计算 Hedges' g 的数据。",
    "\n请检查 excluded_invalid_rows.csv，尤其是对照组 n.c、m.c、sd.c 是否缺失。"
  )
}

# -------------------- 6. 计算每行 Hedges' g --------------------
dat_es <- metafor::escalc(
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
  dplyr::filter(
    is.finite(yi),
    is.finite(vi),
    vi > 0
  ) |>
  dplyr::mutate(
    author_clean = dplyr::coalesce(
      dplyr::na_if(stringr::str_squish(as.character(author)), ""),
      dplyr::na_if(stringr::str_squish(as.character(Study)), ""),
      "Unknown study"
    ),
    year_text = dplyr::if_else(
      is.na(year),
      "n.d.",
      as.character(as.integer(year))
    ),
    study_title_clean = dplyr::coalesce(
      dplyr::na_if(stringr::str_squish(as.character(Study)), ""),
      paste0(author_clean, " ", year_text)
    ),
    study_label = paste0(author_clean, " (", year_text, ")"),
    # separate_stations 是单个 TRUE/FALSE 设置，不应使用向量化 if_else()。
    # 使用普通 if，让 TRUE 分支返回整列站点，FALSE 分支返回等长字符向量。
    station_key = if (isTRUE(separate_stations)) {
      dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(Station)), ""),
        "No station"
      )
    } else {
      rep("All stations", dplyr::n())
    },
    study_id = paste(
      shellfish_group,
      study_type,
      study_title_clean,
      station_key,
      sep = " | "
    )
  )

# -------------------- 7. 研究内合并重复观测 --------------------
if (combine_repeated_within_study) {
  study_es <- dat_es |>
    dplyr::group_by(
      indicator,
      shellfish_group,
      study_type,
      study_id,
      study_label
    ) |>
    dplyr::group_modify(~ pool_within_study(.x)) |>
    dplyr::ungroup()
} else {
  study_es <- dat_es |>
    dplyr::mutate(
      time_label = dplyr::coalesce(
        dplyr::na_if(stringr::str_squish(as.character(Time)), ""),
        paste0("row ", row_id)
      ),
      study_id = paste0(study_id, " | row ", row_id),
      study_label = paste0(study_label, " — ", time_label),
      ci_lb = yi - 1.96 * sqrt(vi),
      ci_ub = yi + 1.96 * sqrt(vi),
      n_raw_rows = 1L
    ) |>
    dplyr::select(
      indicator, shellfish_group, study_type, study_id, study_label,
      yi, vi, ci_lb, ci_ub, n_raw_rows
    )
}

readr::write_csv(
  study_es,
  file.path(output_dir, "study_level_effect_sizes.csv")
)

# -------------------- 8. 总体和各贝类组 Meta 分析 --------------------
meta_summary <- study_es |>
  dplyr::group_by(indicator) |>
  dplyr::group_modify(~ fit_random_effects(.x, minimum_k = min_studies)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    p_fdr = NA_real_,
    p_fdr = dplyr::if_else(
      status == "OK",
      stats::p.adjust(p_value, method = "BH"),
      p_fdr
    )
  ) |>
  dplyr::mutate(
    ci_half_width = (ci_ub - ci_lb) / 2,
    annotation = dplyr::if_else(
      status == "OK",
      paste0(
        "g = ", sprintf("%.3f", pooled_g),
        "\n95% CI = [", sprintf("%.3f", ci_lb),
        ", ", sprintf("%.3f", ci_ub), "]",
        "\n", format_p(p_value),
        "\n", format_fdr_p(p_fdr)
      ),
      status
    )
  )

readr::write_csv(
  meta_summary,
  file.path(output_dir, "meta_analysis_summary_all_shellfish.csv")
)

meta_summary_by_group <- study_es |>
  dplyr::group_by(indicator, shellfish_group) |>
  dplyr::group_modify(~ fit_random_effects(.x, minimum_k = min_studies)) |>
  dplyr::ungroup()

readr::write_csv(
  meta_summary_by_group,
  file.path(output_dir, "meta_analysis_summary_by_shellfish_group.csv")
)

meta_summary_by_study_type <- study_es |>
  dplyr::group_by(indicator, study_type) |>
  dplyr::group_modify(~ fit_random_effects(.x, minimum_k = min_studies)) |>
  dplyr::ungroup()

readr::write_csv(
  meta_summary_by_study_type,
  file.path(output_dir, "meta_analysis_summary_by_study_type.csv")
)

group_counts <- study_es |>
  dplyr::count(indicator, shellfish_group, name = "k_studies") |>
  tidyr::complete(
    indicator,
    shellfish_group = names(shape_values),
    fill = list(k_studies = 0L)
  ) |>
  dplyr::arrange(indicator, match(shellfish_group, names(shape_values)))

readr::write_csv(
  group_counts,
  file.path(output_dir, "study_counts_by_indicator_and_group.csv")
)

study_type_counts <- study_es |>
  dplyr::count(indicator, study_type, name = "k_studies") |>
  tidyr::complete(
    indicator,
    study_type = names(study_type_colors),
    fill = list(k_studies = 0L)
  ) |>
  dplyr::arrange(indicator, match(study_type, names(study_type_colors)))

readr::write_csv(
  study_type_counts,
  file.path(output_dir, "study_counts_by_indicator_and_study_type.csv")
)

# -------------------- 9. 每个指标单独作图 --------------------
valid_indicators <- meta_summary |>
  dplyr::filter(status == "OK") |>
  dplyr::pull(indicator)

if (!is.null(indicators_to_plot)) {
  valid_indicators <- intersect(valid_indicators, indicators_to_plot)
}

if (length(valid_indicators) == 0) {
  stop(
    "没有指标达到作图条件。",
    "\n请查看 meta_analysis_summary_all_shellfish.csv 和 study_counts_by_indicator_and_group.csv。"
  )
}

make_indicator_plot <- function(indicator_name) {
  d <- study_es |>
    dplyr::filter(indicator == indicator_name) |>
    dplyr::mutate(
      shellfish_group = factor(
        shellfish_group,
        levels = names(shape_values)
      ),
      study_type = factor(
        study_type,
        levels = names(study_type_colors)
      )
    ) |>
    dplyr::arrange(study_type, shellfish_group, yi) |>
    dplyr::mutate(
      display_label = stringr::str_trunc(study_label, width = 68),
      display_label = make.unique(display_label),
      display_label = factor(display_label, levels = rev(display_label))
    )

  s <- meta_summary |>
    dplyr::filter(indicator == indicator_name)

  shellfish_counts_text <- group_counts |>
    dplyr::filter(indicator == indicator_name) |>
    dplyr::mutate(text = paste0(shellfish_group, " = ", k_studies)) |>
    dplyr::pull(text) |>
    paste(collapse = "; ")

  study_type_counts_text <- study_type_counts |>
    dplyr::filter(indicator == indicator_name) |>
    dplyr::mutate(text = paste0(study_type, " = ", k_studies)) |>
    dplyr::pull(text) |>
    paste(collapse = "; ")

  annotation_y <- levels(d$display_label)[length(levels(d$display_label))]

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 0.55,
      colour = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = s$pooled_g,
      linetype = "dashed",
      linewidth = 0.55,
      colour = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = ci_lb,
        xend = ci_ub,
        y = display_label,
        yend = display_label
      ),
      linewidth = 0.55,
      colour = "black"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = yi,
        y = display_label,
        shape = shellfish_group,
        colour = study_type
      ),
      size = 4.3,
      stroke = 0.8
    )+
    ggplot2::annotate(
      "text",
      x = Inf,
      y = annotation_y,
      label = s$annotation,
      hjust = 1.05,
      vjust = 1.05,
      size = 3.5,
      lineheight = 1.05,
      fontface = "bold"
    ) +
    ggplot2::scale_shape_manual(
      values = shape_values,
      limits = names(shape_values),
      drop = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = study_type_colors,
      limits = names(study_type_colors),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = indicator_name,
      subtitle = paste0(
        "Study type: ", study_type_counts_text,
        " | Shellfish group: ", shellfish_counts_text,
        ". Solid line = 0; dashed line = pooled effect."
      ),
      x = "Effect size (Hedges' g)",
      y = NULL,
      shape = "Shellfish group",
      colour = "Study type"
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_classic(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 9),
      axis.text.y = ggplot2::element_text(size = 8.5, colour = "black"),
      axis.text.x = ggplot2::element_text(colour = "black"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(8, 24, 8, 8)
    )

  list(plot = p, k = nrow(d))
}

pdf_device <- if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf

plot_index <- seq_along(valid_indicators)
for (i in plot_index) {
  indicator_name <- valid_indicators[[i]]
  result <- make_indicator_plot(indicator_name)

  file_stem <- paste0(
    sprintf("%03d", output_index_start + i - 1), "_",
    safe_file_name(indicator_name)
  )

  plot_height <- max(4.8, min(22, 2.8 + 0.34 * result$k))

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(file_stem, ".png")),
    plot = result$plot,
    width = 10.5,
    height = plot_height,
    dpi = 600,
    bg = "white",
    limitsize = FALSE
  )

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(file_stem, ".tiff")),
    plot = result$plot,
    width = 10.5,
    height = plot_height,
    dpi = 600,
    device = ragg::agg_tiff,
    bg = "white",
    limitsize = FALSE
  )

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(file_stem, ".svg")),
    plot = result$plot,
    width = 10.5,
    height = plot_height,
    device = svglite::svglite,
    bg = "white",
    limitsize = FALSE
  )

  ggplot2::ggsave(
    filename = file.path(output_dir, paste0(file_stem, ".pdf")),
    plot = result$plot,
    width = 10.5,
    height = plot_height,
    device = pdf_device,
    bg = "white",
    limitsize = FALSE
  )
}

# -------------------- 10. 全部指标的分面总图 --------------------
if (make_all_indicator_overview) {
  overview_dat <- study_es |>
    dplyr::filter(indicator %in% valid_indicators) |>
    dplyr::mutate(
      shellfish_group = factor(
        shellfish_group,
        levels = names(shape_values)
      ),
      study_type = factor(
        study_type,
        levels = names(study_type_colors)
      )
    ) |>
    dplyr::arrange(indicator, study_type, shellfish_group, yi) |>
    dplyr::group_by(indicator) |>
    dplyr::mutate(
      local_order = dplyr::row_number(),
      short_label = stringr::str_trunc(study_label, width = 44),
      y_key = paste(indicator, sprintf("%04d", local_order), short_label, sep = "___")
    ) |>
    dplyr::ungroup()

  overview_dat$y_key <- factor(
    overview_dat$y_key,
    levels = rev(unique(overview_dat$y_key))
  )

  overview_summary <- meta_summary |>
    dplyr::filter(indicator %in% valid_indicators, status == "OK")

  overview_plot <- ggplot2::ggplot(overview_dat) +
    ggplot2::geom_vline(
      xintercept = 0,
      linewidth = 0.4,
      colour = "black"
    ) +
    ggplot2::geom_vline(
      data = overview_summary,
      ggplot2::aes(xintercept = pooled_g),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 0.45,
      colour = "black"
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = ci_lb,
        xend = ci_ub,
        y = y_key,
        yend = y_key
      ),
      linewidth = 0.42,
      colour = "black"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = yi,
        y = y_key,
        shape = shellfish_group,
        colour = study_type
      ),
      size = 2.5,
      stroke = 0.6
    ) +
    ggplot2::facet_wrap(
      ~ indicator,
      scales = "free",
      ncol = overview_ncol
    ) +
    ggplot2::scale_y_discrete(
      labels = function(x) sub("^.*___[0-9]+___", "", x)
    ) +
    ggplot2::scale_shape_manual(
      values = shape_values,
      limits = names(shape_values),
      drop = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = study_type_colors,
      limits = names(study_type_colors),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "All indicators: shellfish effects relative to controls",
      subtitle = "Each panel is a separate indicator and a separate random-effects model.",
      x = "Effect size (Hedges' g)",
      y = NULL,
      shape = "Shellfish group",
      colour = "Study type"
    ) +
    ggplot2::theme_classic(base_size = 9, base_family = "sans") +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      axis.text.y = ggplot2::element_text(size = 6.8, colour = "black"),
      axis.text.x = ggplot2::element_text(size = 7.5, colour = "black"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      panel.spacing = grid::unit(1.0, "lines")
    )

  n_panels <- length(valid_indicators)
  overview_height <- max(7, 4.4 * ceiling(n_panels / overview_ncol))

  ggplot2::ggsave(
    filename = file.path(output_dir, "ALL_INDICATORS_faceted_overview.pdf"),
    plot = overview_plot,
    width = 16,
    height = overview_height,
    device = pdf_device,
    bg = "white",
    limitsize = FALSE
  )

  # 若以后恢复总图，也按不低于 300 dpi 导出。
  ggplot2::ggsave(
    filename = file.path(output_dir, "ALL_INDICATORS_faceted_overview.png"),
    plot = overview_plot,
    width = 16,
    height = overview_height,
    dpi = 300,
    bg = "white",
    limitsize = FALSE
  )
}

message("Finished. Files are in: ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
message("有效指标数量：", length(valid_indicators))
message("Study type 配色：Aquaculture = peach; Reef = blue; Mesocosm = teal-green")
message("本脚本针对修订版数据生成所有达到条件的单指标森林图；图形风格参考 009_NH4_flux。")
message("请同时检查 excluded_by_source_review_rows.csv、excluded_invalid_rows.csv 和 study_level_effect_sizes.csv。")

