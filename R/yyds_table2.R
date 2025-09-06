#' Regression Results Table Structured
#' 回归结果表格化
#'
#' @description
#'   - Generates structured regression result tables for various model types.
#'   - 为多种回归模型生成结构化结果表格
#'
#' @param model A fitted regression model object. Supported types:
#'  - Cox proportional hazards model (coxph)
#'  - Generalized linear model (glm) with binomial, poisson, or gaussian family.
#'  - Linear model (lm).
#'
#' @param effect_digits Number of decimal places for effect estimates. Default is 2.
#' @param p_digits Number of decimal places for p-values. Default is 3.
#' @param full Logical, whether to return full model details (estimate, CI bounds).
#'   Default is FALSE.
#' @param event Logical, whether to include event counts for binary outcomes.
#'   Default is FALSE.
#'
#' @return A tibble with formatted analysis results. The columns depend on model type:
#' \itemize{
#'   \item{Cox models: HR (95% CI)}
#'   \item{Logistic regression: OR (95% CI)}
#'   \item{Poisson regression: RR (95% CI)}
#'   \item{Linear regression: β (95% CI)}
#' }
#' Additional columns are included when full=TRUE or event=TRUE.
#'
#' @examples
#' # 示例1: 线性回归模型
#' \dontrun{
#' lm_model <- glm(CKM_stage ~ A_edu2 + A_age + A_sex, data = data)
#' yyds_table2(lm_model)
#' }
#' # 输出:
#' # # A tibble: 7 × 7
#' #   Variable               β (95% CI)      p-value Significance    β lower upper
#' #   <chr>                  <chr>          <chr>    <chr>        <dbl> <dbl> <dbl>
#' # 1 A_edu2                 NA             NA       NA           NA    NA    NA
#' # 2   Bachelor's degree o… Ref            -        ""            0     0     0
#' # 3   High school diploma… 0.44 (0.39-0.… <0.001   ***          0.44  0.39  0.5
#' # 4   Some college         0.23 (0.17-0.… <0.001   ***          0.23  0.17  0.29
#' # 5 A_age                  0.04 (0.03-0.… <0.001   ***          0.04  0.03  0.04
#' # 6 A_sex                  NA             NA       NA           NA    NA    NA
#' # 7   female               Ref            -        ""            0     0     0
#' # 8   male                 0.20 (0.15-0.… <0.001   ***          0.2   0.15  0.24
#'
#' # 示例2: Logistic回归
#' \dontrun{
#' glm_logit <- glm(Die_mortstat ~ A_edu2 + A_age + A_sex,
#'                 family = binomial, data = data)
#' yyds_table2(glm_logit, full = FALSE)
#' }
#' # 输出:
#' # # A tibble: 7 × 5
#' #   Variable               Events, n/N OR (95% CI)      p-value Significance
#' #   <chr>                  <chr>       <chr>            <chr>    <chr>
#' # 1 A_edu2                 NA          NA               NA       NA
#' # 2   Bachelor's degree o… 76/2020     Ref              -        ""
#' # 3   High school diploma… 273/3386    1.33 (0.98-1.79) 0.065    ""
#' # 4   Some college         118/2423    1.20 (0.87-1.65) 0.26     ""
#' # 5 A_age                  NA          1.09 (1.08-1.10) <0.001   ***
#' # 6 A_sex                  NA          NA               NA       NA
#' # 7   female               176/3971    Ref              -        ""
#' # 8   male                 291/3858    1.87 (1.52-2.30) <0.001   ***
#'
#' # 示例3: Cox回归
#' \dontrun{
#' cox_model <- coxph(Surv(Die_permth_int, Die_mortstat) ~ A_edu2 + A_age,
#'                   data = data)
#' yyds_table2(cox_model, event = FALSE, effect_digits = 3)
#' }
#' # 输出:
#' # # A tibble: 4 × 7
#' #   Variable               HR (95% CI)      p-value Significance    hr lower upper
#' #   <chr>                  <chr>            <chr>    <chr>        <dbl> <dbl> <dbl>
#' # 1 A_edu2                 NA               NA       NA           NA    NA    NA
#' # 2   Bachelor's degree o… Ref              -        ""            1     1     1
#' # 3   High school diploma… 1.682 (1.312-2.… <0.001   ***          1.68  1.31  2.16
#' # 4   Some college         1.431 (1.080-1.… 0.013    *            1.43  1.08  1.90
#' # 5 A_age                  1.092 (1.084-1.… <0.001   ***          1.09  1.08  1.10
#'
#' # 示例4: 泊松回归
#' \dontrun{
#' glm_pois <- glm(CKM_stage ~ A_edu2 + A_age, family = poisson, data = data)
#' yyds_table2(glm_pois, full = FALSE, event = FALSE)
#' }
#' # 输出:
#' # # A tibble: 3 × 4
#' #   Variable               RR (95% CI)      p-value Significance
#' #   <chr>                  <chr>            <chr>    <chr>
#' # 1 A_edu2                 NA               NA       NA
#' # 2   Bachelor's degree o… Ref              -        ""
#' # 3   High school diploma… 1.18 (1.13-1.22) <0.001   ***
#' # 4   Some college         1.15 (1.10-1.20) <0.001   ***
#' # 5 A_age                  1.02 (1.02-1.02) <0.001   ***
#'
#' @importFrom survival Surv coxph
#' @importFrom stats as.formula glm binomial poisson gaussian
#' @importFrom broom tidy
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr case_when filter mutate transmute bind_rows
#' @export
yyds_table2 <- function(model,
                        effect_digits = 2,
                        p_digits = 3,
                        full = TRUE,
                        event = TRUE) {

  # 1. 模型类型自动检测 -------------------------------------------------
  if (inherits(model, "coxph")) {
    model_type <- "cox"
    effect_name <- "HR"
    original_data <- model.frame(model)
    surv_obj <- model.response(original_data)
  } else if (inherits(model, "glm")) {
    fam <- family(model)
    if (fam$family == "binomial" && fam$link == "logit") {
      model_type <- "logit"
      effect_name <- "OR"
    } else if (fam$family == "poisson") {
      model_type <- "poisson"
      effect_name <- "RR"
    } else if (fam$family == "gaussian") {
      model_type <- "linear"
      effect_name <- "β"
    } else {
      stop("Unsupported GLM family/link. Supported: binomial(logit), poisson, or gaussian.")
    }
    original_data <- model.frame(model)
    if (model_type %in% c("logit", "poisson")) {
      response_var <- model.response(original_data)
    }
  } else if (inherits(model, "lm") && !inherits(model, "glm")) {
    model_type <- "linear"
    effect_name <- "β"
    original_data <- model.frame(model)
  } else {
    stop("Unsupported model type. Supported: coxph, glm (logit/poisson/linear), or lm.")
  }

  # 2. 初始化结果数据框 -------------------------------------------------
  result <- data.frame(
    Variable = character(),
    Effect = numeric(),
    Lower_CI = numeric(),
    Upper_CI = numeric(),
    p_value = character(),
    stringsAsFactors = FALSE
  )

  # 计算总事件数/总样本量
  total_event_str <- NA
  if (event && model_type %in% c("cox", "logit")) {
    result$event_n <- character()
    if (model_type == "cox") {
      total_events <- sum(surv_obj[, "status"])
      total_n <- nrow(surv_obj)
      total_event_str <- paste0(total_events, "/", total_n)
    } else {
      total_events <- sum(response_var)
      total_n <- length(response_var)
      total_event_str <- paste0(total_events, "/", total_n)
    }
  } else if (event) {
    # 对于非cox/logit模型，如果需要event列，填充总样本量
    result$event_n <- character()
    total_n <- nrow(original_data)
    total_event_str <- paste0(total_n)
  }

  z <- qnorm(0.975)  # 95% CI的z值
  terms_info <- attr(model$terms, "dataClasses")[-1]
  vars_in_model <- names(terms_info)

  # 3. 处理每个变量 -----------------------------------------------------
  for (var in vars_in_model) {
    # 处理字符型变量 -> 因子
    if (is.character(original_data[[var]])) {
      original_data[[var]] <- factor(original_data[[var]])
    }

    # 获取事件数信息（仅分类模型）
    event_str <- NA
    if (event && model_type %in% c("cox", "logit") && is.factor(original_data[[var]])) {
      if (model_type == "cox") {
        event_counts <- table(original_data[[var]], surv_obj[, "status"])
      } else {
        event_counts <- table(original_data[[var]], response_var)
      }
      event_str <- paste0(event_counts[, 2], "/", rowSums(event_counts))
    } else if (event && is.factor(original_data[[var]])) {
      # 对于非cox/logit模型的因子变量，填充各水平的样本量
      event_str <- paste0(table(original_data[[var]]))
    }

    # 处理因子变量
    if (is.factor(original_data[[var]])) {
      levels <- levels(original_data[[var]])

      # 添加变量名行
      var_row <- data.frame(
        Variable = var,
        Effect = NA, Lower_CI = NA, Upper_CI = NA, p_value = NA,
        stringsAsFactors = FALSE
      )
      if (event) var_row$event_n <- NA
      result <- rbind(result, var_row)

      # 添加参考水平行（关键修改点：线性模型填充0，其他填充1）
      ref_value <- if (model_type == "linear") 0 else 1
      ref_row <- data.frame(
        Variable = paste0("  ", levels[1]),
        Effect = ref_value,
        Lower_CI = ref_value,
        Upper_CI = ref_value,
        p_value = "-",
        stringsAsFactors = FALSE
      )
      if (event) ref_row$event_n <- if (!is.na(event_str[1])) event_str[1] else total_event_str
      result <- rbind(result, ref_row)

      # 添加其他水平行
      for (i in seq_along(levels[-1])) {
        lvl <- levels[-1][i]
        coef_name <- paste0(var, lvl)

        # 提取系数和p值（区分模型类型）
        if (model_type == "cox") {
          coef_available <- coef_name %in% names(coef(model))
          if (coef_available) {
            coef_summary <- summary(model)$coefficients[coef_name, ]
            estimate <- coef_summary["coef"]
            se <- coef_summary["se(coef)"]
            p_val <- coef_summary["Pr(>|z|)"]
          }
        } else if (model_type == "linear") {
          coef_available <- coef_name %in% rownames(summary(model)$coefficients)
          if (coef_available) {
            coef_summary <- summary(model)$coefficients[coef_name, ]
            estimate <- coef_summary["Estimate"]
            se <- coef_summary["Std. Error"]
            p_val <- coef_summary["Pr(>|t|)"]  # 线性模型用t检验p值
          }
        } else {
          coef_available <- coef_name %in% rownames(summary(model)$coefficients)
          if (coef_available) {
            coef_summary <- summary(model)$coefficients[coef_name, ]
            estimate <- coef_summary["Estimate"]
            se <- coef_summary["Std. Error"]
            p_val <- coef_summary["Pr(>|z|)"]
          }
        }

        if (coef_available) {
          # 计算效应量和CI
          if (model_type %in% c("cox", "logit", "poisson")) {
            effect <- exp(estimate)
            lower <- exp(estimate - z * se)
            upper <- exp(estimate + z * se)
          } else {
            effect <- estimate
            lower <- estimate - z * se
            upper <- estimate + z * se
          }

          new_row <- data.frame(
            Variable = paste0("  ", lvl),
            Effect = round(effect, effect_digits),
            Lower_CI = round(lower, effect_digits),
            Upper_CI = round(upper, effect_digits),
            p_value = p_val,
            stringsAsFactors = FALSE
          )
          if (event) new_row$event_n <- if (!is.na(event_str[i+1])) event_str[i+1] else total_event_str
          result <- rbind(result, new_row)
        }
      }
    } else {
      # 处理连续变量
      if (model_type == "cox") {
        coef_summary <- summary(model)$coefficients[var, ]
        estimate <- coef_summary["coef"]
        se <- coef_summary["se(coef)"]
        p_val <- coef_summary["Pr(>|z|)"]
      } else if (model_type == "linear") {
        coef_summary <- summary(model)$coefficients[var, ]
        estimate <- coef_summary["Estimate"]
        se <- coef_summary["Std. Error"]
        p_val <- coef_summary["Pr(>|t|)"]  # 线性模型用t检验p值
      } else {
        coef_summary <- summary(model)$coefficients[var, ]
        estimate <- coef_summary["Estimate"]
        se <- coef_summary["Std. Error"]
        p_val <- coef_summary["Pr(>|z|)"]
      }

      # 计算效应量和CI
      if (model_type %in% c("cox", "logit", "poisson")) {
        effect <- exp(estimate)
        lower <- exp(estimate - z * se)
        upper <- exp(estimate + z * se)
      } else {
        effect <- estimate
        lower <- estimate - z * se
        upper <- estimate + z * se
      }

      new_row <- data.frame(
        Variable = var,
        Effect = round(effect, effect_digits),
        Lower_CI = round(lower, effect_digits),
        Upper_CI = round(upper, effect_digits),
        p_value = p_val,
        stringsAsFactors = FALSE
      )
      if (event) new_row$event_n <- total_event_str
      result <- rbind(result, new_row)
    }
  }

  # 4. 添加显著性星号 ---------------------------------------------------


  result$p_value <- ifelse(result$p_value == "-", "-",
                           sprintf(paste0("%.", p_digits, "f"), suppressWarnings(as.numeric(result$p_value))))
  result$p_num <- suppressWarnings(as.numeric(result$p_value))
  result$Significance <- ifelse(result$p_num < 0.001, "***",
                                ifelse(result$p_num < 0.01, "**",
                                       ifelse(result$p_num < 0.05, "*", ""))
  )
  result$p_value2 <- ifelse(result$p_num==0,
                            paste0("<", 10^(-p_digits)),
                            result$p_value)
  result$p_value <- coalesce(result$p_value2, result$p_value)

  # 5. 格式化输出列（动态判断参考组）-------------------------------------
  effect_format <- paste0("%.", effect_digits, "f")
  is_ref <- !is.na(result$p_value) & result$p_value == "-"

  result$Effect_CI <- ifelse(
    is.na(result$Effect), NA,
    ifelse(is_ref, "Ref",
           paste0(
             sprintf(effect_format, result$Effect),
             " (", sprintf(effect_format, result$Lower_CI),
             "-", sprintf(effect_format, result$Upper_CI), ")"
           ))
  )

  # 6. 选择输出列 ------------------------------------------------------
  output_cols <- c("Variable", if (event) "event_n",
                   "Effect_CI", "p_value", "Significance")
  if (full) output_cols <- c(output_cols, "Effect", "Lower_CI", "Upper_CI")

  final_result <- result[, output_cols, drop = FALSE]

  # 7. 设置列名 --------------------------------------------------------
  col_names <- c(
    "Variable",
    if (event) "Events, n/N",
    paste0(effect_name, " (95% CI)"),
    "p-value", "sign",
    if (full) c(tolower(effect_name), "lower", "upper") else NULL
  )
  col_names <- col_names[!sapply(col_names, is.null)]
  names(final_result) <- col_names
  row.names(final_result) <- NULL
  return(final_result)
}



