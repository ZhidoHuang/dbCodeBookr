#' Batch perform univariate analysis
#' 批量单因素分析
#'
#'   - This function performs univariate analysis (Cox, logistic, Poisson, or linear regression)
#' for multiple variables and returns formatted results. It automatically handles
#' both continuous and categorical variables, providing reference groups for factors.
#'   - 本函数执行单因素分析（Cox、逻辑、泊松或线性回归）对多个变量进行分析并返回格式化结果。
#' 自动处理连续变量和分类变量，为因子变量提供参考组
#'
#' @param data A data frame containing the variables to be analyzed.
#' @param vars A character vector of variable names to analyze.
#' @param outcome The outcome variable name (character string).
#' @param time_var For Cox models, the time variable name (character string).
#'   Default is NULL for non-Cox models.
#' @param family For GLM models, the family function (e.g., \code{binomial()},\code{poisson()},\code{gaussian()}).
#'   Required for GLM models, NULL for Cox models.
#' @param effect_digits Number of decimal places for effect estimates. Default is 2.
#' @param p_digits Number of decimal places for p-values. Default is 3.
#' @param full Logical, whether to return full model details (estimate, CI bounds).
#'   Default is FALSE.
#' @param event Logical, whether to include event counts for binary outcomes.
#'   Default is FALSE.
#'
#' @return A tibble with analysis results, formatted for easy interpretation.
#'   The columns depend on the model type and parameters:
#'   - For Cox models: HR (95% CI)
#'   - For logistic regression: OR (95% CI)
#'   - For Poisson regression: RR (95% CI)
#'   - For linear regression: β (95% CI)
#'   Additional columns are included when full=TRUE or event=TRUE.
#'
#' @examples
#' \dontrun{
#' # 1. Cox比例风险模型
#' yyds_uni_analysis(
#'   data = data,
#'   vars = c("A_age", "A_sex", "A_edu2","A_marital"),
#'   outcome = "Die_mortstat",
#'   time_var = "Die_permth_int",
#'   full = TRUE,
#'   event = TRUE
#' )
#'
#'#    Variable      `Events, n/N` `HR (95% CI)` `p-value` hr    lower upper
#'#    <chr>         <chr>         <chr>         <chr>     <chr> <chr> <chr>
#'#  1 "A_age"       -             1.09 (1.09-1… <0.001    1.09  1.09  1.10
#'#  2 "A_sex"       NA            NA            NA        NA    NA    NA
#'#  3 "  female"    191/4379      Ref           -         Ref   1     1
#'#  4 "  male"      312/4229      1.69 (1.41-2… <0.001    1.69  1.41  2.03
#'#  5 "A_edu2"      NA            NA            NA        NA    NA    NA
#'#  6 "  Bachelor'… 79/2179       Ref           -         Ref   1     1
#'#  7 "  High scho… 297/3814      2.24 (1.75-2… <0.001    2.24  1.75  2.88
#'#  8 "  Some coll… 126/2613      1.39 (1.05-1… 0.023     1.39  1.05  1.84
#'#  9 "A_marital"   NA            NA            NA        NA    NA    NA
#'# 10 "  formerly … 216/1850      Ref           -         Ref   1     1
#'# 11 "  married"   246/5177      0.38 (0.32-0… <0.001    0.38  0.32  0.45
#'# 12 "  never mar… 41/1580       0.20 (0.14-0… <0.001    0.20  0.14  0.28
#'
#' # 2. poisson泊松回归
#' yyds_uni_analysis(
#'   data = data,
#'   vars = c("A_age", "A_sex", "A_edu2","A_marital"),
#'   outcome = "CKM_stage",
#'   family = poisson(),
#'   effect_digits = 3,
#'   p_digits = 4
#' )
#'
#'#    Variable                        `RR (95% CI)`       `p-value`
#'#    <chr>                           <chr>               <chr>
#'#  1 "A_age"                         1.018 (1.017-1.019) <0.001
#'#  2 "A_sex"                         NA                  NA
#'#  3 "  female"                      Ref                 -
#'#  4 "  male"                        1.107 (1.074-1.141) <0.001
#'#  5 "A_edu2"                        NA                  NA
#'#  6 "  Bachelor's degree or more"   Ref                 -
#'#  7 "  High school diploma or less" 1.261 (1.213-1.311) <0.001
#'#  8 "  Some college"                1.136 (1.088-1.185) <0.001
#'#  9 "A_marital"                     NA                  NA
#'# 10 "  formerly married"            Ref                 -
#'# 11 "  married"                     0.822 (0.794-0.852) <0.001
#'# 12 "  never married"               0.619 (0.589-0.651) <0.001
#'
#' # 3. logistics逻辑回归（明确指定family）
#' yyds_uni_analysis(
#'   data = data,
#'   vars = c("A_age", "A_sex", "A_edu2","A_marital"),
#'   outcome = "Die_mortstat",
#'   family = binomial(),
#'   full = TRUE,
#'   event = TRUE
#' )
#'
#'#    Variable      `Events, n/N` `OR (95% CI)` `p-value` or    lower upper
#'#    <chr>         <chr>         <chr>         <chr>     <chr> <chr> <chr>
#'#  1 "A_age"       -             1.09 (1.09-1… <0.001    1.09  1.09  1.10
#'#  2 "A_sex"       NA            NA            NA        NA    NA    NA
#'#  3 "  female"    191/4379      Ref           -         Ref   1     1
#'#  4 "  male"      312/4229      1.75 (1.45-2… <0.001    1.75  1.45  2.10
#'#  5 "A_edu2"      NA            NA            NA        NA    NA    NA
#'#  6 "  Bachelor'… 79/2179       Ref           -         Ref   1     1
#'#  7 "  High scho… 297/3814      2.24 (1.75-2… <0.001    2.24  1.75  2.91
#'#  8 "  Some coll… 126/2613      1.35 (1.01-1… 0.042     1.35  1.01  1.80
#'#  9 "A_marital"   NA            NA            NA        NA    NA    NA
#'# 10 "  formerly … 216/1850      Ref           -         Ref   1     1
#'# 11 "  married"   246/5177      0.38 (0.31-0… <0.001    0.38  0.31  0.46
#'# 12 "  never mar… 41/1580       0.20 (0.14-0… <0.001    0.20  0.14  0.28
#'
#' # 4. gaussian线性回归
#' yyds_uni_analysis(
#'   data = data,
#'   vars = c("A_sex", "A_edu2","A_marital"),
#'   outcome = "A_age",
#'   family = gaussian(),  # 必须指定
#'   full = T
#' )
#'#    Variable                        `β (95% CI)`           `p-value` β      lower  upper
#'#    <chr>                           <chr>                  <chr>     <chr>  <chr>  <chr>
#'#  1 "A_sex"                         NA                     NA        NA     NA     NA
#'#  2 "  female"                      Ref                    -         Ref    0      0
#'#  3 "  male"                        0.09 (-0.64-0.83)      0.802     0.09   -0.64  0.83
#'#  4 "A_edu2"                        NA                     NA        NA     NA     NA
#'#  5 "  Bachelor's degree or more"   Ref                    -         Ref    0      0
#'#  6 "  High school diploma or less" 3.57 (2.66-4.48)       <0.001    3.57   2.66   4.48
#'#  7 "  Some college"                -0.71 (-1.70-0.27)     0.156     -0.71  -1.70  0.27
#'#  8 "A_marital"                     NA                     NA        NA     NA     NA
#'#  9 "  formerly married"            Ref                    -         Ref    0      0
#'# 10 "  married"                     -9.37 (-10.19--8.54)   <0.001    -9.37  -10.19 -8.54
#'# 11 "  never married"               -24.83 (-25.87--23.79) <0.001    -24.83 -25.87 -23.79
#' }
#'
#' @importFrom survival Surv coxph
#' @importFrom stats as.formula glm binomial poisson gaussian
#' @importFrom broom tidy
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @importFrom dplyr case_when filter mutate transmute bind_rows
#' @export
yyds_uni_analysis <- function(data, vars, outcome,
                              time_var = NULL,
                              family = NULL,
                              effect_digits = 2,
                              p_digits = 3,
                              full = FALSE,
                              event = FALSE) {

  # 辅助函数：格式化效应值
  format_effect <- function(x, exponentiate = FALSE, digits) {
    if (is.character(x)) return(x)
    if (length(x) == 0) return(NA_character_)
    if (is.na(x)) return(NA_character_)
    if (exponentiate) {
      sprintf(paste0("%.", digits, "f"), exp(x))
    } else {
      sprintf(paste0("%.", digits, "f"), x)
    }
  }

  # 辅助函数：格式化p值
  format_pvalue <- function(p, digits) {
    if (is.character(p)) return(p)
    if (length(p) == 0) return(NA_character_)
    sapply(p, function(x) {
      if (is.na(x)) return(NA_character_)
      if (x < 0.001) {
        "<0.001"
      } else {
        sprintf(paste0("%.", digits, "f"), x)
      }
    })
  }

  # 确定模型类型
  model_type <- ifelse(!is.null(time_var), "cox", "glm")

  # 验证family参数
  if (model_type == "glm" && is.null(family)) {
    stop("For GLM models, you must specify the family parameter (e.g., family = binomial())")
  }

  # 批量分析
  results <- purrr::map_dfr(vars, function(var) {
    if (!var %in% names(data)) {
      stop(paste("Variable", var, "not found in data"))
    }

    if (is.character(data[[var]])) {
      data[[var]] <- as.factor(data[[var]])
    }

    formula <- if (model_type == "cox") {
      as.formula(paste("Surv(", time_var, ",", outcome, ") ~", var))
    } else {
      as.formula(paste(outcome, "~", var))
    }

    fit <- tryCatch({
      if (model_type == "cox") {
        coxph(formula, data = data)
      } else {
        glm(formula, data = data, family = family)
      }
    }, error = function(e) {
      message(paste("Error fitting model for variable", var, ":", e$message))
      return(NULL)
    })

    if (is.null(fit)) {
      return(tibble(
        term = var,
        estimate = NA_character_,
        conf.low = NA_character_,
        conf.high = NA_character_,
        p.value = NA_character_,
        P = NA_real_,
        events_n_N = NA_character_,
        VARS = var
      ))
    }

    exponentiate <- model_type == "cox" ||
      (model_type == "glm" && family$family %in% c("binomial", "poisson", "quasipoisson"))

    sum_tab <- broom::tidy(fit, conf.int = TRUE, exponentiate = FALSE)

    if (is.factor(data[[var]])) {
      ref_level <- levels(data[[var]])[1]
      other_levels <- levels(data[[var]])[-1]

      event_table <- if (model_type == "cox") {
        with(data, table(get(var), get(outcome)))
      } else if (model_type == "glm" && family$family %in% c("binomial", "poisson", "quasipoisson")) {
        with(data, table(get(var), get(outcome)))
      } else {
        table(data[[var]])
      }

      events_total <- if (model_type == "cox" ||
                          (model_type == "glm" &&
                           family$family %in% c("binomial", "poisson", "quasipoisson"))) {
        paste(event_table[, 2], "/", rowSums(event_table), sep = "")
      } else {
        as.character(as.vector(event_table))
      }

      ref_row <- tibble(
        term = paste0("  ", ref_level),
        estimate = "Ref",
        conf.low = ifelse(exponentiate, "1", "0"),
        conf.high = ifelse(exponentiate, "1", "0"),
        p.value = "-",
        P = NA_real_,
        events_n_N = events_total[1],
        VARS = var
      )

      if (length(other_levels) > 0) {
        non_ref_results <- map_dfr(other_levels, function(lvl) {
          term_name <- paste0(var, lvl)
          matched_row <- sum_tab %>% filter(term == term_name)

          if (nrow(matched_row) == 0) {
            tibble(
              term = paste0("  ", lvl),
              estimate = NA_character_,
              conf.low = NA_character_,
              conf.high = NA_character_,
              p.value = NA_character_,
              P = NA_real_,
              events_n_N = events_total[match(lvl, other_levels) + 1],
              VARS = var
            )
          } else {
            tibble(
              term = paste0("  ", lvl),
              estimate = format_effect(matched_row$estimate, exponentiate, effect_digits),
              conf.low = format_effect(matched_row$conf.low, exponentiate, effect_digits),
              conf.high = format_effect(matched_row$conf.high, exponentiate, effect_digits),
              p.value = format_pvalue(matched_row$p.value, p_digits),
              P = matched_row$p.value,
              events_n_N = events_total[match(lvl, other_levels) + 1],
              VARS = var
            )
          }
        })

        bind_rows(
          tibble(term = var, estimate = NA, conf.low = NA, conf.high = NA, p.value = NA, P = NA_real_, events_n_N = NA, VARS = var),
          ref_row,
          non_ref_results
        )
      } else {
        bind_rows(
          tibble(term = var, estimate = NA, conf.low = NA, conf.high = NA, p.value = NA, P = NA_real_, events_n_N = NA, VARS = var),
          ref_row
        )
      }
    } else {
      matched_row <- sum_tab %>% filter(term == var)

      if (nrow(matched_row) == 0) {
        tibble(
          term = var,
          estimate = NA_character_,
          conf.low = NA_character_,
          conf.high = NA_character_,
          p.value = NA_character_,
          P = NA_real_,
          events_n_N = "-",
          VARS = var
        )
      } else {
        tibble(
          term = var,
          estimate = format_effect(matched_row$estimate, exponentiate, effect_digits),
          conf.low = format_effect(matched_row$conf.low, exponentiate, effect_digits),
          conf.high = format_effect(matched_row$conf.high, exponentiate, effect_digits),
          p.value = format_pvalue(matched_row$p.value, p_digits),
          P = matched_row$p.value,
          events_n_N = "-",
          VARS = var
        )
      }
    }
  })

  effect_info <- dplyr::case_when(
    model_type == "cox" ~ list(label = "HR (95% CI)", name = "hr"),
    model_type == "glm" && family$family == "binomial" ~ list(label = "OR (95% CI)", name = "or"),
    model_type == "glm" && family$family %in% c("poisson", "quasipoisson") ~ list(label = "RR (95% CI)", name = "rr"),
    model_type == "glm" && family$family == "gaussian" ~ list(label = "β (95% CI)", name = "β"),
    TRUE ~ list(label = "Coef (95% CI)", name = "coef")
  )

  results$effect_ci <- with(results, {
    ifelse(is.na(estimate), NA_character_,
           ifelse(estimate == "Ref", "Ref",
                  paste0(estimate, " (", conf.low, "-", conf.high, ")")))
  })

  name_mapping <- c(
    "term" = "Variable",
    "events_n_N" = "Events, n/N",
    "effect_ci" = effect_info$label,
    "p.value" = "p-value",
    "estimate" = effect_info$name,
    "conf.low" = "lower",
    "conf.high" = "upper"
  )

  names(results) <- sapply(names(results), function(x) {
    if (x %in% names(name_mapping)) name_mapping[x] else x
  })

  keep_cols <- if (full) {
    if (event) {
      c("Variable", "Events, n/N", effect_info$label, "p-value", effect_info$name, "lower", "upper", "VARS", "P")
    } else {
      c("Variable", effect_info$label, "p-value", effect_info$name, "lower", "upper", "VARS", "P")
    }
  } else {
    if (event) {
      c("Variable", "Events, n/N", effect_info$label, "p-value", "VARS", "P")
    } else {
      c("Variable", effect_info$label, "p-value", "VARS", "P")
    }
  }

  results <- results[, intersect(keep_cols, names(results)), drop = FALSE]

  return(results)
}





