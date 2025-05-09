#' 计算变量的趋势性 P 值
#'
#' @description
#' 该函数用于计算回归模型中指定变量作为连续变量时的趋势P值，
#' 支持多种回归模型类型（线性、逻辑、Cox等）。
#'
#' @param fit 回归模型对象，支持 \code{coxph}、\code{glm}、\code{lm}、（\code{svyglm} 和 \code{svycoxph}，待测试）。
#' @param var 变量名，应为具有等级顺序的变量（如有序因子或整数型）。
#'
#' @return 返回趋势性 P 值（字符型）（保留4位小数，小于0.001时显示为 "<0.001"）。
#' @export
#'
#' @examples
#' library(survival)
#' library(dplyr)
#' library(survey)
#' set.seed(42)
#' n <- 500
#'
#' dt_test <- data.frame(
#'   y_continuous = rnorm(n, 100, 15),
#'   y_count = rpois(n, lambda = 3),
#'   y_bin = rbinom(n, 1, 0.4),
#'   time = rexp(n, 0.1),
#'   status = rbinom(n, 1, 0.5),
#'   exposure = sample(1:4, n, replace = TRUE),
#'   age = rnorm(n, 50, 10),
#'   sex = sample(c(0, 1), n, replace = TRUE),
#'   bmi = rnorm(n, 25, 4),
#'   weight = runif(n, 1, 2),
#'   strata = sample(1:5, n, replace = TRUE),
#'   psu = sample(1:10, n, replace = TRUE)
#' )

#' ## 线性模型
#' dt_test$exposure <- factor(dt_test$exposure, levels = 1:4, labels = c("Q1", "Q2", "Q3", "Q4"))
#' fit_lm <- lm(y_continuous ~ exposure + age + sex + bmi, data = dt_test)
#'
#' # 一般计算
#' dt_test$exposure_trend <- as.numeric(dt_test$exposure)
#' fit_lm_trend <- lm(y_continuous ~ exposure_trend + age + sex + bmi, data = dt_test)
#' summary(fit_lm_trend)$coefficients["exposure_trend", "Pr(>|t|)"]
#' # 使用函数计算
#' yyds_pfortrend(fit_lm, "exposure")
#'
#'
#' ## 泊松回归
#' fit_poisson <- glm(y_count ~ exposure + age + sex + bmi,
#'                   family = poisson(),
#'                    data = dt_test)
#' # 一般计算
#' fit_poisson_trend <- glm(y_count ~ exposure_trend + age + sex + bmi,
#'                          family = poisson(), data = dt_test)
#' summary(fit_poisson_trend)$coefficients["exposure_trend", "Pr(>|z|)"]
# 使用函数计算
#' yyds_pfortrend(fit_poisson, "exposure")
#'
#'
#' ## 加权
#' design_svy <- svydesign(
#'   id = ~psu,
#'   strata = ~strata,
#'   weights = ~weight,
#'   data = dt_test,
#'   nest = TRUE
#' )
#'
#' ## 加权逻辑回归
#' fit_svyglm <- svyglm(y_bin ~ exposure + age + sex + bmi,
#'                      design = design_svy,
#'                      family = quasibinomial())
#' # 一般计算
#' design_svy$variables$exposure_trend <- as.numeric(design_svy$variables$exposure)
#' fit_svyglm_trend <- svyglm(y_bin ~ exposure_trend + age + sex + bmi,
#'                            design = design_svy,
#'                            family = quasibinomial())
#' summary(fit_svyglm_trend)$coefficients["exposure_trend", "Pr(>|t|)"]
#'
#' # 使用函数计算
#' yyds_pfortrend(fit_svyglm, "exposure")
#'
#' ## 加权cox回归
#' fit_svycox <- svycoxph(Surv(time, status) ~ exposure + age + sex + bmi,
#'                        design = design_svy)
#' # 一般计算
#' fit_svycox_trend <- svycoxph(Surv(time, status) ~ exposure_trend + age + sex + bmi,
#'                              design = design_svy)
#' summary(fit_svycox_trend)$coefficients["exposure_trend", "Pr(>|z|)"]
#'
#' # 使用函数计算
#' yyds_pfortrend(fit_svycox, "exposure")
#'
yyds_pfortrend <- function(fit, var) {
  # 支持的模型类型
  supported_models <- c("coxph", "glm", "lm", "svyglm", "svycoxph")
  if (!inherits(fit, supported_models)) {
    stop("模型类型不支持！支持的类型：", paste(supported_models, collapse = ", "))
  }

  # 检查变量是否在模型数据中
  model_data <- tryCatch({
    if (!is.null(fit$model)) {
      fit$model
    } else {
      model.frame(fit)
    }
  }, error = function(e) {
    stop("无法从模型对象中提取数据，请确保模型拟合时保留了数据（例如加上 model=TRUE 参数）")
  })

  if (!var %in% colnames(model_data)) {
    stop("变量 ", var, " 不在模型中！")
  }

  # 检查变量是否适合趋势检验（多于两类）
  var_data <- model_data[[var]]
  unique_vals <- unique(na.omit(var_data))
  n_unique <- length(unique_vals)
  if (n_unique <= 2) {
    return(NA)
  }

  # 将变量转为数值
  model_data[[var]] <- as.numeric(model_data[[var]])

  # 构造新公式
  formula_original <- formula(fit)
  formula_new <- update(formula_original, as.formula(paste(". ~ . -", var, "+ as.numeric(as.factor(", var, "))")))

  # 拟合新模型
  fit_new <- tryCatch(update(fit, formula = formula_new),
                      error = function(e) stop("模型更新失败，请检查变量：", var))

  # 提取系数表
  coef_table <- summary(fit_new)$coefficients
  row_names <- rownames(coef_table)
  var_numeric <- paste0("as.numeric(as.factor(", var, "))")
  possible_names <- c(var_numeric, var)
  matched_name <- possible_names[possible_names %in% row_names]

  if (length(matched_name) == 0) {
    stop("变量 ", var, " 的系数不存在，可能模型未正确拟合。可用变量: ",
         paste(row_names, collapse = ", "))
  }

  # 自动识别P值列名（兼容各种模型）
  p_candidates <- c("Pr(>|z|)", "Pr(>|t|)", "p", "p-value")
  p_col <- intersect(p_candidates, colnames(coef_table))
  if (length(p_col) == 0) {
    stop("找不到合适的P值列，可能模型不支持提取趋势P值。")
  }
  p_col <- p_col[1]

  # 提取P值
  p_trend <- coef_table[matched_name[1], p_col]
  pfortrend <- ifelse(is.na(p_trend), NA,
                      ifelse(p_trend < 0.001, "<0.001", sprintf("%.4f", p_trend)))
  cat(paste0("p for trend: ", pfortrend, "\n"))
  invisible(pfortrend)
}
