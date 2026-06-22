#' 计算回归模型中的交互作用P值
#'
#' @description
#' 计算两个变量交互项的P值，支持普通模型和复杂抽样设计模型。
#' 会根据模型类型自动选择检验方法：
#' - 对于 `glm`、`lm`、`coxph` 模型，使用似然比检验（LRT）
#' - 对于 `svyglm`、`svycoxph` 模型，使用 Wald 检验（`regTermTest()`）
#'
#' @param fit 已拟合的回归模型，支持的模型类型包括：`"glm"`、`"lm"`、`"coxph"`、`"svyglm"`、`"svycoxph"`
#' @param var1 字符串，第一个参与交互项的变量名
#' @param var2 字符串，第二个参与交互项的变量名
#' @param data 普通模型重拟合使用的数据框。用于 `glm`、`lm` 或 `coxph`，与
#'   `design` 二选一。
#' @param design 复杂抽样设计对象。用于 `svyglm` 或 `svycoxph`，与 `data` 二选一。
#'
#' @return
#' 返回交互作用P值（字符型），（保留4位小数，小于0.001时显示为 "<0.001"）。
#'
#' @export
#'
#' @examples
#' library(survey)
#' library(survival)
#'
#' # 模拟数据
#' set.seed(123)
#' df <- data.frame(
#'   weight = runif(300, 1, 3),
#'   strata = sample(1:3, 300, replace = TRUE),
#'   psu = sample(1:15, 300, replace = TRUE),
#'   time = rexp(300, 0.1),
#'   status = sample(0:1, 300, replace = TRUE),
#'   var1 = factor(sample(c("low", "high"), 300, replace = TRUE)),
#'   var2 = rnorm(300),
#'   age = sample(20:70, 300, replace = TRUE),
#'   sex = factor(sample(c("M", "F"), 300, replace = TRUE))
#' )
#'
#' # 创建 survey design 对象
#' design <- svydesign(ids = ~psu, strata = ~strata, weights = ~weight, data = df, nest = TRUE)
#'
#' # 拟合包含交互项的 svycoxph 模型
#' fit_inter <- svycoxph(Surv(time, status) ~ var1 * var2 + age + sex, design = design)
#'
#' # Wald 检验交互项 var1:var2 的显著性
#' test_result <- regTermTest(fit_inter, ~ var1:var2, method = "Wald")
#' print(test_result)
#'
#' # 使用自定义函数进行交互项检验
#' fit <- svycoxph(Surv(time, status) ~ var1 + var2 + age + sex, design = design)
#' yyds_pforinteraction(fit, "var1", "var2", design = design)
#'
#' # glm模型
#' fit <- glm(status ~ var1 + var2 + age + sex, data = df, family = binomial())
#' fit_inter <- glm(status ~ var1 * var2 + age + sex, data = df, family = binomial())
#' anova(fit, fit_inter, test = "Chisq")
#'
#' # 使用自定义函数进行交互项检验
#' yyds_pforinteraction(fit, "var1", "var2", data = df)
#'
yyds_pforinteraction <- function(fit, var1, var2, data = NULL, design = NULL) {
  supported_models <- c("coxph", "glm", "lm", "svyglm", "svycoxph")
  if (!inherits(fit, supported_models)) {
    stop("Model type not supported! Supported models include: \n",
         paste(supported_models, collapse = ", "))
  }
  if (is.null(data) == is.null(design)) {
    stop("data 和 design 必须且只能提供一个。")
  }

  is_survey <- inherits(fit, c("svyglm", "svycoxph"))
  if (is_survey && is.null(design)) {
    stop("svyglm/svycoxph 模型必须通过 design 参数提供复杂抽样设计对象。")
  }
  if (!is_survey && is.null(data)) {
    stop("glm/lm/coxph 模型必须通过 data 参数提供数据框。")
  }
  if (!is_survey && !is.null(design)) {
    stop("普通模型请使用 data 参数；design 仅用于 svyglm/svycoxph 模型。")
  }
  if (is_survey && !is.null(data)) {
    stop("复杂抽样模型请使用 design 参数，不要同时传入 data。")
  }

  model_data <- if (is_survey) design$variables else data
  if (!is.data.frame(model_data)) {
    stop(if (is_survey) "design$variables 必须是数据框。" else "data 必须是数据框。")
  }
  missing_vars <- setdiff(c(var1, var2), names(model_data))
  if (length(missing_vars) > 0) {
    stop("The following variables are not available for interaction testing: \n",
         paste(missing_vars, collapse = ", "))
  }

  for (v in c(var1, var2)) {
    if (is.character(model_data[[v]])) {
      model_data[[v]] <- as.factor(model_data[[v]])
      message("Note: Variable '", v, "' converted from character to factor")
    }
  }
  if (is_survey) {
    design$variables <- model_data
  } else {
    data <- model_data
  }

  bt <- function(x) paste0("`", gsub("`", "\\\\`", x, fixed = TRUE), "`")
  interaction_term <- paste0(var1, ":", var2)
  interaction_formula <- stats::as.formula(
    paste0(". ~ . + ", bt(var1), ":", bt(var2)),
    env = environment(stats::formula(fit))
  )
  formula_interaction <- stats::update.formula(stats::formula(fit), interaction_formula)
  cat("Interaction term: ", interaction_term, " \n", sep = "")

  fit_inter <- tryCatch({
    if (inherits(fit, "svycoxph")) {
      survey::svycoxph(formula_interaction, design = design)
    } else if (inherits(fit, "svyglm")) {
      survey::svyglm(formula_interaction, design = design, family = fit$family)
    } else {
      stats::update(fit, formula = formula_interaction, data = data)
    }
  }, error = function(e) {
    stop("Model refitting failed: ", e$message)
  })

  if (is_survey) {
    test_result <- tryCatch(
      survey::regTermTest(
        fit_inter,
        test.terms = stats::as.formula(paste0("~", bt(var1), ":", bt(var2))),
        method = "Wald"
      ),
      error = function(e) {
        message("regTermTest failed: ", e$message)
        NULL
      }
    )
    test_type <- "Wald test"
    pval <- if (is.null(test_result)) NA_real_ else as.numeric(test_result$p[[1]])
  } else {
    test_result <- tryCatch(
      stats::anova(fit, fit_inter, test = "Chisq"),
      error = function(e) {
        message("anova failed: ", e$message)
        NULL
      }
    )
    test_type <- "LRT"
    pval <- if (!is.null(test_result)) {
      col_name <- grep("Pr.*Chi", colnames(test_result), value = TRUE)
      if (length(col_name) > 0) as.numeric(test_result[[col_name[[1]]]][[2]]) else NA_real_
    } else {
      NA_real_
    }
  }

  pval_fmt <- if (is.na(pval)) "NA" else if (pval < 0.001) "<0.001" else sprintf("%.4f", pval)
  cat(test_type, " p-value for interaction: ", pval_fmt, "\n", sep = "")
  invisible(pval_fmt)
}
