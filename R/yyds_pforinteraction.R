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
#' yyds_pforinteraction(fit, "var1", "var2")
#'
#' # glm模型
#' fit <- glm(status ~ var1 + var2 + age + sex, data = df, family = binomial())
#' fit_inter <- glm(status ~ var1 * var2 + age + sex, data = df, family = binomial())
#' anova(fit, fit_inter, test = "Chisq")
#'
#' # 使用自定义函数进行交互项检验
#' yyds_pforinteraction(fit, "var1", "var2")
#'
yyds_pforinteraction <- function(fit, var1, var2) {
  # 支持的模型类型
  supported_models <- c("coxph", "glm", "lm", "svyglm", "svycoxph")
  if (!inherits(fit, supported_models)) {
    stop("Model type not supported! Supported models include: \n",
         paste(supported_models, collapse = ", "))
  }

  # 获取模型数据（优先使用 model.frame，其次尝试 fit$call$data）
  model_data <- tryCatch({
    if (!is.null(fit$model)) {
      fit$model
    } else if (!is.null(fit$call$data)) {
      eval(fit$call$data, envir = environment(formula(fit)))
    } else {
      model.frame(fit)
    }
  }, error = function(e) {
    stop("无法从模型对象中提取数据，请确保模型拟合时保留了数据（例如加上 model=TRUE 参数）")
  })

  # 检查变量是否存在
  missing_vars <- setdiff(c(var1, var2), names(model_data))
  if (length(missing_vars) > 0) {
    stop("The following variables are not in the model: \n",
         paste(missing_vars, collapse = ", "))
  }

  # 自动转换字符型变量为因子型
  for (v in c(var1, var2)) {
    if (is.character(model_data[[v]])) {
      model_data[[v]] <- as.factor(model_data[[v]])
      message("Note: Variable '", v, "' converted from character to factor")
    }
  }

  # 创建交互项公式
  interaction_term <- paste0(var1, ":", var2)
  formula_base <- formula(fit)
  formula_interaction <- update(formula_base, paste(". ~ . +", interaction_term))
  cat(paste("Interaction term:", interaction_term, "\n"))

  # 重新拟合模型（不使用 update()）
  fit_inter <- tryCatch({
    # 复制原始调用
    new_call <- fit$call
    # 更新公式和数据
    new_call$formula <- formula_interaction
    new_call$data <- model_data  # 使用处理后的数据

    # 特殊处理 survey 设计对象
    if (inherits(fit, c("svyglm", "svycoxph"))) {
      # 确保设计信息被保留
      design <- eval(new_call$design, envir = environment(formula(fit)))
      new_call$design <- design
    }

    eval(new_call)
  }, error = function(e) {
    stop("Model refitting failed: ", e$message)
  })

  # 根据模型类型选择检验方法
  if (inherits(fit, c("svyglm", "svycoxph"))) {
    # 对于复杂调查设计模型，使用 Wald 检验
    interaction_formula <- as.formula(paste0("~ `", var1, "`:`", var2, "`"))
    test_result <- tryCatch({
      regTermTest(fit_inter, test.terms = interaction_formula, method = "Wald")
    }, error = function(e) {
      message("regTermTest failed: ", e$message)
      NULL
    })
    test_type <- "Wald test"
    pval <- if (!is.null(test_result)) test_result$p[1] else NA
  } else {
    # 对于其他回归模型，使用 LRT 检验
    test_result <- tryCatch({
      anova(fit, fit_inter, test = "Chisq")
    }, error = function(e) {
      message("anova failed: ", e$message)
      NULL
    })
    test_type <- "LRT"
    pval <- if (!is.null(test_result)) {
      col_name <- grep("Pr.*Chi", colnames(test_result), value = TRUE)
      if (length(col_name) > 0) test_result[[col_name[1]]][2] else NA
    } else NA
  }

  # 格式化P值输出
  pval_fmt <- if (is.na(pval)) "NA" else
    if (pval < 0.001) "<0.001" else sprintf("%.4f", pval)

  # 输出结果
  cat(paste0(test_type, " p-value for interaction: ", pval_fmt, "\n"))

  # 返回包含更多信息的列表
  invisible(pval_fmt)
}


