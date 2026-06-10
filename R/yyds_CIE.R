#' 基于后向删除的效应改变法（Change-in-Estimate）进行混杂因素筛选。
#'
#' @description
#' 从包含所有候选混杂的全模型出发，迭代删除对暴露效应估计影响最小的变量，
#' 直到所有剩余变量的删除都会导致暴露效应变化超过预设阈值（一般是10%）。
#'
#' **重要前提**：传入的 candidate_vars 必须是已经过 DAG 等先验知识筛选，
#' 排除了中介变量、碰撞变量和暴露后变量的可疑混杂集合。
#'
#' @param data 数据框，包含所有分析变量。
#' @param exposure 字符型，主暴露变量名（在 data 中的列名）。
#' @param outcome 字符型，结局变量名。对于 Cox 模型，该参数表示事件状态变量（0/1），
#'   生存时间由 time_var 指定。
#' @param forced_vars 字符向量，强制调整的变量名（如年龄、性别、设计变量），
#'   这些变量始终保留在模型中，不参与 CIE 筛选。
#' @param candidate_vars 字符向量，候选混杂变量名。这些变量将经过后向 CIE 筛选，
#'   删除对暴露效应估计影响小于阈值的变量。
#' @param threshold 数值型，效应变化阈值（百分比），默认 10。
#'   当删除某变量后暴露系数的相对变化小于该阈值时，该变量被剔除。
#' @param time_var 字符型，生存时间变量名，仅 Cox 模型需要。
#'   默认为 NULL，表示使用 GLM（logistic/线性/Poisson 等）。
#'   指定后自动切换为 Cox 比例风险模型。
#' @param family GLM 的分布族，默认 binomial（logistic 回归）。
#'   可指定 gaussian（线性回归）、poisson（Poisson 回归）等，
#'   该参数将直接传递给 glm()。Cox 模型时自动忽略。
#'
#' @return 不可见返回一个列表（invisible list），包含以下元素：
#'   \item{model_type}{模型类型，"glm" 或 "coxph"}。
#'   \item{family}{GLM 的分布族名称（仅 GLM 模型有值）}。
#'   \item{forced_vars}{强制调整的变量向量}。
#'   \item{final_candidate_vars}{经 CIE 筛选后保留的候选变量向量}。
#'   \item{final_covariates}{最终模型的全部协变量（强制变量 + 保留的候选变量）}。
#'   \item{removed_covariates}{被删除的候选变量向量}。
#'   \item{removal_log}{数据框，记录每一步删除的变量名及效应变化百分比}。
#'   \item{final_model}{最终模型的拟合对象（glm 或 coxph）}。
#'   \item{exposure_coef}{暴露的对数效应系数（log-OR 或 log-HR）}。
#'   \item{exposure_effect}{暴露的效应估计值（OR/HR/系数），
#'     logistic/Cox 模型返回 OR/HR，其他 GLM 返回系数本身}。
#'
#' @details
#' **方法说明**
#'
#' 1. 从全模型（强制变量 + 全部候选混杂）开始。
#' 2. 每次尝试删除一个候选变量，计算暴露系数相对全模型的百分比变化。
#' 3. 选出变化最小的变量：若变化 < 阈值，永久删除该变量并更新全模型；若变化 ≥ 阈值，停止迭代。
#' 4. 重复步骤 2-3 直至不能再删除。
#'
#' **支持的模型类型**
#'
#' - GLM：logistic（family = binomial）、线性（family = gaussian）、
#'   Poisson（family = poisson）、准二项（family = quasibinomial）等
#' - Cox 比例风险模型：通过指定 time_var 参数自动启用
#'
#' **输出信息**
#'
#' 函数会在控制台输出每一步的删除尝试及效应变化百分比，
#' 以及最终的变量筛选结果。
#'
#'
#' @examples
#' \dontrun{
#' # 1. Logistic 回归
#' result <- yyds_CIE(
#'   data = mydata,
#'   exposure = "treatment",
#'   outcome = "death",
#'   forced_vars = c("age", "sex"),
#'   candidate_vars = c("bmi", "smoking", "alcohol", "comorbidity"),
#'   threshold = 10
#' )
#' result$final_covariates   # 查看最终保留的全部协变量
#' result$exposure_effect    # 查看调整后的 OR
#' summary(result$final_model)
#'
#' # 2. Cox 回归
#' result_cox <- yyds_CIE(
#'   data = mydata,
#'   exposure = "treatment",
#'   outcome = "death",
#'   forced_vars = c("age", "sex"),
#'   candidate_vars = c("bmi", "smoking", "stage"),
#'   threshold = 10,
#'   time_var = "survival_time"
#' )
#' result_cox$exposure_effect  # 查看调整后的 HR
#'
#' }
#'
#' @export
yyds_CIE <- function(data, exposure, outcome, forced_vars, candidate_vars,
                     threshold = 10, time_var = NULL,
                     family = binomial) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    install.packages("survival")
  }
  library(survival)

  model_type <- if (!is.null(time_var)) "coxph" else "glm"

  fit_model <- function(covars) {
    rhs <- paste(exposure, "+", paste(forced_vars, collapse = " + "))
    if (length(covars) > 0) {
      rhs <- paste(rhs, "+", paste(covars, collapse = " + "))
    }

    if (model_type == "coxph") {
      f <- as.formula(paste0("Surv(", time_var, ", ", outcome, ") ~ ", rhs))
      model <- coxph(f, data = data)
    } else {
      f <- as.formula(paste(outcome, "~", rhs))
      model <- glm(f, data = data, family = family)
    }

    coef_names <- names(coef(model))
    idx <- grep(paste0("^", exposure, "($|[^[:alnum:]])"), coef_names)[1]

    if (is.na(idx)) {
      stop("Exposure '", exposure, "' not found in coefficient names. ",
           "Available coefficients: ", paste(coef_names, collapse = ", "))
    }

    beta <- coef(model)[idx]
    return(list(model = model, beta = beta))
  }

  current_covars <- candidate_vars
  step_log <- data.frame(step = integer(),
                         removed = character(),
                         pct_change = numeric(),
                         stringsAsFactors = FALSE)

  cat("========================================\n")
  cat("Backward CIE (Change-in-Estimate)\n")
  cat("Model type:", model_type, "\n")
  if (model_type == "glm") cat("Family:", deparse(substitute(family)), "\n")
  cat("Threshold:", threshold, "%\n")
  cat("Exposure:", exposure, "\n")
  cat("Forced vars:", paste(forced_vars, collapse = ", "), "\n")
  cat("Candidate vars (", length(candidate_vars), "): ",
      paste(candidate_vars, collapse = ", "), "\n", sep = "")
  cat("========================================\n")

  repeat {
    if (length(current_covars) == 0) {
      cat("\nAll candidate variables removed.\n")
      break
    }

    full_result <- fit_model(current_covars)
    beta_full <- full_result$beta

    changes <- vapply(current_covars, function(var) {
      reduced_covars <- setdiff(current_covars, var)
      beta_red <- fit_model(reduced_covars)$beta
      abs((beta_red - beta_full) / beta_full) * 100
    }, numeric(1))

    min_var <- names(which.min(changes))
    min_change <- changes[min_var]

    cat(sprintf("Step %2d | Try remove: %-25s | Change: %6.2f%%\n",
                nrow(step_log) + 1, min_var, min_change))

    if (min_change < threshold) {
      step_log <- rbind(step_log,
                        data.frame(step = nrow(step_log) + 1,
                                   removed = min_var,
                                   pct_change = min_change,
                                   stringsAsFactors = FALSE))
      current_covars <- setdiff(current_covars, min_var)
    } else {
      cat("----------------------------------------\n")
      cat(sprintf("Stop: smallest change (%.2f%%) >= threshold (%d%%)\n",
                  min_change, threshold))
      break
    }
  }

  final_result <- fit_model(current_covars)
  final_all <- c(forced_vars, current_covars)

  cat("\n========================================\n")
  cat("RESULTS\n")
  cat("========================================\n")
  cat("Final model covariates (", length(final_all), "): ",
      paste(final_all, collapse = ", "), "\n", sep = "")
  if (nrow(step_log) > 0) {
    cat("Removed covariates (", nrow(step_log), "): ",
        paste(step_log$removed, collapse = ", "), "\n", sep = "")
  }

  invisible(list(
    model_type = model_type,
    family = if (model_type == "glm") deparse(substitute(family)) else NULL,
    forced_vars = forced_vars,
    final_candidate_vars = current_covars,
    final_covariates = final_all,
    removed_covariates = if (nrow(step_log) > 0) step_log$removed else character(0),
    removal_log = step_log,
    final_model = final_result$model,
    exposure_coef = final_result$beta,
    exposure_effect = if (model_type == "coxph") {
      exp(final_result$beta)
    } else if (deparse(substitute(family)) %in% c("binomial", "quasibinomial")) {
      exp(final_result$beta)
    } else {
      final_result$beta
    }
  ))
}
