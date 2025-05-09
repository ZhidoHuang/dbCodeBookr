#' 提取和结构化 Fine-Gray 模型结果表
#'
#' 本函数适用于 `riskRegression::FGR()` 创建的 Fine-Gray 竞争风险回归模型结果（生存分析），
#' 自动提取HR（及其95%置信区间）、P值，并输出结构化的结果表。对于因子变量，还会自动添加参照组行并计算事件发生数。
#'
#' @param fg_model 使用 `FGR()` 拟合得到的 Fine-Gray 模型对象。
#'
#' @return 返回一个数据框，包含以下列：
#' \describe{
#'   \item{Variable}{变量名称及分类变量的水平，参照组自动标注为"ref"}
#'   \item{Events, n/N}{事件数/非事件数/总数}
#'   \item{HR (95% CI)}{风险比及其95%置信区间}
#'   \item{p-value}{P值，小于0.001的标记为<0.001}
#'   \item{sign}{显著性标记：`*` `<0.05`，`**` `<0.01`，`***` `<0.001`}
#'   \item{HR}{HR值（数值型，保留两位小数）}
#'   \item{Lower}{95% CI下限}
#'   \item{Upper}{95% CI上限}
#' }
#'
#' @examples
#' \dontrun{
#' library(riskRegression)
#' library(dplyr)
#' library(tidyr)
#'
#' # 构造模拟数据
#' set.seed(42)
#' n <- 500
#' dt_cox <- data.frame(
#'   Die_permth_int = rexp(n, 0.1),
#'   fg_status = sample(0:2, n, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
#'   dex_SII.q4 = factor(sample(1:4, n, replace = TRUE)),
#'   A_age = rnorm(n, 60, 10),
#'   A_sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
#'   A_race = factor(sample(1:3, n, replace = TRUE)),
#'   A_BMI = rnorm(n, 25, 5),
#'   A_pir1.3 = factor(sample(0:1, n, replace = TRUE)),
#'   A_edu2 = factor(sample(1:3, n, replace = TRUE)),
#'   A_marital = factor(sample(0:1, n, replace = TRUE)),
#'   A_smoke = factor(sample(0:1, n, replace = TRUE)),
#'   A_alcohol_intake = factor(sample(0:1, n, replace = TRUE)),
#'   A_LTPA_all = factor(sample(0:1, n, replace = TRUE))
#' )
#'
#' # 拟合 Fine-Gray 模型
#' fg_model <- FGR(
#'   formula = prodlim::Hist(time = Die_permth_int, event = fg_status) ~
#'     dex_SII.q4 + A_age + A_sex + A_race + A_BMI + A_pir1.3 +
#'     A_edu2 + A_marital + A_smoke + A_alcohol_intake + A_LTPA_all,
#'   data = dt_cox,
#'   cause = 1
#' )
#'
#' # 输出结构化表格
#' fg_table <- yyds_FGR_table(fg_model)
#' print(fg_table)
#' }
#'
#' @export
yyds_FGR_table <- function(fg_model) {

  # 提取模型摘要
  fg_summary <- summary(fg_model)
  coef_data <- fg_summary$coef
  conf_int_data <- fg_summary$conf.int

  # 计算HR (95% CI) 和 P Value
  hr_ci <- paste0(
    sprintf("%.2f", conf_int_data[, "exp(coef)"]), " (",
    sprintf("%.2f", conf_int_data[, "2.5%"]), "-",
    sprintf("%.2f", conf_int_data[, "97.5%"]), ")"
  )

  p_values <- ifelse(
    coef_data[, "p-value"] < 0.001,
    "<0.001",
    formatC(coef_data[, "p-value"], digits = 3, format = "f", zero.print = TRUE)
  )

  # 创建结果数据框
  fg_results <- data.frame(
    Variable = rownames(coef_data),
    `HR (95% CI)` = hr_ci,
    `P Value` = p_values,
    hr = sprintf("%.2f", conf_int_data[, "exp(coef)"]),
    lower = sprintf("%.2f", conf_int_data[, "2.5%"]),
    upper = sprintf("%.2f", conf_int_data[, "97.5%"]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  rownames(fg_results) <- NULL

  # 提取公式中的变量
  textvar <- fg_model$call$formula[[3]]
  ref_vars <- trimws(unlist(strsplit(as.character(textvar), "\\+")))

  # 移除fg_results中已有的变量
  ref_vars <- ref_vars[!ref_vars %in% c(fg_results$Variable, "")]

  # 获取原始数据集
  dt_cox <- eval(fg_model$call$data)

  # 为每个分类变量添加参照组
  for (var in ref_vars) {

    # 获取变量的水平
    levels <- levels(as.factor(dt_cox[[var]]))

    # 找到包含该变量的行
    rows <- min(grep(var, fg_results$Variable))

    # 将fg_results$Variable中的对应变量替换为空格
    fg_results$Variable <- gsub(var, paste0("xx",var), fg_results$Variable)

    # 创建参照组行
    ref_row <- data.frame(
      Variable = c(var, paste0("xx",var,levels[1])),
      `HR (95% CI)` = c("", "ref"),
      `P Value` = c("", "-"),
      hr = c("","1.00"),
      lower = c("","1.00"),
      upper = c("","1.00"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # 插入参照组
    if (rows > 1) {
      fg_results <- bind_rows(
        fg_results[1:(rows - 1), , drop = FALSE],  # 保留数据框结构
        ref_row,
        fg_results[rows:nrow(fg_results), , drop = FALSE]
      )
    } else {
      # 如果rows = 1，直接将ref_row插入到最前面
      fg_results <- bind_rows(ref_row, fg_results)
    }
  }

  # 提取终点
  outcome <- tryCatch({
    fg_model$call$formula[[2]][[3]][[2]]
  }, error = function(e) {
    # 出现错误时执行此部分代码，返回fg_model$call$formula[[2]][[3]]
    fg_model$call$formula[[2]][[3]]
  })

  # 计算分类变量的event
  event_all_list <- sapply(ref_vars, function(var) {
    event_fg <- as.data.frame(table(dt_cox[[var]], dt_cox[[outcome]]))
    event_fg_wide <- pivot_wider(event_fg, names_from = Var2, values_from = Freq, values_fill = list(Freq = 0))
    event_fg_wide$total <- rowSums(event_fg_wide[, -1], na.rm = TRUE)
    event_fg_wide$event <- paste0(event_fg_wide$`1`, "/", event_fg_wide$`2`, "/", event_fg_wide$total)
    event_fg_wide$Var1 <- paste0("xx", var, event_fg_wide$Var1)
    return(event_fg_wide)
  }, simplify = FALSE)
  event_all <- bind_rows(event_all_list)

  # 计算总数，给连续变量
  outcome_table <- table(dt_cox[[outcome]])
  event_tatol <- paste(outcome_table["1"], outcome_table["2"], sum(outcome_table), sep = "/")

  # 链接计算好的事件
  fg_results <-left_join(fg_results, event_all, by = c("Variable" = "Var1"))

  fg_results$event <- ifelse(is.na(fg_results$event)&fg_results$hr!="", event_tatol, fg_results$event)

  # 重新改变量名
  for (var in ref_vars) {
    fg_results$Variable <- gsub(paste0("xx",var), "  ",fg_results$Variable)
  }

  fg_results$Significance <- suppressWarnings(ifelse(fg_results$`P Value` == "<0.001", "***",
                                                     ifelse(as.numeric(fg_results$`P Value`) < 0.01, "**",
                                                            ifelse(as.numeric(fg_results$`P Value`) < 0.05, "*", "")))
  )


  fg_results <- fg_results[,c("Variable","event","HR (95% CI)","P Value","Significance","hr","lower","upper")]
  colnames(fg_results) <- c("Variable","Events, n/N","HR (95% CI)","p-value","sign","HR","Lower","Upper")

  return(fg_results)
}
