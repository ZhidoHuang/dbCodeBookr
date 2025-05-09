#' 简单中介分析（一对多，含可视化）
#'
#' 针对多个暴露变量与一个结局变量之间的关系，执行中介分析，并通过指定的中介变量评估间接效应。
#' 支持一般线性模型（GLM）与生存分析（使用 Weibull 回归），并自动输出中介分析表格及 HTML 格式的可视化图。
#'
#' @param xvar 字符型向量，表示多个暴露变量的名称。
#' @param covar 字符型向量，表示需要调整的协变量名称。
#' @param med 字符串，表示中介变量的名称，可以是二分类或连续变量。
#' @param outcome 字符串，表示结局变量的名称。
#' @param time_var （可选）字符串，表示生存时间变量，仅在进行生存分析时需要指定。
#' @param data 数据框，包含所有用于分析的变量。
#' @param family_type （可选）字符串，指定用于回归模型的分布族类型，可选值包括 `"binomial"`、`"gaussian"` 或 `"poisson"`。若设置了 `time_var`，则不需要此参数。
#' @param sims 整数，表示中介分析中用于 Bootstrap 的模拟次数，默认为 100。
#' @param plot_file （可选）字符串，输出 HTML 可视化图的文件名前缀，默认值为 `"plot_medication"`。
#' @param plot_ncol （可选）整数，指定每行显示的图数量，默认值为 3。
#'
#' @return 返回一个数据框，包含所有变量的中介分析汇总表格（Total effect, ACME, ADE, proportion mediated 等）。
#'
#' @details
#' 函数会对每一个 `xvar` 中的暴露变量执行一次中介分析，分别构建中介模型与结果模型，并使用 `mediation::mediate()` 计算中介效应。
#' 若指定了 `time_var`，则使用 `survival::survreg()` 拟合 Weibull 生存模型；否则根据 `family_type` 使用 `glm()` 拟合广义线性模型。
#'
#' 对于每一次分析，函数会输出：
#' - Total effect（总效应）
#' - ACME（间接效应）
#' - ADE（直接效应）
#' - Proportion Mediated（中介比例）
#' - 模型公式及调用信息
#'
#' 若设置了 `plot_file`，函数还将生成中介路径图并保存为 HTML 文件。
#'
#' @importFrom mediation mediate
#' @importFrom survival Surv survreg
#' @importFrom DiagrammeR grViz
#' @importFrom htmltools save_html tagList tags tagAppendChild
#' @importFrom glue glue
#'
#' @examples
#' # 构造模拟数据
#' set.seed(123)
#' n <- 300
#' data_sim <- data.frame(
#'   age = rnorm(n, mean = 50, sd = 10),
#'   bmi = rnorm(n, mean = 25, sd = 4),
#'   sex = sample(c(0, 1), n, replace = TRUE),     # 0 = female, 1 = male
#'   smoking = sample(c(0, 1), n, replace = TRUE),
#'   crp = rnorm(n, mean = 2, sd = 1),             # 中介变量
#'   followup_years = rexp(n, rate = 0.1),         # 生存时间
#'   event = rbinom(n, 1, 0.3)                     # 结局（事件发生）
#' )
#'
#' # 运行函数示例（生存分析）
#' result <- yyds_1m_mediction(
#'   xvar = c("age", "bmi"),
#'   covar = c("sex", "smoking"),
#'   med = "crp",
#'   outcome = "event",
#'   time_var = "followup_years",
#'   data = data_sim,
#'   sims = 50,
#'   plot_file = "demo_med",
#'   plot_ncol = 2
#' )
#'
#' # 查看结果
#' print(result)
#'
#' @export
yyds_1m_mediction <- function(data,
                              xvar,
                              med,
                              covar,
                              outcome,
                              time_var = NULL,
                              family_type = NULL, sims = 100,
                              plot_file = "plot_medication", plot_ncol = 3
) {
  # 检查输入参数
  # 检查 time_var 和 family_type 是否同时为 NULL
  if (is.null(time_var) && is.null(family_type)) {
    stop("\n Error: Either 'time_var' (for survival analysis) or 'family_type' (for regression) must be provided.")
  }

  # 检查 time_var 和 family_type 不能同时出现
  if (!is.null(time_var) && !is.null(family_type)) {
    stop("\n Error: 'time_var' and 'family_type' cannot be used together. Please choose one.")
  }

  # 检查 med 变量类型是否有效：要么是0/1，要么是连续变量
  if (!all(unique(data[[med]]) %in% c(0, 1)) && !is.numeric(data[[med]])) {
    stop("\n Error: 'med' should be a variable with values 0 and 1, or a continuous variable.")
  }

  med_table_all <- data.frame()
  p_list <- list()
  for (variable in xvar) {
    all_var <- unique(c(variable, med, covar, outcome, time_var))
    covar_final <- setdiff(covar, c(variable, med))
    covar_final_B <- paste(covar_final, collapse = "+")
    datas1 <- na.omit(data[, all_var])

    # 1. 结果模型：根据 time_var 判断是否是生存分析
    if (!is.null(time_var)) {
      f1_formula <- as.formula(paste("Surv(", time_var, ",", outcome, ") ~",
                                     variable, "+", med, "+", covar_final_B))
      model_out <- survival::survreg(f1_formula, dist = "weibull", data = datas1)
    } else {
      f1_formula <- as.formula(paste(outcome, "~", variable, "+", med, "+", covar_final_B))

      # 2. 根据 family_type 传入正确的 family
      if (family_type == "binomial") {
        model_out <- glm(f1_formula, family = binomial(), data = datas1)
      } else if (family_type == "gaussian") {
        model_out <- glm(f1_formula, family = gaussian(), data = datas1)
      } else if (family_type == "poisson") {
        model_out <- glm(f1_formula, family = poisson(), data = datas1)
      } else {
        stop("Unsupported family_type. Please use 'binomial', 'gaussian', or 'poisson'.")
      }
    }

    # 3. 判断 med 是否为二分类变量
    is_binary_med <- length(unique(datas1[[med]])) == 2

    if (is_binary_med) {
      # 二分类变量，使用逻辑回归
      f2_formula <- as.formula(paste(med, "~", variable, "+", covar_final_B))
      model_med <- glm(f2_formula, family = binomial(), data = datas1)
    } else {
      # 连续变量，使用线性回归
      f2_formula <- as.formula(paste(med, "~", variable, "+", covar_final_B))
      model_med <- lm(f2_formula, data = datas1)
    }

    # 4. 中介分析
    warn_messages <- character(0)
    mediate_result <- withCallingHandlers(
      {
        set.seed(123)
        mediation::mediate(model_med, model_out,
                           sims = sims, bootstrap = TRUE, boot.ci.type = "perc",
                           treat = variable, mediator = med)
      },
      warning = function(w) {
        warn_messages <<- c(warn_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )

    # 5. 汇总输出
    med_table <- yyds_med_table(mediate_result)
    med_table[1,6] <- paste0(variable, "→", med, "→", outcome)
    med_table[2,6] <- paste0("sample size: ", mediate_result$model.y$df.residual)
    if (length(warn_messages) > 0) {
      med_table[3,6] <- paste0("Warning: ", paste(warn_messages, collapse = "; "))
    }

    model.m.text <- gsub("f2_formula", paste(deparse(f2_formula), collapse = ""), paste(deparse(mediate_result$model.m$call), collapse = ""))
    model.y.text <- gsub("f1_formula", paste(deparse(f1_formula), collapse = ""), paste(deparse(mediate_result$model.y$call), collapse = ""))
    med_table[1,7] <- paste0(paste(model.m.text,"\n\n"),
                             paste(model.y.text, "\n\n"),
                             paste(deparse(mediate_result$call), collapse = ""))

    cat("\n\n", variable, "→", med, "→", outcome, "\n")
    # 表格汇总
    med_table_all <- rbind(med_table_all, med_table[1:4,], med_table[99,])

    if (!is.null(plot_file)) {
      p <- DiagrammeR::grViz(glue::glue('
digraph simple_mediation {{
  graph [layout = dot, rankdir = LR, ranksep = 0.5, nodesep = 1.5]
  node [shape=box, style=filled, fontname="Helvetica-Bold", fontcolor=black, penwidth=2, fontsize=16, fixedsize=true, width=1.5, height=0.8, margin=0.1]
  X [label = "{variable}", fillcolor = lightblue]
  M [label = "{med}", fillcolor = lightblue]
  Y [label = "{outcome}", fillcolor = lightblue]

  edge [fontname=Helvetica, fontsize=16, penwidth=2, color=darkgray]

  M_text [label="

















ACME = {med_table[2,2]}, p = {med_table[2,5]}







Prop. Mediated = {round(med_table[4,2]*100, 1)}%, p = {med_table[4,5]}",
          style=dashed, shape=plaintext, fontname=Helvetica, fontsize=16, fontcolor=black, width=.1, height=.1]

  {{ rank = same; M_text; M; }}

  X -> M [color = black]
  M -> Y [color = black]
  X -> Y [label = "ADE = {med_table[3,2]}, p = {med_table[3,5]}", fontname=Helvetica, fontcolor=black, color=black]
  X -> Y [label = "Total Effect = {med_table[1,2]}, p = {med_table[1,5]}", fontname=Helvetica, fontcolor=black, style=dashed, color=gray50]
}}
'))
      p_list[[variable]] <- p
    }
  }


  if (!is.null(plot_file)) {

    if (is.null(plot_ncol)) {
      num_per_row <- 3
    } else {
      num_per_row <- plot_ncol
    }

    # 计算多少行
    num_rows <- ceiling(length(p_list) / num_per_row)

    # 创建多个 div 来实现按列排布
    combined <- htmltools::tagList()

    for (row in 1:num_rows) {
      # 获取当前行的图形
      start_index <- (row - 1) * num_per_row + 1
      end_index <- min(row * num_per_row, length(p_list))
      row_plots <- p_list[start_index:end_index]

      # 计算每列的宽度百分比
      col_width <- 100 / num_per_row

      # 使用 flexbox 布局将当前行的图形并排显示
      row_html <- htmltools::tags$div(
        style = "display: flex; flex-wrap: wrap; justify-content: center;",
        lapply(row_plots, function(plot) {
          htmltools::tags$div(
            style = paste0("flex: 1 1 ", col_width, "%; max-width: ", col_width, "%; padding: 5px; box-sizing: border-box;"),
            plot
          )
        })
      )

      # 将当前行的 HTML 添加到 combined 中
      combined <- htmltools::tagAppendChild(combined, row_html)
    }

    # 保存 HTML 文件
    htmltools::save_html(combined, file = paste0(plot_file,".html"))
    cat("\n Plot saved as '",plot_file,".html'.")
  }


  return(med_table_all)
}
