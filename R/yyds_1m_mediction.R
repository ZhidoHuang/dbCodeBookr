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
#' @return 返回一个列表：
#'  - 包含所有变量的中介分析汇总表格（Total effect, ACME, ADE, proportion mediated 等）；
#'  - 报错记录；
#'  - 输出文件名。
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
                              plot_file = "plot_medication", plot_ncol = 3) {
  # 基本检查
  if (is.null(time_var) && is.null(family_type)) {
    stop("\n Error: Either 'time_var' (for survival analysis) or 'family_type' (for regression) must be provided.")
  }
  if (!is.null(time_var) && !is.null(family_type)) {
    stop("\n Error: 'time_var' and 'family_type' cannot be used together. Please choose one.")
  }
  if (!all(unique(data[[med]]) %in% c(0, 1)) && !is.numeric(data[[med]])) {
    stop("\n Error: 'med' should be a variable with values 0 and 1, or a continuous variable.")
  }

  # 工具函数：安全执行并标注阶段
  error_log <- data.frame(variable = character(),
                          stage    = character(),
                          message  = character(),
                          stringsAsFactors = FALSE)

  safe_run <- function(expr, variable, stage) {
    tryCatch(
      force(expr),
      error = function(e) {
        error_log <<- rbind(error_log,
                            data.frame(variable = variable,
                                       stage    = stage,
                                       message  = conditionMessage(e),
                                       stringsAsFactors = FALSE))
        # 抛出以便上层 tryCatch 捕获并跳到下一个变量
        stop(e)
      }
    )
  }

  # 工具函数：拼接公式右侧项，自动忽略空串
  join_terms <- function(...) {
    terms <- unlist(list(...))
    terms <- terms[!is.na(terms) & terms != ""]
    paste(terms, collapse = " + ")
  }

  med_table_all <- data.frame()
  p_list <- list()

  for (variable in xvar) {
    # 每个变量一个大 tryCatch：失败就记录并继续下一个
    tryCatch({
      all_var <- unique(c(variable, med, covar, outcome, time_var))
      covar_final <- setdiff(covar, c(variable, med))
      covar_final_B <- if (length(covar_final)) paste(covar_final, collapse = " + ") else ""

      datas1 <- stats::na.omit(data[, all_var, drop = FALSE])

      # 1) 结果模型
      if (!is.null(time_var)) {
        rhs <- join_terms(variable, med, covar_final_B)
        f1_formula <- as.formula(paste0("Surv(", time_var, ",", outcome, ") ~ ", rhs))
        model_out <- safe_run(survival::survreg(f1_formula, dist = "weibull", data = datas1),
                              variable, "fit_outcome_survreg")
      } else {
        rhs <- join_terms(variable, med, covar_final_B)
        f1_formula <- as.formula(paste(outcome, "~", rhs))
        if (family_type == "binomial") {
          model_out <- safe_run(glm(f1_formula, family = binomial(), data = datas1),
                                variable, "fit_outcome_glm_binomial")
        } else if (family_type == "gaussian") {
          model_out <- safe_run(glm(f1_formula, family = gaussian(), data = datas1),
                                variable, "fit_outcome_glm_gaussian")
        } else if (family_type == "poisson") {
          model_out <- safe_run(glm(f1_formula, family = poisson(), data = datas1),
                                variable, "fit_outcome_glm_poisson")
        } else {
          stop("Unsupported family_type. Please use 'binomial', 'gaussian', or 'poisson'.")
        }
      }

      # 2) 中介模型
      is_binary_med <- length(unique(datas1[[med]])) == 2
      rhs_med <- join_terms(variable, covar_final_B)
      f2_formula <- as.formula(paste(med, "~", rhs_med))
      if (is_binary_med) {
        model_med <- safe_run(glm(f2_formula, family = binomial(), data = datas1),
                              variable, "fit_mediator_glm_binomial")
      } else {
        model_med <- safe_run(lm(f2_formula, data = datas1),
                              variable, "fit_mediator_lm")
      }

      # 3) 中介分析 + 捕获 warning
      warn_messages <- character(0)
      mediate_result <- safe_run(
        withCallingHandlers(
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
        ),
        variable, "mediate"
      )

      # 4) 汇总输出
      med_table <- yyds_med_table(mediate_result)
      med_table[1, 6] <- paste0(variable, "→", med, "→", outcome)
      # 用样本量更直观；若想沿用 df.residual 可改回
      med_table[2, 6] <- paste0("sample size: ", nrow(datas1))
      if (length(warn_messages) > 0) {
        med_table[3, 6] <- paste0("Warning: ", paste(warn_messages, collapse = "; "))
      }

      model.m.text <- gsub("f2_formula", paste(deparse(f2_formula), collapse = ""),
                           paste(deparse(mediate_result$model.m$call), collapse = ""))
      model.y.text <- gsub("f1_formula", paste(deparse(f1_formula), collapse = ""),
                           paste(deparse(mediate_result$model.y$call), collapse = ""))
      med_table[1, 7] <- paste0(
        paste(model.m.text, "\n\n"),
        paste(model.y.text,  "\n\n"),
        paste(deparse(mediate_result$call), collapse = "")
      )

      cat("\n\n", variable, "→", med, "→", outcome, "\n")
      med_table_all <- rbind(med_table_all, med_table[1:4, ], med_table[99, ])

      # 5) 画图（失败也不影响整体）
      if (!is.null(plot_file)) {
        p <- try({
          DiagrammeR::grViz(glue::glue('
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
        }, silent = TRUE)

        if (!inherits(p, "try-error")) {
          p_list[[variable]] <- p
        } else {
          error_log <- rbind(error_log,
                             data.frame(variable = variable,
                                        stage    = "plot",
                                        message  = as.character(attr(p, "condition")$message %||% "plot error"),
                                        stringsAsFactors = FALSE))
        }
      }

    }, error = function(e) {
      # 已在 safe_run 记录；这里静默继续下一个变量
      invisible(NULL)
    })
  }

  # 6) 保存拼图（只要至少有一个成功）
  saved_plot_file <- NULL
  if (!is.null(plot_file) && length(p_list) > 0) {
    num_per_row <- if (is.null(plot_ncol)) 3 else plot_ncol
    num_rows <- ceiling(length(p_list) / num_per_row)
    combined <- htmltools::tagList()

    for (row in 1:num_rows) {
      start_index <- (row - 1) * num_per_row + 1
      end_index <- min(row * num_per_row, length(p_list))
      row_plots <- p_list[start_index:end_index]
      col_width <- 100 / num_per_row

      row_html <- htmltools::tags$div(
        style = "display: flex; flex-wrap: wrap; justify-content: center;",
        lapply(row_plots, function(plot) {
          htmltools::tags$div(
            style = paste0("flex: 1 1 ", col_width, "%; max-width: ", col_width, "%; padding: 5px; box-sizing: border-box;"),
            plot
          )
        })
      )
      combined <- htmltools::tagAppendChild(combined, row_html)
    }

    htmltools::save_html(combined, file = paste0(plot_file, ".html"))
    cat("\n Plot saved as '", plot_file, ".html'.")
    saved_plot_file <- paste0(plot_file, ".html")
  }

  # 返回结果与错误日志
  return(list(
    results    = med_table_all,
    errors     = error_log,
    plots_file = saved_plot_file
  ))
}

