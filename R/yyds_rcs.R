#' 限制性立方样条（RCS）分析可视化函数
#'
#' 该函数基于 \pkg{rms} 包，对 Cox、Logistic 和线性回归模型中的限制性立方样条（Restricted Cubic Spline, RCS）进行建模分析与可视化。
#' 支持自动选择 knots 数量，支持置信区间、参考点、分布图与非线性检验。
#' 支持模型包括：
#' - `cph`: Cox 比例风险模型
#' - `lrm`: Logistic 回归模型
#' - `ols`: 线性回归模型
#'
#' @param fit 模型对象，支持 `rms` 包中的 `cph`、`lrm` 和 `ols`。
#' @param ref_zero 逻辑值。是否设置参考点并计算相对效应（默认 `TRUE`）。
#'   - **Cox 模型**：若 `TRUE`，则 `fun = exp`，输出 “Hazard Ratio (vs Ref)”。
#'   - **Logistic 模型**：若 `TRUE`，则 `fun = exp` 输出 “Odds Ratio (vs Ref)”；否则 `plogis` 输出概率。
#'   - **线性模型**：若 `TRUE`，输出 “Difference in Y (vs Ref)”；否则输出预测值。
#' @param ref 数值型。参考点位置（默认 `NULL`，自动使用中位数）。仅在 `ref_zero = TRUE` 时生效。
#' @param dist_type 字符串，控制分布展示方式：
#'   - `"hist"`：直方图（默认）
#'   - `"density"`：密度曲线
#'   - `"none"`：不显示分布
#' @param rug 逻辑值。是否在 x 轴添加地毯图（默认 `FALSE`）。
#' @param color_dist 字符串。分布图颜色（默认 `"#374E55"`）。
#' @param color_rcs 字符串。RCS 曲线颜色（默认 `"#F79118"`）。
#'
#' @return 返回一个包含以下元素的列表（不可见）：
#' - `plot`: ggplot2 图形对象（可以调用继续用\pkg{ggplot2}作图）
#' - `predictions`: 数据框，包含 x 值、预测值及其置信区间
#' - 输出RCS_info.txt 文件，记录打印信息
#'
#' 结果数据框具有以下属性：
#' - `p_overall`: 整体 P 值（来自 ANOVA）
#' - `p_nonlinear`: 非线性 P 值（来自 ANOVA）
#' - `Inflection_point`: 拐点位置（若检测到）
#' - `best_knot`: 最佳 knots 数量（若为自动选择）
#' - `model_type`: 模型类型（"cox"、"logistic"、"linear"）
#'
#' @examples
#' \dontrun{
#' library(rms)
#'
#' # Cox 模型
#' # 函数里会提取模型信息重新设置，这里仅为了顺利建模rcs()
#' options(datadist = NULL)
#' fit <- cph(Surv(time, status) ~ rcs(age) + sex, data = data)
#' yyds_rcs(fit)
#'
#' # Logistic 模型 + 指定参考点
#' fit <- lrm(death ~ rcs(bmi, 5), data = data)
#' yyds_rcs(fit, ref_zero = TRUE, ref = 22.5)
#'
#' # 线性模型，不设置参考点
#' fit <- ols(y ~ rcs(age, 4), data = data)
#' yyds_rcs(fit, ref_zero = FALSE)
#' }
#'
#' @importFrom rms Predict datadist cph lrm ols
#' @importFrom crayon green red yellow blue bold underline
#' @export
yyds_rcs <- function(fit, ref_zero = TRUE, ref = NULL,
                     dist_type = c("hist", "density","none"), rug = FALSE,
                     color_dist = "#374E55", color_rcs = "#F79118") {

  # 1. 识别模型类型 ----------------------------------------------------------
  model_type <- class(fit)[1]
  if (!model_type %in% c("cph", "lrm", "ols")) {
    stop("仅支持 cph (Cox), lrm (Logistic), ols (Linear) 模型")
  }

  # 2. 提取变量和数据 -------------------------------------------------------
  vars <- all.vars(formula(fit))
  data <- eval(fit$call$data, envir = parent.frame())
  data <- data[, vars]
  data <- na.omit(data)

  # 3. 提取RCS变量名 -------------------------------------------------------
  formula_text <- paste(formula(fit), collapse = "")
  xvar <- gsub(".*rcs\\(([^,]+),?.*\\).*", "\\1", formula_text)

  if (model_type == "cph") {
    O <- vars[2]
  } else {
    O <- vars[1]
  }

  # 定义一个辅助函数，同时输出到控制台和文件
  dual_cat <- function(..., file = "RCS_info.txt", append = TRUE) {
    cat(... )  # 输出到控制台
    clean_text <- gsub("\033\\[[0-9;]+m", "", paste0(...))
    cat(clean_text, file = file, append = append)  # 输出到文件
  }

  # 4. 打印基础信息 --------------------------------------------------------
  dual_cat(paste0(blurred("\n------------------------------------------")," 基础信息 \n"))
  dual_cat(blue("Model type  :", model_type, "\n"))
  dual_cat("Sample size :", nrow(data), "\n")
  dual_cat("Outcome     :", O, "\n")
  dual_cat("Exposure    :", xvar, "\n")
  dual_cat("median      :", round(median(data[[xvar]]), 3), "\n")


  # 5. 设置datadist和参考点 -------------------------------------------------
  dd <- datadist(data)
  if (ref_zero == TRUE) {
    if (!is.null(ref)) {
      dd$limits[[xvar]][2] <- ref
      dual_cat(red(paste0("参考点       :手动指定 ref =", ref, "\n")))
    } else {
      dual_cat(red("参考点       :中位数(默认) \n"))
    }
  } else {
    dual_cat(red("参考点       :无 \n"))
  }
  options(datadist = dd)

  # 6. 提取因变量和协变量 ---------------------------------------------------
  if (model_type == "cph") {
    outcome <- vars[1:2]
  } else {
    outcome <- vars[1]
  }
  all_covars <- vars
  covars <- setdiff(all_covars, outcome)
  other_covars <- setdiff(covars, xvar)

  # 7. Knots选择逻辑 -------------------------------------------------------
  has_knot <- grepl("rcs\\([^,]+,\\s*\\d+\\)", formula_text)

  dual_cat(paste0(blurred("------------------------------------------")," Knots选择 \n"))
  if (!has_knot) {
    fits <- lapply(3:7, function(k) {
      formula_rcs <- as.formula(
        paste0(
          if (model_type == "cph") {
            paste0("Surv(", outcome[1], ", ", outcome[2], ") ~ rcs(", xvar, ", ", k, ")")
          } else {
            paste0(outcome, " ~ rcs(", xvar, ", ", k, ")")
          },
          if (length(other_covars) > 0) paste0(" + ", paste(other_covars, collapse = " + ")) else ""
        )
      )

      switch(model_type,
             "cph" = cph(formula_rcs, data = data, x = TRUE, y = TRUE, surv = TRUE),
             "lrm" = lrm(formula_rcs, data = data, x = TRUE, y = TRUE),
             "ols" = ols(formula_rcs, data = data, x = TRUE, y = TRUE))
    })
    aics <- sapply(fits, AIC)

    # 打印格式化表格
    dual_cat("+--------+----------+----------+----------+----------+----------+\n")
    dual_cat("| Knots  |    3     |    4     |    5     |    6     |    7     |\n")
    dual_cat("+--------+----------+----------+----------+----------+----------+\n")
    dual_cat(sprintf("|  AIC   | %8.2f | %8.2f | %8.2f | %8.2f | %8.2f |\n",
                     aics[1], aics[2], aics[3], aics[4], aics[5]))
    dual_cat("+--------+----------+----------+----------+----------+----------+\n")

    best_k <- which.min(aics)
    fit_rcs <- fits[[best_k]]
    used_knot <- best_k + 2
    dual_cat(red(paste0("选用最佳 knots 数量: ", used_knot, "（AIC 最小）\n")))
  } else {
    fit_rcs <- fit
    used_knot <- length(fit_rcs$Design$parms[[xvar]])
    dual_cat(red(paste0("手动指定的 knots 数量: ", used_knot, "\n")))
    dual_cat(paste0("对应模型 AIC: ", round(AIC(fit_rcs), 2), "\n"))
  }

  # 8. anova 得到 p 值 ------------------------------------------------------
  dual_cat(paste0(blurred("------------------------------------------")," P值结果 \n"))
  aov <- anova(fit_rcs)
  p_overall <- aov[1, "P"]
  p_nonlinear <- aov[2, "P"]
  dual_cat(sprintf("P_overall    = %.4g\n", p_overall))
  dual_cat(sprintf("P_nonlinear  = %.4g\n", p_nonlinear))


  # 9. 模型特异性设置 ------------------------------------------------------
  if (model_type == "cph") {
    fun <- exp
    ylab <- if (ref_zero) "Hazard Ratio (vs Ref)" else "HR (95% CI)"
    hline <- 1
  } else if (model_type == "lrm") {
    fun <- if (ref_zero) exp else plogis
    ylab <- if (ref_zero) "Odds Ratio (vs Ref)" else "Probability"
    hline <- if (ref_zero) 1 else NULL
  } else if (model_type == "ols") {
    fun <- NULL
    ylab <- if (ref_zero) "Difference in Y (vs Ref)" else "Predicted Y"
    hline <- if (ref_zero) 0 else NULL
  }


  # 10. 生成预测值 ----------------------------------------------------------
  p <- do.call(Predict, list(fit_rcs, as.name(xvar), ref.zero = ref_zero, fun = fun))

  # 找到 yhat 的最小值和最大值
  min_yhat <- which.min(p$yhat)
  max_yhat <- which.max(p$yhat)
  min_x <- p[[xvar]][min_yhat]
  max_x <- p[[xvar]][max_yhat]

  # 11. 拐点检测逻辑 -------------------------------------------------------
  dual_cat(paste0(blurred("------------------------------------------")," 拐点观察 \n"))
  # 如果最小值或最大值在中间段，则返回对应的 x 值作为拐点
  if (min_x > quantile(p[[xvar]], 0.05) && min_x < quantile(p[[xvar]], 0.95)) {
    Inflection_point <- min_x
    dual_cat(sprintf("Inflection point (yhat 最小点) = %.3f\n", Inflection_point))
  } else if (max_x > quantile(p[[xvar]], 0.05) && max_x < quantile(p[[xvar]], 0.95)) {
    Inflection_point <- max_x
    dual_cat(sprintf("Inflection point (yhat 最大点) = %.3f\n", Inflection_point))
  } else {
    Inflection_point <- NA  # 没有拐点
    dual_cat(sprintf("没有观察到显著拐点\n"))
  }

  # 添加属性
  attr(p, "p_overall") <- p_overall
  attr(p, "p_nonlinear") <- p_nonlinear
  attr(p, "Inflection_point") <- Inflection_point
  attr(p, "best_knot") <- used_knot
  attr(p, "model_type") <- model_type

  dual_cat(paste0(blurred("====================================================")," \n\n"))

  # 12. 绘图设置 -----------------------------------------------------------
  # 1. 计算密度或直方图数据
  if ("hist" %in% dist_type) {
    hist_data <- hist(data[[xvar]], plot = FALSE, breaks = 30)  # breaks控制分箱数
    hist_df <- data.frame(
      xmin = head(hist_data$breaks, -1),  # 每组左边界
      xmax = tail(hist_data$breaks, -1),  # 每组右边界
      density = hist_data$density         # 使用密度值（与回归曲线比例匹配）
    )

    range_y <- range(c(p$lower, p$upper), na.rm = TRUE)
    scale_factor <- diff(range_y) / max(hist_df$density, na.rm = TRUE) * 0.8  # 添加比例系数

    plot_histogram <- geom_rect(data = hist_df,
                                aes(xmin = xmin, xmax = xmax,
                                    ymin = min(range_y) - diff(range_y)*0.1,  # 从Y轴最小值开始
                                    ymax = min(range_y) - diff(range_y)*0.1 + density * scale_factor),
                                fill = color_dist, alpha = 0.1, color = "white")
  } else if ("density" %in% dist_type) {
    dens <- density(data[[xvar]], na.rm = TRUE)
    density_df <- data.frame(A_BMI = dens$x, density = dens$y)

    range_y <- range(c(p$lower, p$upper), na.rm = TRUE)
    scale_factor <- diff(range_y) / max(density_df$density, na.rm = TRUE) * 0.8  # 添加比例系数

    plot_histogram <- geom_polygon(data = density_df,
                                   aes(x = A_BMI,
                                       y = min(range_y) - diff(range_y)*0.1 + density * scale_factor),  # 从Y轴最小值开始
                                   fill = color_dist, alpha = 0.1)
  } else if ("none" %in% dist_type) {
    scale_factor <- 1
    plot_histogram <- NULL
  }

  rug_layer <- if (rug) {
    list(geom_rug(data = data, aes(x = .data[[xvar]]), sides = "b", color = color_dist,
                  alpha = 0.1))
  } else {
    list()
  }

  ref_vline <- if (ref_zero == TRUE) {
    if (!is.null(ref)) {
      list(geom_vline(xintercept = ref, linetype = "solid"))
    } else {
      list(geom_vline(xintercept = median(data[[xvar]]), linetype = "solid"))
    }
  } else {
    list()
  }

  ref_vline_text <- if (ref_zero == TRUE) {
    if (!is.null(ref)) {
      list(  geom_text(aes(x = ref, y = Inf,
                           label = paste0("ref = ", sprintf("%.3f", ref))),
                       vjust = 8, hjust = -0.1, size = 4.5))
    } else {
      list(geom_text(aes(x = median(data[[xvar]]), y = Inf,
                         label = paste0("median = ", sprintf("%.3f", median(data[[xvar]])))),
                     vjust = 8, hjust = -0.1, size = 4.5))
    }
  } else {
    list()
  }

  hline_layer <- if (!is.null(hline)) {
    geom_hline(yintercept = hline, linetype = "dashed")
  } else {
    NULL
  }

  # 2. 创建图形 -----------------------------------------------------------
  plot_rcs<-ggplot() +
    # 直方图或密度图
    plot_histogram +
    # 地毯线
    rug_layer +
    # 主回归曲线和置信区间
    geom_line(data = p, aes(x = .data[[xvar]], y = yhat), color = color_rcs, linewidth = 1) +
    geom_line(data = p, aes(x = .data[[xvar]], y = lower), color = color_rcs, linewidth = 0.5) +
    geom_line(data = p, aes(x = .data[[xvar]], y = upper), color = color_rcs, linewidth = 0.5) +
    geom_ribbon(data = p, aes(x = .data[[xvar]], ymin = lower, ymax = upper),
                alpha = 0.2, fill = color_rcs) +
    # 添加垂直线
    ref_vline +
    ref_vline_text +
    # 添加参考线
    hline_layer +
    # 添加文本注释
    geom_text(aes(x = -Inf, y = Inf,
                  label = ifelse(p_overall < 0.001,
                                 paste0("italic('P overall < 0.001')"),
                                 paste0("italic('P overall = ", sprintf("%.3f", p_overall), "')"))),
              parse = TRUE, hjust = -0.1, vjust = 2, size = 4.5) +
    geom_text(aes(x = -Inf, y = Inf,
                  label = ifelse(p_nonlinear < 0.001,
                                 paste0("italic('P nonlinear < 0.001')"),
                                 paste0("italic('P nonlinear = ", sprintf("%.3f", p_nonlinear), "')"))),
              parse = TRUE, hjust = -0.1, vjust = 4, size = 4.5) +

    # 双Y轴设置
    scale_y_continuous(
      name = ylab,
      breaks = function(x) {
        b <- scales::pretty_breaks(n = 5)(x)
        b <- sort(unique(c(b, hline)))
        b
      },
      expand = expansion(mult = c(0.05, 0.2))  # 上下各扩展10%的空间
    ) +
    labs(title = NULL, x = xvar) +
    theme_bw() +
    theme(panel.grid = element_blank())
  # 显示图形
  print(plot_rcs)
  # 返回结果
  on.exit(rm(dd), add = TRUE)
  options(datadist = NULL)
  invisible(list(predictions = p, plot = plot_rcs))
}

