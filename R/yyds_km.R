#' Kaplan-Meier 曲线与竞争风险累计发生率曲线（CIF）绘图
#'
#' 根据结局变量编码自动选择绘图方法：结局为 0/1 时绘制 Kaplan-Meier
#' 累计事件概率曲线，结局为 0/1/2 时绘制竞争风险累计发生率曲线（CIF）。
#' 函数支持未调整曲线和协变量调整后的模型预测曲线，并可选显示风险表格。
#'
#' 对于 0/1 结局：
#' \itemize{
#'   \item 未调整时使用 `survival::survfit()` 绘制 `1 - S(t)`。
#'   \item 调整后使用 `survival::coxph()` 拟合 Cox 模型，并通过边际标准化预测累计事件概率。
#'   \item 提供 `design` 时使用 `survey::svykm()` 绘制复杂抽样加权 KM 曲线。
#' }
#'
#' 对于 0/1/2 结局：
#' \itemize{
#'   \item 未调整时使用 `prodlim::prodlim()` 绘制目标事件的 CIF。
#'   \item 调整后使用 `riskRegression::FGR()` 拟合 Fine-Gray 模型，并通过边际标准化预测 CIF。
#' }
#'
#' @param data 数据框，包含随访时间、结局变量、暴露/分组变量以及可能的调整变量。
#'   普通 KM/CIF 分析使用；若提供 `design`，可省略。
#' @param design survey 包创建的复杂抽样设计对象。若提供，则使用
#'   `survey::svykm()` 绘制复杂抽样加权 KM 曲线，并使用
#'   `survey::svylogrank()` 计算加权 log-rank P 值。
#' @param time_var 字符，随访时间变量名。
#' @param outcome 字符，结局变量名。
#'   KM 曲线要求编码为 0/1，其中 0 = 删失，1 = 事件。
#'   CIF 曲线要求编码为 0/1/2，其中 0 = 删失，1 = 目标事件，2 = 竞争事件。
#'   当前目标事件固定为 `cause = 1`。
#' @param exposure 字符，分组变量名。可以是因子、字符或数值变量；绘图时会按该变量分组。
#' @param adjusted NULL 或字符向量。默认为 `NULL`，表示未调整。
#'   若传入协变量名向量，则绘制调整后的模型预测曲线。
#'   注意：`adjusted` 中不需要包含 `exposure`，函数会自动将 `exposure` 加入模型。
#' @param xlim 数值向量，长度为 2，表示 x 轴显示范围，如 `c(0, 120)`。
#'   默认从 0 到分析数据中的最大随访时间。
#' @param ylim 数值向量，长度为 2，表示 y 轴范围，如 `c(0, 1)`。
#'   若为 `NULL`，函数会根据曲线最大值自动生成较美观的 y 轴上限与刻度。
#' @param legend_position 图例位置。默认 `"top"`，也可设为 `"bottom"`、`"left"`、
#'   `"right"` 或 `"none"`。
#' @param x_breaks 数值向量，x 轴刻度位置。
#'   该参数同时控制主图 x 轴刻度和风险表格的统计时间点，因此建议显式指定，
#'   例如 `seq(0, 120, by = 24)`。
#'   若为 `NULL`，函数会基于 `xlim` 使用 `pretty()` 自动生成。
#' @param n_times 整数，模型预测时间点数量。主要用于调整后的 Cox/Fine-Gray 曲线，
#'   也用于未调整 CIF 曲线的预测网格。默认 100。数值越大曲线越细，但计算可能更慢。
#' @param risk_table 逻辑值，是否在主图下方添加 number at risk 表格。
#'   默认 `FALSE`。风险表格的列位置由 `x_breaks` 决定。
#' @param show_p 逻辑值，是否显示未调整曲线的组间比较 P 值。默认 `TRUE`。
#'   仅在 `adjusted = NULL` 时生效；调整后曲线不显示 P 值。
#'   KM 曲线显示 Log-rank P 值，CIF 曲线显示 Gray's test P 值。
#' @param p_x,p_y 数值，P 值标签的 x 和 y 坐标。若为 `NULL`，函数会自动放置。
#'   默认 x 坐标为 `xlim[1]`，y 坐标位于 y 轴范围约 95% 高度处。
#' @param p_digits 整数，P 值显示的小数位数。默认 3。
#'   例如小于 `0.001` 时显示为 `P < 0.001`。
#' @param y_percent_accuracy 数值，y 轴百分比标签精度。默认 1，表示按 1% 显示。
#'   若希望显示到 0.1%，可设为 `0.1`。
#' @param line_width 数值，曲线线宽。默认 1.1。
#' @param palette 字符向量，自定义颜色。可以是未命名向量，顺序需与分组水平一致；
#'   也可以是命名向量，名称与分组名称对应。
#' @param title 字符，图形主标题。默认 `NULL`。
#' @param x_lab 字符，x 轴标题。默认 `"Follow-up time, months"`。
#' @param y_lab 字符，y 轴标题。若为 `NULL`，函数自动设置：
#'   KM 为 `"Cumulative event probability"`，CIF 为 `"Cumulative incidence"`。
#'
#' @return 返回一个列表，包含：
#' \describe{
#'   \item{plot}{组合后的 ggplot 对象；若 `risk_table = TRUE`，则包含主图和风险表格。}
#'   \item{curve_plot}{仅包含曲线的 ggplot 对象。}
#'   \item{risk_table_plot}{风险表格的 ggplot 对象；若未绘制风险表格则为 `NULL`。}
#'   \item{risk_table_data}{风险表格数据框；若未绘制风险表格则为 `NULL`。}
#'   \item{data}{用于绘图的曲线数据框，包含时间、估计值、分组和模型类型。}
#'   \item{model}{拟合模型对象，可能为 `survfit`、`coxph`、`prodlim`、`FGR` 或 `svykm`。}
#'   \item{plot_data}{实际用于分析的数据，即删除相关变量缺失值后的数据。}
#'   \item{design}{实际用于加权 KM 的 survey design；普通分析时为 `NULL`。}
#'   \item{curve_type}{字符，`"km"` 或 `"cif"`。}
#'   \item{adjusted}{逻辑值，表示是否进行了协变量调整。}
#'   \item{weighted}{逻辑值，表示是否使用了复杂抽样加权 KM。}
#'   \item{xlim}{实际使用的 x 轴范围。}
#'   \item{x_breaks}{实际使用的 x 轴刻度，同时也是风险表格统计时间点。}
#'   \item{p_value}{未调整组间比较的 P 值；调整后曲线或未显示 P 值时为 `NA`。}
#'   \item{p_label}{图中显示的 P 值标签；若未显示则为 `NULL`。}
#' }
#'
#' @details
#' 调整后曲线采用边际标准化思想：对每个分组水平，函数将所有个体的
#' `exposure` 设置为该水平，同时保留其他协变量的原始分布；随后预测每个个体
#' 在各时间点的累计风险或 CIF，并对所有个体取平均。因此调整后曲线表示：
#' 在当前分析人群协变量分布下，若所有人都处于某一暴露组时的平均预测风险。
#'
#' 风险表格始终基于原始分析数据计算，即在每个 `x_breaks` 时间点，
#' 统计各暴露组中随访时间仍大于等于该时间点的人数。即使绘制调整后曲线，
#' risk table 也不是模型预测值，而是原始分组的 number at risk。
#'
#' P 值仅用于未调整曲线：
#' \itemize{
#'   \item KM 曲线使用 log-rank 检验，即 `survival::survdiff()`。
#'   \item 加权 KM 曲线使用 survey-weighted log-rank 检验，即 `survey::svylogrank()`。
#'   \item CIF 曲线使用 Gray's test，即 `cmprsk::cuminc()` 返回的检验结果。
#' }
#' 调整后曲线不显示 P 值，避免将未调整组间检验误解为调整后模型检验。
#' 当前 `design` 分支支持未调整的 0/1 KM 曲线；复杂抽样加权下的调整后
#' 边际标准化曲线和 0/1/2 CIF 暂不在本函数中计算。
#'
#' @importFrom survival survfit Surv coxph survdiff
#' @importFrom riskRegression predictRisk FGR
#' @importFrom prodlim Hist prodlim
#' @importFrom dplyr select all_of bind_rows mutate filter
#' @importFrom tidyr drop_na
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_step scale_x_continuous scale_y_continuous
#'   labs theme_classic theme element_blank element_text margin annotate geom_text
#'   scale_color_manual
#' @importFrom scales percent_format
#' @importFrom patchwork plot_layout
#' @importFrom cmprsk cuminc
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' n <- 200
#'
#' df_km <- data.frame(
#'   time = runif(n, 1, 60),
#'   event = rbinom(n, 1, 0.7),
#'   group = factor(sample(c("A", "B"), n, replace = TRUE)),
#'   age = rnorm(n, 60, 10),
#'   sex = factor(sample(c("Female", "Male"), n, replace = TRUE))
#' )
#'
#' # 未调整 KM 曲线，显示 risk table 和 log-rank P 值
#' res_km <- yyds_km(
#'   data = df_km,
#'   time_var = "time",
#'   outcome = "event",
#'   exposure = "group",
#'   xlim = c(0, 60),
#'   x_breaks = seq(0, 60, by = 12),
#'   risk_table = TRUE
#' )
#' res_km$plot
#'
#' # 调整后 Cox 累计事件概率曲线
#' res_cox <- yyds_km(
#'   data = df_km,
#'   time_var = "time",
#'   outcome = "event",
#'   exposure = "group",
#'   adjusted = c("age", "sex"),
#'   xlim = c(0, 60),
#'   x_breaks = seq(0, 60, by = 12),
#'   risk_table = TRUE
#' )
#' res_cox$plot
#'
#' # 竞争风险 CIF 曲线
#' df_cif <- data.frame(
#'   time = runif(n, 1, 60),
#'   status = sample(0:2, n, replace = TRUE, prob = c(0.25, 0.45, 0.30)),
#'   group = factor(sample(c("A", "B"), n, replace = TRUE)),
#'   age = rnorm(n, 60, 10)
#' )
#'
#' res_cif <- yyds_km(
#'   data = df_cif,
#'   time_var = "time",
#'   outcome = "status",
#'   exposure = "group",
#'   xlim = c(0, 60),
#'   x_breaks = seq(0, 60, by = 12),
#'   risk_table = TRUE
#' )
#' res_cif$plot
#'
#' # 固定 y 轴范围
#' yyds_km(
#'   data = df_cif,
#'   time_var = "time",
#'   outcome = "status",
#'   exposure = "group",
#'   xlim = c(0, 60),
#'   x_breaks = seq(0, 60, by = 12),
#'   ylim = c(0, 1)
#' )$plot
#' }
#'
#' @export
yyds_km <- function(
    data = NULL,
    time_var,
    outcome,
    exposure,
    design = NULL,
    adjusted = NULL,
    xlim = NULL,
    ylim = NULL,
    legend_position = "top",
    x_breaks = NULL,
    n_times = 100,
    risk_table = FALSE,
    show_p = TRUE,
    p_x = NULL,
    p_y = NULL,
    p_digits = 3,
    y_percent_accuracy = 1,
    line_width = 1.1,
    palette = NULL,
    title = NULL,
    x_lab = "Follow-up time",
    y_lab = NULL
) {
  requireNamespace("survival")
  requireNamespace("riskRegression")
  requireNamespace("prodlim")
  requireNamespace("dplyr")
  requireNamespace("tidyr")
  requireNamespace("tibble")
  requireNamespace("ggplot2")
  requireNamespace("scales")
  requireNamespace("patchwork")
  requireNamespace("cmprsk")

  is_survey <- !is.null(design)

  if (is_survey) {
    requireNamespace("survey")
    if (is.null(design$variables)) {
      stop("design must be a survey design object with a variables data frame.")
    }
    data <- design$variables
  } else if (is.null(data)) {
    stop("Either data or design must be provided.")
  }

  is_adjusted <- !is.null(adjusted)

  if (is_survey && is_adjusted) {
    stop("design currently supports unadjusted survey-weighted KM curves only; do not set adjusted.")
  }

  if (is_adjusted) {
    covars <- unique(c(exposure, adjusted))
  } else {
    covars <- NULL
  }

  needed_vars <- unique(c(time_var, outcome, exposure, covars))

  design_use <- NULL

  if (is_survey) {
    missing_vars <- setdiff(needed_vars, names(design$variables))
    if (length(missing_vars) > 0) {
      stop("The following variables are not in design$variables: ",
           paste(missing_vars, collapse = ", "))
    }

    complete_rows <- stats::complete.cases(design$variables[, needed_vars, drop = FALSE])
    if (!any(complete_rows)) {
      stop("No complete cases are available for the requested KM analysis.")
    }

    complete_var <- ".yyds_km_complete"
    while (complete_var %in% names(design$variables)) {
      complete_var <- paste0(complete_var, "_")
    }

    design$variables[[complete_var]] <- complete_rows
    design_use <- subset(design, get(complete_var))
    design_use$variables[[complete_var]] <- NULL

    plot_dat <- design_use$variables |>
      dplyr::select(dplyr::all_of(needed_vars))
  } else {
    plot_dat <- data |>
      dplyr::select(dplyr::all_of(needed_vars)) |>
      tidyr::drop_na()
  }

  outcome_values <- sort(unique(plot_dat[[outcome]]))

  if (all(outcome_values %in% c(0, 1))) {
    curve_type <- "km"
  } else if (all(outcome_values %in% c(0, 1, 2)) && any(outcome_values == 2)) {
    curve_type <- "cif"
  } else {
    stop("outcome must be coded as 0/1 for KM or 0/1/2 for CIF.")
  }

  if (is_survey && curve_type != "km") {
    stop("design currently supports 0/1 survey-weighted KM curves only; survey-weighted CIF is not implemented.")
  }

  cause <- 1

  if (is.null(xlim)) {
    xlim <- c(0, max(plot_dat[[time_var]], na.rm = TRUE))
  }

  if (is.null(x_breaks)) {
    x_breaks <- pretty(xlim, n = 6)
    x_breaks <- x_breaks[x_breaks >= xlim[1] & x_breaks <= xlim[2]]
  }

  pred_times <- seq(
    from = xlim[1],
    to = xlim[2],
    length.out = n_times
  )

  group_levels <- if (is.factor(plot_dat[[exposure]])) {
    levels(droplevels(plot_dat[[exposure]]))
  } else {
    sort(unique(plot_dat[[exposure]]))
  }

  make_newdata <- function(g) {
    nd <- plot_dat

    if (is.factor(nd[[exposure]])) {
      nd[[exposure]] <- factor(g, levels = levels(plot_dat[[exposure]]))
    } else {
      nd[[exposure]] <- g
    }

    nd
  }

  quiet_predictRisk <- function(...) {
    out <- NULL

    invisible(capture.output({
      out <- suppressMessages(
        riskRegression::predictRisk(...)
      )
    }))

    out
  }

  make_risk_table <- function(data, time_var, exposure, breaks) {
    group_levels <- if (is.factor(data[[exposure]])) {
      levels(droplevels(data[[exposure]]))
    } else {
      sort(unique(data[[exposure]]))
    }

    dplyr::bind_rows(lapply(group_levels, function(g) {
      dat_g <- data[data[[exposure]] == g, , drop = FALSE]

      tibble::tibble(
        time = breaks,
        group = as.character(g),
        n_risk = sapply(breaks, function(t) {
          sum(dat_g[[time_var]] >= t, na.rm = TRUE)
        })
      )
    }))
  }

  format_p_value <- function(p, digits = 3) {
    if (is.na(p)) {
      return("P = NA")
    }

    threshold <- 10^(-digits)

    if (p < threshold) {
      paste0("P < ", formatC(threshold, format = "f", digits = digits))
    } else {
      paste0("P = ", formatC(p, format = "f", digits = digits))
    }
  }

  get_logrank_p <- function(data, time_var, outcome, exposure) {
    f <- stats::as.formula(
      paste0("survival::Surv(", time_var, ", ", outcome, ") ~ ", exposure)
    )

    sdiff <- survival::survdiff(f, data = data)

    stats::pchisq(
      sdiff$chisq,
      df = length(sdiff$n) - 1,
      lower.tail = FALSE
    )
  }

  get_svy_logrank_p <- function(design, time_var, outcome, exposure) {
    f <- stats::as.formula(
      paste0("survival::Surv(", time_var, ", ", outcome, ") ~ ", exposure)
    )

    lr <- survey::svylogrank(f, design = design)
    as.numeric(lr[[2]][["p"]])
  }

  get_gray_p <- function(data, time_var, outcome, exposure, cause = 1) {
    ci <- cmprsk::cuminc(
      ftime = data[[time_var]],
      fstatus = data[[outcome]],
      group = data[[exposure]]
    )

    tests <- ci$Tests

    if (is.null(tests)) {
      return(NA_real_)
    }

    if ("pv" %in% colnames(tests)) {
      return(as.numeric(tests[cause, "pv"]))
    }

    if ("p" %in% colnames(tests)) {
      return(as.numeric(tests[cause, "p"]))
    }

    NA_real_
  }

  if (curve_type == "km") {
    if (is_survey) {
      km_formula <- stats::as.formula(
        paste0("survival::Surv(", time_var, ", ", outcome, ") ~ ", exposure)
      )

      fit <- survey::svykm(
        km_formula,
        design = design_use,
        se = TRUE
      )

      if (inherits(fit, "svykmlist")) {
        curve_dat <- lapply(names(fit), function(g) {
          tibble::tibble(
            time = as.numeric(fit[[g]]$time),
            estimate = 1 - as.numeric(fit[[g]]$surv),
            group = as.character(g),
            model = "Survey-weighted KM"
          )
        }) |>
          dplyr::bind_rows()
      } else {
        curve_dat <- tibble::tibble(
          time = as.numeric(fit$time),
          estimate = 1 - as.numeric(fit$surv),
          group = as.character(group_levels[1]),
          model = "Survey-weighted KM"
        )
      }

      curve_dat <- curve_dat |>
        dplyr::filter(time >= xlim[1], time <= xlim[2])

      if (xlim[1] == 0) {
        origin_dat <- tibble::tibble(
          time = 0,
          estimate = 0,
          group = as.character(group_levels),
          model = "Survey-weighted KM"
        )

        curve_dat <- dplyr::bind_rows(
          origin_dat,
          curve_dat
        ) |>
          dplyr::arrange(group, time)
      }

    } else if (is_adjusted) {
      rhs <- paste(covars, collapse = " + ")

      cox_formula <- stats::as.formula(
        paste0("survival::Surv(", time_var, ", ", outcome, ") ~ ", rhs)
      )

      fit <- survival::coxph(
        cox_formula,
        data = plot_dat,
        x = TRUE,
        y = TRUE
      )

      curve_dat <- lapply(group_levels, function(g) {
        nd <- make_newdata(g)

        risk_mat <- quiet_predictRisk(
          fit,
          newdata = nd,
          times = pred_times
        )

        tibble::tibble(
          time = pred_times,
          estimate = colMeans(risk_mat, na.rm = TRUE),
          group = as.character(g),
          model = "Adjusted Cox"
        )
      }) |>
        dplyr::bind_rows()


    } else {
      km_formula <- stats::as.formula(
        paste0("survival::Surv(", time_var, ", ", outcome, ") ~ ", exposure)
      )

      fit <- survival::survfit(
        km_formula,
        data = plot_dat
      )

      km_sum <- summary(fit)

      curve_dat <- tibble::tibble(
        time = km_sum$time,
        estimate = 1 - km_sum$surv,
        strata = km_sum$strata
      ) |>
        dplyr::mutate(
          group = sub(paste0("^", exposure, "="), "", strata),
          model = "Unadjusted KM"
        ) |>
        dplyr::filter(time >= xlim[1], time <= xlim[2])

      # 核心修改：为每个分组补充 time = 0、estimate = 0
      if (xlim[1] == 0) {
        origin_dat <- tibble::tibble(
          time = 0,
          estimate = 0,
          group = as.character(group_levels),
          model = "Unadjusted KM"
        )

        curve_dat <- dplyr::bind_rows(
          origin_dat,
          curve_dat
        ) |>
          dplyr::arrange(group, time)
      }

    }

    if (is.null(y_lab)) {
      y_lab <- "Cumulative event probability"
    }
  }

  if (curve_type == "cif") {
    if (is_adjusted) {
      rhs <- paste(covars, collapse = " + ")

      fg_formula <- stats::as.formula(
        paste0(
          "prodlim::Hist(time = ", time_var,
          ", event = ", outcome,
          ") ~ ", rhs
        )
      )

      fit <- riskRegression::FGR(
        formula = fg_formula,
        data = plot_dat,
        cause = cause
      )

      curve_dat <- lapply(group_levels, function(g) {
        nd <- make_newdata(g)

        risk_mat <- quiet_predictRisk(
          fit,
          newdata = nd,
          times = pred_times,
          cause = cause
        )

        tibble::tibble(
          time = pred_times,
          estimate = colMeans(risk_mat, na.rm = TRUE),
          group = as.character(g),
          model = "Adjusted Fine-Gray"
        )
      }) |>
        dplyr::bind_rows()

    } else {
      cif_formula <- stats::as.formula(
        paste0(
          "prodlim::Hist(", time_var,
          ", ", outcome,
          ") ~ ", exposure
        )
      )

      fit <- prodlim::prodlim(
        cif_formula,
        data = plot_dat
      )

      curve_dat <- lapply(group_levels, function(g) {
        nd <- plot_dat[plot_dat[[exposure]] == g, , drop = FALSE]

        risk_mat <- quiet_predictRisk(
          fit,
          newdata = nd,
          times = pred_times,
          cause = cause
        )

        tibble::tibble(
          time = pred_times,
          estimate = colMeans(risk_mat, na.rm = TRUE),
          group = as.character(g),
          model = "Unadjusted CIF"
        )
      }) |>
        dplyr::bind_rows()

    }

    if (is.null(y_lab)) {
      y_lab <- "Cumulative incidence"
    }
  }

  p_value <- NA_real_
  p_label <- NULL

  if (show_p && !is_adjusted) {
    if (curve_type == "km") {
      if (is_survey) {
        p_value <- get_svy_logrank_p(
          design = design_use,
          time_var = time_var,
          outcome = outcome,
          exposure = exposure
        )

        p_label <- paste0(
          "Survey log-rank ",
          format_p_value(p_value, digits = p_digits)
        )
      } else {
        p_value <- get_logrank_p(
          data = plot_dat,
          time_var = time_var,
          outcome = outcome,
          exposure = exposure
        )

        p_label <- paste0(
          "Log-rank ",
          format_p_value(p_value, digits = p_digits)
        )
      }
    }

    if (curve_type == "cif") {
      p_value <- get_gray_p(
        data = plot_dat,
        time_var = time_var,
        outcome = outcome,
        exposure = exposure,
        cause = cause
      )

      p_label <- paste0(
        "Gray's test ",
        format_p_value(p_value, digits = p_digits)
      )
    }

    if (is.null(p_x)) {
      p_x <- xlim[1]
    }


  }

  if (is.null(ylim)) {
    y_max <- max(curve_dat$estimate, na.rm = TRUE)
    y_breaks <- pretty(c(0, y_max * 1.05), n = 4)
    y_breaks <- y_breaks[y_breaks >= 0]
    ylim_use <- c(0, max(y_breaks))
  } else {
    ylim_use <- ylim
    y_breaks <- pretty(ylim_use, n = 4)
    y_breaks <- y_breaks[
      y_breaks >= ylim_use[1] & y_breaks <= ylim_use[2]
    ]
  }


  if (is.null(p_y)) {
    p_y <- ylim_use[1] + 0.95 * diff(ylim_use)
  }

  p <- ggplot2::ggplot(
    curve_dat,
    ggplot2::aes(x = time, y = estimate, color = group)
  ) +
    ggplot2::geom_step(linewidth = line_width) +
    ggplot2::scale_x_continuous(
      limits = xlim,
      breaks = x_breaks
    ) +
    ggplot2::scale_y_continuous(
      limits = ylim_use,
      breaks = y_breaks,
      labels = scales::percent_format(accuracy = y_percent_accuracy)
    ) +
    ggplot2::labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    ggplot2::theme_classic(base_size = 14) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 13),
      axis.text = ggplot2::element_text(size = 12),
      plot.title = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5)
    )

  if (!is.null(p_label)) {
    p <- p +
      ggplot2::annotate(
        "text",
        x = p_x,
        y = p_y,
        label = p_label,
        hjust = 0,
        size = 4.5
      )
  }

  if (!is.null(palette)) {
    p <- p + ggplot2::scale_color_manual(values = palette)
  }

  if (risk_table) {
    rt_dat <- make_risk_table(
      data = plot_dat,
      time_var = time_var,
      exposure = exposure,
      breaks = x_breaks
    )

    p_risk <- ggplot2::ggplot(
      rt_dat,
      ggplot2::aes(x = time, y = group, label = n_risk, color = group)
    ) +
      ggplot2::geom_text(size = 4, show.legend = FALSE) +
      ggplot2::scale_x_continuous(
        limits = xlim,
        breaks = x_breaks
      ) +
      ggplot2::labs(x = x_lab, y = NULL,title = "Number at risk") +
      ggplot2::theme_classic(base_size = 12) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 12,
          face = "bold",
          hjust = 0,
          margin = ggplot2::margin(b = 4)
        ),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 11),
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_blank(),
        legend.position = "none",
        plot.margin = ggplot2::margin(t = 0, r = 5.5, b = 5.5, l = 5.5)
      )

    if (!is.null(palette)) {
      p_risk <- p_risk + ggplot2::scale_color_manual(values = palette)
    }

    p_out <- p / p_risk + patchwork::plot_layout(heights = c(3, 1))
  } else {
    rt_dat <- NULL
    p_risk <- NULL
    p_out <- p
  }

  return(list(
    plot = p_out,
    curve_plot = p,
    risk_table_plot = p_risk,
    risk_table_data = rt_dat,
    data = curve_dat,
    model = fit,
    plot_data = plot_dat,
    design = design_use,
    curve_type = curve_type,
    adjusted = is_adjusted,
    weighted = is_survey,
    xlim = xlim,
    x_breaks = x_breaks,
    p_value = p_value,
    p_label = p_label
  ))
}
