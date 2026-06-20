#' 分层回归分析与森林图绘制
#'
#' @description
#'   - 该函数用于进行亚组分析并绘制森林图，支持Cox比例风险模型。
#'   - 支持Cox比例风险模型、逻辑回归、泊松回归和线性回归。
#'
#' @param data 数据框（data.frame），包含所有分析所需变量。普通回归分析使用。
#' @param design survey 包创建的复杂抽样设计对象。若提供，则自动使用
#'   `survey::svycoxph()` 或 `survey::svyglm()` 进行复杂抽样加权亚组分析，
#'   此时 `data` 可省略。
#' @param time 生存分析中的随访时间变量。若进行 Cox 回归必须提供。
#' @param family_type 回归模型族类。支持 "binomial"（二项）、
#'   "poisson"（泊松）、"gaussian"（高斯）。`design` 加权模式下也支持
#'   "quasibinomial" 和 "quasipoisson"。若为生存分析则设为 NULL。
#' @param status 状态变量，用于表示事件是否发生（0/1）。
#' @param exposure 暴露变量，可以是分类变量或连续变量。
#' @param adjust_vars 向模型中加入的协变量（字符串向量）。
#' @param stratify_vars 用于分层分析的变量（字符串向量）。
#' @param ref 是否展示ref组在森林图中。默认为 TRUE。
#' @param plot_type 图类型 c(1,2,3)。
#' @param plot_column 用于绘图的列索引，可自选展示列和排序。
#' @param xlim X轴范围，用于控制森林图的显示范围。
#' @param ticks_at X轴刻度点，用于控制森林图的坐标刻度。
#'
#' @return 返回格式化后的回归结果汇总表；森林图对象保存在
#'   `attr(result, "forest_plot")` 中。
#'
#' @details
#'   - 若指定 `time`，则进行 Cox 生存分析。
#'   - 若指定 `family_type`，则按指定族类进行广义线性模型分析。
#'   - `time`和`family_type`不能同时出现。
#'   - 交互作用P值计算使用 `yyds_pforinteraction()` 函数。
#'   - `design` 加权分支输出与普通分支一致的 `total_event_N` 和
#'     `Events, n/N` 列；两者均按实际进入 `svycoxph()` / `svyglm()`
#'     的 complete-case 模型样本统计。
#'   - 普通分支和 `design` 加权分支共用同一套森林图生成逻辑。
#'
#' @examples
#' # 加载必要的包
#' library(survival)
#' library(dplyr)
#' library(forestploter)
#'
#' # 创建随机生存数据
#' set.seed(123)
#' n <- 500
#' survival_data <- data.frame(
#'   id = 1:n,
#'   #' 治疗组(0=对照组, 1=治疗组)
#'   treatment = sample(c(0,1), n, replace = TRUE),
#'   #' 年龄(20-80岁)
#'   age = round(runif(n, 20, 80)),
#'   #' 性别(0=女, 1=男)
#'   sex = sample(c(0,1), n, replace = TRUE),
#'   #' 疾病分期(I,II,III)
#'   stage = sample(c("I","II","III"), n, replace = TRUE, prob = c(0.3,0.4,0.3)),
#'   #' 肿瘤部位(A,B)
#'   site = sample(c("A","B"), n, replace = TRUE),
#'   #' 生存时间(1-60个月)
#'   time = round(runif(n, 1, 60)),
#'   #' 事件状态(0=删失, 1=事件)
#'   status = sample(c(0,1), n, replace = TRUE, prob = c(0.2,0.8))
#' )
#'
#' # 将分类变量转为因子
#' survival_data$treatment <- factor(survival_data$treatment, levels = c(0,1), labels = c("Control", "Treatment"))
#' survival_data$sex <- factor(survival_data$sex, levels = c(0,1), labels = c("Female", "Male"))
#' survival_data$stage <- factor(survival_data$stage, levels = c("I","II","III"))
#' survival_data$site <- factor(survival_data$site)
#' #' 进行亚组分析
#' result_cox <- yyds_sub_analysis(
#'   data = survival_data,
#'   time = "time",
#'   status = "status",
#'   exposure = "treatment",
#'   adjust_vars = c("age", "sex"),
#'   stratify_vars = c( "site"),
#'   ref = FALSE,
#'   plot_type = 2,
#'   xlim = c(0.1, 10),
#'   ticks_at = c(0.1, 1, 5, 10)
#' )
#'
#' # 查看结果表格
#' head(result_cox)
#'
#' ##
#' set.seed(123)
#' n <- 400
#' bina_data <- data.frame(
#'   id = 1:n,
#'   #' 治疗组(0=对照组, 1=治疗组)
#'   treatment = sample(c(0,1), n, replace = TRUE),
#'   #' 年龄(20-80岁)
#'   age = round(runif(n, 20, 80)),
#'   #' 性别(0=女, 1=男)
#'   sex = sample(c(0,1), n, replace = TRUE),
#'   #' 疾病严重程度(轻,中,重)
#'   severity = sample(c("Mild","Moderate","Severe"), n, replace = TRUE, prob = c(0.4,0.4,0.2)),
#'   #' 并发症(无,有)
#'   complication = sample(c("No","Yes"), n, replace = TRUE),
#'   #' 治疗结果(0=失败, 1=成功)
#'   outcome = sample(c(0,1), n, replace = TRUE, prob = c(0.3,0.7))
#' )
#'
#' # 将分类变量转为因子
#' bina_data$treatment <- factor(bina_data$treatment, levels = c(0,1), labels = c("Placebo", "Active"))
#' bina_data$sex <- factor(bina_data$sex, levels = c(0,1), labels = c("Female", "Male"))
#' bina_data$severity <- factor(bina_data$severity, levels = c("Mild","Moderate","Severe"))
#' bina_data$complication <- factor(bina_data$complication)
#'
#' # 进行亚组分析
#' result_logistic <- yyds_sub_analysis(
#'   data = bina_data,
#'   family_type = "binomial",
#'   status = "outcome",
#'   exposure = "treatment",
#'   adjust_vars = c("age", "sex"),
#'   stratify_vars = c("severity", "complication"),
#'   ref = TRUE,
#'   plot_type = 3,
#'   xlim = c(0.1, 20),
#'   ticks_at = c(0.1, 1, 5, 10, 20)
#' )
#'
#' # 查看结果表格
#' head(result_logistic)
#'
#' @export
#'
yyds_sub_analysis <- function(data = NULL,
                              design = NULL,
                              time = NULL,
                              family_type = NULL,
                              status,
                              exposure,
                              adjust_vars,
                              stratify_vars,
                              ref = FALSE,
                              plot_type = 1,
                              plot_column = NULL,
                              xlim = NULL,
                              ticks_at = NULL) {

  # time和family_type不能同时为NULL,也不能同时存在
  if (is.null(time) && is.null(family_type)) {
    stop("\n Either 'time' or 'family_type' must be provided.")
  } else if (!is.null(time) && !is.null(family_type)) {
    stop("\n 'time' and 'family_type' cannot both be provided.")
  }

  fmt_p <- function(p, digits = 3) {
    if (length(p) == 0 || is.na(p) || !is.finite(p)) return(NA_character_)
    if (p < 10^(-digits)) {
      paste0("<", sprintf(paste0("%.", digits, "f"), 10^(-digits)))
    } else {
      sprintf(paste0("%.", digits, "f"), p)
    }
  }

  bt <- function(x) paste0("`", x, "`")

  standardize_family <- function(family_type) {
    if (is.null(family_type)) return(NULL)
    family_type <- trimws(tolower(family_type))
    switch(family_type,
           "binomial" = binomial(),
           "quasibinomial" = quasibinomial(),
           "poisson" = poisson(),
           "quasipoisson" = quasipoisson(),
           "gaussian" = gaussian(),
           stop("\n Unsupported family_type type."))
  }

  extract_exposure_rows <- function(tab, exposure, exposure_data, ref) {
    if (is.factor(exposure_data) || is.character(exposure_data)) {
      lv <- levels(factor(exposure_data))
      header_idx <- which(tab$Variable == exposure)[1]
      if (is.na(header_idx)) return(tab[0, , drop = FALSE])
      idx <- seq(header_idx + 1, length.out = length(lv))
      idx <- idx[idx <= nrow(tab)]
      out <- tab[idx, , drop = FALSE]
      if (!isTRUE(ref) && nrow(out) > 0) {
        out <- out[out$Variable != lv[1] & out$Variable != paste0("  ", lv[1]), , drop = FALSE]
      }
      out
    } else {
      tab[tab$Variable == exposure, , drop = FALSE]
    }
  }

  model_counts <- function(fit, has_event) {
    mf <- tryCatch(model.frame(fit), error = function(e) NULL)
    n <- if (is.null(mf)) NA_integer_ else nrow(mf)
    events <- NA_integer_

    if (!is.null(mf) && isTRUE(has_event)) {
      response <- model.response(mf)
      if (inherits(response, "Surv")) {
        status_col <- if ("status" %in% colnames(response)) "status" else ncol(response)
        events <- sum(response[, status_col] == 1, na.rm = TRUE)
      } else if (is.matrix(response) && ncol(response) >= 2) {
        events <- sum(response[, 1], na.rm = TRUE)
      } else {
        events <- sum(response == 1, na.rm = TRUE)
      }
    }

    list(n = n, events = events)
  }

  draw_forest <- function(p) {
    tryCatch(
      print(p),
      error = function(e) {
        grid::grid.newpage()
        grid::grid.draw(p)
      }
    )
    invisible(p)
  }

  make_forest_plot <- function(dt_plot, exposure_data) {
    dt_plot$Stratum <- factor(dt_plot$Stratum, levels = unique(dt_plot$Stratum))
    dt_plot$Subgroup <- paste0("     ", dt_plot$Subgroup)
    dt_plot <- dt_plot %>%
      group_by(Stratum, .drop = FALSE) %>%
      dplyr::do({
        a <- .$Stratum[1]
        b <- .$p_interaction[1]
        .$p_interaction[1] <- NA

        new_row <- .[1, ]
        new_row[] <- ""
        new_row$Stratum <- a
        new_row$Subgroup <- a
        new_row$p_interaction <- b

        rbind(new_row, .)
      }) %>%
      dplyr::ungroup()

    dt_plot[, c(8:10)] <- sapply(dt_plot[, c(8:10)], as.numeric)

    extreme_idx <- is.infinite(dt_plot[[8]]) | is.na(dt_plot[[8]]) |
      is.infinite(dt_plot[[9]]) | is.na(dt_plot[[9]]) |
      is.infinite(dt_plot[[10]]) | is.na(dt_plot[[10]])

    if (any(extreme_idx, na.rm = TRUE)) {
      dt_plot[extreme_idx, 8:10] <- NA
    }

    dt_plot$`         Forestplot` <- paste(rep(" ", 20), collapse = " ")
    dt_plot$p_interaction <- ifelse(is.na(dt_plot$p_interaction), " ", dt_plot$p_interaction)
    dt_plot$sign <- ifelse(is.na(dt_plot$sign), " ", dt_plot$sign)
    dt_plot <- dt_plot[, c(1, 2, 3, 4, 5, 6, 13, 11, 12, 7, 8, 9, 10)]

    theme_forest <- forestploter::forest_theme(base_size = 10)

    if (ref == FALSE) {
      if (any(dt_plot[[5]] == "Ref")) {
        colnames(dt_plot)[3] <- paste0(" (vs. ", levels(as.factor(exposure_data))[1], ")")
      }
      dt_plot[[8]] <- c("", dt_plot[[8]][-nrow(dt_plot)])
      dt_plot <- dt_plot[dt_plot[[5]] != "Ref", ]
    }

    if (!is.null(time) || family_type %in% c("binomial", "quasibinomial", "poisson", "quasipoisson")) {
      ref_line <- 1
    } else if (family_type == "gaussian") {
      ref_line <- 0
    }

    if (!is.null(plot_column)) {
      column <- plot_column
      ci_column <- grep("Forestplot", colnames(dt_plot[, column]))
      if (length(ci_column) == 0) {

        message("\n 注意：plot_column 不要删掉7，为画图区域")
        ci_column <- 1
      }
    } else {
      column <- c(1, 2, 3, 4, 5, 6, 7, 8)
      ci_column <- 7
    }

    if (is.null(xlim) || is.null(ticks_at)) {
      all_estimates <- c(dt_plot[[11]], dt_plot[[12]], dt_plot[[13]])
      all_estimates <- all_estimates[is.finite(all_estimates)]

      if (length(all_estimates) == 0) {
        if (!is.null(time) || family_type %in% c("binomial", "quasibinomial", "poisson", "quasipoisson")) {
          xlim_auto <- c(0.5, 1.5)
        } else {
          xlim_auto <- c(-1, 1)
        }
      } else {
        data_min <- min(all_estimates)
        data_max <- max(all_estimates)
        padding <- (data_max - data_min) * 0.2
        if (!is.finite(padding) || padding == 0) padding <- max(abs(data_max), 1) * 0.2
        xlim_auto <- c(data_min - padding, data_max + padding)
      }

      ticks_auto <- pretty(xlim_auto, n = 5)
    }

    final_xlim <- if (!is.null(xlim)) xlim else xlim_auto
    final_ticks_at <- if (!is.null(ticks_at)) ticks_at else ticks_auto

    p <- suppressWarnings(forestploter::forest(
      dt_plot[, column],
      est = list(dt_plot[[11]]),
      lower = list(dt_plot[[12]]),
      upper = list(dt_plot[[13]]),
      size = 0.5,
      ci_column = ci_column,
      ref_line = ref_line,
      arrow_la = c("Low risk", "High risk"),
      xlim = final_xlim,
      ticks_at = final_ticks_at,
      theme = theme_forest
    ))

    p <- forestploter::add_border(p, part = "header", gp = grid::gpar(lwd = 1, col = "black"))

    if (plot_type == 2) {
      group_start <- c(1, which(dt_plot$Subgroup[-1] != dt_plot$Subgroup[-length(dt_plot$Subgroup)]) + 1)
      group_id <- rep(seq_along(group_start), diff(c(group_start, length(dt_plot$Subgroup) + 1)))
      class1_rows <- which(group_id %% 2 == 1)
      class2_rows <- which(group_id %% 2 == 0)

      p <- forestploter::edit_plot(p, row = class2_rows,
                                   which = "background",
                                   gp = grid::gpar(fill = "white"))
      p <- forestploter::edit_plot(p, row = class1_rows,
                                   which = "background",
                                   gp = grid::gpar(fill = "#EFF3F2"))
    }

    if (plot_type == 3) {
      group_start <- c(1, which(dt_plot$Stratum[-1] != dt_plot$Stratum[-length(dt_plot$Stratum)]) + 1)
      group_id <- rep(seq_along(group_start), diff(c(group_start, length(dt_plot$Stratum) + 1)))
      class1_rows <- which(group_id %% 2 == 1)
      class2_rows <- which(group_id %% 2 == 0)

      p <- forestploter::edit_plot(p, row = class2_rows,
                                   which = "background",
                                   gp = grid::gpar(fill = "white"))
      p <- forestploter::edit_plot(p, row = class1_rows,
                                   which = "background",
                                   gp = grid::gpar(fill = "#EFF3F2"))
    }

    attr(p, "plot_data") <- dt_plot
    p
  }

  # ==================== 复杂抽样加权亚组分析 ====================
  if (!is.null(design)) {
    if (!requireNamespace("survey", quietly = TRUE)) {
      stop("使用 design 参数需要安装 survey 包。")
    }

    if (exposure %in% adjust_vars) {
      stop("\n The exposure '", exposure, "' variable should not be included in 'adjust_vars'.")
    }
    if (exposure %in% stratify_vars) {
      stop("\n The exposure '", exposure, "' variable should not be included in 'stratify_vars'.")
    }

    design_vars <- names(design$variables)
    check_vars <- c(status, time, exposure, adjust_vars, stratify_vars)
    missing_vars <- setdiff(check_vars, design_vars)
    if (length(missing_vars) > 0) {
      stop("\n The following variables are not in design$variables: ", paste(missing_vars, collapse = ", "))
    }

    if (!is.null(time) || family_type %in% c("binomial", "quasibinomial")) {
      status_values <- unique(na.omit(design$variables[[status]]))
      if (length(status_values) != 2) {
        stop("\n For survival analysis or binomial family, status '", status, "' must be a binary variable.")
      }
    }

    family_obj <- standardize_family(family_type)
    res_all <- list()

    for (sub in stratify_vars) {
      levels_sub <- sort(unique(na.omit(as.character(design$variables[[sub]]))))
      p_interaction <- NA_real_

      adjust_interaction <- setdiff(adjust_vars, sub)
      rhs_interaction <- paste0(
        bt(exposure), " * ", bt(sub),
        if (length(adjust_interaction) > 0) {
          paste0(" + ", paste(bt(adjust_interaction), collapse = " + "))
        } else ""
      )
      f_interaction <- if (!is.null(time)) {
        as.formula(paste0("Surv(", bt(time), ", ", bt(status), ") ~ ", rhs_interaction))
      } else {
        as.formula(paste0(bt(status), " ~ ", rhs_interaction))
      }

      fit_interaction <- tryCatch({
        if (!is.null(time)) {
          survey::svycoxph(f_interaction, design = design)
        } else {
          survey::svyglm(f_interaction, design = design, family = family_obj)
        }
      }, error = function(e) NULL)

      if (!is.null(fit_interaction)) {
        p_interaction <- tryCatch({
          as.numeric(survey::regTermTest(
            fit_interaction,
            as.formula(paste0("~", bt(exposure), ":", bt(sub)))
          )$p)
        }, error = function(e) NA_real_)
      }

      res_sub <- lapply(levels_sub, function(lv) {
        design_lv <- subset(design, get(sub) == lv)
        adjust_lv <- setdiff(adjust_vars, sub)
        rhs_lv <- paste0(
          bt(exposure),
          if (length(adjust_lv) > 0) {
            paste0(" + ", paste(bt(adjust_lv), collapse = " + "))
          } else ""
        )
        f_lv <- if (!is.null(time)) {
          as.formula(paste0("Surv(", bt(time), ", ", bt(status), ") ~ ", rhs_lv))
        } else {
          as.formula(paste0(bt(status), " ~ ", rhs_lv))
        }

        fit_lv <- tryCatch({
          if (!is.null(time)) {
            survey::svycoxph(f_lv, design = design_lv)
          } else {
            survey::svyglm(f_lv, design = design_lv, family = family_obj)
          }
        }, error = function(e) NULL)
        if (is.null(fit_lv)) return(NULL)

        has_event <- !is.null(time) || family_type %in% c("binomial", "quasibinomial")
        tab <- yyds_table2(fit_lv, full = TRUE, event = TRUE)
        tab <- extract_exposure_rows(tab, exposure, design_lv$variables[[exposure]], ref = TRUE)
        if (nrow(tab) == 0) return(NULL)

        effect_col <- grep("\\(95% CI\\)", names(tab), value = TRUE)[1]
        effect_num_col <- intersect(c("hr", "or", "rr", "β", "coef"), names(tab))[1]
        if (is.na(effect_num_col)) effect_num_col <- "coef"

        out <- data.frame(
          Subgroup = rep(lv, nrow(tab)),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        out[[exposure]] <- tab$Variable
        out[["Events, n/N"]] <- if ("Events, n/N" %in% names(tab)) tab[["Events, n/N"]] else NA_character_
        out[[effect_col]] <- tab[[effect_col]]
        out[["p-value"]] <- tab[["p-value"]]
        out[["sign"]] <- tab[["sign"]]
        out[[effect_num_col]] <- if (effect_num_col %in% names(tab)) tab[[effect_num_col]] else NA_real_
        out[["lower"]] <- tab[["lower"]]
        out[["upper"]] <- tab[["upper"]]
        out[["p_interaction"]] <- fmt_p(p_interaction)
        out[["adjusted"]] <- paste(adjust_lv, collapse = "+")
        out
      })

      res_all[[sub]] <- dplyr::bind_rows(res_sub)
    }

    combined_results <- dplyr::bind_rows(res_all, .id = "Stratum")

    if (!is.null(time) || family_type %in% c("binomial", "quasibinomial")) {
      combined_results <- combined_results %>%
        mutate(
          event = as.numeric(sapply(strsplit(`Events, n/N`, "/"), `[`, 1)),
          N = as.numeric(sapply(strsplit(`Events, n/N`, "/"), `[`, 2))
        ) %>%
        group_by(Stratum, Subgroup) %>%
        mutate(
          total_event_N = paste0(sum(event, na.rm = TRUE), "/", sum(N, na.rm = TRUE))
        ) %>%
        dplyr::ungroup()

      dt_plot <- combined_results[, c(1:11, 15)]
      result_table <- combined_results[, c(1:3, 15, 4:7, 11)]
    } else {
      combined_results <- combined_results %>%
        group_by(Stratum, Subgroup) %>%
        mutate(
          total_event_N = sum(as.numeric(`Events, n/N`), na.rm = TRUE)
        ) %>%
        dplyr::ungroup()

      dt_plot <- combined_results[, c(1:11, 13)]
      result_table <- combined_results[, c(1:3, 13, 4:7, 11)]
    }

    result_table$Stratum <- factor(result_table$Stratum, levels = unique(result_table$Stratum))
    result_table$Subgroup <- paste0("      ", result_table$Subgroup)
    result_table <- result_table %>%
      group_by(Stratum, .drop = FALSE) %>%
      dplyr::do({
        a <- .$Stratum[1]
        b <- .$p_interaction[1]
        .$p_interaction[1] <- NA

        new_row <- .[1, ]
        new_row[] <- NA
        new_row$Stratum <- a
        new_row$Subgroup <- a
        new_row$p_interaction <- b

        rbind(new_row, .)
      }) %>%
      dplyr::ungroup()

    p <- make_forest_plot(dt_plot, design$variables[[exposure]])
    attr(result_table, "forest_plot") <- p
    draw_forest(p)

    return(invisible(result_table))
  }

  if (is.null(data)) {
    stop("\n普通分析模式下必须提供 data；复杂抽样加权分析请提供 design。")
  }

  check_vars <- c(status, exposure, adjust_vars, stratify_vars)
  missing_vars <- setdiff(check_vars, names(data))

  if (length(missing_vars) > 0) {
    stop("\n The following variables are not in the dataset: ", paste(missing_vars, collapse = ", "))
  }

  # 检查exposure是否跟adjust_vars和stratify_vars重复
  if (exposure %in% adjust_vars) {
    stop("\n The exposure '",exposure,"' variable should not be included in 'adjust_vars'.")
  }
  if (exposure %in% stratify_vars) {
    stop("\n The exposure '", exposure,"' variable should not be included in 'stratify_vars'.")
  }

  # 如果time存在或者family_type为"binomial"，则status必须为2分类1和0
  if (!is.null(time) || family_type == "binomial") {
    if (length(unique(data[[status]])) != 2) {
      stop("\n For survival analysis or binomial family, status '",status,"' must be a binary variable.")
    }
  }

  final_results <- list()

  for (sub in stratify_vars) {
    allvars <- unique(c(status, time, exposure, adjust_vars, sub))
    data_sub <- na.omit(data[allvars])

    current_adjust <- adjust_vars[!adjust_vars %in% sub]
    adjusted <- paste(current_adjust, collapse = "+")

    if (!is.null(family_type)) {
      family_type <- trimws(tolower(family_type))
    }

    if (is.factor(data_sub[[exposure]])) {
      rows <- nlevels(data_sub[[exposure]]) + 1
    } else if (is.character(data_sub[[exposure]])) {
      rows <- length(unique(data_sub[[exposure]])) + 1
    } else {
      rows <- 1
    }

    results <- data_sub %>%
      group_by(!!rlang::sym(sub)) %>%
      group_modify(~ {
        current_data <- .

        if (!is.null(time)) {
          # Cox 模型
          f_formula <- as.formula(paste("Surv(", time, ",", status, ") ~ ", exposure,
                                        if (adjusted != "") paste("+", adjusted) else "", sep = ""))
          model_fit <- tryCatch(coxph(f_formula, data = current_data), error = function(e) NULL)
        } else if (family_type == "binomial") {
          # GLM 模型
          f_formula <- as.formula(paste(status, "~", exposure,
                                        if (adjusted != "") paste("+", adjusted) else "", sep = ""))
          model_fit <- tryCatch(glm(f_formula, family = binomial, data = current_data), error = function(e) NULL)
        } else if (family_type == "poisson") {
          # GLM 模型
          f_formula <- as.formula(paste(status, "~", exposure,
                                        if (adjusted != "") paste("+", adjusted) else "", sep = ""))
          model_fit <- tryCatch(glm(f_formula, family = poisson, data = current_data), error = function(e) NULL)
        } else if (family_type == "gaussian") {
          # GLM 模型
          f_formula <- as.formula(paste(status, "~", exposure,
                                        if (adjusted != "") paste("+", adjusted) else "", sep = ""))
          model_fit <- tryCatch(glm(f_formula, family = gaussian, data = current_data), error = function(e) NULL)
        } else {
          stop("\n Unsupported family_type type.")
        }

        if (is.null(model_fit)) return(data.frame())

        f_table <- tryCatch(yyds_table2(model_fit, full = TRUE), error = function(e) data.frame())

        if (rows == 1) f_table[1:1, ] else f_table[2:rows, ]
      }) %>%
      dplyr::ungroup()

    adjusted2 <- paste(unique(c(adjust_vars, sub)), collapse = "+")

    if (!is.null(time)) {
      f_formula <- as.formula(paste("Surv(", time, ",", status, ") ~ ", exposure, "+", adjusted2))
      model_fit2 <- coxph(f_formula, data = data_sub)
    } else if (family_type == "binomial") {
      f_formula <- as.formula(paste(status, "~", exposure, "+", adjusted2))
      model_fit2 <- glm(f_formula, family = binomial, data = data_sub)
    } else if (family_type == "poisson") {
      f_formula <- as.formula(paste(status, "~", exposure, "+", adjusted2))
      model_fit2 <- glm(f_formula, family = poisson, data = data_sub)
    } else if (family_type == "gaussian") {
      f_formula <- as.formula(paste(status, "~", exposure, "+", adjusted2))
      model_fit2 <- glm(f_formula, family = gaussian, data = data_sub)
    } else {
      stop("\n Unsupported family_type type.")
    }

    p_interaction <- if (!is.null(model_fit2)) {
      tryCatch(yyds_pforinteraction(model_fit2, exposure, sub), error = function(e) NA_real_)
    } else {
      NA_real_
    }

    results[1,10] <- p_interaction
    results[1,11] <- adjusted
    colnames(results)[1] <- "Subgroup"
    colnames(results)[2] <- exposure
    colnames(results)[10] <- "p_interaction"
    colnames(results)[11] <- "adjusted"

    results$Subgroup <- as.factor(results$Subgroup)

    final_results[[sub]] <- results
  }
  combined_results <- bind_rows(final_results, .id = "Stratum")

  if (!is.null(time) || family_type == "binomial") {
    combined_results <- combined_results %>%
      # 将 event_n_N 列拆分为 event 和 N
      mutate(
        event = as.numeric(sapply(strsplit(`Events, n/N`, "/"), `[`, 1)),
        N = as.numeric(sapply(strsplit(`Events, n/N`, "/"), `[`, 2))
      ) %>%
      # 按 var 分组
      group_by(Subgroup) %>%
      # 计算 total_event 和 total_N 并添加 "total_event/total_N" 字符串列
      mutate(
        total_event_N = paste0(sum(event), "/", sum(N))  # 添加 total_event/total_N 列
      )

    dt_plot <- combined_results[,c(1:11,15)]
    result_table <- combined_results[,c(1:3,15,4:7,11)]

  } else {
    combined_results <- combined_results %>%
      # 按 var 分组
      group_by(Subgroup) %>%
      # 计算 total_event 和 total_N 并添加 "total_event/total_N" 字符串列
      mutate(
        total_event_N = sum(as.numeric(`Events, n/N`)))  # 添加 total_event/total_N 列

    dt_plot <- combined_results[,c(1:11,13)]
    result_table <- combined_results[,c(1:3,13,4:7,11)]
  }



  result_table$Stratum <- factor(result_table$Stratum, levels = unique(result_table$Stratum))
  result_table$Subgroup <- paste0("      ",result_table$Subgroup)
  result_table <- result_table %>%
    group_by(Stratum, .drop = FALSE) %>%  # .drop = FALSE 保留所有因子水平
    dplyr::do({
      a <- .$Stratum[1]
      b <- .$p_interaction[1]
      .$p_interaction[1] <- NA

      # 创建汇总行
      new_row <- .[1, ]
      new_row[] <- NA
      new_row$Stratum <- a
      new_row$Subgroup <- a
      new_row$p_interaction <- b

      # 合并
      rbind(new_row, .)
    }) %>%
    dplyr::ungroup()


  p <- make_forest_plot(dt_plot, data[[exposure]])
  attr(result_table, "forest_plot") <- p
  draw_forest(p)

  return(invisible(result_table))
}
