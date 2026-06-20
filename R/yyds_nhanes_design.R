#' 创建 NHANES 多周期加权设计对象
#'
#' 根据用户指定的 NHANES 原始权重变量和分析周期，生成多周期分析权重，
#' 并创建 `survey::svydesign()` 对象。
#'
#' @param data 数据框，包含 NHANES 数据、周期变量、抽样设计变量和候选权重变量。
#' @param cycle 字符向量，指定需要合并的分析周期，例如 `"1999-2018"` 或
#'   `c("1999-2002", "2003-2006")`。若为 `NULL`，表示不做跨周期合并，
#'   函数会按 `YEAR` 分别构造权重，再合并为一个 `survey.design` 对象。
#'   若传入多个分析周期，函数会按各分段分别构造权重，再合并为一个
#'   `survey.design` 对象。
#' @param weight 字符向量，候选原始权重变量名，例如
#'   `c("WTMEC4YR", "WTMEC2YR", "WTMECPRP", "WTPH2YR")`。变量名匹配
#'   不区分大小写，但所有传入变量都必须能在 `data` 中唯一匹配。
#' @param year_var 周期标签变量名。默认为 `"YEAR"`。期望标签包括
#'   `"1999-2000"`、`"2017-2020"`、`"2021-2023"` 等。
#' @param strata 分层变量名。默认为 `"SDMVSTRA"`。
#' @param psu PSU/抽样单元变量名。默认为 `"SDMVPSU"`。
#' @param out_weight 生成的分析权重变量名。默认为 `"NHANES_wt"`。
#' @param out_cycle 生成的分析周期变量名。默认为 `"NHANES_cycle"`。
#' @param nest 传递给 `survey::svydesign()` 的逻辑值。默认为 `TRUE`。
#' @param lonely_psu 可选，设置 `options(survey.lonely.psu)` 的值。
#'   默认为 `"adjust"`；若设为 `NULL`，则不修改当前选项。
#'
#' @return 返回 `survey.design` 对象。生成后的权重变量和分析周期变量会保存在
#'   `design$variables` 中；同一份数据保存在 `attr(design, "data")`；
#'   权重构造日志保存在 `attr(design, "weight_log")`。
#'
#' @details
#' 本函数不会猜测应该使用哪一类科学权重。用户通过 `weight` 显式传入候选原始
#' 权重变量，函数只根据 NHANES 周期规则选择和缩放权重：
#' \itemize{
#'   \item 1999-2002 使用可用的 `*4YR` 权重。
#'   \item 普通完整两年周期使用可用的 `*2YR` 权重。
#'   \item 2017-2020 使用可用的 `*PRP` pre-pandemic 权重，并按 3.2 年处理。
#'   \item 2021-2023 使用可用的 `*2YR` 权重；`WTPH2YR` 与其他 `*2YR`
#'     权重按相同规则比较，不再固定优先。
#' }
#' 若同一 YEAR 存在多个同类候选权重，函数会分别在 `2YR`、`4YR`、`PRP`
#' 类型内部比较有效数据量。有效权重定义为可转为数值且大于 0；有效数为 0
#' 的变量不参与选择，其余变量中有效数最少者优先，并列时按 `weight` 的传入
#' 顺序选择。
#' 对于合并周期，生成权重的计算公式为：
#' `原始权重 * 该原始权重代表年数 / 分析周期总年数`。
#' 函数会始终在创建 `survey::svydesign()` 前剔除生成权重缺失、为 0 或小于
#' 0 的样本，并在控制台简要输出分析周期、权重类型、缩放系数和无效权重数量。
#' 候选权重变量可以是数值型，或可转为数值的字符/因子型变量；空字符串会按缺失处理。
#'
#' @examples
#' \dontrun{
#' des <- yyds_nhanes_design(
#'   data = dt,
#'   cycle = c("1999-2018"),
#'   weight = c("WTMEC4YR", "WTMEC2YR", "WTMECPRP", "WTPH2YR")
#' )
#'
#' des_segmented <- yyds_nhanes_design(
#'   data = dt,
#'   cycle = c("1999-2002", "2003-2006"),
#'   weight = c("WTMEC4YR", "WTMEC2YR")
#' )
#' }
#'
#' @export
yyds_nhanes_design <- function(data,
                               cycle = NULL,
                               weight,
                               year_var = "YEAR",
                               strata = "SDMVSTRA",
                               psu = "SDMVPSU",
                               out_weight = "NHANES_wt",
                               out_cycle = "NHANES_cycle",
                               nest = TRUE,
                               lonely_psu = "adjust") {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("yyds_nhanes_design() requires the survey package.")
  }

  if (!is.data.frame(data)) {
    stop("data must be a data.frame.")
  }

  if (missing(weight) || length(weight) == 0) {
    stop("weight 必须至少包含一个候选原始权重变量名。")
  }

  weight <- .yyds_nhanes_resolve_weight_names(weight, names(data))

  required_vars <- c(year_var, strata, psu)
  missing_required <- setdiff(required_vars, names(data))
  if (length(missing_required) > 0) {
    stop("The following required variables are missing: ",
         paste(missing_required, collapse = ", "))
  }

  if (is.null(cycle)) {
    cycle_specs <- .yyds_nhanes_default_cycle_specs(unique(as.character(data[[year_var]])))
    no_merge <- TRUE
  } else {
    cycle_specs <- stats::setNames(as.list(cycle), cycle)
    no_merge <- FALSE
    .yyds_nhanes_validate_cycle_specs(
      cycle_specs = cycle_specs,
      available_years = unique(as.character(data[[year_var]]))
    )
  }

  original_lonely <- getOption("survey.lonely.psu")
  if (!is.null(lonely_psu)) {
    options(survey.lonely.psu = lonely_psu)
    on.exit(options(survey.lonely.psu = original_lonely), add = TRUE)
  }

  make_one_design <- function(period, print_log = TRUE) {
    dat <- data
    dat[[out_weight]] <- NA_real_
    dat[[out_cycle]] <- if (is.null(period)) as.character(dat[[year_var]]) else period
    period_label <- if (is.null(period)) "不合并" else period

    selected_cycles <- if (is.null(period)) {
      .yyds_nhanes_order_cycles(unique(as.character(dat[[year_var]])))
    } else {
      .yyds_nhanes_expand_period(period, unique(as.character(dat[[year_var]])))
    }

    if (length(selected_cycles) == 0) {
      stop("No YEAR values match cycle = ", period)
    }

    dat <- dat[as.character(dat[[year_var]]) %in% selected_cycles, , drop = FALSE]

    if (nrow(dat) == 0) {
      stop("No rows remain after selecting cycle = ", ifelse(is.null(period), "NULL", period))
    }

    total_years <- if (is.null(period)) NA_real_ else .yyds_nhanes_sum_cycle_years(selected_cycles)
    log_rows <- vector("list", length(selected_cycles))
    prefer_two_year_early <- no_merge || (
      length(selected_cycles) == 1 &&
        selected_cycles[[1]] %in% c("1999-2000", "2001-2002")
    )

    for (i in seq_along(selected_cycles)) {
      yr <- selected_cycles[[i]]
      rule <- .yyds_nhanes_cycle_rule(
        year_label = yr,
        weights = weight,
        data = dat,
        year_var = year_var,
        prefer_two_year_1999_2002 = prefer_two_year_early
      )
      idx <- as.character(dat[[year_var]]) == yr
      multiplier <- if (is.null(period)) 1 else rule$represented_years / total_years

      raw_weight <- .yyds_nhanes_as_numeric_weight(dat[[rule$weight_var]])
      dat[[out_weight]][idx] <- raw_weight[idx] * multiplier
      bad_weight_n <- sum(is.na(dat[[out_weight]][idx]) | dat[[out_weight]][idx] <= 0)
      log_total_years <- if (is.null(period)) rule$represented_years else total_years

      log_rows[[i]] <- data.frame(
        analysis_cycle = if (is.null(period)) yr else period,
        YEAR = yr,
        weight_var = rule$weight_var,
        represented_years = rule$represented_years,
        total_years = log_total_years,
        multiplier = multiplier,
        n_valid_weight = rule$n_valid,
        n_rows = sum(idx),
        n_missing_or_nonpositive = bad_weight_n,
        stringsAsFactors = FALSE
      )
    }

    weight_log <- do.call(rbind, log_rows)

    bad_weight <- is.na(dat[[out_weight]]) | dat[[out_weight]] <= 0
    bad_weight_total <- sum(bad_weight)
    if (bad_weight_total > 0) {
      dat <- dat[!bad_weight, , drop = FALSE]
    }

    if (print_log) {
      .yyds_nhanes_cat_design_log(
        period_label = period_label,
        weight_log = weight_log,
        n_after_filter = nrow(dat),
        n_bad_weight = bad_weight_total
      )
    }

    if (nrow(dat) == 0) {
      stop("No rows remain after filtering missing/nonpositive analysis weights.")
    }

    des <- survey::svydesign(
      ids = stats::as.formula(paste0("~", psu)),
      strata = stats::as.formula(paste0("~", strata)),
      weights = stats::as.formula(paste0("~", out_weight)),
      data = dat,
      nest = nest
    )

    attr(des, "data") <- dat
    attr(des, "weight_log") <- weight_log
    attr(des, "cycle") <- period
    des
  }

  out <- lapply(cycle_specs, make_one_design, print_log = !no_merge && length(cycle_specs) == 1)

  if (length(out) == 1) {
    out[[1]]
  } else {
    combined_data <- do.call(rbind, lapply(out, function(x) attr(x, "data")))
    rownames(combined_data) <- NULL
    weight_log_all <- do.call(rbind, lapply(out, function(x) attr(x, "weight_log")))
    rownames(weight_log_all) <- NULL

    .yyds_nhanes_cat_design_log(
      period_label = paste(names(cycle_specs), collapse = ", "),
      weight_log = weight_log_all,
      n_after_filter = nrow(combined_data),
      n_bad_weight = sum(weight_log_all$n_missing_or_nonpositive)
    )

    des <- survey::svydesign(
      ids = stats::as.formula(paste0("~", psu)),
      strata = stats::as.formula(paste0("~", strata)),
      weights = stats::as.formula(paste0("~", out_weight)),
      data = combined_data,
      nest = nest
    )

    attr(des, "data") <- combined_data
    attr(des, "weight_log") <- weight_log_all
    attr(des, "cycle") <- if (no_merge) NULL else cycle
    des
  }
}

.yyds_nhanes_parse_year_range <- function(x) {
  nums <- regmatches(x, gregexpr("[0-9]{4}", x))[[1]]
  if (length(nums) < 2) {
    stop("Cannot parse cycle/year range: ", x)
  }
  as.integer(nums[1:2])
}

.yyds_nhanes_cat_design_log <- function(period_label,
                                        weight_log,
                                        n_after_filter,
                                        n_bad_weight) {
  cat("\n分析周期: ", period_label, "\n", sep = "")
  cat("加权规则:\n", sep = "")
  .yyds_nhanes_cat_weight_summary(weight_log)
  cat("样本: n=", n_after_filter, "\n", sep = "")

  bad_rows <- weight_log[weight_log$n_missing_or_nonpositive > 0, , drop = FALSE]
  if (nrow(bad_rows) > 0) {
    cat("    权重缺失/0/负数:\n", sep = "")
    for (i in seq_len(nrow(bad_rows))) {
      cat("    - ", bad_rows$YEAR[[i]], ": ", bad_rows$n_missing_or_nonpositive[[i]], "\n", sep = "")
    }
    cat("    - 总计: ", n_bad_weight, "\n", sep = "")
  }
}

.yyds_nhanes_cat_weight_summary <- function(weight_log) {
  show_block_label <- !all(weight_log$analysis_cycle == weight_log$YEAR)

  for (analysis_cycle in unique(weight_log$analysis_cycle)) {
    block <- weight_log[weight_log$analysis_cycle == analysis_cycle, , drop = FALSE]
    if (show_block_label) {
      cat("  ", analysis_cycle, "\n", sep = "")
    }

    prefix <- if (show_block_label) "    - " else "  - "
    for (i in seq_len(nrow(block))) {
      coef <- paste0(
        .yyds_nhanes_format_years(block$represented_years[[i]]),
        "/",
        .yyds_nhanes_format_years(block$total_years[[i]])
      )
      cat(prefix, block$YEAR[[i]], ": ", block$weight_var[[i]], " x ", coef, "\n", sep = "")
    }
  }
}

.yyds_nhanes_format_years <- function(x) {
  x <- as.numeric(x)
  out <- ifelse(abs(x - round(x)) < .Machine$double.eps^0.5,
                as.character(as.integer(round(x))),
                format(x, trim = TRUE, scientific = FALSE))
  out
}

.yyds_nhanes_compact_years <- function(x) {
  x <- as.character(x)
  if (length(x) <= 3) {
    return(paste(x, collapse = ", "))
  }
  paste0(x[[1]], " 至 ", x[[length(x)]], "（", length(x), "个YEAR）")
}

.yyds_nhanes_default_cycle_specs <- function(available_years) {
  out <- .yyds_nhanes_order_cycles(as.character(available_years))
  if ("2017-2020" %in% out) {
    out <- setdiff(out, "2017-2018")
  }
  stats::setNames(as.list(out), out)
}

.yyds_nhanes_validate_cycle_specs <- function(cycle_specs, available_years) {
  available_years <- as.character(available_years)
  selected <- lapply(cycle_specs, .yyds_nhanes_expand_period, available_years = available_years)
  expected <- lapply(cycle_specs, .yyds_nhanes_expected_cycles)

  missing_details <- unlist(Map(function(period, wanted) {
    missing <- setdiff(wanted, available_years)
    if (length(missing) == 0) character(0) else {
      paste0(period, " 缺少：", paste(missing, collapse = ", "))
    }
  }, names(cycle_specs), expected), use.names = FALSE)
  if (length(missing_details) > 0) {
    stop(
      "cycle 指定的分析周期在 data$YEAR 中不完整：\n  - ",
      paste(missing_details, collapse = "\n  - ")
    )
  }

  selected_long <- data.frame(
    analysis_cycle = rep(names(cycle_specs), lengths(selected)),
    YEAR = unlist(selected, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  overlap_years <- unique(selected_long$YEAR[duplicated(selected_long$YEAR)])
  if (length(overlap_years) > 0) {
    details <- vapply(overlap_years, function(year) {
      periods <- selected_long$analysis_cycle[selected_long$YEAR == year]
      paste0(year, " -> ", paste(periods, collapse = ", "))
    }, character(1))
    stop(
      "cycle 分段存在重叠 YEAR，合并为一个 design 会重复纳入样本：\n  - ",
      paste(details, collapse = "\n  - ")
    )
  }

  invisible(TRUE)
}

.yyds_nhanes_expected_cycles <- function(period) {
  canonical <- c(
    "1999-2000", "2001-2002", "2003-2004", "2005-2006", "2007-2008",
    "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018",
    "2017-2020", "2021-2023"
  )
  target <- .yyds_nhanes_parse_year_range(period)
  selected <- canonical[vapply(canonical, function(year) {
    bounds <- .yyds_nhanes_parse_year_range(year)
    .yyds_nhanes_range_overlap(bounds[[1]], bounds[[2]], target[[1]], target[[2]])
  }, logical(1))]

  if (target[[2]] >= 2020 && target[[1]] <= 2020) {
    selected <- setdiff(selected, "2017-2018")
  } else {
    selected <- setdiff(selected, "2017-2020")
  }
  .yyds_nhanes_order_cycles(selected)
}

.yyds_nhanes_resolve_weight_names <- function(weight, data_names) {
  if (!is.character(weight) || anyNA(weight) || any(!nzchar(trimws(weight)))) {
    stop("weight 必须是仅包含非空变量名的字符向量。")
  }

  requested <- trimws(weight)
  data_key <- toupper(data_names)
  requested_key <- toupper(requested)
  hits <- lapply(requested_key, function(x) which(data_key == x))

  ambiguous <- which(lengths(hits) > 1)
  if (length(ambiguous) > 0) {
    details <- vapply(ambiguous, function(i) {
      paste0(requested[[i]], " -> ", paste(data_names[hits[[i]]], collapse = ", "))
    }, character(1))
    stop(
      "data 中存在忽略大小写后重名的列，以下 weight 变量无法唯一匹配：\n  - ",
      paste(details, collapse = "\n  - ")
    )
  }

  missing <- which(lengths(hits) == 0)
  if (length(missing) > 0) {
    available <- data_names[grepl("(2YR|4YR|PRP)$", data_names, ignore.case = TRUE)]
    available_text <- if (length(available) == 0) {
      "  - （无）"
    } else {
      paste0("  - ", available, collapse = "\n")
    }
    stop(
      "weight 中以下变量未在 data 中找到（忽略大小写后仍未匹配）：\n  - ",
      paste(requested[missing], collapse = "\n  - "),
      "\ndata 中可用的 2YR/4YR/PRP 权重变量：\n", available_text
    )
  }

  resolved <- data_names[vapply(hits, `[[`, integer(1), 1L)]
  unsupported <- resolved[!grepl("(2YR|4YR|PRP)$", resolved, ignore.case = TRUE)]
  if (length(unsupported) > 0) {
    stop(
      "以下 weight 变量无法识别权重类型；变量名必须以 2YR、4YR 或 PRP 结尾：\n  - ",
      paste(unsupported, collapse = "\n  - ")
    )
  }

  resolved[!duplicated(toupper(resolved))]
}

.yyds_nhanes_as_numeric_weight <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(x))
}

.yyds_nhanes_range_overlap <- function(a_start, a_end, b_start, b_end) {
  a_start <= b_end && b_start <= a_end
}

.yyds_nhanes_expand_period <- function(period, available_years) {
  target <- .yyds_nhanes_parse_year_range(period)
  target_start <- target[[1]]
  target_end <- target[[2]]
  available_years <- as.character(available_years)

  if (target_end >= 2020 && "2017-2020" %in% available_years) {
    selected <- available_years[vapply(available_years, function(y) {
      if (identical(y, "2017-2018")) {
        return(FALSE)
      }
      yr <- .yyds_nhanes_parse_year_range(y)
      .yyds_nhanes_range_overlap(yr[[1]], yr[[2]], target_start, target_end)
    }, logical(1))]
  } else {
    selected <- available_years[vapply(available_years, function(y) {
      if (identical(y, "2017-2020")) {
        return(FALSE)
      }
      yr <- .yyds_nhanes_parse_year_range(y)
      .yyds_nhanes_range_overlap(yr[[1]], yr[[2]], target_start, target_end)
    }, logical(1))]
  }

  .yyds_nhanes_order_cycles(selected)
}

.yyds_nhanes_order_cycles <- function(x) {
  x[order(vapply(x, function(y) .yyds_nhanes_parse_year_range(y)[[1]], integer(1)))]
}

.yyds_nhanes_sum_cycle_years <- function(cycles) {
  sum(vapply(cycles, function(y) .yyds_nhanes_cycle_rule_years(y), numeric(1)))
}

.yyds_nhanes_cycle_rule_years <- function(year_label) {
  if (identical(year_label, "2017-2020")) {
    return(3.2)
  }
  if (identical(year_label, "2021-2023")) {
    return(2)
  }
  yr <- .yyds_nhanes_parse_year_range(year_label)
  yr[[2]] - yr[[1]] + 1
}

.yyds_nhanes_cycle_rule <- function(year_label,
                                    weights,
                                    data,
                                    year_var,
                                    prefer_two_year_1999_2002 = FALSE) {
  year_label <- as.character(year_label)

  if (year_label %in% c("1999-2000", "2001-2002")) {
    if (prefer_two_year_1999_2002) {
      choice <- .yyds_nhanes_choose_weight(data, year_var, year_label, weights, "2YR")
      .yyds_nhanes_require_weight(choice, year_label, "2YR")
    } else {
      choice_4yr <- .yyds_nhanes_choose_weight(data, year_var, year_label, weights, "4YR")
      if (!is.na(choice_4yr$weight_var)) {
        choice <- choice_4yr
      } else {
        choice_2yr <- .yyds_nhanes_choose_weight(data, year_var, year_label, weights, "2YR")
        if (is.na(choice_2yr$weight_var)) {
          .yyds_nhanes_stop_no_weight(year_label, list(choice_4yr, choice_2yr))
        }
        choice <- choice_2yr
      }
    }
    represented_years <- if (identical(choice$type, "4YR")) 4 else 2
    return(list(
      weight_var = choice$weight_var,
      represented_years = represented_years,
      n_valid = choice$n_valid
    ))
  }

  if (identical(year_label, "2017-2020")) {
    choice <- .yyds_nhanes_choose_weight(data, year_var, year_label, weights, "PRP")
    .yyds_nhanes_require_weight(choice, year_label, "PRP")
    return(list(weight_var = choice$weight_var, represented_years = 3.2, n_valid = choice$n_valid))
  }

  if (identical(year_label, "2021-2023")) {
    choice <- .yyds_nhanes_choose_weight(data, year_var, year_label, weights, "2YR")
    .yyds_nhanes_require_weight(choice, year_label, "2YR")
    return(list(weight_var = choice$weight_var, represented_years = 2, n_valid = choice$n_valid))
  }

  choice <- .yyds_nhanes_choose_weight(data, year_var, year_label, weights, "2YR")
  .yyds_nhanes_require_weight(choice, year_label, "2YR")

  list(weight_var = choice$weight_var, represented_years = 2, n_valid = choice$n_valid)
}

.yyds_nhanes_choose_weight <- function(data, year_var, year_label, weights, type) {
  candidates <- weights[grepl(paste0(type, "$"), weights, ignore.case = TRUE)]
  idx <- as.character(data[[year_var]]) == year_label
  counts <- vapply(candidates, function(x) {
    value <- .yyds_nhanes_as_numeric_weight(data[[x]])
    sum(!is.na(value[idx]) & value[idx] > 0)
  }, integer(1))

  usable <- counts > 0
  selected <- if (any(usable)) candidates[which(usable)[which.min(counts[usable])]] else NA_character_
  selected_n <- if (is.na(selected)) NA_integer_ else unname(counts[[selected]])

  list(
    type = type,
    weight_var = selected,
    n_valid = selected_n,
    candidates = candidates,
    counts = counts
  )
}

.yyds_nhanes_require_weight <- function(choice, year_label, type) {
  if (is.na(choice$weight_var)) {
    .yyds_nhanes_stop_no_weight(year_label, list(choice), required_type = type)
  }
  invisible(choice)
}

.yyds_nhanes_stop_no_weight <- function(year_label, choices, required_type = NULL) {
  details <- vapply(choices, function(choice) {
    if (length(choice$candidates) == 0) {
      paste0(choice$type, ": 未传入同类候选变量")
    } else {
      paste0(
        choice$type, ": ",
        paste0(choice$candidates, "=", unname(choice$counts), collapse = ", ")
      )
    }
  }, character(1))
  type_text <- if (is.null(required_type)) {
    paste(vapply(choices, `[[`, character(1), "type"), collapse = " 或 ")
  } else {
    required_type
  }
  stop(
    "YEAR ", year_label, " 没有可用的 ", type_text, " 权重。",
    "有效权重定义为可转为数值且大于 0；有效数为 0 的候选不会被选择。\n  - ",
    paste(details, collapse = "\n  - ")
  )
}
