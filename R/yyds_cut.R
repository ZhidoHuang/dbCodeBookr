#' 便捷切割函数
#'
#' 本函数用于对数据框中的变量进行分组切割，根据指定的断点或分位数，将变量划分为多个类别。
#' 支持通过 `value`、`probs` 或 `ncut` 参数指定切割标准。
#'
#' @param data 数据框，包含需要切割的变量。与 `design` 二选一。
#' @param design `survey.design` 或 `svyrep.design` 对象。与 `data` 二选一。
#' @param vars 需要进行切割的变量名称（字符向量）。
#' @param value 指定自定义的切割点。如果提供，则忽略 `probs` 和 `ncut` 参数。
#' @param probs 分位数，指定根据分位数进行切割（例如 `probs = c(0.25, 0.5, 0.75)`）。
#' @param ncut 通过指定切割的数量来进行分组（例如 `ncut = 4` 进行四分位切割）。
#' @param suffix 新生成的变量名称的后缀，默认为 "q"（如变量 `age` 切割后生成变量 `ageq`）。
#' @param right 是否将区间右端点包含在内，默认为 `TRUE`，即(a,b]。
#' @param verbose 是否在控制台输出详细信息，默认为 `TRUE`。
#' @param file 输出文本文件名或路径。默认为 `NULL`，不输出文件；例如
#'   `file = "四分位"` 会生成 `四分位.txt`。若未以 `.txt` 结尾，函数会自动补全。
#' @details 使用 `data` 时，`probs` 和 `ncut` 的断点由普通 `quantile()` 计算；
#'   使用 `design` 时，断点由 `survey::svyquantile()` 计算，会纳入抽样权重及
#'   design 的复杂抽样结构。`value` 是用户指定的固定断点，不进行加权计算。
#'
#' @return 输入为数据框时，返回带有新切割变量的数据框；输入为
#'   `survey.design`/`svyrep.design` 时，返回在 `$variables` 中增加新变量的同类
#'   design 对象。新变量名为原变量名加上 `suffix` 后缀。
#'
#' @examples
#' \dontrun{
#' # 示例数据
#' set.seed(42)
#' data <- data.frame(
#'   age = sample(18:80, 100, replace = TRUE),
#'   income = sample(10000:100000, 100, replace = TRUE)
#' )
#'
#' # 使用分位数进行切割
#' data_cut <- yyds_cut(data, vars = c("age", "income"), probs = c(0.25, 0.5, 0.75))
#' }
#'
#' @export
yyds_cut <- function(data = NULL, vars, value = NULL, probs = NULL, ncut = NULL,
                     suffix = "q", right = TRUE, verbose = TRUE, file = NULL,
                     design = NULL) {
  # 兼容 yyds_cut(design = des, c("x", "y"), ...) 的位置参数写法。
  if (!is.null(design) && missing(vars) && is.character(data)) {
    vars <- data
    data <- NULL
  }

  if (is.null(data) == is.null(design)) {
    stop("data 和 design 必须且只能提供一个。")
  }

  is_design <- !is.null(design)
  if (is_design) {
    if (!inherits(design, "survey.design") && !inherits(design, "svyrep.design")) {
      stop("design 必须是 survey.design 或 svyrep.design 对象。")
    }
    data <- design$variables
    if (!is.data.frame(data)) {
      stop("design 对象中未找到可用的 $variables 数据框。")
    }
  } else if (!is.data.frame(data)) {
    stop("data 必须是数据框。")
  }

  if (!is.null(file)) {
    if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(trimws(file))) {
      stop("file 必须为 NULL 或单个非空字符路径。")
    }
    file <- trimws(file)
    if (!grepl("\\.txt$", file, ignore.case = TRUE)) {
      file <- paste0(file, ".txt")
    }
  }

  file_output <- character()
  emit_output <- function(lines, show_console = verbose) {
    if (isTRUE(show_console)) {
      cat(paste(lines, collapse = "\n"), "\n", sep = "")
    }
    if (!is.null(file)) {
      file_output <<- c(file_output, lines)
    }
  }

  get_quantiles <- function(var, probs) {
    if (!is_design) {
      return(stats::quantile(data[[var]], probs = probs, na.rm = TRUE))
    }

    result <- survey::svyquantile(
      stats::reformulate(var),
      design = design,
      quantiles = probs,
      na.rm = TRUE,
      ci = FALSE
    )
    as.numeric(result[[1]])
  }

  for (var in vars) {
    tryCatch({
      vec <- data[[var]]

      # 生成 breaks 和 labels
      if (!is.null(value)) {
        cuts <- sort(unique(value))
        breaks <- c(-Inf, cuts, Inf)

        # 根据 right 调整标签方向
        if (right) {
          labels <- c(
            paste0("≤", cuts[1]),
            paste0(cuts[-length(cuts)], "–", cuts[-1]),
            paste0("≥", cuts[length(cuts)])
          )
        } else {
          labels <- c(
            paste0("<", cuts[1]),
            paste0(cuts[-length(cuts)], "–", cuts[-1]),
            paste0(">", cuts[length(cuts)])
          )
        }
      } else if (!is.null(probs)) {
        quantiles <- get_quantiles(var, probs)
        cuts <- unique(quantiles)
        breaks <- c(-Inf, cuts, Inf)
        pcts <- round(100 * c(0, probs, 1))

        # 根据 right 调整标签方向
        if (right) {
          labels <- paste0(pcts[-length(pcts)], "–", pcts[-1], "th")
        } else {
          labels <- paste0(pcts[-1], "–", pcts[-length(pcts)], "th")
        }
      } else if (!is.null(ncut)) {
        probs_seq <- seq(0, 1, length.out = ncut + 1)
        cuts <- get_quantiles(var, probs_seq[-c(1, length(probs_seq))])
        cuts <- unique(cuts)
        breaks <- c(-Inf, cuts, Inf)

        # 修正：直接生成 Q1 到 Qncut 的标签
        labels <- paste0("Q", seq_len(ncut))
      } else {
        stop("请提供 value、probs 或 ncut 参数之一。")
      }

      # 创建新变量
      new_varname <- paste0(var, suffix)
      data[[new_varname]] <- cut(vec, breaks = breaks, labels = labels,
                                 include.lowest = TRUE, right = right)
      data[[new_varname]] <- as.factor(data[[new_varname]])

      break_label <- if (!is.null(value)) {
        "断点（指定）"
      } else if (is_design) {
        "断点（复杂抽样加权）"
      } else {
        "断点"
      }
      output <- c(
        paste0("\n[", var, "] → ", new_varname),
        paste0(break_label, ": ", paste(round(breaks, 4), collapse = ", ")),
        paste0("标签: ", paste(labels, collapse = " | ")),
        capture.output(print(table(data[[new_varname]])))
      )
      emit_output(output)

    }, error = function(e) {
      emit_output(
        paste0("Error with variable ", var, ": ", e$message),
        show_console = TRUE
      )
    })
  }

  if (!is.null(file)) {
    writeLines(file_output, con = file, useBytes = TRUE)
  }

  if (is_design) {
    design$variables <- data
    return(design)
  }

  return(data)
}
