#' 便捷切割函数
#'
#' 本函数用于对数据框中的变量进行分组切割，根据指定的断点或分位数，将变量划分为多个类别。
#' 支持通过 `value`、`probs` 或 `ncut` 参数指定切割标准。
#'
#' @param data 数据框，包含需要切割的变量。
#' @param vars 需要进行切割的变量名称（字符向量）。
#' @param value 指定自定义的切割点。如果提供，则忽略 `probs` 和 `ncut` 参数。
#' @param probs 分位数，指定根据分位数进行切割（例如 `probs = c(0.25, 0.5, 0.75)`）。
#' @param ncut 通过指定切割的数量来进行分组（例如 `ncut = 4` 进行四分位切割）。
#' @param suffix 新生成的变量名称的后缀，默认为 "q"（如变量 `age` 切割后生成变量 `ageq`）。
#' @param right 是否将区间右端点包含在内，默认为 `TRUE`，即(a,b]。
#' @param verbose 是否在控制台输出详细信息，默认为 `TRUE`。
#'
#' @return 返回带有新切割变量的数据框。新变量名为原变量名加上 `suffix` 后缀。
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
yyds_cut <- function(data, vars, value = NULL, probs = NULL, ncut = NULL,
                     suffix = "q", right = TRUE, verbose = TRUE) {
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
        quantiles <- quantile(vec, probs = probs, na.rm = TRUE)
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
        cuts <- quantile(vec, probs = probs_seq[-c(1, length(probs_seq))], na.rm = TRUE)
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

      # 控制台输出（不影响返回结果）
      if (verbose) {
        message(paste0("\n[", var, "] → ", new_varname))
        message("断点: ", paste(round(breaks, 4), collapse = ", "))
        message("标签: ", paste(labels, collapse = " | "))
        print(table(data[[new_varname]]))
      }

    }, error = function(e) {
      message(paste("Error with variable", var, ":", e$message))
    })
  }

  return(data)
}
