#' 快速合并多列数据
#'
#' 根据指定的列名模式合并数据框中的多列，使用dplyr::coalesce函数优先选择非NA值。
#'
#' @param data 要处理的数据框
#' @param key_chr 要合并的列名中包含的关键字符（后缀、前缀或中间部分），默认为c("_a", "_b", "_c")
#' @param pattern 匹配模式，可选："ends_with"（后缀匹配）、"starts_with"（前缀匹配）或"contains"（包含匹配），默认为"ends_with"
#' @param new_suffix 新列名的后缀，如果为NULL则使用基础名
#' @param valid_var 用于验证合并结果的变量名（通常是ID变量），如果提供则会输出合并统计信息
#' @param remove_original 是否移除原始列，默认为TRUE
#'
#' @return 返回处理后的数据框，包含合并后的新列（可能已移除原始列）
#'
#' @details
#' 该函数主要用于处理具有相同变量但来自不同来源的数据（如调查数据中的多波次测量）。
#' 它会自动识别具有相同基础变量名但不同后缀/前缀的列，并使用dplyr::coalesce合并它们。
#'
#' @examples
#' \dontrun{
#' # 示例数据
#' df <- data.frame(
#'   id = 1:5,
#'   score_a = c(1, NA, 3, NA, 5),
#'   score_b = c(NA, 2, 3, 4, NA),
#'   value_x = c(10, NA, 30, NA, NA),
#'   value_y = c(NA, 20, NA, 40, 50)
#' )
#'
#' # 合并以后缀_a和_b结尾的列
#' yyds_coalesce(df, key_chr = c("_a", "_b"), pattern = "ends_with")
#'
#' # 合并并保留原始列，添加"_merged"后缀
#' yyds_coalesce(df, key_chr = c("_a", "_b"), pattern = "ends_with",
#'               new_suffix = "_merged", remove_original = FALSE)
#'
#' # 合并并验证结果（使用id列作为验证变量）
#' yyds_coalesce(df, key_chr = c("_a", "_b"), pattern = "ends_with",
#'               valid_var = "id")
#' }
#'
#' @importFrom dplyr coalesce
#' @importFrom crayon red
#' @export
yyds_coalesce <- function(data,
                          key_chr = c("_a", "_b", "_c"),
                          pattern = c("ends_with", "starts_with", "contains"),
                          new_suffix = NULL,
                          valid_var = NULL,
                          remove_original = TRUE) {

  # 参数验证
  if (!is.data.frame(data)) stop("\n data must be a data frame")
  if (length(key_chr) < 2) stop("\n key_chr must contain at least 2 elements for coalescing")
  pattern <- match.arg(pattern)
  if (!is.null(valid_var) && !valid_var %in% names(data)) {
    stop(paste("Validation variable", valid_var, "not found in data"))
  }

  # 辅助函数：转义正则特殊字符
  escape_regex <- function(x) {
    gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
  }

  # 根据pattern类型构建正则表达式
  regex_pattern <- switch(pattern,
                          "ends_with" = paste0("(", paste(escape_regex(key_chr), collapse = "|"), ")$"),
                          "starts_with" = paste0("^(", paste(escape_regex(key_chr), collapse = "|"), ")"),
                          "contains" = paste0("(", paste(escape_regex(key_chr), collapse = "|"), ")")
  )

  # 获取匹配的列名
  matched_cols <- grep(regex_pattern, names(data), value = TRUE)

  # 提取基础列名（去除key_chr部分）
  base_names <- switch(pattern,
                       "ends_with" = sub(paste0("(", paste(escape_regex(key_chr), collapse = "|"), ")$"), "", matched_cols),
                       "starts_with" = sub(paste0("^(", paste(escape_regex(key_chr), collapse = "|"), ")"), "", matched_cols),
                       "contains" = sapply(matched_cols, function(x) {
                         for (k in key_chr) {
                           if (grepl(escape_regex(k), x)) {
                             return(sub(escape_regex(k), "", x))
                           }
                         }
                         return(x)
                       })
  )

  # 统计每个基础名出现的次数
  base_counts <- table(base_names)
  bases_to_process <- names(base_counts)[base_counts >= 2]  # 至少有两个匹配列才处理

  # 对每个基础名进行处理
  for (base in bases_to_process) {

    # 获取该基础名对应的所有列
    cols_to_coalesce <- matched_cols[base_names == base]

    # 判断是否要覆盖原始列
    if (!is.null(new_suffix) && new_suffix %in% key_chr) {
      # 如果new_suffix匹配key_chr中的一个，则覆盖对应的原始列
      target_col <- paste0(base, new_suffix)
      if (target_col %in% cols_to_coalesce) {
        # 直接覆盖该列
        data[[target_col]] <- do.call(dplyr::coalesce, data[cols_to_coalesce])
        # 更新cols_to_coalesce，排除被覆盖的列
        cols_to_coalesce <- setdiff(cols_to_coalesce, target_col)
      } else {
        # 如果目标列不存在，则创建新列
        data[[target_col]] <- do.call(dplyr::coalesce, data[cols_to_coalesce])
      }
    } else {
      # 常规情况：创建新列
      new_col <- if (!is.null(new_suffix)) paste0(base, new_suffix) else base
      data[[new_col]] <- do.call(dplyr::coalesce, data[cols_to_coalesce])
    }

    # 验证合并结果（如果有验证变量）
    if (!is.null(valid_var)) {
      # 确定要显示的目标列名
      display_col <- if (!is.null(new_suffix) && new_suffix %in% key_chr) {
        paste0(base, new_suffix)
      } else {
        ifelse(!is.null(new_suffix), paste0(base, new_suffix), base)
      }

      validation <- data %>%
        dplyr::group_by(.data[[valid_var]]) %>%
        dplyr::summarise(
          dplyr::across(all_of(cols_to_coalesce), ~sum(!is.na(.))),
          "{display_col}" := sum(!is.na(.data[[display_col]])),
          .groups = "drop"
        )

      cat(crayon::red("\n", display_col, "\n"))
      print(as.data.frame(validation))  # 避免tibble的截断打印
    }

    # 移除原始列（如果需要）
    if (remove_original && length(cols_to_coalesce) > 0) {
      data <- data[, !names(data) %in% cols_to_coalesce, drop = FALSE]
    }
  }

  return(data)
}
