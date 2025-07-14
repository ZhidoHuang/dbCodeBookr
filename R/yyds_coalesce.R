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
#' 合并列位于第一个子列之前，便于后续分析。
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
                          valid_var  = NULL,
                          remove_original = TRUE) {

  # -------- 参数检查 --------
  if (!is.data.frame(data))           stop("data must be a data frame")
  if (length(key_chr) < 2)            stop("key_chr must have ≥2 elements")
  pattern <- match.arg(pattern)
  if (!is.null(valid_var) && !valid_var %in% names(data))
    stop(paste("Validation variable", valid_var, "not found in data"))

  # -------- 内部工具 --------
  esc <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)

  regex_pattern <- switch(pattern,
                          ends_with   = paste0("(", paste(esc(key_chr), collapse = "|"), ")$"),
                          starts_with = paste0("^(", paste(esc(key_chr), collapse = "|"), ")"),
                          contains    = paste0("(", paste(esc(key_chr), collapse = "|"), ")")
  )

  matched_cols <- grep(regex_pattern, names(data), value = TRUE)
  if (length(matched_cols) == 0) return(data)    # 没匹配直接返回

  base_names <- switch(pattern,
                       ends_with   = sub(paste0("(", paste(esc(key_chr), collapse = "|"), ")$"), "", matched_cols),
                       starts_with = sub(paste0("^(", paste(esc(key_chr), collapse = "|"), ")"), "", matched_cols),
                       contains    = vapply(matched_cols, function(x) {
                         for (k in key_chr) if (grepl(esc(k), x)) return(sub(esc(k), "", x))
                         x
                       }, FUN.VALUE = character(1))
  )

  # -------- 主循环 --------
  for (base in unique(base_names[duplicated(base_names)])) {

    cols_to_merge <- matched_cols[base_names == base]
    first_subcol  <- cols_to_merge[which.min(match(cols_to_merge, names(data)))]
    new_col       <- if (!is.null(new_suffix)) paste0(base, new_suffix) else base


    validation1 <- data %>%
      dplyr::group_by(.data[[valid_var]]) %>%
      dplyr::summarise(
        dplyr::across(all_of(cols_to_merge), ~ sum(!is.na(.x))),
        .groups = "drop"
      )



    # 1) 生成新列
    data <- dplyr::mutate(
      data,
      !!new_col := dplyr::coalesce(!!!rlang::syms(cols_to_merge))
    )

    # 2) 把新列挪到第一子列前
    data <- dplyr::relocate(data, !!new_col, .before = dplyr::all_of(first_subcol))

    new_col2 <- paste0("  -→ ", new_col)

    # 3) 打印验证信息（仅打印，不写回 data）
    if (!is.null(valid_var)) {
      validation2 <- data %>%
        dplyr::group_by(.data[[valid_var]]) %>%
        dplyr::summarise(
          "{new_col2}" := sum(!is.na(.data[[new_col]])),
          .groups = "drop"
        )
      validation <- dplyr::left_join(validation1, validation2, by = valid_var)
      cat(crayon::red("\n", new_col, "\n"))
      print(as.data.frame(validation))
    }

    # 4) 删除旧列
    if (remove_original) {
      cols_to_remove <- setdiff(cols_to_merge, new_col)
      data <- dplyr::select(data, -dplyr::all_of(cols_to_remove))
    }
  }

  data
}
