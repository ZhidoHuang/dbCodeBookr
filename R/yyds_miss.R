#' 计算变量缺失数量和缺失占比
#'
#' 该函数用于计算数据框中每个变量的缺失值（NA）数量及百分比，
#' 返回包含变量名、缺失数量和缺失百分比的数据框。
#'
#' @param data 需要分析缺失值的数据框或tibble
#'
#' @return 返回包含三列的数据框：
#' \item{variable}{字符型。变量/列名}
#' \item{missing_count}{整型。变量中的缺失值数量（NA）}
#' \item{missing_pct}{数值型。缺失值百分比，保留1位小数}
#'
#' @examples
#' df <- data.frame(
#'   年龄 = c(25, NA, 30, 35, NA),
#'   性别 = c("男", "女", NA, "女", "男"),
#'   分数 = c(85, 90, NA, NA, 88)
#' )
#' yyds_miss(df)
#'
#' @export
yyds_miss <- function(data) {
  # 输入验证：检查输入是否为数据框
  if (!is.data.frame(data)) {
    stop("输入必须是数据框或tibble")
  }

  # 检查数据框是否有列
  if (ncol(data) == 0) {
    stop("数据框没有列")
  }

  # 检查数据框是否有行
  if (nrow(data) == 0) {
    warning("数据框有0行，所有缺失百分比将为0或NaN")
  }

  # 使用sapply计算每个变量的缺失数量
  missing_counts <- sapply(data, function(x) sum(is.na(x)))

  # 计算缺失百分比，保留1位小数
  missing_pcts <- round(missing_counts / nrow(data) * 100, 1)

  # 创建结果数据框
  result <- data.frame(
    variable = names(data),           # 变量名
    missing_count = missing_counts,   # 缺失数量
    missing_pct = missing_pcts,       # 缺失百分比
    stringsAsFactors = FALSE
  )

  # 按缺失百分比降序排列（便于查看缺失最严重的变量）
  result <- result[order(result$missing_pct, decreasing = TRUE), ]
  rownames(result) <- NULL  # 重置行名

  return(result)
}
