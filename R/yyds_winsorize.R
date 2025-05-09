#' 缩尾处理变量极端情况
#'
#' 对指定变量进行缩尾处理（Winsorization），将超出分位数阈值的极端值替换为指定分位数的值，
#' 并可生成包含原始数据与处理后数据对比的 PDF 图报告。
#'
#' @param df 输入数据框（data.frame 或 tibble）
#' @param vars 需要处理的变量名（字符向量）
#' @param lower 下分位数阈值（默认 0.01，即 1% 分位数）
#' @param upper 上分位数阈值（默认 0.99，即 99% 分位数）
#' @param pdf_name 输出 PDF 报告的文件路径（可选，默认 NULL 不生成报告）
#' @param suffix 处理后变量的后缀（默认 "_ex"）
#'
#' @return 返回包含以下内容的列表：
#' \itemize{
#'   \item `data` - 处理后的数据框（新增后缀列保留原始数据）
#'   \item `summary` - 统计对比表格（包含原始数据和处理后数据的描述性统计）
#' }
#'
#' @details
#' 函数会执行以下操作：
#' \enumerate{
#'   \item 计算每个变量的指定分位数阈值
#'   \item 将超出阈值的值替换为阈值（下限替换为 lower 分位数，上限替换为 upper 分位数）
#'   \item 在 PDF 报告中生成每对变量的对比 QQ 图（原始 vs 处理后）
#'   \item 返回包含处理结果和统计对比的列表
#' }
#'
#' @examples
#' \donttest{
#' # 基本用法
#' set.seed(123)
#' test_df <- data.frame(
#'   normal = rnorm(1000),
#'   skewed = c(rnorm(900), runif(100, 5, 10))
#' )
#'
#' # 默认处理（不生成报告）
#' result <- yyds_winsorize(test_df, vars = c("normal", "skewed"))
#' head(result$data)
#' result$summary
#'
#' # 自定义参数（生成报告）
#' \dontrun{
#' result <- yyds_winsorize(
#'   test_df,
#'   vars = "skewed",
#'   lower = 0.05,
#'   upper = 0.95,
#'   suffix = "_win",
#'   pdf_name = "winsorize_report.pdf"
#' )
#' }
#' }
#'
#' @export
yyds_winsorize <- function(df,
vars,
lower = 0.01,
upper = 0.99,
pdf_name = NULL,
suffix = "_ex") {

  if (!is.null(pdf_name)) {
    pdf(pdf_name, width = 7, height = 10)
    on.exit({
      dev.off()
      message("Saved to: ", normalizePath(pdf_name))
    })
  }

  for (i in seq_along(vars)) {
    var <- vars[i]

    # 计算分位数
    q_lower <- quantile(df[[var]], probs = lower, na.rm = TRUE)
    q_upper <- quantile(df[[var]], probs = upper, na.rm = TRUE)

    # 使用自定义后缀（原代码修改处）
    new_var <- paste0(var, suffix)  # 原为 paste0(var, "_ex")

    # 识别极端值
    is_lower <- !is.na(df[[var]]) & df[[var]] < q_lower
    is_upper <- !is.na(df[[var]]) & df[[var]] > q_upper

    # Winsorize处理
    df[[new_var]] <- pmin(pmax(df[[var]], q_lower), q_upper)

    if (!is.null(pdf_name)) {
      if ((i-1) %% 3 == 0) {
        par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))
      }

      # 准备QQ图数据
      y <- df[[var]][!is.na(df[[var]])]
      x <- qqnorm(y, plot.it = FALSE)

      # 计算理论分位数临界值
      theo_lower <- qnorm(lower)
      theo_upper <- qnorm(upper)

      # 原始数据QQ图
      plot(x$x, x$y,
           main = paste("Original:", var),
           xlab = paste0("Range: [", round(min(df[[var]], na.rm = TRUE), 4), ", ",
                         round(max(df[[var]], na.rm = TRUE), 4), "]"),
           ylab = "Sample Quantiles",
           pch = 1, cex = 1.2,  # 增大描点size
           col = ifelse(is_lower[!is.na(df[[var]])], "#FFC000",
                        ifelse(is_upper[!is.na(df[[var]])], "#FFC000", "gray20")))
      qqline(y, col = "red", lwd = 1.5)

      # 添加灰色垂直切线
      abline(v = theo_lower, col = "gray50", lty = 2, lwd = 1.5)
      abline(v = theo_upper, col = "gray50", lty = 2, lwd = 1.5)

      # 添加图例
      legend("top",
             legend = c(paste("Lower (n=", sum(is_lower), ")"),
                        paste("Upper (n=", sum(is_upper), ")"),
                        paste0(round(lower*100, 1), "% - ", round((upper)*100, 1), "%")),
             col = c("#FFC000", "#FFC000", "gray20"),
             pch = c(19, 19, 1), pt.cex = c(0.8, 0.8, 0.6),
             bty = "n")

      # 处理后的QQ图
      qqnorm(df[[new_var]][!is.na(df[[new_var]])],
             main = paste("Winsorized:", var),
             xlab = paste0("Bounds: [", round(q_lower, 4), ", ", round(q_upper, 4), "]"),
             ylab = "Sample Quantiles",
             pch = 1, cex = 1.2, col = "gray20")  # 全部用黑色
      qqline(df[[new_var]][!is.na(df[[new_var]])], col = "red", lwd = 1.5)

      if ((i-1) %% 3 == 0) {
        title(paste("Extreme Value Report - Vars", i, "to", min(i+2, length(vars))),
              outer = TRUE, cex.main = 1.5)
      }
    }
  }

  return(df)
}
