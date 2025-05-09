#' @title 数据CT扫描诊断报告
#' @description
#' 生成自动化数据诊断报告，通过多维度可视化（箱线图、直方图、条形图等）
#' 快速识别数值型变量的异常值和分类变量的分布问题，支持低基数数值变量自动转换。
#'
#' \itemize{
#'   \item 数值变量：箱线图、QQ图、直方图、密度曲线四联图
#'   \item 分类变量：横向条形图显示频数分布
#'   \item 智能处理：自动将低基数（≤5类）数值变量转为分类变量分析
#' }
#' @param data 待分析的数据框或tibble
#' @param file_name 输出PDF文件名，默认"data_report.pdf"
#' @param cat_per_page 每页显示的分类变量图数量，默认6个
#' @param max_categories 单个分类变量最大显示类别数，默认10（超限显示高频类别）
#' @param cex_factor 图形文本缩放系数，默认0.9
#' @return 无返回值，直接生成PDF报告
#' @export
#' @examples
#' \dontrun{
#' # 基本用法
#' yyds_eda_pdf(iris)
#'
#' # 自定义输出
#' yyds_eda_pdf(mtcars, "car_report.pdf", max_categories = 10)
#' }
yyds_eda_pdf <- function(data, file_name = "data_report.pdf",
                    cat_per_page = 6, max_categories = 10,
                    cex_factor = 0.9) {
  if (length(data) == 0) {
    message("没有可报告的异常值检测结果")
    return(NULL)
  }

  pdf(file_name, width = 11, height = 8.5)  # 使用A4横向尺寸

  # 识别数值型变量但类别数≤5的变量
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  numeric_low_cardinality <- sapply(numeric_cols, function(col) {
    x <- na.omit(data[[col]])
    length(unique(x)) <= 5
  })
  numeric_low_cardinality_cols <- numeric_cols[numeric_low_cardinality]

  # 处理真正的数值型变量（类别数>5）
  true_numeric_cols <- setdiff(numeric_cols, numeric_low_cardinality_cols)
  for (col_name in true_numeric_cols) {
    x <- na.omit(data[[col_name]])

    par(mfrow = c(2, 2), oma = c(0, 0, 3, 0))

    boxplot(x, main = paste("Boxplot of", col_name))
    qqnorm(x, main = paste("Q-Q Plot of", col_name))
    qqline(x)
    hist(x, main = paste("Histogram of", col_name), xlab = col_name)
    plot(density(x), main = paste("Density Plot of", col_name))
    curve(dnorm(x, mean = mean(x), sd = sd(x)), col = "red", add = TRUE)
    legend("topright", legend = c("Data", "Normal"), col = c("black", "red"), lty = 1)

    mtext(paste("num:", col_name), outer = TRUE, line = 1, cex = 1.2, font = 2)
  }

  # 分类变量处理（包括低基数数值变量）
  cat_cols <- names(data)[sapply(data, function(x) is.factor(x) | is.character(x) | is.logical(x))]
  all_cat_cols <- unique(c(cat_cols, numeric_low_cardinality_cols))

  if (length(all_cat_cols) > 0) {
    n_pages <- ceiling(length(all_cat_cols) / cat_per_page)

    for (page in 1:n_pages) {
      current_cols <- all_cat_cols[seq((page-1)*cat_per_page + 1, min(page*cat_per_page, length(all_cat_cols)))]

      # 每页单独设置布局
      layout(matrix(1:cat_per_page, nrow = cat_per_page/2, ncol = 2, byrow = TRUE))
      par(mar = c(4, 5, 2, 5),  # 增加左侧边距给长类别名
          oma = c(2, 0, 2, 0),
          cex = cex_factor)

      for (col_name in current_cols) {
        x <- na.omit(data[[col_name]])
        # 对于低基数数值变量，转换为因子
        if (col_name %in% numeric_low_cardinality_cols) {
          x <- factor(x)
        }
        freq <- sort(table(x))
        n_categories <- length(freq)

        # 处理过多类别的情况
        if (n_categories > max_categories) {
          freq <- tail(freq, max_categories)
          main_title <- paste0(col_name, "\n(Top ", max_categories, " most frequent categories)")
        } else {
          main_title <- col_name
        }

        # 计算比例和标签
        pct <- prop.table(freq)*100
        pct <- sprintf("%.1f%%", pct)

        # 横向条形图
        bp <- barplot(freq, horiz = TRUE, las = 1,
                      main = main_title, col = "skyblue",
                      xlab = "", names.arg = rep("", length(freq)),
                      border = NA, xlim = c(0, max(freq)*1.2))

        # 添加类别标签和频数
        text(x = rep(0, length(bp)), y = bp,
             labels = names(freq),
             pos = 4, cex = 1, xpd = TRUE)

        # 右侧标签：频数和百分比
        text(x = max(freq)*1.25, y = bp,
             labels = paste0(freq, " (", pct, ")"),
             pos = 2, cex = 0.8, col = "red",xpd = TRUE)

        # 添加变量信息注释
        var_type <- ifelse(col_name %in% numeric_low_cardinality_cols,
                           "Few-category numeric", "Categorical")
        mtext(paste0("[", var_type, "] Categories: ", n_categories,
                     " | Missing: ", sum(is.na(data[[col_name]]))),
              side = 1, line = 2.5, cex = 0.7)
      }
    }
  }

  dev.off()
  message(sprintf("数据检测报告已生成: %s", file_name))
}
