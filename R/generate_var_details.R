#' @title 生成变量分布详情HTML
#' @description 为数据集中的每个变量生成分布详情HTML，包括分类变量的频数百分比分布表和数值变量的统计摘要
#' @param data 数据框或列表，包含要分析的变量
#' @param bar_color 条形图的颜色，默认为"#AE4D4D"（深红色）
#' @return 字符向量，每个元素对应一个变量的HTML描述
#' @examples
#' \dontrun{
#' # 创建示例数据
#' data <- data.frame(
#'   category = sample(c("A", "B", "C", "D"), 100, replace = TRUE),
#'   score = rnorm(100, mean = 50, sd = 10),
#'   binary = sample(0:1, 100, replace = TRUE)
#' )
#'
#' # 生成分布详情
#' details <- generate_var_details(data, bar_color = "#4DA6FF")
#' print(details[1])
#' }
#' @export
generate_var_details <- function(data, bar_color = "#AE4D4D") {

  if (!is.data.frame(data) && !is.list(data)) {
    stop("'data' must be data frame")
  }

  sapply(seq_along(data), function(i) {
    var <- data[[i]]
    var_name <- if (is.data.frame(data)) names(data)[i] else paste("Var", i)

    if (is.character(var) || is.factor(var) || length(unique(var[!is.na(var)])) <= 15) {
      # 包含NA值的统计
      value_counts <- table(var)
      value_counts <- value_counts[value_counts > 0]  # 移除计数为0的类别

      if (length(value_counts) == 0) {
        return("No data")
      }

      # 计算百分比
      total <- sum(value_counts)
      percentages <- round(value_counts / total * 100, 1)

      # 按百分比从高到低排序
      sorted_indices <- order(percentages, decreasing = TRUE)
      sorted_counts <- value_counts[sorted_indices]
      sorted_percentages <- percentages[sorted_indices]

      # 动态生成CSS样式，包含条形颜色参数
      # 注意：使用 %s 替代 %d 来避免格式错误
      table_style <- sprintf("
    <style>
      .fixed-table-%s {
        border-collapse: collapse;
        width: auto;
        font-family: Arial, sans-serif;
        margin: 5px 0;
      }
      .fixed-table-%s td {
        padding: 2px 8px;
        white-space: nowrap;
        border: 1px solid #ddd;
      }
      .fixed-table-%s td:nth-child(1) {
        width: 60px;
        text-align: right;
      }
      .fixed-table-%s td:nth-child(2) {
        width: 60px;
        text-align: right;
      }
      .fixed-table-%s td:nth-child(3) {
        width: 100px;
      }
      .fixed-table-%s td:nth-child(4) {
        text-align: left;
        max-width: 200px;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      .bar-container-%s {
        width: 100px;
        height: 15px;
        background-color: #f0f0f0;
        border-radius: 3px;
        overflow: hidden;
      }
      .bar-%s {
        height: 100%%;
        background-color: %s;
        border-radius: 3px;
      }
    </style>",
                             i, i, i, i, i, i, i, bar_color)

      if (length(sorted_counts) > 15) {
        # 取前15个（已经按比例排序）
        display_counts <- head(sorted_counts, 15)
        display_percentages <- head(sorted_percentages, 15)

        table_content <- paste0(
          table_style,
          sprintf("<table class='fixed-table-%s'>", i),
          paste(
            sapply(1:length(display_counts), function(j) {
              paste0(
                "<tr>",
                "<td>", as.numeric(display_counts[j]), "</td>",
                "<td>", display_percentages[j], "%</td>",
                sprintf("<td><div class='bar-container-%s'><div class='bar-%s' style='width:", i, i),
                display_percentages[j], "%%'></div></div></td>",
                "<td>", names(display_counts)[j], "</td>",
                "</tr>"
              )
            }),
            collapse = ""
          ),
          "</table>"
        )
        return(paste("Top 15 类别分布:<br>", table_content))
      } else {
        # 显示所有类别（已经按比例排序）
        table_content <- paste0(
          table_style,
          sprintf("<table class='fixed-table-%s'>", i),
          paste(
            sapply(1:length(sorted_counts), function(j) {
              paste0(
                "<tr>",
                "<td>", as.numeric(sorted_counts[j]), "</td>",
                "<td>", sorted_percentages[j], "%</td>",
                sprintf("<td><div class='bar-container-%s'><div class='bar-%s' style='width:", i, i),
                sorted_percentages[j], "%%'></div></div></td>",
                "<td>", names(sorted_counts)[j], "</td>",
                "</tr>"
              )
            }),
            collapse = ""
          ),
          "</table>"
        )
        return(paste("类别分布:<br>", table_content))
      }
    } else if (is.numeric(var)) {
      # 检查唯一值的数量
      unique_values <- unique(var[!is.na(var)])
      if (length(unique_values) <= 10) {
        # 当唯一值 <= 10，按分类变量处理
        value_counts <- table(var[!is.na(var)])
        if (length(value_counts) > 0) {
          # 按计数从高到低排序
          sorted_counts <- sort(value_counts, decreasing = TRUE)
          return(paste("            ", paste(names(sorted_counts), "(", as.numeric(sorted_counts), ")", collapse = ", ")))
        } else {
          return("            No data")
        }
      } else {
        # 当唯一值 > 10，显示范围信息
        if (sum(!is.na(var)) > 0) {
          return(sprintf(
            "范围:  <br>%.2f (%.2f ~ %.2f)",
            round(mean(var, na.rm = TRUE), 2),
            round(min(var, na.rm = TRUE), 2),
            round(max(var, na.rm = TRUE), 2)
          ))
        } else {
          return("No data")
        }
      }
    } else {
      return(paste("        Unsupported type"))
    }
  })
}

