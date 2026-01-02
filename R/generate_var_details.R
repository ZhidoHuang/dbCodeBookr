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
#' }
#' @export
generate_var_details <- function(data, bar_color = "#AE4D4D") {
  stopifnot(is.list(data) || is.data.frame(data))

  sapply(seq_along(data), function(i) {
    var <- data[[i]]

    # ========= 类别型变量 =========
    if (is.character(var) || is.factor(var) ||
        length(unique(var[!is.na(var)])) <= 15) {

      value_counts <- table(var)
      value_counts <- value_counts[value_counts > 0]

      if (length(value_counts) == 0) return("No data")

      total <- sum(value_counts)
      percentages <- round(value_counts / total * 100, 1)

      ord <- order(percentages, decreasing = TRUE)
      sorted_counts <- value_counts[ord]
      sorted_percentages <- percentages[ord]

      table_style <- sprintf("
      <style>
        .fixed-table {
          border-collapse: collapse;
          width: auto;
          font-family: Arial, sans-serif;
        }
        .fixed-table td {
          padding: 2px 8px;
          white-space: nowrap;
        }
        .fixed-table td:nth-child(1),
        .fixed-table td:nth-child(2) {
          width: 60px;
          text-align: right;
        }
        .fixed-table td:nth-child(3) {
          width: 100px;
        }
        .fixed-table td:nth-child(4) {
          text-align: left;
          max-width: 200px;
          overflow: hidden;
          text-overflow: ellipsis;
        }
        .bar-container {
          width: 100px;
          height: 15px;
          background-color: #f0f0f0;
          border-radius: 3px;
        }
        .bar {
          height: 100%%;
          background-color: %s;
          border-radius: 3px;
        }
      </style>
      ", bar_color)

      make_rows <- function(cnt, pct) {
        paste0(
          sapply(seq_along(cnt), function(j) {
            paste0(
              "<tr>",
              "<td>", as.numeric(cnt[j]), "</td>",
              "<td>", pct[j], "%</td>",
              "<td><div class='bar-container'><div class='bar' style='width:",
              pct[j], "%'></div></div></td>",
              "<td>", names(cnt)[j], "</td>",
              "</tr>"
            )
          }),
          collapse = ""
        )
      }

      if (length(sorted_counts) > 15) {
        cnt <- head(sorted_counts, 15)
        pct <- head(sorted_percentages, 15)
        table_content <- paste0(
          table_style,
          "<table class='fixed-table'>",
          make_rows(cnt, pct),
          "</table>"
        )
        return(paste("Top 15 类别分布:<br>", table_content))
      } else {
        table_content <- paste0(
          table_style,
          "<table class='fixed-table'>",
          make_rows(sorted_counts, sorted_percentages),
          "</table>"
        )
        return(paste("类别分布:<br>", table_content))
      }

      # ========= 数值型变量 =========
    } else if (is.numeric(var)) {

      uniq <- unique(var[!is.na(var)])

      if (length(uniq) <= 10) {
        value_counts <- table(var[!is.na(var)])
        if (length(value_counts) == 0) {
          return("            No data")
        }
        sorted_counts <- sort(value_counts, decreasing = TRUE)
        return(paste(
          "            ",
          paste(names(sorted_counts), "(", as.numeric(sorted_counts), ")", collapse = ", ")
        ))
      } else {
        return(paste0(
          "范围:  <br>",
          round(mean(var, na.rm = TRUE), 2),
          " (", round(min(var, na.rm = TRUE), 2),
          " ~ ", round(max(var, na.rm = TRUE), 2), ")"
        ))
      }

    } else {
      return("        Unsupported type")
    }
  })
}
