#' @title 生成变量分布详情HTML
#' @description 为数据集中的每个变量生成分布详情HTML，包括分类变量的频数百分比分布表和数值变量的统计摘要
#' @param data 数据框或列表，包含要分析的变量
#' @param bar_color 条形图的颜色，默认为"#AE4D4D"（深红色）
#' @param show_hist 逻辑值，是否显示连续变量的直方图，默认为FALSE
#' @param hist_binwidth 直方图组距，默认为NULL（自动）
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
generate_var_details <- function(data, bar_color = "#AE4D4D", show_hist = FALSE, hist_binwidth = NULL) {
  stopifnot(is.list(data) || is.data.frame(data))
  stopifnot(is.logical(show_hist))

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
        n <- length(na.omit(var))
        mean_val <- round(mean(var, na.rm = TRUE), 2)
        min_val <- round(min(var, na.rm = TRUE), 2)
        max_val <- round(max(var, na.rm = TRUE), 2)

        stats_text <- sprintf("范围: %s (%s ~ %s) ; &emsp;&emsp; 样本量: %d", mean_val, min_val, max_val, n)

        if (show_hist) {
          # 生成断点
          if (!is.null(hist_binwidth) && is.numeric(hist_binwidth) && hist_binwidth > 0) {
            n_breaks <- max(5, min(50, ceiling((max_val - min_val) / hist_binwidth)))
            breaks <- pretty(range(var, na.rm = TRUE), n = n_breaks)
          } else {
            breaks <- pretty(range(var, na.rm = TRUE), n = 20)
          }

          hist_data <- hist(var, plot = FALSE, breaks = breaks)
          counts <- hist_data$counts
          max_count <- max(counts)
          breaks <- hist_data$breaks

          # 每个柱子宽度（像素）
          bar_width <- 40
          bar_margin <- 2

          # 生成柱子
          hist_bars <- sapply(seq_along(counts), function(j) {
            height_pct <- if (max_count > 0) counts[j] / max_count * 100 else 0
            if (height_pct == 0 && counts[j] > 0) height_pct <- 5
            sprintf("<div style='height: %.1f%%; width: %dpx; background-color: %s; display: inline-block; margin-right: %dpx; flex-shrink: 0;' title='[%.1f, %.1f): %d个'> </div>",
                    height_pct, bar_width, bar_color, bar_margin, breaks[j], breaks[j+1], counts[j])
          })

          # 生成横坐标标签
          x_labels <- sapply(seq_along(counts), function(j) {
            sprintf("<div style='width: %dpx; margin-right: %dpx; text-align: center; font-size: 11px; flex-shrink: 0;'>%.1f</div>",
                    bar_width, bar_margin, (breaks[j] + breaks[j+1]) / 2)
          })

          hist_html <- paste0(
            "<div style='margin-top: 15px; max-width: 100%; overflow-x: auto;'>",
            "<div style='display: flex; flex-direction: column; align-items: center; min-width: fit-content;'>",
            "<div style='height: 120px; display: flex; align-items: flex-end; border-left: 1px solid #ccc; border-bottom: 1px solid #ccc;'>",
            paste(hist_bars, collapse = ""),
            "</div>",
            "<div style='display: flex; margin-top: 8px;'>",
            paste(x_labels, collapse = ""),
            "</div>",
            "</div>",
            "</div>"
          )

          return(paste(stats_text, hist_html, sep = "<br>"))
        } else {
          return(stats_text)
        }
      }

    } else {
      return("        Unsupported type")
    }
  })
}
