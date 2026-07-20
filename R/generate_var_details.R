#' @title 生成变量分布详情HTML
#' @description 为数据集中的每个变量生成分布详情HTML，包括分类变量的频数百分比分布表和数值变量的统计摘要
#' @details
#' 当`show_cycle_heatmap = TRUE`时，函数会在每个变量详情末尾追加一个原生HTML周期热图。
#' 热图按`cycle_order`显示该变量在各周期的非缺失数；无数据单元格留空。
#' 同一次调用中的全部变量共用统一色阶上限。热图表格使用`width: 100%`和
#' `table-layout: fixed`，最小总宽度按`length(cycle_order) * 42px`计算；
#' 周期较多时隐藏原生滚动条，并在热图左右边缘按需显示半透明渐变箭头；
#' 每次点击平滑移动约一个可视宽度并保留一格重叠，触屏设备仍可直接横向滑动。
#' 当`show_distribution_nav = TRUE`时，分类变量的完整类别分布表或连续变量的完整直方图
#' 会各自获得独立的横向导航；它们不与周期热图共享滚动状态。
#' @param data 数据框或列表，包含要分析的变量
#' @param bar_color 条形图的颜色，默认为"#AE4D4D"（深红色）
#' @param show_hist 逻辑值，是否显示连续变量的直方图，默认为FALSE
#' @param hist_binwidth 直方图组距，默认为NULL（自动）
#' @param show_distribution_nav 逻辑值，是否为完整类别分布表或连续变量直方图启用独立横向导航，默认为FALSE
#' @param show_cycle_heatmap 逻辑值，是否在每个变量详情末尾追加周期非缺失数热图，默认为FALSE
#' @param cycle_col 字符串，周期列名；仅在`show_cycle_heatmap = TRUE`时使用
#' @param cycle_order 字符向量，周期显示顺序；仅在`show_cycle_heatmap = TRUE`时使用
#' @param heatmap_color 周期热图主题色，默认为`bar_color`
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
#'
#' # 在每个变量详情末尾追加周期非缺失数热图
#' data$year <- sample(c("2011", "2013", "2015"), 100, replace = TRUE)
#' details_with_cycle <- generate_var_details(
#'   data,
#'   show_cycle_heatmap = TRUE,
#'   cycle_col = "year",
#'   cycle_order = c("2011", "2013", "2015"),
#'   heatmap_color = "#2C7FB8"
#' )
#' }
#' @export
generate_var_details <- function(data, bar_color = "#AE4D4D", show_hist = FALSE, hist_binwidth = NULL,
                                 show_distribution_nav = FALSE,
                                 show_cycle_heatmap = FALSE, cycle_col = NULL,
                                 cycle_order = NULL, heatmap_color = bar_color) {
  stopifnot(is.list(data) || is.data.frame(data))
  stopifnot(is.logical(show_hist))
  stopifnot(is.logical(show_distribution_nav))
  stopifnot(is.logical(show_cycle_heatmap))

  build_cycle_heatmap <- function() {
    if (!isTRUE(show_cycle_heatmap)) return(NULL)

    if (!is.character(cycle_col) || length(cycle_col) != 1 || is.na(cycle_col) || !nzchar(cycle_col)) {
      stop("show_cycle_heatmap = TRUE 时，cycle_col 必须是一个非空列名。", call. = FALSE)
    }
    if (!cycle_col %in% names(data)) {
      stop("周期列不存在：", cycle_col, call. = FALSE)
    }
    if (is.null(cycle_order) || length(cycle_order) == 0) {
      stop("show_cycle_heatmap = TRUE 时，cycle_order 不能为空。", call. = FALSE)
    }

    cycle_order_chr <- as.character(cycle_order)
    cycle_order_chr <- cycle_order_chr[!is.na(cycle_order_chr) & nzchar(cycle_order_chr)]
    if (length(cycle_order_chr) == 0) {
      stop("cycle_order 不能为空，且不能只包含 NA 或空字符串。", call. = FALSE)
    }

    cycle_values <- as.character(data[[cycle_col]])
    if (!any(cycle_values %in% cycle_order_chr, na.rm = TRUE)) {
      stop("cycle_order 与数据中的周期值无法对应。", call. = FALSE)
    }

    tryCatch(
      grDevices::col2rgb(heatmap_color),
      error = function(e) stop("heatmap_color 不是有效颜色：", heatmap_color, call. = FALSE)
    )

    count_matrix <- vapply(data, function(x) {
      vapply(cycle_order_chr, function(cy) {
        idx <- !is.na(cycle_values) & cycle_values == cy
        sum(!is.na(x[idx]))
      }, numeric(1))
    }, numeric(length(cycle_order_chr)))
    count_matrix <- t(count_matrix)
    colnames(count_matrix) <- cycle_order_chr

    global_max <- suppressWarnings(max(count_matrix, na.rm = TRUE))
    if (!is.finite(global_max) || global_max <= 0) global_max <- 0

    lighten_color <- function(color, amount = 0.25) {
      rgb <- grDevices::col2rgb(color)
      lighter <- rgb + (255 - rgb) * amount
      grDevices::rgb(lighter[1, ], lighter[2, ], lighter[3, ], maxColorValue = 255)
    }

    endpoint_color <- lighten_color(heatmap_color, amount = 0.25)
    ramp <- grDevices::colorRamp(c("#f7f6f4", endpoint_color))
    cell_color <- function(n) {
      if (is.na(n) || n <= 0 || global_max <= 0) return("#f7f6f4")
      rgb_vec <- ramp(n / global_max)
      sprintf("rgb(%d,%d,%d)", round(rgb_vec[1]), round(rgb_vec[2]), round(rgb_vec[3]))
    }
    text_color <- function(bg) {
      if (grepl("^rgb\\(", bg)) {
        rgb_values <- as.numeric(strsplit(gsub("^rgb\\(|\\)$", "", bg), ",")[[1]])
      } else {
        rgb_values <- as.numeric(grDevices::col2rgb(bg))
      }
      luminance <- (0.299 * rgb_values[1] + 0.587 * rgb_values[2] + 0.114 * rgb_values[3]) / 255
      if (luminance < 0.48) "#ffffff" else "#1f2933"
    }
    esc <- function(x) {
      x <- as.character(x)
      x <- gsub("&", "&amp;", x, fixed = TRUE)
      x <- gsub("<", "&lt;", x, fixed = TRUE)
      x <- gsub(">", "&gt;", x, fixed = TRUE)
      x <- gsub('"', "&quot;", x, fixed = TRUE)
      x <- gsub("'", "&#39;", x, fixed = TRUE)
      x
    }

    table_min_width <- max(1L, length(cycle_order_chr)) * 42L

    css <- "
<style>
.dbcb-var-cycle-nav{position:relative;max-width:100%;}
.dbcb-var-cycle-wrap{width:100%;min-width:0;max-width:100%;overflow-x:auto;scroll-behavior:smooth;scrollbar-width:none;-ms-overflow-style:none;overscroll-behavior-x:contain;}
.dbcb-var-cycle-wrap::-webkit-scrollbar{display:none;}
.dbcb-var-cycle-arrow{display:none;position:absolute;top:0;bottom:0;z-index:2;width:34px;padding:0;border:0;border-radius:0;color:#4b5563;font-family:Arial,sans-serif;font-size:20px;line-height:1;align-items:center;justify-content:center;cursor:pointer;text-shadow:0 1px 2px rgba(255,255,255,.9);}
.dbcb-var-cycle-arrow.is-visible{display:inline-flex;}
.dbcb-var-cycle-prev{left:0;background:linear-gradient(90deg,rgba(255,255,255,.94) 0%,rgba(255,255,255,.68) 48%,rgba(255,255,255,0) 100%);}
.dbcb-var-cycle-next{right:0;background:linear-gradient(270deg,rgba(255,255,255,.94) 0%,rgba(255,255,255,.68) 48%,rgba(255,255,255,0) 100%);}
.dbcb-var-cycle-block{margin-top:8px;max-width:100%;}
.dbcb-var-cycle-title{margin:0;color:#7f8c8d;font-family:inherit;font-size:10px;line-height:1.55;font-weight:400;}
.dbcb-var-cycle-table{border-collapse:collapse;width:100%;table-layout:fixed;font-family:Arial,sans-serif;}
.dbcb-var-cycle-wrap .dbcb-var-cycle-table td.dbcb-var-cycle-cell{min-width:0;padding:2px 2px;border:1px solid #DDDDDD !important;text-align:center;white-space:nowrap;font-family:Arial,sans-serif;font-size:11px;line-height:1.1;font-weight:400;}
.dbcb-var-cycle-wrap .dbcb-var-cycle-table td.dbcb-var-cycle-cell .dbcb-var-cycle-label{display:block;font-family:Arial,sans-serif;font-size:9px;line-height:1.1;font-weight:400;color:#374151;}
.dbcb-var-cycle-wrap .dbcb-var-cycle-table td.dbcb-var-cycle-cell .dbcb-var-cycle-count{display:block;font-family:Arial,sans-serif;font-size:12px;line-height:1.15;font-weight:600;min-height:16px;}
</style>
<script>
(function(){
  if(window.dbcbCycleNavInitialized)return;
  window.dbcbCycleNavInitialized=true;
  function update(wrap){
    var nav=wrap.closest('.dbcb-var-cycle-nav');
    if(!nav)return;
    var table=wrap.querySelector('.dbcb-var-cycle-table');
    var overflow=!!table&&table.scrollWidth>wrap.clientWidth+1;
    var max=Math.max(0,wrap.scrollWidth-wrap.clientWidth);
    var prev=nav.querySelector('.dbcb-var-cycle-prev');
    var next=nav.querySelector('.dbcb-var-cycle-next');
    var canPrev=overflow&&wrap.scrollLeft>1;
    var canNext=overflow&&wrap.scrollLeft<max-1;
    if(prev){
      prev.disabled=!canPrev;
      prev.classList.toggle('is-visible',canPrev);
      prev.setAttribute('aria-hidden',canPrev?'false':'true');
    }
    if(next){
      next.disabled=!canNext;
      next.classList.toggle('is-visible',canNext);
      next.setAttribute('aria-hidden',canNext?'false':'true');
    }
  }
  function init(){
    document.querySelectorAll('.dbcb-var-cycle-wrap').forEach(function(wrap){
      if(!wrap.dataset.dbcbCycleBound){
        wrap.addEventListener('scroll',function(){update(wrap);},{passive:true});
        wrap.dataset.dbcbCycleBound='true';
      }
      update(wrap);
    });
  }
  document.addEventListener('click',function(event){
    var button=event.target.closest('.dbcb-var-cycle-arrow');
    if(!button||button.disabled)return;
    var nav=button.closest('.dbcb-var-cycle-nav');
    var wrap=nav&&nav.querySelector('.dbcb-var-cycle-wrap');
    if(!wrap)return;
    var cell=wrap.querySelector('.dbcb-var-cycle-cell');
    var cellWidth=cell?cell.getBoundingClientRect().width:42;
    var distance=Math.max(cellWidth,wrap.clientWidth-cellWidth);
    var direction=button.classList.contains('dbcb-var-cycle-prev')?-1:1;
    wrap.scrollBy({left:direction*distance,behavior:'smooth'});
    window.setTimeout(function(){update(wrap);},350);
  });
  window.addEventListener('resize',init);
  if(document.readyState==='loading'){
    document.addEventListener('DOMContentLoaded',init,{once:true});
  }else{
    window.requestAnimationFrame(init);
  }
})();
</script>"

    lapply(seq_len(nrow(count_matrix)), function(i) {
      counts <- count_matrix[i, ]
      cells <- vapply(seq_along(cycle_order_chr), function(j) {
        n <- counts[j]
        label <- esc(cycle_order_chr[j])
        display <- if (is.na(n) || n <= 0) "" else format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
        bg_color <- cell_color(n)
        sprintf(
          "<td class='dbcb-var-cycle-cell' style='background-color:%s;' title='%s: %s'><span class='dbcb-var-cycle-label'>%s</span><span class='dbcb-var-cycle-count' style='color:%s;'>%s</span></td>",
          bg_color,
          label,
          if (nzchar(display)) display else "无数据",
          label,
          text_color(bg_color),
          display
        )
      }, character(1))
      paste0(
        css,
        "<div class='dbcb-var-cycle-block'><div class='dbcb-var-cycle-title'>周期分布:</div><div class='dbcb-var-cycle-nav'><button type='button' class='dbcb-var-cycle-arrow dbcb-var-cycle-prev' aria-label='向左查看周期' aria-hidden='true' disabled>&lsaquo;</button><div class='dbcb-var-cycle-wrap'><table class='dbcb-var-cycle-table' style='min-width:max(100%, ",
        table_min_width,
        "px);'><tr>",
        paste(cells, collapse = ""),
        "</tr></table></div><button type='button' class='dbcb-var-cycle-arrow dbcb-var-cycle-next' aria-label='向右查看周期' aria-hidden='true' disabled>&rsaquo;</button></div></div>"
      )
    })
  }

  cycle_heatmaps <- build_cycle_heatmap()
  append_cycle_heatmap <- function(detail_html, var_index) {
    if (!isTRUE(show_cycle_heatmap)) return(detail_html)
    paste0(detail_html, cycle_heatmaps[[var_index]])
  }

  distribution_nav_assets <- if (isTRUE(show_distribution_nav)) {
    "
<style>
.dbcb-var-distribution-nav{position:relative;width:100%;max-width:100%;}
.dbcb-var-distribution-wrap{width:100%;min-width:0;max-width:100%;overflow-x:auto;scroll-behavior:smooth;scrollbar-width:none;-ms-overflow-style:none;overscroll-behavior-x:contain;}
.dbcb-var-distribution-wrap::-webkit-scrollbar{display:none;}
.dbcb-var-distribution-arrow{display:none;position:absolute;top:0;bottom:0;z-index:2;width:34px;padding:0;border:0;border-radius:0;color:#4b5563;font-family:Arial,sans-serif;font-size:20px;line-height:1;align-items:center;justify-content:center;cursor:pointer;text-shadow:0 1px 2px rgba(255,255,255,.9);}
.dbcb-var-distribution-arrow.is-visible{display:inline-flex;}
.dbcb-var-distribution-prev{left:0;background:linear-gradient(90deg,rgba(255,255,255,.94) 0%,rgba(255,255,255,.68) 48%,rgba(255,255,255,0) 100%);}
.dbcb-var-distribution-next{right:0;background:linear-gradient(270deg,rgba(255,255,255,.94) 0%,rgba(255,255,255,.68) 48%,rgba(255,255,255,0) 100%);}
</style>
<script>
(function(){
  if(window.dbcbDistributionNavInitialized)return;
  window.dbcbDistributionNavInitialized=true;
  function update(wrap){
    var nav=wrap.closest('.dbcb-var-distribution-nav');
    if(!nav)return;
    var overflow=wrap.scrollWidth>wrap.clientWidth+1;
    var max=Math.max(0,wrap.scrollWidth-wrap.clientWidth);
    var prev=nav.querySelector('.dbcb-var-distribution-prev');
    var next=nav.querySelector('.dbcb-var-distribution-next');
    var canPrev=overflow&&wrap.scrollLeft>1;
    var canNext=overflow&&wrap.scrollLeft<max-1;
    if(prev){
      prev.disabled=!canPrev;
      prev.classList.toggle('is-visible',canPrev);
      prev.setAttribute('aria-hidden',canPrev?'false':'true');
    }
    if(next){
      next.disabled=!canNext;
      next.classList.toggle('is-visible',canNext);
      next.setAttribute('aria-hidden',canNext?'false':'true');
    }
  }
  function init(){
    document.querySelectorAll('.dbcb-var-distribution-wrap').forEach(function(wrap){
      if(!wrap.dataset.dbcbDistributionBound){
        wrap.addEventListener('scroll',function(){update(wrap);},{passive:true});
        wrap.dataset.dbcbDistributionBound='true';
      }
      update(wrap);
    });
  }
  document.addEventListener('click',function(event){
    var button=event.target.closest('.dbcb-var-distribution-arrow');
    if(!button||button.disabled)return;
    var nav=button.closest('.dbcb-var-distribution-nav');
    var wrap=nav&&nav.querySelector('.dbcb-var-distribution-wrap');
    if(!wrap)return;
    var selector=wrap.getAttribute('data-scroll-item');
    var item=selector?wrap.querySelector(selector):null;
    var overlap=item?item.getBoundingClientRect().width:60;
    var distance=Math.max(overlap,wrap.clientWidth-overlap);
    var direction=button.classList.contains('dbcb-var-distribution-prev')?-1:1;
    wrap.scrollBy({left:direction*distance,behavior:'smooth'});
    window.setTimeout(function(){update(wrap);},350);
  });
  window.addEventListener('resize',init);
  if(document.readyState==='loading'){
    document.addEventListener('DOMContentLoaded',init,{once:true});
  }else{
    window.requestAnimationFrame(init);
  }
})();
</script>"
  } else {
    ""
  }

  wrap_distribution_nav <- function(content, kind, label, item_selector, extra_class = "") {
    if (!isTRUE(show_distribution_nav)) return(content)
    wrap_class <- trimws(paste("dbcb-var-distribution-wrap", extra_class))
    paste0(
      distribution_nav_assets,
      "<div class='dbcb-var-distribution-nav' data-distribution-kind='", kind, "'>",
      "<button type='button' class='dbcb-var-distribution-arrow dbcb-var-distribution-prev' aria-label='向左查看",
      label,
      "' aria-hidden='true' disabled>&lsaquo;</button>",
      "<div class='", wrap_class, "' data-scroll-item='", item_selector, "'>",
      content,
      "</div>",
      "<button type='button' class='dbcb-var-distribution-arrow dbcb-var-distribution-next' aria-label='向右查看",
      label,
      "' aria-hidden='true' disabled>&rsaquo;</button>",
      "</div>"
    )
  }

  # 直方图样式
  hist_style <- sprintf("
  <style>
    .hist-bar {
      display: inline-block;
      flex-shrink: 0;
      background-color: %s;
    }
    .hist-bar:hover {
      opacity: 0.8;
      cursor: pointer;
    }
    .hist-label {
      width: 40px;
      margin-right: 2px;
      text-align: center;
      font-size: 11px;
      flex-shrink: 0;
    }
    .hist-container {
      margin-top: 15px;
      max-width: 100%%
      overflow-x: auto;
    }
    .hist-inner {
      display: flex;
      flex-direction: column;
      align-items: center;
      min-width: fit-content;
    }
    .hist-bars-wrapper {
      height: 120px;
      display: flex;
      align-items: flex-end;
      border-left: 1px solid #ccc;
      border-bottom: 1px solid #ccc;
    }
    .hist-labels-wrapper {
      display: flex;
      margin-top: 8px;
    }
  </style>
  ", bar_color)

  sapply(seq_along(data), function(i) {
    var <- data[[i]]

    # ========= 类别型变量 =========
    if (is.character(var) || is.factor(var) ||
        length(unique(var[!is.na(var)])) <= 15) {

      value_counts <- table(var)
      value_counts <- value_counts[value_counts > 0]

      if (length(value_counts) == 0) return(append_cycle_heatmap("No data", i))

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
          vapply(seq_along(cnt), function(j) {
            paste0(
              "<tr>",
              "<td>", as.numeric(cnt[j]), "</td>",
              "<td>", pct[j], "%</td>",
              "<td><div class='bar-container'><div class='bar' style='width:",
              pct[j], "%'></div></div></td>",
              "<td>", names(cnt)[j], "</td>",
              "</tr>"
            )
          }, character(1)),
          collapse = ""
        )
      }

      if (length(sorted_counts) > 15) {
        cnt <- head(sorted_counts, 15)
        pct <- head(sorted_percentages, 15)
        category_title <- "Top 15 类别分布:"
      } else {
        cnt <- sorted_counts
        pct <- sorted_percentages
        category_title <- "类别分布:"
      }

      table_content <- paste0(
        table_style,
        wrap_distribution_nav(
          paste0(
            "<table class='fixed-table'>",
            make_rows(cnt, pct),
            "</table>"
          ),
          "category",
          "类别分布表",
          "td"
        )
      )

      return(append_cycle_heatmap(paste0(category_title, "<br> ", table_content), i))

      # ========= 数值型变量 =========
    } else if (is.numeric(var)) {

      uniq <- unique(var[!is.na(var)])

      if (length(uniq) <= 10) {
        value_counts <- table(var[!is.na(var)])
        if (length(value_counts) == 0) {
          return(append_cycle_heatmap("            No data", i))
        }
        sorted_counts <- sort(value_counts, decreasing = TRUE)
        return(append_cycle_heatmap(paste(
          "            ",
          paste(names(sorted_counts), "(", as.numeric(sorted_counts), ")", collapse = ", ")
        ), i))
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
          bar_width <- 30
          bar_margin <- 3

          # 生成柱子（使用CSS类）
          hist_bars <- sapply(seq_along(counts), function(j) {
            height_pct <- if (max_count > 0) counts[j] / max_count * 100 else 0
            if (height_pct == 0 && counts[j] > 0) height_pct <- 5
            sprintf(
              "<div class='hist-bar' style='height: %.1f%%; width: %dpx; margin-right: %dpx;' title='[%.1f, %.1f): %d个'></div>",
              height_pct, bar_width, bar_margin, breaks[j], breaks[j+1], counts[j]
            )
          })

          # 生成横坐标标签（使用CSS类）
          x_labels <- sapply(seq_along(counts), function(j) {
            sprintf(
              "<div class='hist-label' style='width: %dpx; margin-right: %dpx;'>%.1f</div>",
              bar_width, bar_margin, (breaks[j] + breaks[j+1]) / 2
            )
          })

          hist_body <- paste0(
            "<div class='hist-inner'>",
            "<div class='hist-bars-wrapper'>",
            paste(hist_bars, collapse = ""),
            "</div>",
            "<div class='hist-labels-wrapper'>",
            paste(x_labels, collapse = ""),
            "</div>",
            "</div>"
          )
          hist_html <- paste0(
            hist_style,
            if (isTRUE(show_distribution_nav)) {
              wrap_distribution_nav(
                hist_body,
                "histogram",
                "连续变量柱状图",
                ".hist-bar",
                "hist-container"
              )
            } else {
              paste0("<div class='hist-container'>", hist_body, "</div>")
            }
          )

          return(append_cycle_heatmap(paste(stats_text, hist_html, sep = "<br>"), i))
        } else {
          return(append_cycle_heatmap(stats_text, i))
        }
      }

    } else {
      return(append_cycle_heatmap("        Unsupported type", i))
    }
  })
}
