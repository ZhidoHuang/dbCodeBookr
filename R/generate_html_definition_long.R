#' @title CodeBook HTML 组件
#' @encoding UTF-8
#' @description
#' **`generate_html_definition_long()`** 将两个已格式化的数据框组合为一段**自包含的
#' HTML 片段**，包括
#' 1. **定义的组分概览**：按 *category* 分组的频数热力图
#' 2. **定义的组分详情**：可折叠的 *variable-map* 卡片（点击展开标签 / 取值说明）
#'    - 变量名、原始变量映射、简要描述、详细注释
#'
#' 该片段可以直接插入 R Markdown / Shiny / Quarto，或写入文件并在浏览器中
#' 打开。所有样式（CSS）、交互逻辑（JS）均已内嵌，无外部依赖。
#'
#' @details
#' ### 输入数据格式
#' * **`heat_df`**
#'   | 必含列 | 说明 |
#'   |---------|---------------------------------------------------------------|
#'   | `category` | 一级分类（将生成 *details* 折叠块） |
#'   | `Variable` | 变量名（将生成锚点 `#var-<Variable>`） |
#'   | 年份列    | **以&nbsp;4 位数字**或 **`Year` / `Wave`** 开头的列（如`2019`,  `Year2019`, `Wave_1`） (= 年度频数 / 统计量) |
#'   | 其他列    | 视为 *辅助信息*，会显示在热力图左侧 |
#'
#' * **`meta_df`**
#'   | 必含列 | 说明 |
#'   |---------|------------------------------------------------------------|
#'   | `Variable` | 必须与 `heat_df$Variable` 一一匹配 |
#'   | `original_vars` | 原始变量名称（可用逗号分隔多个） |
#'   | `easylabel`  | 一句话摘要 |
#'   | `detail`   | **已编码/转义好的** HTML 字符串，作为展开内容 |
#'   其余列将被忽略。
#'
#' ### 颜色与布局
#' * 渐变只按 **年份列** 取值全局计算；`0` 或 `NA` 单元格留白。
#' * 奇数行：白 → `#264653`；偶数行：白 → 深红 (`#8b0000`)。
#' * 表头和分类标题使用深色圆角条以保持视觉一致。
#'
#' ### 交互
#' * 热力图中的变量名、分类名自动生成锚点，可与变量卡片互相跳转。
#' * 点击 **卡片本身** 切换显示 / 隐藏标签内容（无需箭头按钮）。
#'
#' ### 注意事项
#' 1. **字符编码**：函数按 *UTF-8* 写文件；Windows 用户请使用 UTF-8 R 会话或后续
#'    用 `fileEncoding = 'UTF-8'` 读取。
#' 2. **`detail` 列务必自行 `htmlEscape()` 或手动写安全 HTML**——函数不会再次
#'    转义它（便于插入 `<ul><li>` 等复杂格式）。
#' 3. 如果某分类下无行数据，`split()` 会生成空块；当前实现会自动跳过，不会报错。
#' 4. 本函数仅输出 **片段**，若需完整网页请自行包裹
#'    `<!DOCTYPE html><html><head>…</head><body> … </body></html>`。
#' 5. 依赖包：`htmltools`；其余均为基准 R。
#'
#' @param heat_df `data.frame` – 见 **输入数据格式**
#' @param meta_df `data.frame` – 见 **输入数据格式**
#' @param file    `character(1)` | `NULL`
#'                - 目标 HTML 路径；`NULL` 时仅返回字符串（默认）
#'
#' @return 字符串形式的 HTML 代码；若 `file` 非空，还会写入该文件。
#'         返回值 **invisibly** 便于管道操作。
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # 构造示例数据 ----
#' heat_df <- data.frame(
#'   category = rep(c("Demographics", "Labs"), each = 2),
#'   Variable = c("AGE", "SEX", "HB", "GLU"),
#'   `2017`   = c(10102, 10109, 9980, 9870),
#'   `2019`   = c( 9875,  9901, 9720, 9600),
#'   units    = c("years", "", "g/dL", "mg/dL"),
#'   stringsAsFactors = FALSE
#' )
#'
#' meta_df <- data.frame(
#'   Variable = c("AGE", "SEX", "HB", "GLU"),
#'   original_vars = c("RIDAGEYR", "RIAGENDR", "LBXHGB", "LBXGLU"),
#'   easylabel  = c("Age at exam", "Sex", "Hemoglobin", "Fasting glucose"),
#'   detail   = c(
#'     "Continuous variable (0–80).",
#'     "1 = Male, 2 = Female.",
#'     "<p>Measured via Coulter&nbsp;LH 700.</p>",
#'     "<em>mmol/L</em> converted to mg/dL ×18."
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 生成并写入
#' generate_html_definition_long(heat_df, meta_df, file = "codebook_demo.html")
#' }
generate_html_definition_long <- function(heat_df, meta_df,file = NULL, color1 = "#2C3E50",color2= "#8b0000") {

  ## ---- helpers -----------------------------------------------------------
  esc  <- function(x) htmltools::htmlEscape(x, attribute = FALSE)
  slug <- function(x) gsub("[^A-Za-z0-9_-]", "-", x)      # safe-id helper

  ## ---- identify column groups -------------------------------------------
  year_cols <- grep("^(\\d{4}|Year|Wave)", names(heat_df), value = TRUE)   # 年份列：名称以4位数字起头
  aux_cols  <- setdiff(names(heat_df),
                       c("category", "Variable", year_cols))   # 其余即辅助信息列

  ## ---- colour settings（仅用年份列计算渐变） -----------------------------
  all_vals <- suppressWarnings(as.numeric(unlist(heat_df[year_cols])))
  rng_vals <- all_vals[!is.na(all_vals) & all_vals != 0]
  rng      <- if (length(rng_vals) > 0) range(rng_vals) else c(0, 1)

  ramp_odd  <- colorRamp(c("#f4f2f0", color1))
  ramp_even <- colorRamp(c("#f4f2f0", color2))

  cell_colour <- function(val, row_parity) {
    val_num <- suppressWarnings(as.numeric(val))
    if (is.na(val_num) || val_num == 0) return("#f4f2f0")
    if (diff(rng) == 0) return(color1)          # 全部相同值时退化
    t       <- (val_num - rng[1]) / diff(rng)
    rgb_vec <- if (row_parity) ramp_odd(t) else ramp_even(t)
    sprintf("rgb(%d,%d,%d)", round(rgb_vec[1]), round(rgb_vec[2]), round(rgb_vec[3]))
  }

  ## ---- build heat-map table ---------------------------------------------
  make_heatmap <- function() {

    # —— 表头
    head <- paste0(
      "<tr><th>Variable</th>",
      paste(sprintf("<th>%s</th>", esc(aux_cols)),  collapse = ""),
      paste(sprintf("<th>%s</th>", esc(year_cols)), collapse = ""),
      "</tr>"
    )

    # —— 分类拆分
    split_list <- split(heat_df, heat_df$category)
    rows <- mapply(function(cat_name, block) {

      colspan <- 1 + length(aux_cols) + length(year_cols)
      cat_id  <- sprintf("cat-%s", slug(cat_name))
      cat_row <- sprintf(
        paste0('<tr><td colspan="%d" style="background-color:',color1,';color:#fff;font-weight:bold;padding:6px;border:transparent;border-radius:6px;font-size:20px;text-align:center;"><a href="#%s" style="color:#fff;text-decoration:none;">%s</a></td></tr>'),
        colspan, cat_id, esc(cat_name)
      )

      var_rows <- vapply(seq_len(nrow(block)), function(i) {
        r      <- block[i, ]
        parity <- i %% 2L
        varid  <- esc(r[["Variable"]])

        # 辅助列单元格
        cells_aux <- vapply(aux_cols, function(ac) {
          sprintf('<td style="background:#f4f2f0;color:#333;font-size:12px;text-align:center;">%s</td>',
                  esc(r[[ac]]))
        }, character(1))

        # 年份列单元格
        cells_year <- vapply(year_cols, function(y) {
          v <- r[[y]]
          sprintf(
            '<td class="year-cell" style="background-color:%s;color:#000;text-align:center;">%s</td>',
            cell_colour(v, parity),
            ifelse(is.na(v) || v == 0, "", format(v, big.mark = ",")))
        }, character(1))

        paste0(
          '<tr>',
          '<td style="font-weight:600;text-align:center;">',
          sprintf('<a href="#var-%s" style="text-decoration:none;color:black;">%s</a></td>', varid, varid),
          paste(cells_aux,  collapse = ""),
          paste(cells_year, collapse = ""),
          '</tr>'
        )
      }, character(1))

      paste0(cat_row, paste(var_rows, collapse = ""))
    },
    names(split_list), split_list,
    SIMPLIFY = FALSE, USE.NAMES = FALSE)

    css <- paste0('
<style>
/* ---- table ---- */
.heatmap-table{border-collapse:collapse;width:100%;font-size:15px;}
/* ---- header ---- */
.heatmap-table th{background:',color2,';border:transparent;color:#fff; text-align:center;padding:6px;border-radius:6px;}
/* ---- body cells (default) ---- */
.heatmap-table td{border:1px solid #ddd;padding:8px;vertical-align:middle;border-radius:0;}
/* first column (Variable) */
.heatmap-table td:first-child{font-weight:600;}
.heatmap-table td.year-cell,
.heatmap-table th.year-cell{border-left:none !important;border-right:none !important;border-top:1px solid #ddd !important;border-bottom:1px solid #ddd !important;}
</style>')


    paste0(css, '<table class="heatmap-table">', head,
           paste(rows, collapse = ""), '</table>')
  }

  ## ---- build variable-map cards（与之前一致） ---------------------------
  make_varmap <- function() {
    cat_blocks <- split(meta_df, heat_df$category[match(meta_df$Variable,
                                                        meta_df$Variable)])
    details <- vapply(names(cat_blocks), function(cat) {
      cards <- apply(cat_blocks[[cat]], 1, function(r) {
        var  <- esc(r[["Variable"]])
        ori  <- esc(r[["original_vars"]])
        summ <- esc(r[["easylabel"]])
        det  <- r[["detail"]]
        sprintf('
<div class="data-card" onclick="toggleLabel(this)">
  <div class="var-name-line">
    <span class="var-name" id="var-%s">%s</span>
    <div class="mapping-container"><div class="mapping-info">← <span class="original">%s</span></div></div>
    <span class="var-summary">%s</span>
  </div>
  <div class="var-label"><span class="label-text">%s</span></div>
</div>', var, var, ori, summ, det)
      })
      sprintf('<details id="cat-%s"><summary>%s</summary><div class="category-content">%s</div></details>',
              slug(cat), esc(cat), paste(cards, collapse = ""))
    }, character(1))

    js <- '
<script>
function toggleLabel(card){
  const lbl = card.querySelector(".var-label");
  lbl.style.display = lbl.style.display === "block" ? "none" : "block";
}
</script>'
  css <- '
<style>
.variable-map details{margin-top:15px;padding:0 12px;background:#fff;border-radius:0 6px 6px 0;box-shadow:0 1px 3px rgba(0,0,0,.05)}
.variable-map details summary{cursor:pointer;font-size:20px;font-weight:600;color:#2C3E50;background:linear-gradient(90deg,#F2F6F9 0%,#E7EEF4 100%);padding:11px 16px;margin:0 -12px;border-radius: 0px;border-left:4px solid #2C3E50;border-right:4px solid #2C3E50;user-select:none;transition:background .3s ease,color .3s ease}
.variable-map details summary:hover,.variable-map details[open]>summary{background:linear-gradient(90deg,#E7EEF4 0%,#DAE3EC 100%)}
.variable-map details summary::-webkit-details-marker{display:none}
.variable-map details summary {border-top:none;border-bottom:none;list-style: none;position: relative;padding-left: 36px;}
.variable-map details summary::before {content: "▶";position: absolute; left: 14px; top: 50%; transform: translateY(-50%); font-size: 15px; transition: transform 0.3s; line-height: 1;}
.variable-map details[open] summary::before {content: "▼";}
.variable-map .category-content{padding:8px 0 4px 0}
.variable-map .data-card{background:#f4f2f0;border-left:4px solid darkred;box-shadow:0 1px 3px rgba(0,0,0,.05);padding:12px 18px;margin-bottom:12px;border-radius:0 6px 6px 0;cursor:pointer}
.variable-map .var-name-line{margin-bottom:6px;display:flex;align-items:center;flex-wrap:wrap;gap:8px}
.variable-map .var-name{font-weight:bold;color:#000;font-size:18px}
.variable-map .mapping-container{display:inline-flex;align-items:center;position:relative;margin-left:8px}
.variable-map .mapping-info{font-size:12px;background:#f8fafb;color:darkred;padding:2px 6px;border-radius:4px;border:1px solid #e0e0e0;display:inline-block}
.variable-map .mapping-info:before{content:"变量映射";position:absolute;top:-10px;left:10px;background:#f8fafb;padding:0 5px;font-size:.8em;color:#7f8c8d}
.variable-map .var-label{font-size:14px;color:#7f8c8d;background:#f8fafb;padding:4px 14px 10px;display:inline-block}
.variable-map .var-summary{font-size:18px;color:#2C3E50;margin-left:8px;padding:2px 6px;border-radius:4px}
.var-label .fixed-table{
  border-collapse:collapse;
  width:auto;
  display:inline-table;
}

  </style>'
paste0(css, '<div class="variable-map">', paste(details, collapse = ""),
       '</div>', js)
  }

## ---- assemble ---------------------------------------------------------
# 创建一个辅助函数来生成折叠内容
make_collapsible <- function(title, content, level = 2) {
  paste0(
    paste(rep("#", level), collapse = ""), " ", title, "\n\n",
    '<details data-toggle-details class="custom-details">
<summary data-toggle-summary>
  <span class="icon">+</span>
  <span class="text">
    <span class="open-text">点击收起详情</span>
    <span class="close-text">点击查看详情</span>
  </span>
</summary>
',
    content,
    '\n</details>'
  )
}

# 生成最终的 HTML
html <- paste(
  make_collapsible("定义的组分概览", make_heatmap()),
  make_collapsible("定义的组分详情", make_varmap()),
  sep = "\n\n"
)

# 添加 CSS 样式
css <- '<style>
.custom-details summary {list-style: none;cursor: pointer;padding: 10px 15px; background: #f8fafb;border-radius: 6px; border: 1px solid #e0e0e0;}
.custom-details .icon {display: inline-block; width: 20px;  height: 20px; text-align: center; line-height: 18px; background: #4a6ee0; color: white;
  border-radius: 50%; margin-right: 10px; font-size: 16px; font-weight: bold;  transition: all 0.3s;}
.custom-details[open] .icon { background: #e04a5e;transform: rotate(45deg); line-height: 20px;}
.open-text {display: none;color: #e04a5e;}
.close-text { display: inline;color: #333;}
.custom-details[open] .open-text {display: inline;}
.custom-details[open] .close-text { display: none;}
.custom-details .text { font-weight: 500;}
</style>'

# 最终结果
final_html <- paste(html, css, sep = "\n\n")

if (!is.null(file)) writeLines(final_html, file, useBytes = TRUE)
invisible(final_html)
}
