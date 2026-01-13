#' 生成变量代码本（Codebook）的 HTML 页面（卡片式说明）
#'
#' 根据变量元数据表生成一个用于展示代码本（codebook）的 HTML 页面。
#' 页面以表格形式呈现，每个变量的详细说明通过可点击的卡片展开/收起，
#' 适合用于问卷、数据库或衍生变量的定义文档展示。
#'
#' @param meta_df 变量元数据数据框（data.frame）。
#' 必须包含以下列：
#' \itemize{
#'   \item \code{Variable}：变量名称（字符型，用于显示在卡片标题中）；
#'   \item \code{original_vars}：对应的原始变量名或来源说明（字符型）；
#'   \item \code{detail}：变量的详细定义说明（可为 HTML 或纯文本）。
#' }
#' 除上述列外的其他列，将作为普通列按顺序展示在表格中。
#'
#' @param file 输出 HTML 文件路径。
#' 若为 \code{NULL}（默认），则不写入文件，仅返回生成的 HTML 字符串。
#'
#' @return
#' 返回一个不可见的字符向量（invisible），内容为完整的 HTML 文本。
#' 如果指定了 \code{file}，同时会将 HTML 写入该文件。
#'
#' @details
#' 函数执行流程如下：
#' \enumerate{
#'   \item 检查 \code{meta_df} 是否为数据框，并验证必要列是否存在；
#'   \item 自动确定需要在表格中展示的列（除 \code{Variable} 与
#'         \code{original_vars} 外的所有列）；
#'   \item 构建 HTML 表格结构：
#'     \itemize{
#'       \item 普通列使用居中单元格直接展示；
#'       \item \code{detail} 列渲染为卡片（data-card），
#'             显示变量名、原始变量映射以及可展开的详细说明；
#'     }
#'   \item 内嵌 CSS，用于控制表格样式、卡片布局、字体大小及颜色；
#'   \item 内嵌 JavaScript，实现点击卡片后说明区域的展开/收起；
#'   \item 将样式、脚本与表格拼接为完整 HTML 文本并（可选）写入文件。
#' }
#'
#' 生成的 HTML 可直接在浏览器中打开，也可嵌入到
#' R Markdown、Quarto 或 Shiny 应用中作为静态代码本页面。
#'
#' @examples
#' \dontrun{
#' meta_df <- data.frame(
#'   Variable = c("age", "gender"),
#'   original_vars = c("ba001", "bb002"),
#'   label = c("Age", "Gender"),
#'   detail = c(
#'     "Age in completed years at interview.",
#'     "1 = Male; 2 = Female"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 仅生成 HTML 字符串
#' html_txt <- generate_html_definition(meta_df)
#'
#' # 生成并写入文件
#' generate_html_definition(meta_df, file = "Codebook.html")
#' }
#
#' @export
generate_html_definition <- function(meta_df, file = NULL) {

  stopifnot(is.data.frame(meta_df))
  stopifnot(all(c("Variable", "original_vars", "detail") %in% names(meta_df)))

  show_cols <- setdiff(names(meta_df), c("Variable", "original_vars"))

  make_table <- function() {

    header <- paste0(
      "<tr>",
      paste(sprintf("<th>%s</th>", show_cols), collapse = ""),
      "</tr>"
    )

    rows <- vapply(seq_len(nrow(meta_df)), function(i) {

      r <- meta_df[i, ]

      cells <- vapply(show_cols, function(col) {

        if (col == "detail") {

          sprintf(
            '<td class="card-cell">
  <div class="card-wrapper">
    <div class="data-card" onclick="toggleLabel(this)">
      <div class="var-name-line">
        <span class="var-name">%s</span>
        <div class="mapping-block">
          <span class="mapping-arrow">←</span>
          <div class="mapping-info">%s</div>
        </div>
      </div>
      <div class="var-label">%s</div>
    </div>
  </div>
</td>',
            r[["Variable"]],
            r[["original_vars"]],
            r[["detail"]]
          )

        } else {

          sprintf('<td class="plain-cell">%s</td>', r[[col]])

        }

      }, character(1))

      paste0("<tr>", paste(cells, collapse = ""), "</tr>")

    }, character(1))

    paste0(
      "<table class='heatmap-table'>",
      header,
      paste(rows, collapse = ""),
      "</table>"
    )
  }

  html <- paste0(
    '
<style>

/* ---- table ---- */
.heatmap-table{
  border-collapse:collapse;
  width:100%;
  font-size:15px;
}
.heatmap-table th{
  border:1px solid #darkred;
  background:darkred;
  color:#fff;
  text-align:center;
  padding:6px;
}
.heatmap-table td{
  border:1px solid #2C3E5050;
  padding:8px;
  vertical-align:middle;
}
.heatmap-table td:first-child{
  font-weight:600;
}

/* ---- plain cells ---- */
.plain-cell{
  text-align:center;
  font-size:15px;
  color:#2C3E50;
}

/* ---- card ---- */
.card-cell{min-width:220px;max-width:360px}
.card-wrapper{min-width:200px}

.data-card{
  background:#F4F2F0;
  border-left:4px solid darkred;
  box-shadow:0 1px 3px rgba(0,0,0,.05);
  padding:12px 18px;
  border-radius:0 6px 6px 0;
  cursor:pointer;
}

/* ---- name + mapping (CENTER ALIGN) ---- */
.var-name-line{
  display:flex;
  align-items:center;     /* ✅ 纵向居中 */
  gap:10px;
}

.var-name{
  font-size:18px;
  font-weight:bold;
  line-height:1.4;
  flex-shrink:0;
}

/* mapping block treated as ONE unit */
.mapping-block{
  display:flex;
  align-items:center;     /* arrow + info 居中 */
  gap:6px;
  min-width:0;
}

.mapping-arrow{
  font-size:13px;
  color:darkred;
  flex-shrink:0;
}

.mapping-info{
  font-size:10px;
  line-height:1.4;
  background:#f8fafb;
  color:darkred;
  padding:2px 6px;
  border-radius:4px;
  border:1px solid #ddd;
  white-space:normal;
  word-break:break-word;
}

/* ---- var-label ---- */
.var-label{
  margin-top:10px;
  padding:10px 14px;
  background:#f8fafb;
  border-radius:4px;
  font-size:10px;
  line-height:1.55;
  color:#7f8c8d;
  display:block;
}

/* ---- fixed-table ---- */
.var-label .fixed-table{
  border-collapse:collapse;

}
.var-label .fixed-table td{
  font-size:10px;
  color:#7f8c8d;
  padding:2px 8px;
  white-space:nowrap;
  font-weight:400 !important;
  border:1px solid #ddd !important;
}

/* ---- bar ---- */
.bar-container{
  width:60px;
  height:14px;
  background:#eef2f4;
  border-radius:3px;
  overflow:hidden;
}
.bar{
  height:100%;
  background:#AE4D4D;
  border-radius:3px;
}

</style>

<script>
function toggleLabel(card){
  const lbl = card.querySelector(".var-label");
  if(!lbl) return;
  lbl.style.display =
    (lbl.style.display === "block") ? "none" : "block";
}
</script>

## 定义
',
make_table()
  )

  if (!is.null(file)) writeLines(html, file, useBytes = TRUE)

  invisible(html)
}

