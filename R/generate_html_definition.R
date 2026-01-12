#' 生成变量定义说明的 HTML 文档（卡片式交互表格）
#'
#' 根据元数据表（meta_df）生成一个包含变量概览与详细说明的 HTML 页面。
#' 页面以表格形式展示，每一行对应一个变量，其中“detail”列以可点击卡片方式
#' 展示变量标签、原始变量映射及扩展说明，支持前端展开/收起。
#'
#' @param meta_df 变量元数据数据框（data.frame 或 tibble）。
#' 必须至少包含以下列：
#' \itemize{
#'   \item \code{Variable}：变量名称（字符型，唯一标识）；
#'   \item \code{original_vars}：来源或原始变量名称（字符型，可合并展示）；
#'   \item \code{detail}：变量的详细说明（HTML 或纯文本，允许包含表格与样式）。
#' }
#' 其余列将自动作为普通列一并展示在表格中。
#'
#' @param file 输出 HTML 文件路径。
#' 若为 \code{NULL}（默认），则不写入文件，仅返回生成的 HTML 字符串。
#'
#' @return
#' 返回一个不可见的字符向量（invisible），内容为完整的 HTML 文本。
#' 若指定了 \code{file}，同时会将 HTML 写入对应文件。
#'
#' @details
#' 函数主要执行流程如下：
#' \enumerate{
#'   \item 校验 \code{meta_df} 是否包含必要列（\code{Variable}、
#'         \code{original_vars}、\code{detail}）；
#'   \item 自动确定需要展示的列（除 \code{original_vars} 外的所有列）；
#'   \item 构建 HTML 表格：
#'     \itemize{
#'       \item \code{Variable} 列生成锚点链接，便于页面内跳转；
#'       \item \code{detail} 列渲染为卡片结构，点击可展开/收起说明；
#'       \item 其他列以普通单元格形式居中展示；
#'     }
#'   \item 内嵌 CSS 样式，用于表格布局、卡片样式及条形图展示；
#'   \item 内嵌 JavaScript，用于控制变量说明卡片的显示与隐藏；
#'   \item 组合为完整 HTML 文本并（可选）写入文件。
#' }
#'
#' 生成的 HTML 适合直接在浏览器中打开，也可嵌入到
#' R Markdown / Quarto / Shiny 输出中使用。
#'
#' @examples
#' \dontrun{
#' meta_df <- data.frame(
#'   Variable = c("age", "gender"),
#'   original_vars = c("ba001", "bb002"),
#'   label = c("Age of respondent", "Gender"),
#'   detail = c(
#'     "Age measured in years at interview.",
#'     "1 = Male; 2 = Female"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # 仅生成 HTML 字符串
#' html_txt <- generate_html_definition(meta_df)
#'
#' # 生成并写入文件
#' generate_html_definition(meta_df, file = "Definition.html")
#' }
#'
#' @importFrom htmltools htmlEscape
#' @export
generate_html_definition <- function(meta_df, file = NULL) {

  esc <- function(x) htmltools::htmlEscape(x, attribute = FALSE)

  stopifnot("Variable" %in% names(meta_df))
  stopifnot("original_vars" %in% names(meta_df))
  stopifnot("detail" %in% names(meta_df))

  ## ---- columns to show --------------------------------------------------
  show_cols <- setdiff(names(meta_df), "original_vars")

  ## ---- build table ------------------------------------------------------
  make_table <- function() {

    ## header
    header <- paste0(
      "<tr>",
      paste(sprintf("<th>%s</th>", esc(show_cols)), collapse = ""),
      "</tr>"
    )

    ## rows
    rows <- vapply(seq_len(nrow(meta_df)), function(i) {

      r <- meta_df[i, ]

      cells <- vapply(show_cols, function(col) {

        if (col == "Variable") {
          sprintf(
            '<td class="var-cell">
               <a href="#var-%s">%s</a>
             </td>',
            esc(r[[col]]), esc(r[[col]])
          )

        } else if (col == "detail") {
          sprintf(
            '<td class="card-cell">
               <div class="card-wrapper">
                 <div class="data-card" onclick="toggleLabel(this)">
                   <div class="var-name-line">
                     <span class="var-name" id="var-%s">%s</span>
                     <div class="mapping-container">
                       <div class="mapping-info">
                         ← <span class="original">%s</span>
                       </div>
                     </div>
                   </div>
                   <div class="var-label">%s</div>
                 </div>
               </div>
             </td>',
            esc(r[["Variable"]]),
            esc(r[["Variable"]]),
            esc(r[["original_vars"]]),
            r[["detail"]]
          )

        } else {
          sprintf(
            '<td class="plain-cell">%s</td>',
            esc(r[[col]])
          )
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

  ## ---- assemble HTML ----------------------------------------------------
  html <- paste0(
    '<!-- html-start -->
<style>

/* ---- table ---- */
.heatmap-table{
  border-collapse:collapse;
  width:100%;
  font-size:13px;
  margin-bottom:30px;
}
.heatmap-table th{
  background:darkred;
  color:#fff;
  text-align:center;
  padding:6px;
}
.heatmap-table td{
  border-top:1px solid #ddd;
  border-bottom:1px solid #ddd;
  padding:4px;
  vertical-align:middle;
}

/* ---- plain cells ---- */
.plain-cell{
  text-align:center;
  font-size:12px;
  color:#2C3E50;
}

/* ---- variable cell ---- */
.var-cell{
  font-weight:600;
  text-align:center;
  white-space:nowrap;
}
.var-cell a{
  text-decoration:none;
  color:#000;
}

/* ---- card layout ---- */
.card-cell{
  min-width:420px;
  max-width:760px;
}
.card-wrapper{
  position:relative;
  overflow:visible;
  min-width:400px;
}

/* ---- data-card ---- */
.data-card{
  background:#fff;
  border-left:4px solid darkred;
  box-shadow:0 1px 3px rgba(0,0,0,.05);
  padding:12px 18px;
  border-radius:0 6px 6px 0;
  cursor:pointer;
}
.var-name-line{
  margin-bottom:6px;
  display:flex;
  align-items:center;
  flex-wrap:wrap;
  gap:8px;
}
.var-name{
  font-weight:bold;
  font-size:18px;
}
.mapping-info{
  font-size:12px;
  background:#f0f3f5;
  color:darkred;
  padding:2px 6px;
  border-radius:4px;
  border:1px solid #e0e0e0;
}

/* ---- var-label ---- */
.var-label{
  margin-top:10px;
  padding:10px 14px;
  background:#f8fafb;
  border-radius:4px;
  font-size:14px;
  line-height:1.55;
  color:#2c3e50;
  display:block;
}

/* ---- fixed-table / bar ---- */
.var-label .fixed-table{
  border-collapse:collapse;
  table-layout:fixed;
  width:auto;
}
.var-label .fixed-table td{
  padding:2px 8px;
  white-space:nowrap;
  font-size:13px;
}
.var-label .fixed-table td:nth-child(1),
.var-label .fixed-table td:nth-child(2){
  width:64px;
  text-align:right;
  font-variant-numeric: tabular-nums;
}
.var-label .fixed-table td:nth-child(3){
  width:120px;
}
.var-label .fixed-table td:nth-child(4){
  max-width:160px;
  overflow:hidden;
  text-overflow:ellipsis;
  color:#7f8c8d;
}

.bar-container{
  width:120px;
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

## 模块概览
',
make_table(),
'<!-- html-end -->'
  )

  if (!is.null(file)) {
    writeLines(html, file, useBytes = TRUE)
  }

  invisible(html)
}

