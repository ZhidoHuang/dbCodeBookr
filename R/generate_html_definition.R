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
      "<table class='hm-table'>",
      header,
      paste(rows, collapse = ""),
      "</table>"
    )
  }

  html <- paste0(
    '
<style>
.hm-table{border-collapse:collapse;width:100%;font-size:15px;}
.hm-table th{border:transparent;background:darkred;color:#fff;text-align:center;padding:6px;font-size:20px;border-radius:6px;}
.hm-table td{border:1px solid #2C3E5050;padding:8px;vertical-align:middle;}
.hm-table td:first-child{font-weight:600;font-size:18px;}
.plain-cell{text-align:center;font-size:15px;color:#2C3E50;}
.card-cell{min-width:220px;max-width:360px;}
.card-wrapper{min-width:200px;}
.data-card{background:#F4F2F0;border-left:4px solid darkred;box-shadow:0 1px 3px rgba(0,0,0,.05);padding:12px 18px;border-radius:0 6px 6px 0;cursor:pointer;}
.var-name-line{display:flex;align-items:center;gap:10px;}
.var-name{font-size:18px;font-weight:bold;line-height:1.4;flex-shrink:0;}
.mapping-block{display:flex;align-items:center;gap:6px;min-width:0;}
.mapping-arrow{font-size:13px;color:darkred;flex-shrink:0;}
.mapping-info{font-size:10px;line-height:1.4;background:#f8fafb;color:darkred;padding:2px 6px;border-radius:4px;border:1px solid #ddd;white-space:normal;word-break:break-word;}
.var-label{margin-top:10px;padding:10px 14px;background:#f8fafb;border-radius:4px;font-size:10px;line-height:1.55;color:#7f8c8d;display:block;}
.var-label .fixed-table{border-collapse:collapse;}
.var-label .fixed-table td{font-size:10px;color:#7f8c8d;padding:2px 8px;white-space:nowrap;font-weight:400!important;border:1px solid #ddd!important;}
.bar-container{width:60px;height:14px;background:#eef2f4;border-radius:3px;overflow:hidden;}
.bar{height:100%;background:#AE4D4D;border-radius:3px;}
</style>

<script>
function toggleLabel(card){const lbl=card.querySelector(".var-label");if(!lbl)return;lbl.style.display=(lbl.style.display==="block")?"none":"block";}
</script>
',
    make_table()
  )

  if (!is.null(file)) writeLines(html, file, useBytes = TRUE)

  invisible(html)
}
