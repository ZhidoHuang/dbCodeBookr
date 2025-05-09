#' 整理中介分析结果为表格
#'
#' 此函数用于提取由 \code{mediate} 函数（来自 \pkg{mediation} 包）生成的中介分析对象中的关键结果，
#' 并将其格式化为易于阅读的数据框表格，方便后续展示或导出。
#'
#' @param med_res 一个 \code{mediate} 类对象，通常由 \code{mediate()} 函数生成。
#'
#' @return 返回一个数据框（data.frame），包含以下列：
#' \describe{
#'   \item{Effect}{效应类型，包括总效应、平均/处理组/对照组的中介效应（ACME）、直接效应（ADE）和中介比例（Proportion Mediated）。}
#'   \item{Estimate}{效应的点估计值，保留3位小数。}
#'   \item{CI.Lower}{置信区间的下限，保留3位小数。}
#'   \item{CI.Upper}{置信区间的上限，保留3位小数。}
#'   \item{p.value}{保留3位小数，小于0.001时显示为 "<0.001"。}
#' }
#'
#' @examples
#' \dontrun{
#'   library(mediation)
#'   med_fit <- mediate(model.m, model.y, treat = "treat", mediator = "med", boot = TRUE, sims = 500)
#'   yyds_med_table(med_fit)
#' }
#'
#' @export
yyds_med_table <- function(med_res) {

  # 检查对象类型
  if (!inherits(med_res, "mediate")) {
    stop("输入对象必须是 'mediate' 类对象。请使用 mediation::mediate() 生成。")
  }

  # 格式化p值函数
  format_p <- function(p) {
    ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  }

  s <- summary(med_res)
  data.frame(
    Effect = c("Total Effect",
               "ACME (average)", "ADE (average)", "Prop. Mediated (average)",
               "ACME (control)", "ACME (treated)",
               "ADE (control)", "ADE (treated)",
               "Prop. Mediated (control)", "Prop. Mediated (treated)"),
    Estimate = round(c(s$tau.coef,
                       s$d.avg, s$z.avg, s$n.avg,
                       s$d0, s$d1,
                       s$z0, s$z1,
                       s$n0, s$n1), 3),
    CI.Lower = round(c(s$tau.ci[1],
                       s$d.avg.ci[1], s$z.avg.ci[1], s$n.avg.ci[1],
                       s$d0.ci[1], s$d1.ci[1],
                       s$z0.ci[1], s$z1.ci[1],
                       s$n0.ci[1], s$n1.ci[1]), 3),
    CI.Upper = round(c(s$tau.ci[2],
                       s$d.avg.ci[2], s$z.avg.ci[2], s$n.avg.ci[2],
                       s$d0.ci[2], s$d1.ci[2],
                       s$z0.ci[2], s$z1.ci[2],
                       s$n0.ci[2], s$n1.ci[2]), 3),
    p.value = format_p(c(s$tau.p,
                         s$d.avg.p, s$z.avg.p, s$n.avg.p,
                         s$d0.p, s$d1.p,
                         s$z0.p, s$z1.p,
                         s$n0.p, s$n1.p)),
    stringsAsFactors = FALSE
  )
}
