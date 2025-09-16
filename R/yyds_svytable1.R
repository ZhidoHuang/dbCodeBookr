#' 复杂抽样加权基线表生成器（Table 1）
#'
#' 在 \pkg{survey} 复杂抽样设计下，按分组变量一次性生成“基线表”（Table 1）。
#' 支持三类变量：\emph{分类}（比例、可选 95\%CI/SE）、\emph{连续近似正态}（Mean(SE) 或 Mean(95\%CI)）、
#' \emph{连续非正态}（Median[IQR] 或 Median(95\%CI)）；\emph{差异检验}（Rao–Scott 设计型卡方（分类），
#'   设计型 t 检验 / Wald F（近似正态），以及设计型 Wilcoxon / 秩 ANOVA（非正态））。
#'
#' @param design A \code{\link[survey]{survey.design}} 或 \code{\link[survey]{svyrep.design}} 对象。
#'   要分析的变量（含 \code{group}）需存在于 \code{design$variables} 中。
#' @param group 分组公式字符串，例如 \code{"~sex"}。将按其因子水平生成分组列。
#' @param vars_cont \code{character}。近似正态的连续变量名向量。若 \code{ci_cont=TRUE} 显示 Mean(95\% CI)，
#'   否则显示 Mean(SE)。
#' @param vars_nn_cont \code{character}。非正态的连续变量名向量。若 \code{ci_nn_cont=TRUE} 显示
#'   Median(95\% CI)，否则显示 Median[IQR]。
#' @param vars_categ \code{character}。分类变量名向量（建议确保为因子；其 \code{levels} 决定显示顺序）。
#' @param categ_style 比例单元格样式。可选：
#' \itemize{
#'   \item `"percent"`：只显示百分比（不带 \% 符号；行名会追加 `", %"`）；
#'   \item `"number_percent"`：未加权计数 + 百分比（如 `n (xx.x)`）；
#'   \item `"Number_percent"`：加权计数 + 百分比（如 `N (xx.x)`，\eqn{N} 为分析权重求和后四舍五入）；
#'   \item `"percent_SE"`：百分比 + SE（如 `xx.x (SE)`，SE 也以“百分点”呈现）。当 `ci_categ=TRUE` 时，始终以 CI 为准，`percent_SE` 被自动忽略。
#' }
#' @param ci_cont \code{logical}。连续（近似正态）是否显示 95\% CI（TRUE=Mean(95\% CI)，FALSE=Mean(SE)）。
#' @param ci_nn_cont \code{logical}。连续（非正态）是否显示 95\% CI（TRUE=Median(95\% CI)，FALSE=Median[IQR]）。
#' @param ci_categ \code{logical}。分类比例是否显示 95\% CI（若与 \code{se_categ} 同为 TRUE，则 CI 优先）。
#' @param ci_categ_method \code{character}。\code{\link[survey]{svyciprop}} 的区间方法。
#'   当前函数严格接受以下 6 个全名：\code{"logit"}, \code{"likelihood"}, \code{"asin"},
#'   \code{"beta"}, \code{"xlogit"}, \code{"mean"}。
#' @param digits_cont \code{integer}。连续型数值的小数位（均值/SE/CI/中位数/IQR）。
#' @param digits_categ \code{integer}。比例的小数位。
#' @param digits_p \code{integer}。P 值显示小数位；当 \eqn{p < 10^{-digits\_p}} 时显示为
#'   \code{"<0.00...1"}（例如 \code{digits_p=3} 时显示 \code{"<0.001"}）。
#' @param show_n \code{logical}。是否显示表头未加权样本量 \code{N (unweighted)}。
#' @param show_N \code{logical}。是否显示表头加权样本量 \code{N (weighted)}。其含义为按组
#'   \code{\link[survey]{svytable}} 的权重和（回退为权重求和）；\emph{不一定等于总体人数}。
#' @param showOverall \code{logical}。是否增加 \code{Overall} 列。
#' @param showAllLevels \code{logical}。若为 FALSE 且变量为二分类，仅展示第二个水平
#'   \code{levels(x)[2]}，并与变量名同行输出；同时在 \code{Test} 列\emph{追加}该水平名（形如
#'   \code{"Yes  |  Rao–Scott χ² (design-based F)"}）。如需固定展示“阳性/Yes”，
#'   请在外部将因子水平设为 \code{c("No","Yes")}。
#'
#' @return
#' 一个 `data.frame`，列包括：
#' \itemize{
#'   \item `"Characteristics"`：变量名（必要时带单位后缀，如 `", %"`；二分类合并时只保留 `level2` 的一行）；
#'   \item 各组列名为分组因子水平（若 `showOverall=TRUE` 还包含 `"Overall"`）；
#'   \item `"P"`：对应的组间比较 P 值（按格式规则打印）；
#'   \item `"Test"`：所用检验方法；二分类合并行会在方法前拼接所统计的水平标签（例如 `"Yes  |  Rao–Scott χ² (design-based F)"`）。
#' }
#'
#' @details
#' \strong{分类变量：}
#' \itemize{
#'   \item 单元格：对每个水平构造指示变量 \code{.ind}，对每个分组子设计计算
#'         \code{svyciprop(~.ind, ...)}（若 \code{ci_categ=TRUE}）或
#'         \code{svyby(~.ind, ~.g, svymean, vartype="se")}（若 \code{categ_style="percent_SE"}）。
#'   \item P 值：\code{svychisq(~ var + group, statistic="F")}（Rao–Scott 设计型卡方的 F 近似）。
#'   \item 二分类合并：当 \code{showAllLevels=FALSE} 时，仅显示 \code{levels(var)[2]}。
#' }
#'
#' \strong{连续（近似正态）：}
#' \itemize{
#'   \item 单元格：\code{svyby(~x, ~group, svymean, vartype="se"/"ci")}；Overall 用
#'         \code{svymean} + \code{confint}。
#'   \item P 值：两组用 \code{svyttest(x ~ group)}；三组及以上用
#'         \code{svyglm(x ~ group)} + \code{regTermTest()}（Wald F）。
#' }
#'
#' \strong{连续（非正态）：}
#' \itemize{
#'   \item 单元格：默认 \code{Median [Q1, Q3]}（\code{svyquantile} 取 25/50/75\% 分位）；
#'         若 \code{ci_nn_cont=TRUE} 输出 \code{Median (95\% CI)}。
#'   \item P 值：两组用 \code{svyranktest(x ~ group)}；三组及以上将秩作为响应做
#'         \code{svyglm(rank(x) ~ group)} + \code{regTermTest()}。
#' }
#'
#' 区间估计默认使用 \code{degf(design)} 作为自由度；当计算失败（如某域权重为 0）时，
#' 函数会优雅回退到点估计或留空字符串。
#'
#' @section 选择分类比例区间方法（\code{ci_categ_method}）：
#' 常用选项包括：
#' \itemize{
#'   \item \code{"logit"}：Logit 变换区间，稳定常用；
#'   \item \code{"beta"}：Beta 分布区间，适合比例接近 0/1 或小样本；
#'   \item 其余 \code{"likelihood"}, \code{"asin"}, \code{"xlogit"}, \code{"mean"} 见 \code{?survey::svyciprop}。
#' }
#'
#' @examples
#' \donttest{
#' library(survey)
#' data(api)
#' dclus1 <- svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
#'
#' # 示例 1：按 stype 分组的基线表（分类 + 连续）
#' tab1 <- yyds_svytable1(
#'   design = dclus1,
#'   group  = "~stype",
#'   vars_cont    = c("api00"),   # 近似正态 → Mean(SE)
#'   vars_nn_cont = c("enroll"),  # 非正态 → Median[IQR]
#'   vars_categ   = c("sch.wide", "comp.imp"),
#'   categ_style  = "number_percent" #c("number_percent","Number_percent","percent","percent_SE")
#' )
#' head(tab1)
#'
#' # 示例 2：仅连续变量，且显示总体列
#' tab2 <- yyds_svytable1(
#'   design = dclus1,
#'   group  = "~stype",
#'   vars_cont    = c("api00"),   # 近似正态 → Mean(SE)
#'   vars_nn_cont = c("enroll"),  # 非正态 → Median[IQR]
#'   vars_categ   = c("sch.wide", "comp.imp"),
#'   ci_cont      = TRUE,
#'   ci_categ     = TRUE,
#'   ci_categ_method= "logit",
#'   show_n       = TRUE,
#'   show_N       = TRUE,
#'   showOverall  = FALSE,
#'   showAllLevels= TRUE,
#'   digits_categ = 1,
#'   digits_cont  = 2,
#'   digits_p     = 3
#' )
#' head(tab2)
#' }
#'
#' @seealso
#' \code{\link[survey]{svymean}}, \code{\link[survey]{svyciprop}},
#' \code{\link[survey]{svychisq}}, \code{\link[survey]{svyttest}},
#' \code{\link[survey]{svyranktest}}, \code{\link[survey]{svyglm}},
#' \code{\link[survey]{regTermTest}}, \code{\link[survey]{svyquantile}}
#'
#' @export
yyds_svytable1 <- function(
    design,
    group,

    vars_cont      = character(),           # 近似正态：Mean(SE) 或 Mean(95% CI)
    vars_nn_cont   = character(),           # 非正态：Median[IQR] 或 Median(95% CI)
    vars_categ     = character(),
    categ_style    = c("number_percent","Number_percent","percent","percent_SE"),

    ci_cont        = FALSE,                 # 均值的 95% CI（TRUE=Mean(95% CI)，FALSE=Mean(SE)）
    ci_nn_cont     = FALSE,                 # 中位数的 95% CI（TRUE=Median(95% CI)，FALSE=Median[IQR]）
    ci_categ       = FALSE,                 # 比例的 95% CI
    ci_categ_method= "logit",               # svyciprop 方法: logit/likelihood/asin/beta/xlogit/mean

    digits_cont    = 2,
    digits_categ   = 1,
    digits_p       = 3,                     # P 值小数位
    show_n         = TRUE,                  # 表头 N (unweighted)
    show_N         = FALSE,                 # 表头 N (weighted)
    showOverall    = FALSE,                 # 是否增加 Overall 列
    showAllLevels  = TRUE                   # FALSE 且二分类：仅 level2，变量名与数值同一行，Test 拼接水平名
){
  # ---- 选项与工具 ----
  categ_style <- match.arg(categ_style)
  ci_categ_method <- match.arg(tolower(ci_categ_method),
                               c("logit","likelihood","asin","beta","xlogit","mean"))

  g <- all.vars(as.formula(group))[1]
  grp <- factor(design$variables[[g]])
  lev_grp  <- levels(grp)
  lev_cols <- if (isTRUE(showOverall)) c("Overall", lev_grp) else lev_grp

  safe_num <- function(x) as.numeric(unlist(x))
  fmt_p <- function(p, d = digits_p) {
    if (length(p) == 0 || is.na(p) || !is.finite(p)) return("")
    thr <- 10^(-d)
    if (p < thr) {
      paste0("<", sprintf(paste0("%.", d, "f"), thr))
    } else {
      sprintf(paste0("%.", d, "f"), p)
    }
  }
  fmt_pct    <- function(p, d=digits_categ) sprintf(paste0("%.",d,"f"), 100*safe_num(p))
  fmt_pct_ci <- function(ph, lo, hi, d=digits_categ)
    sprintf(paste0("%.",d,"f (%.",d,"f-%.",d,"f)"), 100*ph, 100*lo, 100*hi)
  fmt_pct_se <- function(ph, se, d=digits_categ)
    sprintf(paste0("%.",d,"f (%.",d,"f)"), 100*ph, 100*se)

  row_df <- function(varname, cells_named_vec = c(), p = "", test = ""){
    cells_all <- setNames(rep("", length(lev_cols)), lev_cols)
    if (length(cells_named_vec)) {
      nm <- intersect(names(cells_named_vec), lev_cols)
      cells_all[nm] <- as.character(cells_named_vec[nm])
    }
    as.data.frame(c(list("Characteristics" = varname), as.list(cells_all), list("P" = p, "Test" = test)),
                  check.names = FALSE, stringsAsFactors = FALSE)
  }

  # 兼容 svyby 返回不同列名（连续）
  find_col <- function(df, main, v) {
    hits <- c(paste0(main, ".", v), paste0(main, "_", v), main)
    hits[hits %in% names(df)][1]
  }

  # 中位数 (95% CI) 文本
  median_ci_str <- function(design_sub, var, d=digits_cont) {
    fml <- as.formula(paste0("~", var))
    obj <- try(svyquantile(fml, design_sub, quantiles = 0.5, ci = TRUE, na.rm = TRUE),
               silent = TRUE)
    if (inherits(obj, "try-error")) return("")
    med <- tryCatch(as.numeric(coef(obj)[1]), error = function(e) NA_real_)
    ci  <- tryCatch(confint(obj), error = function(e) NULL)
    if (is.na(med)) return("")
    if (is.null(ci)) {
      sprintf(paste0("%.", d, "f"), med)
    } else {
      if (is.matrix(ci)) { lo <- as.numeric(ci[1,1]); hi <- as.numeric(ci[1,2]) }
      else               { lo <- as.numeric(ci[1]);   hi <- as.numeric(ci[2])   }
      sprintf(paste0("%.", d, "f (%.", d, "f-%.", d, "f)"), med, lo, hi)
    }
  }

  rows <- list()

  # ---- 表头 N ----
  if (isTRUE(show_n)) {
    n_unwt <- table(grp)
    n_vec  <- setNames(as.numeric(n_unwt[lev_grp]), lev_grp)
    cells  <- n_vec
    if (isTRUE(showOverall)) cells <- c(Overall = sum(n_vec, na.rm = TRUE), cells)
    rows <- c(rows, list(row_df("N (unweighted)", cells)))
  }

  if (isTRUE(show_N)) {
    tb <- tryCatch(svytable(as.formula(paste0("~", g)), design), error = function(e) NULL)
    counts <- setNames(rep(NA_real_, length(lev_grp)), lev_grp)
    overall_val <- NA_real_
    if (!is.null(tb)) {
      counts[names(tb)] <- as.numeric(tb)
      overall_val <- sum(as.numeric(tb), na.rm = TRUE)
    } else {
      w <- tryCatch(weights(design, type = "analysis"), error = function(e) NULL)
      if (!is.null(w)) {
        grp_fac <- factor(design$variables[[g]], levels = lev_grp)
        sums <- tapply(w, grp_fac, sum, na.rm = TRUE)
        if (!is.null(sums)) counts[names(sums)] <- as.numeric(sums)
        overall_val <- sum(as.numeric(w), na.rm = TRUE)
      }
    }
    out_counts <- if (isTRUE(showOverall)) c(Overall = overall_val, counts) else counts
    fmt <- ifelse(is.na(out_counts), "", round(out_counts, 0))
    rows <- c(rows, list(row_df("N (weighted)", fmt)))
  }

  # ---- df（t 分布自由度）----
  df_full <- tryCatch(survey::degf(design), error = function(e) Inf)

  # =======================
  # A) 分类变量
  # =======================
  if (length(vars_categ)) {
    for (v in vars_categ) {
      xf_all <- factor(design$variables[[v]])
      lev_x <- levels(xf_all)

      ok <- !is.na(design$variables[[g]]) & !is.na(design$variables[[v]])
      des_ok <- subset(design, ok)
      xf <- factor(des_ok$variables[[v]], levels = lev_x)
      gf <- factor(des_ok$variables[[g]], levels = lev_grp)

      tab_unwt <- if (length(xf) && length(gf)) table(xf, gf) else
        array(0, dim = c(length(lev_x), length(lev_grp)), dimnames = list(lev_x, lev_grp))

      w_ok <- tryCatch(weights(des_ok, type="analysis"), error=function(e) rep(1, nrow(des_ok$variables)))
      tab_wt <- if (length(xf) && length(gf)) {
        suppressWarnings(as.matrix(xtabs(w_ok ~ xf + gf)))
      } else {
        matrix(0, nrow = length(lev_x), ncol = length(lev_grp),
               dimnames = list(lev_x, lev_grp))
      }
      wt_colsum <- colSums(tab_wt, na.rm = TRUE)
      wt_total_all <- sum(tab_wt, na.rm = TRUE)

      # 设计型卡方
      p_val <- ""; method_used <- ""
      ch <- try(svychisq(as.formula(paste0("~", v, " + ", g)),
                         design = design, statistic = "F"), silent = TRUE)
      if (!inherits(ch, "try-error")) {
        p_val <- fmt_p(safe_num(ch$p.value))
        method_used <- "Rao–Scott χ² (design-based F)"
      }

      # 决定此变量是否显示CI/SE
      show_ci <- isTRUE(ci_categ)
      show_se <- (!show_ci) && (categ_style == "percent_SE")

      # Characteristics 后缀
      suffix <- if (show_ci || show_se) ", %" else
        switch(categ_style,
               percent = ", %",
               number_percent = ", n (%)",
               Number_percent = ", N (%)")

      # 二分类合并一行（取 level2）
      binary_collapse <- (!showAllLevels && length(lev_x) == 2)
      if (binary_collapse) {
        lv <- lev_x[2]
        cells <- setNames(rep("", length(lev_cols)), lev_cols)
        des_dom <- update(design,
                          .g   = factor(design$variables[[g]], levels = lev_grp),
                          .ind = as.numeric(design$variables[[v]] == lv))

        if (show_ci) {
          for (gg in lev_grp) {
            des_g <- subset(des_dom, .g == gg)
            est_g <- try(svyciprop(~ .ind, design = des_g,
                                   method = ci_categ_method, level = 0.95,
                                   df = df_full, na.rm = TRUE),
                         silent = TRUE)
            if (!inherits(est_g, "try-error")) {
              ph <- as.numeric(est_g)
              ci <- attr(est_g, "ci"); if (is.null(ci)) ci <- tryCatch(confint(est_g), error=function(e) NULL)
              cells[gg] <- if (!is.null(ci)) fmt_pct_ci(ph, ci[1], ci[2]) else fmt_pct(ph)
            } else {
              p_hat <- if (wt_colsum[gg] > 0) tab_wt[lv, gg] / wt_colsum[gg] else NA_real_
              if (is.finite(p_hat)) cells[gg] <- fmt_pct(p_hat)
            }
          }
        } else if (show_se) {
          res <- try(svyby(~.ind, ~.g, design = des_dom,
                           FUN = svymean, na.rm = TRUE, vartype = "se"),
                     silent = TRUE)
          if (!inherits(res, "try-error")) {
            est <- as.numeric(coef(res)); se <- as.numeric(SE(res))
            names(est) <- names(se) <- as.character(res$.g)
            for (gg in lev_grp) if (gg %in% names(est)) cells[gg] <- fmt_pct_se(est[gg], se[gg])
          } else {
            for (gg in lev_grp) {
              p_hat <- if (wt_colsum[gg] > 0) tab_wt[lv, gg] / wt_colsum[gg] else NA_real_
              if (is.finite(p_hat)) cells[gg] <- fmt_pct(p_hat)
            }
          }
        } else {
          for (gg in lev_grp) {
            p_hat <- if (wt_colsum[gg] > 0) tab_wt[lv, gg] / wt_colsum[gg] else NA_real_
            pct_txt <- if (is.finite(p_hat)) fmt_pct(p_hat) else ""
            if (categ_style == "percent") {
              cells[gg] <- pct_txt
            } else if (categ_style == "number_percent") {
              n_unw <- as.numeric(tab_unwt[lv, gg])
              cells[gg] <- if (pct_txt == "") as.character(n_unw) else sprintf("%d (%s)", n_unw, pct_txt)
            } else if (categ_style == "Number_percent") {
              n_w <- round(as.numeric(tab_wt[lv, gg]), 0)
              cells[gg] <- if (pct_txt == "") as.character(n_w) else sprintf("%d (%s)", n_w, pct_txt)
            }
          }
        }

        if (isTRUE(showOverall)) {
          if (show_ci) {
            est_all <- try(svyciprop(~.ind, des_dom, method = ci_categ_method,
                                     level = 0.95, df = df_full, na.rm = TRUE),
                           silent = TRUE)
            if (!inherits(est_all, "try-error")) {
              ph <- as.numeric(est_all)
              ci <- tryCatch(confint(est_all), error=function(e) NULL)
              if (is.null(ci)) ci <- tryCatch(attr(est_all, "ci"), error=function(e) NULL)
              cells["Overall"] <- if (!is.null(ci)) fmt_pct_ci(ph, ci[1], ci[2]) else fmt_pct(ph)
            } else {
              p_all <- if (wt_total_all > 0) sum(tab_wt[lv, , drop = TRUE]) / wt_total_all else NA_real_
              cells["Overall"] <- if (is.finite(p_all)) fmt_pct(p_all) else ""
            }
          } else if (show_se) {
            m_all <- try(svymean(~.ind, des_dom, na.rm = TRUE), silent = TRUE)
            if (!inherits(m_all, "try-error")) {
              ph <- safe_num(coef(m_all)[1]); se <- safe_num(SE(m_all)[1])
              cells["Overall"] <- fmt_pct_se(ph, se)
            } else {
              p_all <- if (wt_total_all > 0) sum(tab_wt[lv, , drop = TRUE]) / wt_total_all else NA_real_
              cells["Overall"] <- if (is.finite(p_all)) fmt_pct(p_all) else ""
            }
          } else {
            p_all <- if (wt_total_all > 0) sum(tab_wt[lv, , drop = TRUE]) / wt_total_all else NA_real_
            pct_txt_all <- if (is.finite(p_all)) fmt_pct(p_all) else ""
            if (categ_style == "percent") {
              cells["Overall"] <- pct_txt_all
            } else if (categ_style == "number_percent") {
              n_unw_all <- sum(tab_unwt[lv, , drop = TRUE])
              cells["Overall"] <- if (pct_txt_all == "") as.character(n_unw_all) else sprintf("%d (%s)", n_unw_all, pct_txt_all)
            } else if (categ_style == "Number_percent") {
              n_w_all <- round(sum(tab_wt[lv, , drop = TRUE]), 0)
              cells["Overall"] <- if (pct_txt_all == "") as.character(n_w_all) else sprintf("%d (%s)", n_w_all, pct_txt_all)
            }
          }
        }

        test_str <- if (nzchar(method_used)) paste0(lv, "  |  ", method_used) else lv
        rows <- c(rows, list(row_df(paste0(v, suffix), cells, p_val, test_str)))
        next
      }

      # 常规（所有水平或非二分类）
      rows <- c(rows, list(row_df(paste0(v, suffix), c(), p_val, method_used)))
      lev_show <- if (!showAllLevels && length(lev_x) == 2) lev_x[1] else lev_x

      for (lv in lev_show) {
        cells <- setNames(rep("", length(lev_cols)), lev_cols)
        des_dom <- update(design,
                          .g   = factor(design$variables[[g]], levels = lev_grp),
                          .ind = as.numeric(design$variables[[v]] == lv))

        if (show_ci) {
          for (gg in lev_grp) {
            des_g <- subset(des_dom, .g == gg)
            est_g <- try(svyciprop(~ .ind, design = des_g,
                                   method = ci_categ_method, level = 0.95,
                                   df = df_full, na.rm = TRUE),
                         silent = TRUE)
            if (!inherits(est_g, "try-error")) {
              ph <- as.numeric(est_g)
              ci <- attr(est_g, "ci"); if (is.null(ci)) ci <- tryCatch(confint(est_g), error=function(e) NULL)
              cells[gg] <- if (!is.null(ci)) fmt_pct_ci(ph, ci[1], ci[2]) else fmt_pct(ph)
            } else {
              p_hat <- if (wt_colsum[gg] > 0) tab_wt[lv, gg] / wt_colsum[gg] else NA_real_
              if (is.finite(p_hat)) cells[gg] <- fmt_pct(p_hat)
            }
          }
        } else if (show_se) {
          res <- try(svyby(~.ind, ~.g, design = des_dom,
                           FUN = svymean, na.rm = TRUE, vartype = "se"),
                     silent = TRUE)
          if (!inherits(res, "try-error")) {
            est <- as.numeric(coef(res)); se <- as.numeric(SE(res))
            names(est) <- names(se) <- as.character(res$.g)
            for (gg in lev_grp) if (gg %in% names(est)) cells[gg] <- fmt_pct_se(est[gg], se[gg])
          } else {
            for (gg in lev_grp) {
              p_hat <- if (wt_colsum[gg] > 0) tab_wt[lv, gg] / wt_colsum[gg] else NA_real_
              if (is.finite(p_hat)) cells[gg] <- fmt_pct(p_hat)
            }
          }
        } else {
          for (gg in lev_grp) {
            p_hat <- if (wt_colsum[gg] > 0) tab_wt[lv, gg] / wt_colsum[gg] else NA_real_
            pct_txt <- if (is.finite(p_hat)) fmt_pct(p_hat) else ""
            if (categ_style == "percent") {
              cells[gg] <- pct_txt
            } else if (categ_style == "number_percent") {
              n_unw <- as.numeric(tab_unwt[lv, gg])
              cells[gg] <- if (pct_txt == "") as.character(n_unw) else sprintf("%d (%s)", n_unw, pct_txt)
            } else if (categ_style == "Number_percent") {
              n_w <- round(as.numeric(tab_wt[lv, gg]), 0)
              cells[gg] <- if (pct_txt == "") as.character(n_w) else sprintf("%d (%s)", n_w, pct_txt)
            }
          }
        }

        if (isTRUE(showOverall)) {
          if (show_ci) {
            est_all <- try(svyciprop(~.ind, des_dom, method = ci_categ_method,
                                     level = 0.95, df = df_full, na.rm = TRUE),
                           silent = TRUE)
            if (!inherits(est_all, "try-error")) {
              ph <- as.numeric(est_all)
              ci <- tryCatch(confint(est_all), error=function(e) NULL)
              if (is.null(ci)) ci <- tryCatch(attr(est_all, "ci"), error=function(e) NULL)
              cells["Overall"] <- if (!is.null(ci)) fmt_pct_ci(ph, ci[1], ci[2]) else fmt_pct(ph)
            } else {
              p_all <- if (wt_total_all > 0) sum(tab_wt[lv, , drop = TRUE]) / wt_total_all else NA_real_
              cells["Overall"] <- if (is.finite(p_all)) fmt_pct(p_all) else ""
            }
          } else if (show_se) {
            m_all <- try(svymean(~.ind, des_dom, na.rm = TRUE), silent = TRUE)
            if (!inherits(m_all, "try-error")) {
              ph <- safe_num(coef(m_all)[1]); se <- safe_num(SE(m_all)[1])
              cells["Overall"] <- fmt_pct_se(ph, se)
            } else {
              p_all <- if (wt_total_all > 0) sum(tab_wt[lv, , drop = TRUE]) / wt_total_all else NA_real_
              cells["Overall"] <- if (is.finite(p_all)) fmt_pct(p_all) else ""
            }
          } else {
            p_all <- if (wt_total_all > 0) sum(tab_wt[lv, , drop = TRUE]) / wt_total_all else NA_real_
            pct_txt_all <- if (is.finite(p_all)) fmt_pct(p_all) else ""
            if (categ_style == "percent") {
              cells["Overall"] <- pct_txt_all
            } else if (categ_style == "number_percent") {
              n_unw_all <- sum(tab_unwt[lv, , drop = TRUE])
              cells["Overall"] <- if (pct_txt_all == "") as.character(n_unw_all) else sprintf("%d (%s)", n_unw_all, pct_txt_all)
            } else if (categ_style == "Number_percent") {
              n_w_all <- round(sum(tab_wt[lv, , drop = TRUE]), 0)
              cells["Overall"] <- if (pct_txt_all == "") as.character(n_w_all) else sprintf("%d (%s)", n_w_all, pct_txt_all)
            }
          }
        }

        rows <- c(rows, list(row_df(paste0("    ", lv), cells, "", "")))
      }
    }
  }

  # =======================
  # B) 连续变量（近似正态）
  # =======================
  if (length(vars_cont)) {
    for (v in vars_cont) {
      vt <- if (ci_cont) "ci" else "se"
      by <- svyby(as.formula(paste0("~", v)), as.formula(group),
                  design, svymean, vartype = vt, na.rm = TRUE, keep.names = TRUE)
      lev_m <- as.character(by[[g]])
      mean_col <- if (v %in% names(by)) v else find_col(by, v, v)
      if (is.na(mean_col)) {
        num_cols <- which(vapply(by, is.numeric, TRUE)); mean_col <- names(by)[ num_cols[1] ]
      }

      if (ci_cont) {
        ci_l_col <- find_col(by, "ci_l", v); ci_u_col <- find_col(by, "ci_u", v)
        if (is.na(ci_l_col) || is.na(ci_u_col)) {
          CI <- attr(by, "ci"); m <- safe_num(by[[mean_col]])
          cell_vec <- sprintf(paste0("%.",digits_cont,"f (%.",digits_cont,"f-%.",digits_cont,"f)"),
                              m, safe_num(CI[,1]), safe_num(CI[,2]))
        } else {
          m <- safe_num(by[[mean_col]])
          cl <- safe_num(by[[ci_l_col]]); cu <- safe_num(by[[ci_u_col]])
          cell_vec <- sprintf(paste0("%.",digits_cont,"f (%.",digits_cont,"f-%.",digits_cont,"f)"),
                              m, cl, cu)
        }
      } else {
        m <- safe_num(by[[mean_col]])
        se_col <- find_col(by, "se", v)
        if (is.na(se_col)) {
          cell_vec <- sprintf(paste0("%.",digits_cont,"f"), m)
        } else {
          se <- safe_num(by[[se_col]])
          cell_vec <- sprintf(paste0("%.",digits_cont,"f (%.",digits_cont,"f)"), m, se)
        }
      }
      names(cell_vec) <- lev_m

      # Overall
      if (isTRUE(showOverall)) {
        m_all_obj <- svymean(as.formula(paste0("~", v)), design, na.rm = TRUE)
        if (ci_cont) {
          ci_all <- tryCatch(confint(m_all_obj), error=function(e) NULL)
          m_all  <- safe_num(coef(m_all_obj)[1])
          if (!is.null(ci_all)) {
            if (is.matrix(ci_all)) { lo <- as.numeric(ci_all[1,1]); hi <- as.numeric(ci_all[1,2]) }
            else                   { lo <- as.numeric(ci_all[1]);   hi <- as.numeric(ci_all[2])   }
            overall_cell <- sprintf(paste0("%.",digits_cont,"f (%.",digits_cont,"f-%.",digits_cont,"f)"),
                                    m_all, lo, hi)
          } else {
            overall_cell <- sprintf(paste0("%.",digits_cont,"f"), m_all)
          }
        } else {
          m_all  <- safe_num(coef(m_all_obj)[1])
          se_all <- safe_num(SE(m_all_obj)[1])
          overall_cell <- sprintf(paste0("%.",digits_cont,"f (%.",digits_cont,"f)"), m_all, se_all)
        }
        cell_vec <- c(Overall = overall_cell, cell_vec)
      }

      # 检验
      des_ok <- subset(design, !is.na(design$variables[[v]]) & !is.na(design$variables[[g]]))
      k <- nlevels(factor(des_ok$variables[[g]]))
      method_used <- ""; p_val <- NA_real_
      if (k == 2) {
        tmp <- update(des_ok, .g = factor(des_ok$variables[[g]]))
        tt <- try(svyttest(as.formula(paste0(v, " ~ .g")), tmp), silent = TRUE)
        if (!inherits(tt, "try-error")) {
          method_used <- "Design-based t-test (svyttest)"
          p_val <- tryCatch(safe_num(summary(tt)$p.value), error=function(e) NA_real_)
        } else k <- 3
      }
      if (k >= 3 && is.na(p_val)) {
        fit <- try(svyglm(as.formula(paste0(v, " ~ ", g)), design = des_ok, na.action = na.omit), silent = TRUE)
        if (!inherits(fit, "try-error")) {
          p_val <- tryCatch(regTermTest(fit, as.formula(paste0("~", g)))$p, error=function(e) NA_real_)
          method_used <- "Wald F (svyglm + regTermTest)"
        }
      }

      row_name <- paste0(v, if (ci_cont) ", Mean (95% CI)" else ", Mean (SE)")
      rows <- c(rows, list(row_df(row_name, cell_vec, fmt_p(p_val), method_used)))
    }
  }

  # =======================
  # C) 连续变量（非正态）
  # =======================
  if (length(vars_nn_cont)) {
    for (v in vars_nn_cont) {
      if (!isTRUE(ci_nn_cont)) {
        qtxt <- setNames(rep("", length(lev_grp)), lev_grp)
        for (L in lev_grp) {
          desL <- subset(design, design$variables[[g]] == L & !is.na(design$variables[[v]]))
          qq <- try(svyquantile(as.formula(paste0("~", v)), desL,
                                quantiles = c(.25,.5,.75), ci = FALSE, na.rm = TRUE),
                    silent = TRUE)
          if (!inherits(qq, "try-error")) {
            vals <- safe_num(qq)
            if (length(vals) >= 3)
              qtxt[L] <- sprintf(paste0("%.",digits_cont,"f [%.",digits_cont,"f, %.",digits_cont,"f]"),
                                 vals[2], vals[1], vals[3])
          }
        }
        cells <- qtxt
      } else {
        medtxt <- setNames(rep("", length(lev_grp)), lev_grp)
        for (L in lev_grp) {
          desL <- subset(design, design$variables[[g]] == L & !is.na(design$variables[[v]]))
          medtxt[L] <- median_ci_str(desL, v, digits_cont)
        }
        cells <- medtxt
      }

      if (isTRUE(showOverall)) {
        overall_cell <- if (!isTRUE(ci_nn_cont)) {
          qq_all <- try(svyquantile(as.formula(paste0("~", v)), design,
                                    quantiles = c(.25,.5,.75), ci = FALSE, na.rm = TRUE),
                        silent = TRUE)
          if (!inherits(qq_all, "try-error")) {
            va <- safe_num(qq_all)
            if (length(va) >= 3)
              sprintf(paste0("%.",digits_cont,"f [%.",digits_cont,"f, %.",digits_cont,"f]"),
                      va[2], va[1], va[3]) else ""
          } else ""
        } else {
          median_ci_str(design, v, digits_cont)
        }
        cells <- c(Overall = overall_cell, cells)
      }

      # 检验：两组秩检验 / 多组秩 ANOVA
      des_ok <- subset(design, !is.na(design$variables[[v]]) & !is.na(design$variables[[g]]))
      k <- nlevels(factor(des_ok$variables[[g]]))
      method_used <- ""; p_val <- NA_real_
      if (k == 2) {
        rt <- try(svyranktest(as.formula(paste0(v, " ~ ", g)), design = des_ok), silent = TRUE)
        if (!inherits(rt, "try-error")) {
          method_used <- "Design-based Wilcoxon (svyranktest)"
          p_val <- tryCatch(safe_num(summary(rt)$p.value), error=function(e) NA_real_)
        }
      } else if (k >= 3) {
        ranks <- rank(des_ok$variables[[v]], na.last = "keep", ties.method = "average")
        des_r <- update(des_ok, .rank = ranks, .g = factor(des_ok$variables[[g]]))
        fit <- try(svyglm(.rank ~ .g, design = des_r, na.action = na.omit), silent = TRUE)
        if (!inherits(fit, "try-error")) {
          p_val <- tryCatch(regTermTest(fit, ~ .g)$p, error=function(e) NA_real_)
          method_used <- "Design-based rank ANOVA (svyglm on ranks)"
        }
      }

      row_name <- paste0(v, if (isTRUE(ci_nn_cont)) ", Median (95% CI)" else ", Median [IQR]")
      rows <- c(rows, list(row_df(row_name, cells, fmt_p(p_val), method_used)))
    }
  }

  # ---- 合并输出 ----
  if (length(rows) == 0) {
    return(data.frame(Characteristics=character(), P=character(), Test=character(), check.names=FALSE))
  }
  out <- do.call(rbind, rows)
  all_cols <- c("Characteristics", lev_cols, "P", "Test")
  miss <- setdiff(all_cols, names(out)); if (length(miss)) out[miss] <- ""
  out <- out[, all_cols, drop = FALSE]
  rownames(out) <- NULL
  out
}
