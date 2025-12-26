#' 批量将农历日期转换为公历日期（支持行筛选与并行）
#'
#' 使用 hongkong::lunarCal 批量转换 CHARLS 农历出生日期为公历日期。
#' 根据农历年、月、日变量及行筛选条件，对指定样本生成公历出生日期。
#'
#' @param df 要处理的数据框（data.frame 或可转换为 data.table）。
#' @param y_col 农历“年”列名，默认为 `"ba002_1"`。
#' @param m_col 农历“月”列名，默认为 `"ba002_2"`。
#' @param d_col 农历“日”列名，默认为 `"ba002_3"`。
#' @param out_col 输出的公历日期列名，默认为 `"birth_gregorian"`；
#' 若列不存在将自动创建，未被筛选的行保持为 NA。
#' @param row_filter 行筛选条件，用于指定哪些行需要进行转换。
#' 支持以下形式：
#' \itemize{
#'   \item \code{NULL}：处理全部行；
#'   \item \code{logical} 向量：长度必须等于 \code{nrow(df)}，NA 将视为 FALSE；
#'   \item \code{expression / quote()}：在 data.table 环境中求值；
#'   \item \code{character}：可解析的单行表达式字符串（如 \code{'ba003 == "Lunar calendar"'}）。
#' }
#' @param ignore_leap 是否忽略农历闰月，传递给 \code{hongkong::lunarCal()}，
#' 默认为 TRUE。
#' @param workers 并行计算使用的进程数，默认为 6；
#' 设为 1 时使用串行计算。
#' @param chunk_size 每个并行任务中包含的“唯一日期”数量；
#' 若为 NULL，则根据 \code{chunks_per_worker} 自动估算。
#' @param chunks_per_worker 自动分块时，每个 worker 的目标 chunk 数，
#' 默认为 24。
#' @param load_balance 是否使用动态负载均衡；
#' TRUE 使用 \code{parLapplyLB()}，FALSE 使用 \code{parLapply()}。
#'
#' @return 返回一个 list，包含：
#' \itemize{
#'   \item \code{data}：写入公历日期后的数据框；
#'   \item \code{bad_unique}：转换失败的唯一农历日期（字符向量，格式 \code{"YYYY-MM-DD"}）。
#' }
#'
#' @details
#' 函数执行流程如下：
#' \enumerate{
#'   \item 根据 \code{row_filter} 生成严格的行索引（NA 视为 FALSE）；
#'   \item 仅对被筛选且年/月/日完整的行提取唯一农历日期组合；
#'   \item 对唯一日期进行农历→公历转换（支持并行）；
#'   \item 将结果映射回原始行，仅写回被筛选的行；
#'   \item 自动统计缺失情况与转换失败情况，并打印摘要信息。
#' }
#'
#' 统计输出中：
#' \itemize{
#'   \item \strong{Rows processed}：满足 row_filter 的行数；
#'   \item \strong{Missing(y/m/d any NA)}：被筛选行中年/月/日任一缺失的行数；
#'   \item \strong{Error Lunar dates}：年/月/日完整但转换失败的行数（按行计）；
#'   \item \strong{Unique error dates}：转换失败的唯一农历日期数量。
#' }
#'
#' 函数默认启用计时功能，执行结束时会自动输出总耗时。
#'
#' @examples
#' \dontrun{
#' # expression / quote（推荐）
#' res <- yyds_lunar_to_gregorian(
#'   data,
#'   y_col   = "ba002_1",
#'   m_col   = "ba002_2",
#'   d_col   = "ba002_3",
#'   out_col = "birth_gregorian",
#'   row_filter = quote(ba003 == "Lunar calendar")
#' )
#'
#' # 字符串形式（不要写 data$）
#' res <- yyds_lunar_to_gregorian(
#'   data,
#'   y_col   = "ba002_1",
#'   m_col   = "ba002_2",
#'   d_col   = "ba002_3",
#'   out_col = "birth_gregorian",
#'   row_filter = 'ba003 == "Lunar calendar"'
#' )
#'
#' # 逻辑向量（NA 自动视为 FALSE）
#' res <- yyds_lunar_to_gregorian(
#'   data,
#'   y_col   = "ba002_1",
#'   m_col   = "ba002_2",
#'   d_col   = "ba002_3",
#'   out_col = "birth_gregorian",
#'   row_filter = data$ba003 == "Lunar calendar"
#' )
#' }
#'
#' @importFrom data.table as.data.table setkey
#' @importFrom parallel makeCluster stopCluster parLapply parLapplyLB
#' @export
yyds_lunar_to_gregorian <- function(df,
                                     y_col   = "ba002_1",
                                     m_col   = "ba002_2",
                                     d_col   = "ba002_3",
                                     out_col = "birth_gregorian",
                                     row_filter = NULL,
                                     ignore_leap = TRUE,
                                     workers = 6,
                                     chunk_size = NULL,
                                     chunks_per_worker = 24L,
                                     load_balance = TRUE) {


  t0 <- proc.time()[3]
  on.exit({
    t1 <- proc.time()[3]
    cat(sprintf("Elapsed: %.3f sec\n", t1 - t0))
  }, add = TRUE)

  stopifnot(is.data.frame(df))
  for (v in c(y_col, m_col, d_col)) if (!v %in% names(df)) stop("Column not found: ", v)

  if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required.")
  if (!requireNamespace("hongkong", quietly = TRUE)) stop("Package 'hongkong' is required.")
  dt <- data.table::as.data.table(df)

  # =========================================================
  # Strict row_filter: NULL / logical / quote(expr) / "expr"
  # - must return logical vector of length nrow(dt)
  # - NA will be treated as FALSE
  # =========================================================
  .eval_row_filter_strict <- function(dt, row_filter) {
    n <- nrow(dt)

    if (is.null(row_filter)) {
      return(rep(TRUE, n))
    }

    if (is.logical(row_filter)) {
      if (length(row_filter) != n) stop("row_filter (logical) must have length nrow(df).")
      out <- row_filter
      out[is.na(out)] <- FALSE
      return(out)
    }

    if (is.character(row_filter)) {
      if (length(row_filter) != 1L) stop("row_filter (character) must be a single string.")
      expr <- try(parse(text = row_filter)[[1]], silent = TRUE)
      if (inherits(expr, "try-error")) {
        stop("row_filter string cannot be parsed. Provided: ", row_filter)
      }
    } else {
      expr <- row_filter
    }

    out <- dt[, eval(expr)]
    if (!is.logical(out) || length(out) != n) {
      stop("row_filter must evaluate to a logical vector of length nrow(df).")
    }
    out[is.na(out)] <- FALSE
    out
  }

  # ===== row filter (strict) =====
  idx <- .eval_row_filter_strict(dt, row_filter)

  # ensure out_col exists (so unselected rows remain NA)
  if (!out_col %in% names(dt)) dt[, (out_col) := as.Date(NA)]

  # ===== stats: missing =====
  total_rows <- sum(idx)
  missing_count <- dt[idx & (is.na(get(y_col)) | is.na(get(m_col)) | is.na(get(d_col))), .N]

  # ===== uniq ymd =====
  uniq <- unique(
    dt[idx & !is.na(get(y_col)) & !is.na(get(m_col)) & !is.na(get(d_col)),
       .(yy = as.integer(get(y_col)),
         mm = as.integer(get(m_col)),
         dd = as.integer(get(d_col)))]
  )

  # helper: safe conversion (returns list(date=..., err=0/1))
  lunar_to_gregorian_safe <- function(y, m, d) {
    out <- try(
      hongkong::lunarCal(c(Year = y, Month = m, Day = d), ignoreLeap = ignore_leap),
      silent = TRUE
    )
    if (inherits(out, "try-error") || is.null(out)) return(list(date = as.Date(NA), err = 1L))
    if (inherits(out, "Date")) return(list(date = out, err = 0L))
    if (is.numeric(out) && length(out) >= 1) return(list(date = as.Date(out[1], origin = "1970-01-01"), err = 0L))
    if (is.character(out) && length(out) >= 1) return(list(date = as.Date(out[1]), err = 0L))
    list(date = as.Date(NA), err = 1L)
  }

  # no work case
  if (nrow(uniq) == 0L) {
    message(sprintf(
      "\nRows processed = %d\nMissing(y/m/d any NA) = %d\nError(y/m/d present but failed) = 0\nBad unique dates = 0\n",
      total_rows, missing_count
    ))
    return(list(data = as.data.frame(dt), bad_unique = character(0)))
  }

  # ===== chunking =====
  n <- nrow(uniq)
  workers <- as.integer(max(1L, workers))

  if (is.null(chunk_size)) {
    target_chunks <- max(workers * as.integer(chunks_per_worker), workers)
    chunk_size <- max(1L, ceiling(n / target_chunks))
  } else {
    chunk_size <- as.integer(max(1L, chunk_size))
  }

  grp <- ceiling(seq_len(n) / chunk_size)
  chunks <- split(uniq, grp)

  message(sprintf("uniq=%d | workers=%d | chunk_size=%d | chunks=%d | ignore_leap=%s",
                  n, workers, chunk_size, length(chunks), ignore_leap))

  fun_chunk <- function(sub) {
    lapply(seq_len(nrow(sub)), function(k) {
      lunar_to_gregorian_safe(sub$yy[k], sub$mm[k], sub$dd[k])
    })
  }

  # ===== compute =====
  if (workers == 1L) {
    res_chunks <- lapply(chunks, fun_chunk)
  } else {
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, { library(hongkong) })
    parallel::clusterExport(cl, varlist = c("lunar_to_gregorian_safe", "ignore_leap"), envir = environment())

    if (isTRUE(load_balance)) {
      res_chunks <- parallel::parLapplyLB(cl, chunks, fun_chunk)
    } else {
      res_chunks <- parallel::parLapply(cl, chunks, fun_chunk)
    }
  }

  res <- unlist(res_chunks, recursive = FALSE, use.names = FALSE)
  greg_chr <- vapply(res, function(x) as.character(x$date), character(1L))
  err_vec  <- vapply(res, function(x) as.integer(x$err), integer(1L))

  uniq[, greg := data.table::as.IDate(greg_chr)]
  uniq[, err  := err_vec]

  # ===== error outputs =====
  bad_unique <- uniq[err == 1L, sprintf("%04d-%02d-%02d", yy, mm, dd)]
  bad_unique <- sort(unique(bad_unique))

  # row-level error_count: only among idx & complete ymd
  i_dt <- dt[idx & !is.na(get(y_col)) & !is.na(get(m_col)) & !is.na(get(d_col)),
             .(yy = as.integer(get(y_col)),
               mm = as.integer(get(m_col)),
               dd = as.integer(get(d_col)))]

  error_count <- uniq[i_dt, on = .(yy, mm, dd)][err == 1L, .N]

  message(sprintf(
    "
    Rows processed = %d
    Missing(y/m/d any NA) = %d
    Error Lunar dates = %d
    Unique error dates = %d
    ",
    total_rows, missing_count, error_count, length(bad_unique)
  ))

  # ===== write back (ONLY idx rows) =====
  data.table::setkey(uniq, yy, mm, dd)

  i_write <- dt[idx, .(yy = as.integer(get(y_col)),
                       mm = as.integer(get(m_col)),
                       dd = as.integer(get(d_col)))]

  dt[idx, (out_col) := uniq[i_write, on = .(yy, mm, dd), greg]]

  list(
    data = as.data.frame(dt),
    bad_unique = bad_unique
  )
}
