#' 配合dbcodebook笔记，解压合并数据（analysis_db.xlsx）和字典（analysis_codebook.xlsx）
#'
#' @description
#' 自动完成以下操作：
#' 1. 解压工作路径下所有 ZIP 文件
#' 2. 递归搜索并读取所有包含 **analysis_db** 的 **xlsx** 文件，按对应数据库的 ID 键全连接合并
#' 3. 递归搜索并读取所有包含 **analysis_codebook** 的 **xlsx** 文件，合并为变量字典
#' 4. 输出 db.csv、db.rds、codebook.csv 到工作路径
#'
#' @param work_dir 字符，工作路径。默认为 NULL，即使用当前 RStudio 脚本所在目录；
#'                 若不在 RStudio 中运行，则使用当前工作目录 getwd()。
#'                 也可手动指定路径，如 "D:/project/data"
#' @param db 字符，数据库类型。可选：
#'           \itemize{
#'             \item "nhanes" - NHANES 数据库，ID 键为 "SEQN"，固定变量为 "YEAR"
#'             \item "charls" - CHARLS 数据库，ID 键为 "ID"，固定变量为 c("id", "year")
#'             \item "elsa"   - ELSA 数据库，ID 键为 "ID"，固定变量为 c("idauniq", "Wave")
#'           }
#'
#' @return 不可见返回一个列表，包含两个数据框：
#'         \itemize{
#'           \item db - 合并后的数据库，ID 列为第一列，时间变量紧随其后
#'           \item codebook - 合并后的变量字典，已排除 ID 列和时间变量条目
#'         }
#'
#' @note
#' 依赖包：openxlsx、tidyverse。若未安装会自动安装。
#' ZIP 解压后不会删除原文件。
#' 若目录下无 ZIP 文件，会提示但不会中断运行。
#'
#' **重要：待读取的文件命名必须包含 "analysis_db" 或 "analysis_codebook"，且为 xlsx 格式。**
#'
#' @export
#'
#' @examples
#' # NHANES 数据库（默认）
#' yyds_dbcodebook_note()
#'
#' # CHARLS 数据库
#' yyds_dbcodebook_note(db = "charls")
#'
#' # ELSA 数据库，指定工作路径
#' yyds_dbcodebook_note("D:/ELSA/data", db = "elsa")
#'
#' # 接收返回值
#' result <- yyds_dbcodebook_note(db = "charls")
#' db <- result$db
#' codebook <- result$codebook
yyds_dbcodebook_note <- function(work_dir = NULL, db = c("nhanes", "charls", "elsa")) {

  # 匹配数据库类型
  db <- match.arg(db)

  # 根据数据库类型设置 ID 键和时间变量
  if (db == "nhanes") {
    id_col <- "SEQN"
    time_cols <- "YEAR"
  } else if (db == "charls") {
    id_col <- "ID"
    time_cols <- c("id", "year")
  } else if (db == "elsa") {
    id_col <- "ID"
    time_cols <- c("idauniq", "Wave")
  }

  # 所有需要从 codebook 排除的列（ID + 时间变量）
  exclude_cols <- unique(c(id_col, time_cols))

  cat("当前数据库类型:", db, "\n")
  cat("链接键:", id_col, "; ")
  cat("固定变量:", paste(time_cols, collapse = ", "), "\n\n")

  # 自动检查并安装缺失的包
  required_packages <- c("openxlsx", "tidyverse")

  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      cat("📦 正在安装缺失的包:", pkg, "...\n")
      install.packages(pkg)
    }
  }

  # 设置当前文件所在位置，为工作路径
  if (is.null(work_dir)) {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  } else {
    setwd(work_dir)
  }

  cat("当前工作路径:", getwd(), "\n\n\n")

  # 获取所有 zip 文件
  zip_files <- list.files(pattern = "\\.zip$", ignore.case = TRUE)
  # 解压
  if (length(zip_files) == 0) {
    cat("❌ 当前目录下没有找到 zip 文件\n")
  } else {
    cat("找到", length(zip_files), "个 zip 文件:\n")

    for (f in zip_files) {
      # 创建文件夹名（去掉.zip后缀）
      folder <- sub("\\.zip$", "", f, ignore.case = TRUE)

      # 解压
      unzip(f, exdir = folder)
      cat("解压:", f, "\n")
    }
    cat("解压完成！\n\n\n")
  }


  # 读取
  library(openxlsx)

  # 获取当前路径下所有子文件夹中包含 "analysis_db" 的 xlsx 文件
  file_list <- list.files(
    path = ".",                           # 当前路径
    pattern = "analysis_db.*\\.xlsx$",    # 匹配包含 analysis_db 的 xlsx 文件
    recursive = TRUE,                    # 递归搜索子文件夹
    full.names = TRUE                    # 返回完整路径
  )

  # 读取所有文件到列表中
  db_list <- list()

  cat("db 读取ing 请等待…\n")

  for (file in file_list) {
    # 提取文件名（不含路径和扩展名）作为列表元素名称
    file_name <- tools::file_path_sans_ext(basename(file))

    # 使用 openxlsx 读取 Excel 文件
    db_list[[file_name]] <- read.xlsx(file)
    # 计算排除 ID 列和时间变量后的列数
    col_names <- names(db_list[[file_name]])
    cols_excluded <- col_names[!col_names %in% exclude_cols]
    n_cols_excluded <- length(cols_excluded)

    cat("已读取:", n_cols_excluded, "个变量 ——", file_name, "\n")
  }

  cat("共读取", length(db_list), "个 db\n")

  suppressMessages(library(tidyverse))

  # 全连接合并所有数据框，使用对应数据库的 ID 键
  db <- reduce(db_list, full_join, by = id_col)

  # 处理时间变量：合并同名的 .x .y .x.x 等后缀列
  for (tc in time_cols) {
    # 跳过与 id_col 同名的列
    if (tc == id_col) next

    # 找出当前时间变量的所有列（含本体和后缀）
    pattern_body <- paste0("^", tc, "$")
    pattern_suffix <- paste0("^", tc, "\\.")

    body_col <- names(db)[grepl(pattern_body, names(db))]
    suffix_cols <- names(db)[grepl(pattern_suffix, names(db))]

    tc_all <- c(body_col, suffix_cols)

    if (length(tc_all) > 1) {
      # 用 coalesce 合并
      db[[tc]] <- reduce(db[, tc_all, drop = FALSE], coalesce)

      # 删除后缀列
      db <- db[, !names(db) %in% suffix_cols]
    }
  }

  # 收集实际存在于 db 中的时间列（去重、去 ID）
  time_cols_exist <- intersect(time_cols, names(db))
  time_cols_exist <- setdiff(time_cols_exist, id_col)

  # 其余列
  other_cols <- setdiff(names(db), c(id_col, time_cols_exist))

  # 排序列
  db <- db[, c(id_col, time_cols_exist, other_cols)]

  cat("db合并完成\n\n\n")

  # 获取当前路径下所有子文件夹中包含 "analysis_codebook" 的 xlsx 文件
  file_list <- list.files(
    path = ".",                                    # 当前路径
    pattern = "analysis_codebook.*\\.xlsx$",       # 匹配包含 analysis_codebook 的 xlsx 文件
    recursive = TRUE,                              # 递归搜索子文件夹
    full.names = TRUE                              # 返回完整路径
  )

  # 读取所有文件到列表中
  codebook_list <- list()

  cat("codebook 读取ing 请等待…\n")

  for (file in file_list) {
    # 提取文件名（不含路径和扩展名）作为列表元素名称
    file_name <- tools::file_path_sans_ext(basename(file))

    # 使用 openxlsx 读取 Excel 文件
    codebook_list[[file_name]] <- read.xlsx(file)
    codebook_list[[file_name]] <- codebook_list[[file_name]][
      !codebook_list[[file_name]]$Variable %in% exclude_cols,
    ]

    n_row_codebook <- nrow(codebook_list[[file_name]])

    cat("已读取:", n_row_codebook, "个变量字典 ——", file_name, "\n")
  }

  cat("共读取", length(codebook_list), "个 codebook\n")

  codebook <- bind_rows(codebook_list)

  cat("codebook合并完成\n\n\n")

  cat("还没结束, 正在写出, db和codebook\n")

  write.csv(db, "db.csv", row.names = FALSE, na = "")
  write.csv(codebook, "codebook.csv", row.names = FALSE, na = "")
  saveRDS(db,"db.rds")

  cat("\n已完成, db（csv和rds两个格式, rds读取更快）,\n返回[", getwd(), "] 进行查看\n")

  invisible(list(db = db, codebook = codebook))
}
