#' @title Recode Character or factor Variables
#' 重新编码字符或因子变量
#'
#' @name recode.chr
#'
#' @description
#' \code{recode.chr()} Directly identifies variable categories Insert
#' \code{case_when()} code in RStudio to help quickly recoding character variables.
#'
#' \code{recode.chr()}直接识别变量类目在RStudio中插入\code{case_when()}代码，
#' 帮助快速重新编码。
#'
#' @usage \code{recode.chr(df$var)}
#' @param x A character or factor vector to be recoded.
#' @return Invisibly returns the input vector. Main purpose is side effect of
#'   generating recode code in RStudio.
#'
#'   返回\code{case_when()}结构的重编码代码。
#' @export
#' @importFrom rstudioapi isAvailable getActiveDocumentContext modifyRange
#' @importFrom dplyr case_when
#' @examples
#' # 创建示例数据
#' df <- data.frame(
#'   age = c(28, 20, 30, 35, 36, "> 80 year", "<20 year"),
#'   edu = c("college or above", "Below high school", "High school",
#'          "College or above", "Below high school", "High school",
#'          "College, or Above")
#' )
#'
#' \dontrun{
#' recode.chr(df$edu)
#' }
#'
#' # 生成的代码如下,
#'
#' # recode.chr(df$edu)
#' df$edu <- case_when(
#'   df$edu == "college or above" ~ "college or above",
#'   df$edu == "Below high school" ~ "Below high school",
#'   df$edu == "High school" ~ "High school",
#'   df$edu == "College or above" ~ "College or above",
#'   df$edu == "College, or Above" ~ "College, or Above",
#'   TRUE ~ NA_character_
#' )
#'
recode.chr <- function(x) {
  # 获取唯一值
  unique_values <- unique(na.omit(x))
  if (length(unique_values) == 0) return(invisible(x))

  # 生成代码
  var_name <- deparse(substitute(x))
  code <- paste0("# recode.chr(",var_name,")\n",var_name, " <- case_when(\n")
  for (val in unique_values) {
    code <- paste0(code, "  ", var_name, " == \"", val, "\" ~ \"", val, "\",\n")
  }
  code <- paste0(code, "  TRUE ~ NA_character_\n)\n")

  # RStudio环境处理
  if (rstudioapi::isAvailable()) {
    context <- rstudioapi::getActiveDocumentContext()

    # 获取当前执行的完整调用表达式
    current_call <- paste0("recode.chr(", var_name, ")")

    # 查找完全匹配的行
    target_lines <- which(trimws(context$contents) == current_call)

    if (length(target_lines) > 0) {
      # 替换第一个匹配的行（通常就是当前执行的）
      rstudioapi::modifyRange(
        rstudioapi::document_range(
          rstudioapi::document_position(target_lines[1], 1),
          rstudioapi::document_position(target_lines[1], Inf)
        ),
        code
      )
    } else {
      message("未找到匹配的recode.chr调用行")
    }
  } else {
    cat("# 生成的代码：\n", code, "\n")
  }

  invisible(x)
}

#' @title Recode non-numeric numeric variables
#' 重新编码非数值型的数值变量
#'
#' @name recode.num
#'
#' @description
#' \code{recode.chr()} Directly identifies non-numeric items Insert
#' \code{case_when()} code in RStudio to help quickly recoding numeric variables.
#'
#' \code{recode.chr()}直接识别非数字类目在RStudio中插入\code{case_when()}代码，
#' 帮助快速重新编码。
#'
#' @usage \code{recode.chr(df$var)}
#' @param x A vector to be converted to numeric
#' @return Invisibly returns the input vector. Main purpose is side effect of
#'   generating recode code in RStudio.
#'
#'   返回\code{case_when()}结构的重编码代码。
#' @export
#' @importFrom rstudioapi isAvailable getActiveDocumentContext modifyRange
#' @importFrom dplyr case_when
#' @examples
#' # 创建示例数据
#' df <- data.frame(
#'   age = c(28, 20, 30, 35, 36, "> 80 year", "<20 year"),
#'   edu = c("college or above", "Below high school", "High school",
#'          "College or above", "Below high school", "High school",
#'          "College, or Above")
#' )
#'
#' \dontrun{
#' recode.num(df$age)
#' }
#'
#' # 生成的代码如下,
#'
#' # recode.num(df$age)
#' df$age <- case_when(
#'   df$age == "> 80 year" ~ "> 80 year",
#'   df$age == "<20 year" ~ "<20 year",
#'   TRUE ~ df$age
#' ) %>% as.numeric()
#'
recode.num <- function(x) {
  var_name <- deparse(substitute(x))

  # 情况1：已经是数值型
  if (is.numeric(x)) {
    message(var_name, " 已经是数值型，无需编码")
    return(invisible(x))
  }

  # 检查是否可以全部转换为数值
  can_convert <- !any(is.na(suppressWarnings(as.numeric(x[!is.na(x)]))))

  # 情况2：可以全部转换为数值
  if (can_convert) {
    code <- paste0("# recode.num(", var_name, ")\n", var_name, " <- as.numeric(", var_name, ")")

    if (rstudioapi::isAvailable()) {
      context <- rstudioapi::getActiveDocumentContext()
      target_lines <- which(trimws(context$contents) == paste0("recode.num(", var_name, ")"))
      if (length(target_lines) > 0) {
        rstudioapi::modifyRange(
          rstudioapi::document_range(
            rstudioapi::document_position(target_lines[1], 1),
            rstudioapi::document_position(target_lines[1], Inf)
          ),
          code
        )
      }
    } else {
      cat(code, "\n")
    }

    message(var_name, " 完全由数值组成，无需编码，已生成转换代码")
    return(invisible(x))
  }

  # 情况3：需要编码非数值项
  unique_values <- unique(x[is.na(suppressWarnings(as.numeric(x))) & !is.na(x)])
  if (length(unique_values) == 0) return(invisible(x))

  code <- paste0("# recode.num(", var_name, ")\n", var_name, " <- case_when(\n")
  code <- paste0(code, paste0("  ", var_name, " == \"", unique_values, "\" ~ \"", unique_values, "\",", collapse = "\n"), "\n")
  code <- paste0(code, "  TRUE ~ ", var_name, "\n) %>% as.numeric()")

  if (rstudioapi::isAvailable()) {
    context <- rstudioapi::getActiveDocumentContext()
    target_lines <- which(trimws(context$contents) == paste0("recode.num(", var_name, ")"))
    if (length(target_lines) > 0) {
      rstudioapi::modifyRange(
        rstudioapi::document_range(
          rstudioapi::document_position(target_lines[1], 1),
          rstudioapi::document_position(target_lines[1], Inf)
        ),
        code
      )
    }
  } else {
    cat(code, "\n")
  }

  invisible(x)
}
