#' 识别变量类型（正态/非正态连续，分类变量）
#'
#'   - 偏度(Skewness)：0为对称，>0右偏，<0左偏
#'   - 峰度(Kurtosis)：3为正态，>3尖峰，<3低峰
#'   - 经验法则：偏度绝对值<2且峰度接近3(通常2-4之间)可认为近似正态
#'
#' @param data 待分类的数据框
#' @param skew_threshold 偏度阈值，绝对值小于该值的变量视为对称分布（默认：2）
#' @param kurtosis_threshold 超额峰度阈值，绝对值小于该值的变量视为近似正态（默认：4）
#' @param min_unique_values 连续变量所需的最小唯一值数量（默认：10）
#'
#' @return 返回包含两个组件的列表：
#' \itemize{
#'   \item \code{result_table}: 详细结果数据框，包含：
#'   \itemize{
#'     \item Variable: 变量名
#'     \item Type: 分类类型("categorical"/"normal"/"non_normal")
#'     \item Skewness: 偏度值(仅连续变量)
#'     \item Kurtosis: 峰度值(仅连续变量)
#'     \item Excess_Kurtosis: 超额峰度(峰度-3，仅连续变量)
#'     \item Valid_N: 有效观测值数量
#'     \item NA_Count: 缺失值数量
#'     \item Unique_Values: 唯一值数量
#'   }
#'   \item \code{variable_types}: 按类型分组的变量名列表：
#'   \itemize{
#'     \item categorical: 分类变量名
#'     \item normal: 正态连续变量名
#'     \item non_normal: 非正态连续变量名
#'   }
#' }
#'
#' @examples
#' \donttest{
#' # 基本用法
#' data(iris)
#' result <- yyds_type(iris)
#'
#' # 查看正态分布变量
#' print(result$variable_types$normal)
#'
#' # 查看完整结果表
#' View(result$result_table)
#' }
#'
#' @note 函数使用经验法则：
#' - 分类变量判定：非数值型或唯一值数 < min_unique_values
#' - 正态性判定：同时满足偏度<阈值且超额峰度<阈值
#'
#' @importFrom moments skewness kurtosis
#' @export
yyds_type <- function(data,
                       skew_threshold = 2,
                       kurtosis_threshold = 4,
                       min_unique_values = 10) {

  # 检查输入是否为数据框
  if (!is.data.frame(data)) {
    stop("输入必须是数据框")
  }

  # 初始化结果表格
  result_table <- data.frame(
    Variable = names(data),
    Type = NA_character_,          # 分类结果（"categorical"/"normal"/"non_normal"）
    Skewness = NA_real_,           # 偏度（仅连续变量）
    Kurtosis = NA_real_,           # 峰度（仅连续变量）
    Excess_Kurtosis = NA_real_,    # 超额峰度（仅连续变量）
    Valid_N = NA_integer_,         # 有效样本量（所有变量）
    NA_Count = NA_integer_,        # 缺失值数（所有变量）
    Unique_Values = NA_integer_,   # 唯一值数（所有变量）
    stringsAsFactors = FALSE
  )

  # 遍历每个变量
  for (i in seq_along(data)) {
    x <- data[[i]]
    var_name <- names(data)[i]

    # 处理缺失值并计算基本统计量
    x_clean <- na.omit(x)
    n_valid <- length(x_clean)
    n_na <- length(x) - n_valid
    n_unique <- length(unique(x_clean))

    # 填充所有变量共有的统计量
    result_table$Valid_N[i] <- n_valid
    result_table$NA_Count[i] <- n_na
    result_table$Unique_Values[i] <- n_unique

    # 检查是否为数值型变量
    if (!is.numeric(x)) {
      result_table$Type[i] <- "categorical"
      next
    }

    # 检查样本量和唯一值数是否达标
    if (n_unique < min_unique_values) {
      result_table$Type[i] <- "categorical"
      next
    }

    # 计算偏度和峰度（仅连续变量）
    skew <- moments::skewness(x_clean)
    kurt <- moments::kurtosis(x_clean)  # 注意：正态分布的峰度=3

    # 判断是否近似正态
    is_normal <- abs(skew) < skew_threshold &
      abs(kurt - 3) < kurtosis_threshold

    # 填充连续变量的统计量和分类结果
    result_table$Type[i] <- ifelse(is_normal, "normal", "non_normal")
    result_table$Skewness[i] <- skew
    result_table$Kurtosis[i] <- kurt
    result_table$Excess_Kurtosis[i] <- kurt - 3
  }

  # 提取三类变量名
  variable_types <- list(
    categorical = result_table$Variable[result_table$Type == "categorical"],
    normal = result_table$Variable[result_table$Type == "normal"],
    non_normal = result_table$Variable[result_table$Type == "non_normal"]
  )

  # 返回结构化结果
  list(
    result_table = result_table,
    variable_types = variable_types
  )
}
