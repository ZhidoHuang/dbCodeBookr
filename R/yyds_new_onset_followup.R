#' 长格式转宽格式，生成疾病的事件状态和新发随访时间
#'
#' 将包含重复随访记录的长格式数据转换为受试者层面的宽格式数据，
#' 并对指定的多个疾病变量统一计算事件状态和随访时间。
#'
#' @param data 长格式数据框或 tibble。每行表示一位受试者在一个
#'   随访时间点的一条记录。
#'
#' @param disease_cols 字符向量，指定需要处理的疾病状态变量名。
#'   每个疾病变量应使用以下编码：
#'   \itemize{
#'     \item \code{1}：存在该疾病；
#'     \item \code{0}：不存在该疾病；
#'     \item \code{NA}：该次随访未测量或疾病状态缺失。
#'   }
#'
#' @param id_col 单个字符串，受试者唯一标识变量名。
#'   默认值为 \code{"id"}。
#'
#' @param visit_year_col 单个字符串，随访年份变量名。
#'   该变量与随访月份变量共同用于确定随访时间。
#'   默认值为 \code{"iyear"}。
#'
#' @param visit_month_col 单个字符串，随访月份变量名。
#'   默认值为 \code{"imonth"}。
#'
#' @param keep_year_col 单个字符串，需要保留在最终结果中的年份变量名。
#'   每位受试者保留其最早有效随访记录对应的该变量值。
#'   默认值为 \code{"year"}。
#'
#' @details
#' 函数首先使用随访年份和月份构造连续月份：
#'
#' \deqn{absolute\ month = visit\ year \times 12 + visit\ month}
#'
#' 每位受试者最早的有效随访年月被定义为统一随访起点。
#' 该起点与具体疾病状态是否缺失无关。
#'
#' 对每位受试者的每种疾病，按照以下规则计算：
#'
#' \enumerate{
#'   \item 如果该疾病至少出现过一次状态 \code{1}，
#'   则疾病事件状态记为 \code{1}，并以第一次出现 \code{1}
#'   的时间作为事件时间。
#'
#'   \item 如果第一次出现状态 \code{1} 的时间等于统一随访起点，
#'   则认为该受试者在基线时已经患有该疾病。此时疾病事件状态
#'   仍保留为 \code{1}，但对应的随访时间设为 \code{NA}。
#'
#'   \item 如果该疾病至少有一次非缺失测量，但整个随访期间
#'   始终未出现状态 \code{1}，则疾病事件状态记为 \code{0}，
#'   并以该疾病最后一次非缺失测量时间作为删失时间。
#'
#'   \item 如果该疾病在所有随访时间点均为缺失，
#'   则疾病事件状态和对应的随访时间均设为 \code{NA}。
#' }
#'
#' 随访时间的计算规则为：
#'
#' \itemize{
#'   \item 随访期间发生疾病：
#'   \code{首次出现 1 的时间 - 统一随访起点}；
#'
#'   \item 随访期间未发生疾病：
#'   \code{最后一次非缺失测量时间 - 统一随访起点}；
#'
#'   \item 基线已有疾病：
#'   疾病事件状态保留为 \code{1}，随访时间为 \code{NA}；
#'
#'   \item 所有疾病状态均缺失：
#'   疾病事件状态和随访时间均为 \code{NA}。
#' }
#'
#' 随访时间的单位为月。每个随访时间变量按照以下方式命名：
#'
#' \preformatted{
#' 原疾病变量名_followup
#' }
#'
#' 例如，疾病变量 \code{JB_heart_problem} 对应的随访时间变量为
#' \code{JB_heart_problem_followup}。
#'
#' @section 基线已有疾病的处理:
#' 基线已有疾病定义为：该疾病第一次出现状态 \code{1} 的时间，
#' 与该受试者统一随访起点相同。
#'
#' 基线已有疾病者的输出为：
#'
#' \itemize{
#'   \item 原疾病变量：\code{1}；
#'   \item 对应的 \code{*_followup}：\code{NA}。
#' }
#'
#' 因此，疾病状态变量表示是否观察到过疾病，而随访时间是否缺失
#' 可用于识别基线已有疾病者。但需要注意，疾病状态全程缺失者的
#' 随访时间也为 \code{NA}，其疾病状态变量则同样为 \code{NA}。
#'
#' @section 缺失值处理:
#' ID、随访年份或随访月份缺失的记录不会参与计算，因为无法确定
#' 受试者或随访时间。
#'
#' 疾病状态缺失的记录不会在计算统一随访起点前被删除，因为统一
#' 随访起点与具体疾病状态无关。
#'
#' 但是，某疾病状态缺失的随访时间点不会作为该疾病的删失时间。
#' 无事件者的删失时间仅取该疾病最后一次非缺失测量的时间。
#'
#' 如果受试者某疾病的全部状态均缺失，则该疾病事件状态和随访时间
#' 均返回 \code{NA}。
#'
#' @section 年份变量和结果排序:
#' 每位受试者保留最早有效随访年月对应的
#' \code{keep_year_col} 值。
#'
#' 如果最早随访年月存在多条记录，则保留原始数据中位置最靠前
#' 的记录对应的年份值。
#'
#' 最终结果首先按照 \code{keep_year_col} 排序；当年份相同时，
#' 按受试者在原始数据中第一次出现的顺序排列。
#'
#' @section 数据编码要求:
#' 疾病变量应仅包含 \code{0}、\code{1} 和 \code{NA}。
#' 其他非缺失值不会被识别为事件，但会被视为一次非缺失测量，
#' 可能导致不符合预期的事件状态或删失时间。
#'
#' @return 一个宽格式 tibble，每位受试者一行。
#'
#' 返回列依次包括：
#'
#' \itemize{
#'   \item 受试者唯一标识变量；
#'   \item 保留的年份变量；
#'   \item 每个原疾病事件状态变量；
#'   \item 每个疾病对应的 \code{*_followup} 随访时间变量。
#' }
#'
#' 疾病事件状态变量的含义为：
#'
#' \itemize{
#'   \item \code{1}：至少观察到一次疾病状态为 \code{1}，
#'   包括基线已有疾病；
#'   \item \code{0}：至少有一次非缺失测量，但始终未观察到
#'   疾病状态为 \code{1}；
#'   \item \code{NA}：该疾病在所有随访时间点均缺失。
#' }
#'
#' 随访时间变量的含义为：
#'
#' \itemize{
#'   \item 非负数：从统一随访起点到事件或删失时间的月数；
#'   \item \code{NA}：基线已有疾病，或该疾病所有测量均缺失。
#' }
#'
#' @examples
#' \dontrun{
#' disease_vars <- c(
#'   "JB_heart_problem",
#'   "JB_stroke"
#' )
#'
#' result <- yyds_new_onset_followup(
#'   data = df_long,
#'   disease_cols = disease_vars
#' )
#'
#' result <- yyds_new_onset_followup(
#'   data = df_long,
#'   disease_cols = c(
#'     "JB_heart_problem",
#'     "JB_stroke",
#'     "JB_diabetes"
#'   ),
#'   id_col = "id",
#'   visit_year_col = "iyear",
#'   visit_month_col = "imonth",
#'   keep_year_col = "year"
#' )
#'
#' # 基线已有疾病者：
#' # 疾病状态为 1，但对应的随访时间为 NA
#' result |>
#'   dplyr::filter(
#'     JB_heart_problem == 1,
#'     is.na(JB_heart_problem_followup)
#'   )
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr across all_of arrange case_when filter group_by
#' @importFrom dplyr left_join mutate rename_with row_number select summarise
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom rlang .data
#'
#' @export
yyds_new_onset_followup <- function(
    data,
    disease_cols,
    id_col = "id",
    visit_year_col = "iyear",
    visit_month_col = "imonth",
    keep_year_col = "year"
) {

  # ============================================================
  # 总体逻辑
  # ============================================================
  # 1. 每位受试者第一次出现的时间为统一随访起点，
  #    与疾病状态是否缺失无关。
  #
  # 2. 只要出现疾病状态，事件记为 1：
  #
  # 3. 若某疾病始终未出现状态 1：
  #    以该疾病最后一次非缺失测量时间为删失点。
  #
  # 4. 随访时间 = 终点 - 统一随访起点，单位为月。
  #    如果基线已出现疾病，则随访时间为NA
  #
  # 5. 原疾病变量表示事件状态；
  #    “原疾病变量_followup”表示随访月数。

  # ============================================================
  # 0. 检查输入
  # ============================================================

  required_cols <- c(
    id_col,
    visit_year_col,
    visit_month_col,
    keep_year_col,
    disease_cols
  )

  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      "以下变量不存在：",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (length(disease_cols) == 0) {
    stop("disease_cols 不能为空。")
  }

  if (anyDuplicated(disease_cols)) {
    stop("disease_cols 中存在重复变量名。")
  }

  # 最终输出列顺序
  output_order <- c(
    id_col,
    keep_year_col,
    as.vector(
      rbind(
        disease_cols,
        paste0(disease_cols, "_followup")
      )
    )
  )

  # ============================================================
  # 1. 整理原始数据
  # ============================================================

  data_prepared <- data %>%
    mutate(
      # 保存原始行顺序，用于最终恢复 ID 的原始出现顺序
      .row_order = row_number(),

      # 将年月转换为连续月份
      .abs_month =
        .data[[visit_year_col]] * 12 +
        .data[[visit_month_col]]
    ) %>%
    filter(
      # ID 或随访年月缺失时无法计算随访时间
      !is.na(.data[[id_col]]),
      !is.na(.abs_month)
    )

  # ============================================================
  # 2. 提取受试者层面的统一信息
  # ============================================================

  subject_info <- data_prepared %>%
    group_by(
      across(all_of(id_col))
    ) %>%
    summarise(
      # ID 在原始数据中第一次出现的位置
      .id_order = min(.row_order),

      # 统一随访起点：
      # 该受试者最早的有效随访年月
      .first_obs = min(.abs_month),

      # 保留最早随访记录对应的 year
      # 若最早年月有多条记录，取原始位置最靠前的一条
      "{keep_year_col}" := {
        index <- order(.abs_month, .row_order)[1]
        .data[[keep_year_col]][index]
      },

      .groups = "drop"
    )

  # ============================================================
  # 3. 统一处理所有疾病
  # ============================================================

  disease_result <- data_prepared %>%

    # 将多个疾病变量转成长格式
    pivot_longer(
      cols = all_of(disease_cols),
      names_to = ".disease",
      values_to = ".status"
    ) %>%

    # 每位受试者、每种疾病分别汇总
    group_by(
      across(all_of(id_col)),
      .disease
    ) %>%

    summarise(
      # 当前疾病是否至少有一次非缺失测量
      .has_measurement = any(!is.na(.status)),

      # 当前疾病是否至少出现过一次状态 1
      .has_event = any(.status == 1, na.rm = TRUE),

      # 当前疾病最后一次非缺失测量时间
      # 无事件时作为删失时间
      .last_obs = if (any(!is.na(.status))) {
        max(.abs_month[!is.na(.status)])
      } else {
        NA_real_
      },

      # 当前疾病第一次出现状态 1 的时间
      .event_abs = if (any(.status == 1, na.rm = TRUE)) {
        min(.abs_month[.status == 1], na.rm = TRUE)
      } else {
        NA_real_
      },

      .groups = "drop"
    ) %>%

    # 加入每位受试者的统一随访起点
    left_join(
      subject_info %>%
        dplyr::select(
          all_of(id_col),
          .first_obs
        ),
      by = id_col
    ) %>%

    mutate(
      # 是否为基线已有疾病：
      # 第一次出现状态 1 的时间等于统一随访起点
      .baseline_event =
        .has_event &
        !is.na(.event_abs) &
        .event_abs == .first_obs,

      # 疾病事件状态：
      # 1  = 至少出现过一次状态 1，包括基线已有疾病
      # 0  = 有非缺失测量，但始终未出现状态 1
      # NA = 该疾病所有测量均缺失
      event = case_when(
        !.has_measurement ~ NA_integer_,
        .has_event        ~ 1L,
        TRUE              ~ 0L
      ),

      # 随访时间，单位为月
      followup = case_when(
        # 该疾病所有状态均缺失
        is.na(event) ~ NA_real_,

        # 基线已经为 1：
        # 疾病状态保留为 1，但随访时间设为 NA
        .baseline_event ~ NA_real_,

        # 随访期间发生事件
        event == 1L ~ .event_abs - .first_obs,

        # 始终未发生事件：
        # 最后一次非缺失测量时间作为删失点
        event == 0L ~ .last_obs - .first_obs
      )
    ) %>%

    dplyr::select(
      all_of(id_col),
      .disease,
      event,
      followup
    ) %>%

    # ==========================================================
  # 4. 转回宽格式
  # ==========================================================

  pivot_wider(
    names_from = .disease,
    values_from = c(event, followup),
    names_glue = "{.disease}_{.value}"
  ) %>%

    # 将事件变量恢复成原疾病变量名
    # 例如：
    # JB_heart_problem_event -> JB_heart_problem
    rename_with(
      ~ sub("_event$", "", .x),
      ends_with("_event")
    )

  # ============================================================
  # 5. 加回 year、排序并返回
  # ============================================================

  disease_result %>%
    left_join(
      subject_info %>%
        dplyr::select(
          all_of(id_col),
          all_of(keep_year_col),
          .id_order
        ),
      by = id_col
    ) %>%

    # 先按 year 排序；
    # 同一年内保持 ID 在原始数据中的首次出现顺序
    arrange(
      .data[[keep_year_col]],
      .id_order
    ) %>%

    # 调整最终列顺序，并删除临时排序变量
    dplyr::select(
      all_of(output_order)
    )
}
