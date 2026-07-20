test_that("generate_var_details keeps default output unchanged", {
  fixture <- data.frame(
    period = c("T1", "T1", "T2", "T2"),
    group = c("A", "B", "A", NA),
    value = c(1, 2, 3, 4),
    stringsAsFactors = FALSE
  )

  expect_identical(
    generate_var_details(fixture),
    generate_var_details(fixture, show_cycle_heatmap = FALSE)
  )
  expect_false(any(grepl("dbcb-var-cycle-table", generate_var_details(fixture), fixed = TRUE)))
  expect_false(any(grepl("dbcb-var-distribution-nav", generate_var_details(fixture), fixed = TRUE)))
})

test_that("generate_var_details keeps the complete category table in one scroller", {
  fixture <- data.frame(
    period = rep(c("P1", "P2"), each = 4),
    group = c(
      "A very long first category",
      "A very long second category",
      "A very long third category",
      "A very long fourth category",
      "A very long first category",
      "A very long second category",
      "A very long third category",
      "A very long fourth category"
    ),
    stringsAsFactors = FALSE
  )

  details <- generate_var_details(
    fixture,
    show_distribution_nav = TRUE,
    show_cycle_heatmap = TRUE,
    cycle_col = "period",
    cycle_order = c("P1", "P2")
  )
  html <- details[[2]]

  expect_equal(length(gregexpr("class='dbcb-var-distribution-wrap'", html, fixed = TRUE)[[1]]), 1)
  expect_equal(length(gregexpr("class='dbcb-var-distribution-nav'", html, fixed = TRUE)[[1]]), 1)
  expect_match(html, "data-distribution-kind='category'")
  expect_match(html, "data-scroll-item='td'")
  expect_match(html, "aria-label='向左查看类别分布表'")
  expect_match(html, "aria-label='向右查看类别分布表'")
  expect_match(html, "dbcb-var-distribution-wrap\\{[^}]*overflow-x:auto")
  expect_match(html, "dbcb-var-distribution-wrap::-webkit-scrollbar\\{display:none")
  expect_match(html, "selector=wrap\\.getAttribute\\('data-scroll-item'\\)")

  table_html <- regmatches(
    html,
    regexpr(
      "<table class='fixed-table'>.*?</table>",
      html,
      perl = TRUE
    )
  )
  expect_match(table_html, "bar-container")
  expect_match(table_html, "33\\.3%|25%")
  expect_match(table_html, "A very long first category")
  expect_false(grepl("dbcb-var-category-layout", html, fixed = TRUE))
  expect_false(grepl("dbcb-var-category-bars", html, fixed = TRUE))

  expect_equal(length(gregexpr("class='dbcb-var-cycle-wrap'", html, fixed = TRUE)[[1]]), 1)
})

test_that("generate_var_details gives a continuous histogram its own scroller", {
  fixture <- data.frame(
    period = rep(c("P1", "P2"), each = 50),
    value = seq_len(100)
  )

  details <- generate_var_details(
    fixture,
    show_hist = TRUE,
    hist_binwidth = 5,
    show_distribution_nav = TRUE,
    show_cycle_heatmap = TRUE,
    cycle_col = "period",
    cycle_order = c("P1", "P2")
  )
  html <- details[[2]]

  expect_match(html, "data-distribution-kind='histogram'")
  expect_match(html, "class='dbcb-var-distribution-wrap hist-container'")
  expect_match(html, "data-scroll-item='\\.hist-bar'")
  expect_match(html, "aria-label='向左查看连续变量柱状图'")
  expect_match(html, "aria-label='向右查看连续变量柱状图'")
  expect_match(html, "hist-bars-wrapper")
  expect_match(html, "hist-labels-wrapper")
  expect_equal(length(gregexpr("class='dbcb-var-distribution-wrap hist-container'", html, fixed = TRUE)[[1]]), 1)
  expect_equal(length(gregexpr("class='dbcb-var-cycle-wrap'", html, fixed = TRUE)[[1]]), 1)
  expect_false(grepl("class='fixed-table'", html, fixed = TRUE))
})

test_that("generate_var_details adds year-like cycle heatmap counts", {
  fixture <- data.frame(
    year = c("2011", "2011", "2013", "2015", "2015"),
    x = c(1, NA, 3, 4, NA),
    y = c("A", "B", NA, "A", "B"),
    stringsAsFactors = FALSE
  )

  details <- generate_var_details(
    fixture,
    show_cycle_heatmap = TRUE,
    cycle_col = "year",
    cycle_order = c("2015", "2013", "2011"),
    heatmap_color = "#2C7FB8"
  )

  expect_true(all(grepl("dbcb-var-cycle-table", details, fixed = TRUE)))
  expect_match(details[[2]], "2015.*1.*2013.*1.*2011.*1")
  expect_match(details[[3]], "2015.*2.*2013.*dbcb-var-cycle-count' style='color:[^']*;'></span></td>.*2011.*2")
  expect_false(grepl("2013</span><span class='dbcb-var-cycle-count'>0", details[[3]], fixed = TRUE))
})

test_that("generate_var_details supports wave-like cycle names", {
  fixture <- data.frame(
    wave = c("W1", "W2", "W2", "W3"),
    score = c(10, 20, NA, 40),
    flag = c(1, 0, 1, NA)
  )

  details <- generate_var_details(
    fixture,
    show_cycle_heatmap = TRUE,
    cycle_col = "wave",
    cycle_order = c("W1", "W2", "W3"),
    heatmap_color = "#6A51A3"
  )

  expect_true(all(grepl("dbcb-var-cycle-table", details, fixed = TRUE)))
  expect_match(details[[2]], "W1.*1.*W2.*1.*W3.*1")
  expect_match(details[[3]], "W1.*1.*W2.*2.*W3.*dbcb-var-cycle-count' style='color:[^']*;'></span></td>")
})

test_that("generate_var_details uses a global heatmap color scale", {
  fixture <- data.frame(
    cycle = c(rep("A", 4), rep("B", 2)),
    high = c(1, 2, 3, 4, 5, 6),
    low = c(1, NA, NA, NA, 2, NA)
  )

  details <- generate_var_details(
    fixture,
    show_cycle_heatmap = TRUE,
    cycle_col = "cycle",
    cycle_order = c("A", "B"),
    heatmap_color = "#AE4D4D"
  )

  high_color <- regmatches(details[[2]], regexpr("rgb\\([0-9]+,[0-9]+,[0-9]+\\)", details[[2]]))
  low_color <- regmatches(details[[3]], regexpr("rgb\\([0-9]+,[0-9]+,[0-9]+\\)", details[[3]]))

  expect_match(details[[2]], "A.*4.*B.*2")
  expect_match(details[[3]], "A.*1.*B.*1")
  expect_false(identical(high_color, low_color))
})

test_that("generate_var_details cycle heatmap is responsive and isolated", {
  fixture_5 <- data.frame(
    period = rep(paste0("P", 1:5), each = 2),
    x = seq_len(10),
    y = c(1, NA, 2, NA, 3, NA, 4, NA, 5, NA)
  )

  details_5 <- generate_var_details(
    fixture_5,
    show_cycle_heatmap = TRUE,
    cycle_col = "period",
    cycle_order = paste0("P", 1:5),
    heatmap_color = "#2C7FB8"
  )

  expect_match(details_5[[2]], "width:100%;table-layout:fixed")
  expect_match(details_5[[2]], "min-width:max\\(100%, 210px\\)")
  expect_match(details_5[[2]], "td\\.dbcb-var-cycle-cell\\{min-width:0;padding:2px 2px")
  expect_match(details_5[[2]], "border:1px solid #DDDDDD !important")
  expect_match(details_5[[2]], "td\\.dbcb-var-cycle-cell \\.dbcb-var-cycle-label\\{[^}]*font-size:9px")
  expect_match(details_5[[2]], "td\\.dbcb-var-cycle-cell \\.dbcb-var-cycle-count\\{[^}]*font-size:12px")
  expect_match(details_5[[2]], "dbcb-var-cycle-title\\{[^}]*color:#7f8c8d;[^}]*font-family:inherit;[^}]*font-size:10px;[^}]*line-height:1.55;[^}]*font-weight:400")
  expect_match(details_5[[2]], "dbcb-var-cycle-block\\{margin-top:8px")
  expect_match(details_5[[2]], "scrollbar-width:none")
  expect_match(details_5[[2]], "dbcb-var-cycle-prev")
  expect_match(details_5[[2]], "dbcb-var-cycle-next")
  expect_match(details_5[[2]], "dbcb-var-cycle-nav\\{position:relative")
  expect_match(details_5[[2]], "dbcb-var-cycle-arrow\\{display:none;position:absolute")
  expect_match(details_5[[2]], "dbcb-var-cycle-arrow\\.is-visible\\{display:inline-flex")
  expect_match(details_5[[2]], "linear-gradient\\(90deg,rgba\\(255,255,255,.94\\)")
  expect_match(details_5[[2]], "linear-gradient\\(270deg,rgba\\(255,255,255,.94\\)")
  expect_match(details_5[[2]], "canPrev=overflow&&wrap\\.scrollLeft>1")
  expect_match(details_5[[2]], "canNext=overflow&&wrap\\.scrollLeft<max-1")
  expect_match(details_5[[2]], "wrap\\.clientWidth-cellWidth")
  expect_false(grepl("<br>\\s*<style>\\s*\\.dbcb-var-cycle-nav", details_5[[2]], perl = TRUE))
  expect_equal(sum(gregexpr("!important", details_5[[2]], fixed = TRUE)[[1]] > 0), 1)
  expect_equal(length(gregexpr("周期分布:", details_5[[2]], fixed = TRUE)[[1]]), 1)
  expect_match(details_5[[2]], "dbcb-var-cycle-title\\{margin:0;")

  fixture_10 <- data.frame(
    period = paste0("P", 1:10),
    x = seq_len(10)
  )
  details_10 <- generate_var_details(
    fixture_10,
    show_cycle_heatmap = TRUE,
    cycle_col = "period",
    cycle_order = paste0("P", 1:10),
    heatmap_color = "#2C7FB8"
  )

  expect_match(details_10[[2]], "overflow-x:auto")
  expect_false(grepl("dbcb-var-cycle-nav.is-scrollable", details_10[[2]], fixed = TRUE))
  expect_match(details_10[[2]], "min-width:max\\(100%, 420px\\)")
})

test_that("generate_var_details cycle heatmap keeps equal counts equal colors and blanks missing counts", {
  fixture <- data.frame(
    period = c("A", "A", "B", "B", "C"),
    x = c(1, NA, 2, NA, NA),
    z = c(1, 2, 3, 4, 5)
  )

  details <- generate_var_details(
    fixture,
    show_cycle_heatmap = TRUE,
    cycle_col = "period",
    cycle_order = c("A", "B", "C"),
    heatmap_color = "#AE4D4D"
  )

  colors <- regmatches(details[[2]], gregexpr("background-color:[^;']+", details[[2]]))[[1]]
  cell_colors <- colors[grepl("rgb\\(|#f7f6f4", colors)]

  expect_identical(cell_colors[1], cell_colors[2])
  expect_match(details[[2]], "C</span><span class='dbcb-var-cycle-count' style='color:[^']*;'></span>")
  expect_false(grepl(">0</span>", details[[2]], fixed = TRUE))
})

test_that("generate_var_details validates cycle heatmap arguments", {
  fixture <- data.frame(
    period = c("P1", "P2"),
    x = c(1, 2)
  )

  expect_error(
    generate_var_details(fixture, show_cycle_heatmap = TRUE, cycle_col = "missing", cycle_order = "P1"),
    "周期列不存在"
  )
  expect_error(
    generate_var_details(fixture, show_cycle_heatmap = TRUE, cycle_col = "period", cycle_order = character()),
    "cycle_order 不能为空"
  )
  expect_error(
    generate_var_details(fixture, show_cycle_heatmap = TRUE, cycle_col = "period", cycle_order = "P9"),
    "无法对应"
  )
  expect_error(
    generate_var_details(fixture, show_cycle_heatmap = TRUE, cycle_col = "period", cycle_order = "P1", heatmap_color = "not-a-color"),
    "heatmap_color"
  )
})
