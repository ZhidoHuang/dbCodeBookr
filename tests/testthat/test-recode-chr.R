test_that("recode.chr prints the unqualified case_when template", {
  skip_if(rstudioapi::isAvailable(), "requires console output")
  values <- c("A", "B", "A", NA)
  output <- capture.output(result <- withVisible(recode.chr(values)))

  expect_identical(output, c(
    "# \u751f\u6210\u7684\u4ee3\u7801\uff1a",
    " # recode.chr(values)",
    "values <- case_when(",
    '  values == "A" ~ "A",',
    '  values == "B" ~ "B",',
    "  TRUE ~ NA_character_",
    ")",
    " "
  ))
  expect_identical(result$value, values)
  expect_false(result$visible)

  local({
    was_attached <- "package:dplyr" %in% search()
    if (!was_attached) {
      suppressPackageStartupMessages(library("dplyr"))
      on.exit(detach("package:dplyr", character.only = TRUE), add = TRUE)
    }
    evaluation_env <- new.env(parent = globalenv())
    evaluation_env$values <- c("A", "B", "C", NA)
    eval(parse(text = paste(output[-1], collapse = "\n")), evaluation_env)
    expect_identical(evaluation_env$values, c("A", "B", NA_character_, NA_character_))
  })
})
