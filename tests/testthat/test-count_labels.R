testthat::test_that("make_count_text requires label to be a character(1)", {
  testthat::expect_no_error(make_count_text(label = "a", countmax = 50))
  testthat::expect_error(make_count_text(label = character(0), countmax = 50), "label")
  testthat::expect_error(make_count_text(label = 1, countmax = 50), "label")
})

testthat::test_that("make_count_text requires countmax to be a numeric(1)", {
  testthat::expect_no_error(make_count_text(label = "a", countmax = 100))
  testthat::expect_error(make_count_text(label = "a", countmax = "100"), "countmax")
  testthat::expect_error(make_count_text(label = "a", countmax = numeric(0)), "countmax")
})


testthat::test_that("make_count_text requires countnow to be a numeric(1), NULL or missing", {
  testthat::expect_no_error(make_count_text(label = "a", countmax = 100, countnow = 1))
  testthat::expect_no_error(make_count_text(label = "a", countmax = 100, countnow = NULL))
  testthat::expect_no_error(make_count_text(label = "a", countmax = 100))
  testthat::expect_error(make_count_text(label = "a", countmax = 100, countnow = "50"), "countnow")
  testthat::expect_error(make_count_text(label = "a", countmax = 100, countnow = numeric(0)), "countnow")
})

testthat::test_that("make_count_text returns label + (countnow/countmax)", {
  testthat::expect_identical(
    make_count_text(label = "a", countmax = 100, countnow = 50),
    "a (50/100)"
  )
})

testthat::test_that("make_count_text returns label + (countmax) when countnow is null or missing", {
  testthat::expect_identical(
    make_count_text(label = "a", countmax = 100, countnow = NULL),
    "a (100)"
  )
 testthat::expect_identical(
    make_count_text(label = "a", countmax = 100),
    "a (100)"
  )
})

# countLabel -------
testthat::test_that("countLabel requires InputId to be a character(1)", {
  testthat::expect_no_error(countLabel(inputId = "a", label = "a", countmax = 50))
  testthat::expect_error(countLabel(label = "a", countmax = 50), "inputId")
  testthat::expect_error(countLabel(inputId = NULL, label = "a", countmax = 50), "inputId")
  testthat::expect_error(countLabel(inputId = character(0), label = "a", countmax = 50), "inputId")
  testthat::expect_error(countLabel(inputId = 1L, label = "a", countmax = 50), "inputId")
})

testthat::test_that("countLabel requires label to be a character(1)", {
  testthat::expect_no_error(countLabel(inputId = "a", label = "a", countmax = 50))
  testthat::expect_error(countLabel(inputId = "a", label = character(0), countmax = 50), "label")
  testthat::expect_error(countLabel(inputId = "a", label = 1, countmax = 50), "label")
})

testthat::test_that("countLabel requires countmax to be a numeric(1)", {
  testthat::expect_no_error(countLabel(inputId = "a", label = "a", countmax = 100))
  testthat::expect_error(countLabel(inputId = "a", label = "a", countmax = "100"), "countmax")
  testthat::expect_error(countLabel(inputId = "a", label = "a", countmax = numeric(0)), "countmax")
})


testthat::test_that("countLabel requires countnow to be a numeric(1), NULL or missing", {
  testthat::expect_no_error(countLabel(inputId = "a", label = "a", countmax = 100, countnow = 1))
  testthat::expect_no_error(countLabel(inputId = "a", label = "a", countmax = 100, countnow = NULL))
  testthat::expect_no_error(countLabel(inputId = "a", label = "a", countmax = 100))
  testthat::expect_error(countLabel(inputId = "a", label = "a", countmax = 100, countnow = "50"), "countnow")
  testthat::expect_error(countLabel(inputId = "a", label = "a", countmax = 100, countnow = numeric(0)), "countnow")
})

testthat::test_that("countLabel returns a div with id, class and label text", {
  label <- "a"
  countmax <- 100
  countnow <- 50

  expected <- list(
    name = "div",
    attribs = list(
      id = "a-count_text",
      class = "state-count-text"
    ),
    children = list(
      make_count_text(label = label, countmax = countmax, countnow = countnow)
    )
  )
  out <- countLabel(inputId = "a", label = label, countmax = countmax, countnow = countnow)
})

# countBar -------
testthat::test_that("countBar requires InputId to be a character(1)", {
  testthat::expect_no_error(countBar(inputId = "a", countmax = 50, counttotal = 200))
  testthat::expect_error(countBar(countmax = 50, counttotal = 200), "inputId")
  testthat::expect_error(countBar(inputId = NULL, countmax = 50, counttotal = 200), "inputId")
  testthat::expect_error(countBar(inputId = character(0), countmax = 50, counttotal = 200), "inputId")
  testthat::expect_error(countBar(inputId = 1L, countmax = 50, counttotal = 200), "inputId")
})


testthat::test_that("countBar requires countmax to be a numeric(1)", {
  testthat::expect_no_error(countBar(inputId = "a", countmax = 100, counttotal = 200))
  testthat::expect_error(countBar(inputId = "a", countmax = "100", counttotal = 200), "countmax")
  testthat::expect_error(countBar(inputId = "a", countmax = numeric(0), counttotal = 200), "countmax")
})

testthat::test_that("countBar requires counttotal to be a numeric(1)", {
  testthat::expect_no_error(countBar(inputId = "a", countmax = 100, counttotal = 200))
  testthat::expect_error(countBar(inputId = "a", countmax = 100, counttotal = "200"), "counttotal")
  testthat::expect_error(countBar(inputId = "a", countmax = 100, counttotal = numeric(0)), "counttotal")
})

testthat::test_that("countBar requires countnow to be a numeric(1), NULL or missing", {
  testthat::expect_no_error(countBar(inputId = "a", countmax = 100, countnow = 1, counttotal = 200))
  testthat::expect_no_error(countBar(inputId = "a", countmax = 100, countnow = NULL, counttotal = 200))
  testthat::expect_no_error(countBar(inputId = "a", countmax = 100, counttotal = 200))
  testthat::expect_error(countBar(inputId = "a", countmax = 100, countnow = "50", counttotal = 200), "countnow")
  testthat::expect_error(countBar(inputId = "a", countmax = 100, countnow = numeric(0), counttotal = 200), "countnow")
})

testthat::test_that("countBar returns a div with class and two progressbars", {
  countmax <- 150
  countnow <- 50
  counttotal <- 200

  expected <- list(
    name = "div",
    attribs = list(class = "progress state-count-container"),
    children = list(
      list(
        name = "div",
        attribs = list(
          id = "a-count_bar_filtered",
          class = "progress-bar state-count-bar-filtered",
          style = "width: 25%",
          role = "progressbar"
        ),
        children = list()
      ),
      list(
        name = "div",
        attribs = list(
          id = "a-count_bar_unfiltered",
          class = "progress-bar state-count-bar-unfiltered",
          style = "width: 50%",
          role = "progressbar"
        ),
        children = list()
      )
    )
  )
  out <- rapply(countBar(inputId = "a", countmax = countmax, countnow = countnow, counttotal), unclass, how = "list")
  testthat::expect_identical(out, expected)
})

# countBarLabel ------------
testthat::test_that("countBarLabel requires InputId to be a character(1)", {
  testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 50))
  testthat::expect_error(countBarLabel(label = "a", countmax = 50), "inputId")
  testthat::expect_error(countBarLabel(inputId = NULL, label = "a", countmax = 50), "inputId")
  testthat::expect_error(countBarLabel(inputId = character(0), label = "a", countmax = 50), "inputId")
  testthat::expect_error(countBarLabel(inputId = 1L, label = "a", countmax = 50), "inputId")
})

testthat::test_that("countBarLabel requires label to be a character(1)", {
  testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 50))
  testthat::expect_error(countBarLabel(inputId = "a", label = character(0), countmax = 50), "label")
  testthat::expect_error(countBarLabel(inputId = "a", label = 1, countmax = 50), "label")
})

testthat::test_that("countBarLabel requires countmax to be a numeric(1)", {
  testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 100))
  testthat::expect_error(countBarLabel(inputId = "a", label = "a", countmax = "100"), "countmax")
  testthat::expect_error(countBarLabel(inputId = "a", label = "a", countmax = numeric(0)), "countmax")
})

testthat::test_that("countBarLabel requires countnow to be a numeric(1), NULL or missing", {
  testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 100, countnow = 1))
  testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 100, countnow = NULL))
  testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 100))
  testthat::expect_error(countBarLabel(inputId = "a", label = "a", countmax = 100, countnow = "50"), "countnow")
  testthat::expect_error(countBarLabel(inputId = "a", label = "a", countmax = 100, countnow = numeric(0)), "countnow")
})

testthat::test_that("countBarLabel requires counttotal to be a numeric(1) or missing", {
  testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 100, counttotal = 200))
    testthat::expect_no_error(countBarLabel(inputId = "a", label = "a", countmax = 100))
  testthat::expect_error(countBarLabel(inputId = "a", label = "a", countmax = 100, counttotal = "200"), "counttotal")
  testthat::expect_error(
    countBarLabel(inputId = "a", label = "a", countmax = 100, counttotal = numeric(0)), "counttotal"
  )
})

testthat::test_that("countBarLabel returns a div containing a progress bar and a label", {
  label <- "a"
  countmax <- 150
  countnow <- 50
  counttotal <- 200

  out <- countBarLabel(inputId = "a", label = label, countmax = countmax, countnow = countnow, counttotal = counttotal)

  expected <- tags$div(
    countBar(inputId = "a", countmax = countmax, countnow = countnow, counttotal = counttotal),
    countLabel(inputId = "a", label = label, countmax = countmax, countnow = countnow)
  )
  testthat::expect_identical(out, expected)
})

testthat::test_that("countBarLabel sets counttotal to countmax when missing", {
  out <- countBarLabel(inputId = "a", label = "a", countmax = 100, countnow = 50)
  expected <- tags$div(
    countBar(inputId = "a", countmax = 100, countnow = 50, counttotal = 100),
    countLabel(inputId = "a", label = label, countmax = 100, countnow = 50)
  )
  testthat::expect_identical(out, expected)
})

# countBarLabels -----
countsmax <- c(3, 7, 10)
choices <- c("a", "b", "c")
countsnow <- c(2, 6, 9)

testthat::test_that("countBarLabels requires InputId to be a character(1)", {
  testthat::expect_no_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_error(
    countBarLabels(inputId = NULL, choices = choices, countsmax = countsmax, countsnow = countsnow),
    "inputId"
  )
  testthat::expect_error(
    countBarLabels(inputId = character(0), choices = choices, countsmax = countsmax, countsnow = countsnow),
    "inputId"
  )
  testthat::expect_error(
    countBarLabels(inputId = 1, choices = choices, countsmax = countsmax, countsnow = countsnow),
    "inputId"
  )
  testthat::expect_error(
    countBarLabels(choices = choices, countsmax = countsmax, countsnow = countsnow),
    "inputId"
  )
})

testthat::test_that("countBarLabels requires choices to be a vector", {
  testthat::expect_no_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_no_error(
    countBarLabels(inputId = "a", choices = c(1, 2, 3), countsmax = countsmax, countsnow = countsnow),
  )
  testthat::expect_error(
    countBarLabels(inputId = "a", countsmax = countsmax, countsnow = countsnow),
    "choices"
  )

  testthat::expect_no_error(countBarLabels(inputId = "a", label = "a", countmax = 100))
  testthat::expect_error(countBarLabels(inputId = "a", label = "a", countmax = "100"), "countmax")
  testthat::expect_error(countBarLabels(inputId = "a", label = "a", countmax = numeric(0)), "countmax")
})

testthat::test_that("countBarLabels requires countsmax to be a numeric of the same length as choices", {
  testthat::expect_no_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_error(
    countBarLabels(inputId = "a", choices = choices, countsnow = countsnow),
    "countsmax"
  )
  testthat::expect_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = as.character(countsmax), countsnow = countsnow),
    "countsmax"
  )
  testthat::expect_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = c(3, 7), countsnow = countsnow),
    "countsmax"
  )
})


testthat::test_that("countBarLabels requires counstnow to be a numeric lower than countsmax, NULL or missing", {
  testthat::expect_no_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_no_error(countBarLabels(inputId = "a", choices = choices, countsmax = countsmax))
  testthat::expect_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = c(0, 0)),
    "countsmax"
  )
  testthat::expect_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = c(1, 20, 2)),
    "countsmax"
  )
  testthat::expect_error(
    countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = as.character(countsnow)),
    "countsmax"
  )
})

testthat::test_that("countBarLabels returns a list of countBarLabel(s)", {
  out <- countBarLabels(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)

  ns <- NS("a")
  expected <- lapply(seq_along(choices), function(i) {
    countBarLabel(
      inputId = ns(i),
      label = choices[i],
      countmax = countsmax[i],
      countnow = countsnow[i],
      counttotal = sum(countsmax)
    )
  })

  testthat::expect_identical(out, expected)
})
