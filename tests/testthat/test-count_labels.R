# make_count_text ----
testthat::test_that("make_count_text requires label to be a character(1)", {
  testthat::expect_no_error(make_count_text(label = "a", countmax = 50))
  testthat::expect_error(make_count_text(label = character(0), countmax = 50), "Assertion on 'label' failed")
  testthat::expect_error(make_count_text(label = 1, countmax = 50), "Assertion on 'label' failed")
})

testthat::test_that("make_count_text requires countmax to be a numeric(1)", {
  testthat::expect_no_error(make_count_text(label = "a", countmax = 100))
  testthat::expect_error(make_count_text(label = "a", countmax = "100"), "Assertion on 'countmax' failed")
  testthat::expect_error(make_count_text(label = "a", countmax = numeric(0)), "Assertion on 'countmax' failed")
})


testthat::test_that("make_count_text requires countnow to be a numeric(1) or NULL", {
  testthat::expect_no_error(make_count_text(label = "a", countmax = 100))
  testthat::expect_no_error(make_count_text(label = "a", countmax = 100, countnow = 1))
  testthat::expect_error(
    make_count_text(label = "a", countmax = 100, countnow = "50"),
    "Assertion on 'countnow' failed"
  )
  testthat::expect_error(
    make_count_text(label = "a", countmax = 100, countnow = numeric(0)),
    "Assertion on 'countnow' failed"
  )
})

testthat::test_that("make_count_text returns label + (countnow/countmax)", {
  testthat::expect_identical(
    make_count_text(label = "a", countmax = 100, countnow = 50),
    "a (50/100)"
  )
})

testthat::test_that("make_count_text returns label + (countmax) when countnow is NULL", {
  testthat::expect_identical(
    make_count_text(label = "a", countmax = 100, countnow = NULL),
    "a (100)"
  )
})

# countBar -------
testthat::test_that("countBar requires InputId to be a character(1)", {
  testthat::expect_no_error(countBar(inputId = "a", label = "a", countmax = 50, counttotal = 200))
  testthat::expect_error(
    countBar(label = "a", countmax = 50, counttotal = 200),
    "argument \"inputId\" is missing, with no default"
  )
  testthat::expect_error(
    countBar(inputId = NULL, label = "a", countmax = 50, counttotal = 200),
    "Assertion on 'inputId' failed"
  )
  testthat::expect_error(
    countBar(inputId = character(0), label = "a", countmax = 50, counttotal = 200),
    "Assertion on 'inputId' failed"
  )
  testthat::expect_error(
    countBar(inputId = 1L, label = "a", countmax = 50, counttotal = 200),
    "Assertion on 'inputId' failed"
  )
})

testthat::test_that("countBar requires label to be a character(1)", {
  testthat::expect_no_error(countBar(inputId = "a", label = "a", countmax = 50, counttotal = 200))
  testthat::expect_error(
    countBar(inputId = "a", label = NULL, countmax = 50, counttotal = 200),
    "Assertion on 'label' failed"
  )
  testthat::expect_error(
    countBar(inputId = "a", label = character(0), countmax = 50, counttotal = 200),
    "Assertion on 'label' failed"
  )
  testthat::expect_error(
    countBar(inputId = "a", label = 1L, countmax = 50, counttotal = 200),
    "Assertion on 'label' failed"
  )
})

testthat::test_that("countBar requires countmax to be a numeric(1)", {
  testthat::expect_no_error(countBar(inputId = "a", label = "a", countmax = 100, counttotal = 200))
  testthat::expect_error(
    countBar(inputId = "a", label = "a", countmax = "100", counttotal = 200),
    "Assertion on 'countmax' failed"
  )
  testthat::expect_error(
    countBar(inputId = "a", label = "a", countmax = numeric(0), counttotal = 200),
    "Assertion on 'countmax' failed"
  )
})

testthat::test_that("countBar requires counttotal to be a numeric(1)", {
  testthat::expect_no_error(countBar(inputId = "a", label = "a", countmax = 100, counttotal = 200))
  testthat::expect_error(
    countBar(inputId = "a", label = "a", countmax = 100, counttotal = "200"),
    "Assertion on 'counttotal' failed"
  )
  testthat::expect_error(
    countBar(inputId = "a", label = "a", countmax = 100, counttotal = numeric(0)),
    "Assertion on 'counttotal' failed"
  )
})

testthat::test_that("countBar requires countnow to be a numeric(1), NULL", {
  testthat::expect_no_error(countBar(inputId = "a", label = "a", countmax = 100, countnow = 1, counttotal = 200))
  testthat::expect_no_error(countBar(inputId = "a", label = "a", countmax = 100, counttotal = 200))
  testthat::expect_error(
    countBar(inputId = "a", label = "a", countmax = 100, countnow = "50", counttotal = 200),
    "Assertion on 'countnow' failed"
  )
  testthat::expect_error(
    countBar(inputId = "a", label = "a", countmax = 100, countnow = numeric(0), counttotal = 200),
    "Assertion on 'countnow' failed"
  )
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
        children = list("a (50/150)")
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
  out <- rapply(
    countBar(inputId = "a", label = "a", countmax = countmax, countnow = countnow, counttotal),
    unclass,
    how = "list"
  )
  testthat::expect_identical(out, expected)
})

# countBars -----
countsmax <- c(3, 7, 10)
choices <- c("a", "b", "c")
countsnow <- c(2, 6, 9)

testthat::test_that("countBars requires InputId to be a character(1)", {
  testthat::expect_no_error(
    countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_error(
    countBars(inputId = NULL, choices = choices, countsmax = countsmax, countsnow = countsnow),
    "Assertion on 'inputId' failed"
  )
  testthat::expect_error(
    countBars(inputId = character(0), choices = choices, countsmax = countsmax, countsnow = countsnow),
    "Assertion on 'inputId' failed"
  )
  testthat::expect_error(
    countBars(inputId = 1, choices = choices, countsmax = countsmax, countsnow = countsnow),
    "Assertion on 'inputId' failed"
  )
  testthat::expect_error(
    countBars(choices = choices, countsmax = countsmax, countsnow = countsnow),
    "argument \"inputId\" is missing, with no default"
  )
})

testthat::test_that("countBars requires choices to be a vector", {
  testthat::expect_no_error(
    countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_no_error(
    countBars(inputId = "a", choices = c(1, 2, 3), countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_error(
    countBars(inputId = "a", countsmax = countsmax, countsnow = countsnow),
    "argument \"choices\" is missing, with no default"
  )
})

testthat::test_that("countBars requires countsmax to be a numeric of the same length as choices", {
  testthat::expect_no_error(
    countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_error(
    countBars(inputId = "a", choices = choices, countsnow = countsnow),
    "argument \"countsmax\" is missing, with no default"
  )
  testthat::expect_error(
    countBars(inputId = "a", choices = choices, countsmax = as.character(countsmax), countsnow = countsnow),
    "Assertion on 'countsmax' failed"
  )
  testthat::expect_error(
    countBars(inputId = "a", choices = choices, countsmax = c(3, 7), countsnow = countsnow),
    "Assertion on 'countsmax' failed"
  )
})


testthat::test_that("countBars requires counstnow to be a numeric lower than countsmax, NULL", {
  testthat::expect_no_error(
    countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)
  )
  testthat::expect_no_error(countBars(inputId = "a", choices = choices, countsmax = countsmax))
  testthat::expect_error(
    countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = c(0, 0)),
    "Assertion on 'countsnow' failed"
  )
  testthat::expect_error(
    countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = c(1, 20, 2)),
    "Assertion on '.+' failed"
  )
  testthat::expect_error(
    countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = as.character(countsnow)),
    "Assertion on 'countsnow' failed"
  )
})

testthat::test_that("countBars returns a list of countBar(s)", {
  out <- countBars(inputId = "a", choices = choices, countsmax = countsmax, countsnow = countsnow)

  ns <- NS("a")
  expected <- lapply(seq_along(choices), function(i) {
    countBar(
      inputId = ns(i),
      label = choices[i],
      countmax = countsmax[i],
      countnow = countsnow[i],
      counttotal = sum(countsmax)
    )
  })

  testthat::expect_identical(out, expected)
})
