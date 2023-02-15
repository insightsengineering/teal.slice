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
