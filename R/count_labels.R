#' @rdname countBarLabels
#'
#' @description
#' `shiny` element showing progressbar counts. Each element can has
#' unique `id` attribute so each can be used independently.
#' Progress bar size is dependent on the ratio `choicesnow[i] / countsmax[i]`.
#' Label is `choices[i] (countsnow[i]/countsmax)`
#' @param inputId (`character(1)`) `shiny` id
#' @param choices (`vector`) determines label text.
#' @param countsmin (`numeric`) determining minimal count of each element.
#'  Length should be the same as `choices`
#' @param countsmax (`numeric`) determining maximal count of each element.
#'  Length should be the same as `choices`.
#' @param countsnow (`numeric`) actual counts of each element.
#'  Length should be the same as `choices`.
#' @param counttotal (`numeric(1)`) Determines the size of the whole progress bar.
#'  For example, in case there are three choices with some counts, one might like
#'  to display each progress bar with the relative size to the `totalcount`.
#' @param countmin (`numeric(1)`) `countsmin[i]`
#' @param countmax (`numeric(1)`) `countsmax[i]`.
#' @param countnow (`numeric(1)`) `countsnow[i]`.
NULL

#' @rdname countBarLabels
#' @examples
#'
#' choices <- sample(as.factor(c("a", "b", "c")), size = 20, replace = TRUE)
#' counts <- table(choices)
#' labels <- countBarLabels(
#'   inputId = "counts",
#'   choices = c("a", "b", "c"),
#'   countsmin = c(0, 0, 0),
#'   countsmax = c(20, 20, 20),
#'   countsnow = unname(counts)
#' )
#'
#'
#' shinyApp(
#'   ui = fluidPage(
#'     div(
#'       class = "choices_state",
#'       teal.slice:::include_js_files("count-bar-labels.js"),
#'       checkboxGroupInput(
#'         inputId = "choices",
#'         choices = levels(choices),
#'         selected = levels(choices),
#'         label = NULL
#'       ),
#'       labels
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$choices, {
#'       new_counts <- counts
#'       new_counts[!names(new_counts) %in% input$choices] <- 0
#'
#'       updateCountBarLabels(
#'         inputId = "counts",
#'         choices = levels(choices),
#'         countsmin = c(0, 0, 0),
#'         countsmax = c(20, 20, 20),
#'         countsnow = unname(new_counts)
#'       )
#'     })
#'   }
#' )
#'
countBarLabels <- function(inputId, choices, countsmin, countsmax, countsnow = NULL) {
  checkmate::assert_string(inputId)
  checkmate::assert_numeric(countsmin)
  checkmate::assert_numeric(countsmax)
  checkmate::assert_numeric(countsnow, null.ok = TRUE)

  ns <- NS(inputId)

  lapply(seq_along(choices), function(i) {
    choice <- choices[i]
    countmin <- countsmin[i]
    countmax <- countsmax[i]
    countnow <- if (is.null(countsnow)) countmax else countsnow[i]

    countBarLabel(
      inputId = ns(i),
      label = choice,
      countmin = countmin,
      countmax = countmax,
      countnow = countnow,
      counttotal = sum(countsmax)
    )
  })
}

countBarLabel <- function(inputId, label = "", countmin, countmax, countnow = NULL, counttotal = countmax) {
  checkmate::assert_string(inputId)
  checkmate::assert_string(label)
  checkmate::assert_number(countmin)
  checkmate::assert_number(countmax)
  checkmate::assert_number(countnow, null.ok = TRUE)
  checkmate::assert_number(counttotal)

  label_html <- countLabel(inputId = inputId, label = label, countmax = countmax, countnow = countnow)
  progress_html <- countBar(inputId = inputId, countmin = countmin, countmax = countmax,
                                    countnow = countnow, counttotal = counttotal)
  tags$div(progress_html, label_html)
}

countBar <- function(inputId, countmin, countmax, countnow, counttotal) {
  ns <- NS(inputId)
  tags$div(
    class = "progress state-count-container",
    # * .9 to not exceed width of the parent html element
    style = sprintf("width: %s%%", countmax / counttotal * 100 * .9),
    tags$div(
      id = ns("count_bar"),
      class = "progress-bar state-count-bar",
      `aria-valuemin` = countmin,
      `aria-valuemax` = countmax,
      `aria-valuenow` = countnow,
      style = sprintf("width: %s%%", countnow / countmax * 100),
      role = "progressbar"
    )
  )
}

countLabel <- function(inputId, label, countmax, countnow) {
  ns <- NS(inputId)
  label <- make_count_text(label = label, countmax = countmax, countnow = countnow)
  label_html <- tags$div(id = ns("count_text"), class = "state-count-text", label)
}

updateCountBarLabels <-  function(session = getDefaultReactiveDomain(), inputId, choices,
                                  countsmin, countsmax, countsnow = NULL) {
    ns <- NS(inputId)
    lapply(seq_along(choices), function(i) {
      choice <- choices[i]
      countmin <- countsmin[i]
      countmax <- countsmax[i]
      countnow <- if (is.null(countsnow)) countmax else countsnow[i]
      updateCountLabel(
        inputId = ns(i),
        label = choice,
        countmax = countmax,
        countnow = countnow
      )
      updateCountBar(
        inputId = ns(i),
        countmin = countmin,
        countmax = countmax,
        countnow = countnow
      )
    })
  invisible(NULL)
}

updateCountBarLabel <- function(session = getDefaultReactiveDomain(), inputId, label = "",
                                  countmin, countmax, countnow = NULL, counttotal = sum(countmax), value) {
  checkmate::assert_string(inputId)
  # todo: asserts

  label <- make_count_label(label, countmax = countmax, countnow = countnow)
  if (is.null(countnow)) countnow <- countmax

  updateCountLabel(inputId = inputId, label = label, countmax = countmax, countnow = countnow)
  updateCountBar(inputId = inputId, countmin = countmin, countmax = countmax, countnow = countnow)

  invisible(NULL)
}

updateCountLabel <- function(session = getDefaultReactiveDomain(), inputId, label, countmax, countnow) {
  label <- make_count_text(label = label, countmax = countmax, countnow = countnow)
  session$sendCustomMessage(
    type = "updateCountLabel",
    message = list(
      id = session$ns(inputId),
      label = label
    )
  )
}

updateCountBar <- function(session = getDefaultReactiveDomain(), inputId, countmin, countmax, countnow) {
  session$sendCustomMessage(
    type = "updateCountBar",
    message = list(
      id = session$ns(inputId),
      countmin = countmin,
      countmax = countmax,
      countnow = countnow
    )
  )
}

make_count_text <- function(label = "", countmax, countnow = NULL) {
  checkmate::assert_string(label)
  checkmate::assert_number(countmax)
  checkmate::assert_number(countnow, null.ok = TRUE)

  sprintf(
    "%s(%s%s)",
    if (label == "") "" else sprintf("%s ",label),
    if (is.null(countnow)) "" else sprintf("%s/", countnow),
    countmax
  )
}