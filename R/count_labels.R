#' Progress bars with labels
#'
#' `shiny` element showing progress bar counts. Each element can have an
#' unique `id` attribute so each can be used independently.
#' Progress bar size is dependent on the ratio `choicesnow[i] / countsmax[i]`.
#' Label is `choices[i] (countsnow[i]/countsmax)`
#' @param session (`session`) object passed to function given to `shinyServer`.
#' @param inputId (`character(1)`) `shiny` id
#' @param choices (`vector`) determines label text.
#' @param countsmax (`numeric`) determining maximal count of each element.
#'  Length should be the same as `choices`.
#' @param countsnow (`numeric`) actual counts of each element.
#'  Length should be the same as `choices`.
#' @return list of `shiny.tag`
#' @examples
#'
#' choices <- sample(as.factor(c("a", "b", "c")), size = 20, replace = TRUE)
#' counts <- table(choices)
#' labels <- teal.slice:::countBars(
#'   inputId = "counts",
#'   choices = c("a", "b", "c"),
#'   countsmax = counts,
#'   countsnow = unname(counts)
#' )
#'
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     div(
#'       class = "choices_state",
#'       teal.slice:::include_js_files("count-bar-labels.js"),
#'       teal.slice:::include_css_files(pattern = "filter-panel"),
#'       checkboxGroupInput(
#'         inputId = "choices",
#'         selected = levels(choices),
#'         choiceNames = labels,
#'         choiceValues = levels(choices),
#'         label = NULL
#'       )
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$choices, {
#'       new_counts <- counts
#'       new_counts[!names(new_counts) %in% input$choices] <- 0
#'       teal.slice:::updateCountBars(
#'         inputId = "counts",
#'         choices = levels(choices),
#'         countsmax = counts,
#'         countsnow = unname(new_counts)
#'       )
#'     })
#'   }
#' )
#' }
#' @keywords internal
countBars <- function(inputId, choices, countsmax, countsnow = NULL) { # nolint
  checkmate::assert_string(inputId)
  checkmate::assert_vector(choices)
  checkmate::assert_numeric(countsmax, len = length(choices))
  checkmate::assert_numeric(countsnow, len = length(choices), null.ok = TRUE)
  if (!is.null(countsnow)) {
    checkmate::assert_true(all(countsnow <= countsmax))
  }

  ns <- NS(inputId)
  counttotal <- sum(countsmax)

  mapply(
    countBar,
    inputId = ns(seq_along(choices)),
    label = as.character(choices),
    countmax = countsmax,
    countnow = if (is.null(countsnow)) rep(list(NULL), length(choices)) else countsnow,
    MoreArgs = list(
      counttotal = sum(countsmax)
    ),
    SIMPLIFY = FALSE, USE.NAMES = FALSE
  )
}

#' Progress bar with label
#'
#' Progress bar with label
#' @param session (`session`) object passed to function given to `shinyServer`.
#' @param inputId (`character(1)`) `shiny` id
#' @param label (`character(1)`) Text to display followed by counts
#' @param countmax (`numeric(1)`) maximal possible count for a single item.
#' @param countnow (`numeric(1)`) current count of a single item.
#' @param counttotal (`numeric(1)`) total count to make whole progress bar
#'  taking part of the container. Ratio between `countmax / counttotal`
#'  determines `<style="width: <countmax / counttotal>%""`.
#' @return `shiny.tag` object with a progress bar and a label.
#' @keywords internal
countBar <- function(inputId, label, countmax, countnow = NULL, counttotal = countmax) { # nolint
  checkmate::assert_string(inputId)
  checkmate::assert_string(label)
  checkmate::assert_number(countmax)
  checkmate::assert_number(countnow, null.ok = TRUE, upper = countmax)
  checkmate::assert_number(counttotal, lower = countmax)

  label <- make_count_text(label, countmax = countmax, countnow = countnow)
  ns <- NS(inputId)
  if (is.null(countnow)) countnow <- 0
  tags$div(
    class = "progress state-count-container",
    # * .9 to not exceed width of the parent html element
    tags$div(
      id = ns("count_bar_filtered"),
      class = "progress-bar state-count-bar-filtered",
      style = sprintf("width: %s%%", countnow / counttotal * 100),
      role = "progressbar",
      label
    ),
    tags$div(
      id = ns("count_bar_unfiltered"),
      class = "progress-bar state-count-bar-unfiltered",
      style = sprintf("width: %s%%", (countmax - countnow) / counttotal * 100),
      role = "progressbar"
    )
  )
}

#' @rdname countBars
updateCountBars <- function(session = getDefaultReactiveDomain(), inputId, choices, # nolint
                            countsmax, countsnow = NULL) {
  checkmate::assert_string(inputId)
  checkmate::assert_vector(choices)
  checkmate::assert_numeric(countsmax, len = length(choices))
  checkmate::assert_numeric(countsnow, len = length(choices), null.ok = TRUE)

  ns <- NS(inputId)
  mapply(
    updateCountBar,
    inputId = ns(seq_along(choices)),
    label = choices,
    countmax = countsmax,
    countnow = if (is.null(countsnow)) rep(list(NULL), length(choices)) else countsnow,
    MoreArgs = list(
      counttotal = sum(countsmax)
    )
  )
  invisible(NULL)
}

#' @rdname countBar
updateCountBar <- function(session = getDefaultReactiveDomain(), inputId, label, # nolint
                           countmax, countnow = NULL, counttotal) {
  checkmate::assert_string(inputId)
  checkmate::assert_string(label)
  checkmate::assert_number(countmax)
  checkmate::assert_number(countnow, null.ok = TRUE)
  checkmate::assert_number(counttotal)

  label <- make_count_text(label, countmax = countmax, countnow = countnow)
  if (is.null(countnow)) countnow <- countmax
  session$sendCustomMessage(
    type = "updateCountBar",
    message = list(
      id = session$ns(inputId),
      label = label,
      countmax = countmax,
      countnow = countnow,
      counttotal = counttotal
    )
  )

  invisible(NULL)
}

#' @rdname countBar
updateCountText <- function(session = getDefaultReactiveDomain(), inputId, label, countmax, countnow) { # nolint
  checkmate::assert_string(inputId)
  checkmate::assert_string(label)
  checkmate::assert_number(countmax)
  checkmate::assert_number(countnow, null.ok = TRUE)
  label <- make_count_text(label, countmax = countmax, countnow = countnow)
  session$sendCustomMessage(
    type = "updateCountText",
    message = list(
      id = session$ns(inputId),
      label = label
    )
  )
}

#' Make a count text
#'
#' Returns a text describing filtered counts. The text is composed in the following way:
#' - when `countnow` is not `NULL`: `<label> (<countnow>/<countmax>)`
#' - when `countnow` is `NULL`: `<label> (<countmax>)`
#' @param label (`character(1)`) Text displayed before counts
#' @param countnow (`numeric(1)`) filtered counts
#' @param countmax (`numeric(1)`) unfiltered counts
#' @return `character(1)`
#' @keywords internal
make_count_text <- function(label, countmax, countnow = NULL) {
  checkmate::assert_string(label)
  checkmate::assert_number(countmax)
  checkmate::assert_number(countnow, null.ok = TRUE)
  sprintf(
    "%s (%s%s)",
    label,
    if (is.null(countnow)) "" else sprintf("%s/", countnow),
    countmax
  )
}
