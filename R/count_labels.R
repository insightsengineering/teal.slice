#' Progress bars with labels
#'
#' `shiny` element displaying a series of progress bars and observation counts.
#'
#' @param inputId (`character(1)`) `shiny` id of the parent element (e.g. a check-box group input).
#' @param choices (`vector`) Available values. Used to determine label text.
#' @param countsmax (`numeric`) Maximum counts of each element. Must be the same length `choices`.
#' @param countsnow (`numeric`) Current counts of each element. Must be the same length `choices`.
#' @param session (`session`) `shiny` `session` object passed to function given to `shinyServer`.
#'
#' @return List of `shiny.tag`s.
#'
#' Creates a number of progress bar elements, one for each value of `choices`.
#' The widths of all progress bars add up to the full width of the container.
#' Each progress bar has a text label that contains the name of the value and the number of counts.
#'
#' If the filter panel is used with `count_type = "all"`, the progress bars will be filled
#' according to the number of counts remaining in the current selection and the label will show
#' both the current and the total number of counts.
#'
#' Each child element can have a unique `id` attribute to be used independently.
#'
#' @examples
#' # use non-exported function from teal.slice
#' include_js_files <- getFromNamespace("include_js_files", "teal.slice")
#' include_css_files <- getFromNamespace("include_css_files", "teal.slice")
#' countBars <- getFromNamespace("countBars", "teal.slice")
#' updateCountBars <- getFromNamespace("updateCountBars", "teal.slice")
#'
#' library(shiny)
#'
#' choices <- sample(as.factor(c("a", "b", "c")), size = 20, replace = TRUE)
#' counts <- table(choices)
#' labels <- countBars(
#'   inputId = "counts",
#'   choices = c("a", "b", "c"),
#'   countsmax = counts,
#'   countsnow = unname(counts)
#' )
#'
#' ui <- fluidPage(
#'   tags$div(
#'     class = "choices_state",
#'     include_js_files("count-bar-labels.js"),
#'     include_css_files(pattern = "filter-panel"),
#'     checkboxGroupInput(
#'       inputId = "choices",
#'       selected = levels(choices),
#'       choiceNames = labels,
#'       choiceValues = levels(choices),
#'       label = NULL
#'     )
#'   )
#' )
#' server <- function(input, output, session) {
#'   observeEvent(input$choices, {
#'     new_counts <- counts
#'     new_counts[!names(new_counts) %in% input$choices] <- 0
#'     updateCountBars(
#'       inputId = "counts",
#'       choices = levels(choices),
#'       countsmax = counts,
#'       countsnow = unname(new_counts)
#'     )
#'   })
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @keywords internal
#'
countBars <- function(inputId, choices, countsmax, countsnow = NULL) { # nolint
  checkmate::assert_string(inputId)
  checkmate::assert_vector(choices)
  checkmate::assert_numeric(countsmax, len = length(choices))
  checkmate::assert_numeric(countsnow, len = length(choices), null.ok = TRUE)
  if (!is.null(countsnow)) {
    checkmate::assert_true(all(countsnow <= countsmax))
  }

  ns <- NS(inputId)

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
#' `shiny` element displaying a progress bar and observation count.
#'
#' A progress bar is created to visualize the number of counts in a variable, with filling and a text label.
#' - progress bar width is derived as a fraction of the container width: `style = "width: <countmax> / <counttotal>%"`,
#' - progress bar is filled up to the fraction `<countnow> / <countmax>`,
#' - text label is obtained by `<label> (<countnow> / <countmax>)`.
#'
#' @param inputId (`character(1)`) `shiny` id of the parent element (e.g. a check-box group input).
#' @param label (`character(1)`) Text to display followed by counts.
#' @param countmax (`numeric(1)`) Maximum count for a single element.
#' @param countnow (`numeric(1)`) Current count for a single element.
#' @param counttotal (`numeric(1)`) Sum total of maximum counts of all elements, see `Details`.
#' @param session (`session`) `shiny` `session` object passed to function given to `shinyServer`.
#'
#' @return `shiny.tag` object with a progress bar and a label.
#'
#' @keywords internal
#'
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
updateCountBars <- function(session = getDefaultReactiveDomain(), inputId, choices, countsmax, countsnow = NULL) { # nolint
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
updateCountBar <- function(session = getDefaultReactiveDomain(), inputId, label, countmax, countnow = NULL, counttotal) { # nolint
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

#' Build count text
#'
#' Returns a text label describing filtered counts. The text is composed in the following way:
#' - when `countnow` is not `NULL`: `<label> (<countnow>/<countmax>)`
#' - when `countnow` is `NULL`: `<label> (<countmax>)`
#'
#' @param label (`character(1)`) Text displayed before counts.
#' @param countnow (`numeric(1)`) Number of filtered counts.
#' @param countmax (`numeric(1)`) Number of unfiltered counts.
#'
#' @return A character string.
#'
#' @keywords internal
#'
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


#' Adjust counts to match choices
#'
#' @param choices (`character`) Choices to match.
#' @param counts (`named numeric`) Counts to adjust.
#' @keywords internal
pair_counts <- function(choices, counts) {
  checkmate::assert_numeric(counts)
  counts <- counts[match(choices, names(counts))]
  counts[is.na(counts)] <- 0
  names(counts) <- choices
  counts
}
