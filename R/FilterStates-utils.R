
.filterable_class <- c("logical", "integer", "numeric", "factor", "character", "Date", "POSIXct", "POSIXlt")

#' Initialize `FilterStates` object
#'
#' Initialize `FilterStates` object
#' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
#'   the R object which `subset` function is applied on.
#'
#' @param data_filtered (`reactive`)\cr
#'   should return an object constistent with the `FilterState` class.
#'
#' @param input_dataname (`character(1)` or `name` or `call`)\cr
#'   name of the data used on lhs of the expression
#'   specified to the function argument attached to this `FilterStates`.
#'
#' @param output_dataname (`character(1)` or `name` or `call`)\cr
#'   name of the output data on the lhs of the assignment expression.
#'
#' @param datalabel (`character(0)` or `character(1)`)\cr
#'   text label value.
#'
#' @param ... (optional)
#'   additional arguments for specific classes: keys
#' @keywords internal
#' @export
#' @examples
#' df <- data.frame(
#'   character = letters,
#'   numeric = seq_along(letters),
#'   date = seq(Sys.Date(), length.out = length(letters), by = "1 day"),
#'   datetime = seq(Sys.time(), length.out = length(letters), by = "33.33 hours")
#' )
#' rf <- teal.slice:::init_filter_states(
#'   data = df,
#'   input_dataname = "DF",
#'   output_dataname = "DF_OUTPUT",
#'   varlabels = c(
#'     "character variable", "numeric variable", "date variable", "datetime variable"
#'   )
#' )
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("clear", span(icon("xmark"), "Remove all filters")),
#'     rf$ui_add_filter_state(id = "add", data = df),
#'     rf$ui("states"),
#'     verbatimTextOutput("expr"),
#'   ),
#'   server = function(input, output, session) {
#'     rf$srv_add_filter_state(id = "add", data = df)
#'     rf$server(id = "states")
#'     output$expr <- renderText({
#'       deparse1(rf$get_call(), collapse = "\n")
#'     })
#'     observeEvent(input$clear, rf$queue_empty())
#'   }
#' )
#' }
init_filter_states <- function(data,
                               data_filtered = reactive(data),
                               input_dataname,
                               output_dataname = input_dataname,
                               datalabel = character(0),
                               ...) {
  UseMethod("init_filter_states")
}

#' @keywords internal
#' @export
init_filter_states.data.frame <- function(data, # nolint
                                          data_filtered = reactive(data),
                                          input_dataname,
                                          output_dataname = input_dataname,
                                          datalabel = character(0),
                                          varlabels = character(0),
                                          keys = character(0),
                                          ...) {
  DFFilterStates$new(
    data = data,
    data_filtered = data_filtered,
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel,
    varlabels = varlabels,
    keys = keys
  )
}

#' @keywords internal
#' @export
init_filter_states.matrix <- function(data, # nolint
                                      data_filtered = reactive(data),
                                      input_dataname,
                                      output_dataname = input_dataname,
                                      datalabel = character(0),
                                      ...) {
  MatrixFilterStates$new(
    data = data,
    data_filtered = data_filtered,
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel
  )
}

#' @keywords internal
#' @export
init_filter_states.MultiAssayExperiment <- function(data, # nolint
                                                    data_filtered = reactive(data),
                                                    input_dataname,
                                                    output_dataname = input_dataname,
                                                    datalabel = character(0),
                                                    varlabels,
                                                    keys = character(0),
                                                    ...) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
  }
  MAEFilterStates$new(
    data = data,
    data_filtered = data_filtered,
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel,
    varlabels = varlabels,
    keys = keys
  )
}

#' @keywords internal
#' @export
init_filter_states.SummarizedExperiment <- function(data, # nolint
                                                    data_filtered = reactive(data),
                                                    input_dataname,
                                                    output_dataname = input_dataname,
                                                    datalabel = character(0),
                                                    ...) {
  if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
    stop("Cannot load SummarizedExperiment - please install the package or restart your session.")
  }
  SEFilterStates$new(
    data = data,
    data_filtered = data_filtered,
    input_dataname = input_dataname,
    output_dataname = output_dataname,
    datalabel = datalabel
  )
}

#' Gets supported filterable variable names
#'
#' Gets filterable variable names from a given object. The names match variables
#' of classes in an array `teal.slice:::.filterable_class`.
#' @param data (`object`)\cr
#'   the R object containing elements which class can be checked through `vapply` or `apply`.
#'
#' @examples
#' df <- data.frame(
#'   a = letters[1:3],
#'   b = 1:3,
#'   c = Sys.Date() + 1:3,
#'   d = Sys.time() + 1:3,
#'   z = complex(3)
#' )
#' teal.slice:::get_supported_filter_varnames(df)
#' @return `character` the array of the matched element names
#' @keywords internal
get_supported_filter_varnames <- function(data) {
  UseMethod("get_supported_filter_varnames")
}

#' @keywords internal
#' @export
get_supported_filter_varnames.default <- function(data) { # nolint
  is_expected_class <- vapply(
    X = data,
    FUN = function(x) any(class(x) %in% .filterable_class),
    FUN.VALUE = logical(1)
  )
  names(is_expected_class[is_expected_class])
}

#' @keywords internal
#' @export
get_supported_filter_varnames.matrix <- function(data) { # nolint
  # all columns are the same type in matrix
  is_expected_class <- class(data[, 1]) %in% .filterable_class
  if (is_expected_class && !is.null(names(data))) {
    names(data)
  } else {
    character(0)
  }
}

#' @keywords internal
#' @export
get_supported_filter_varnames.FilteredDataset <- function(data) { # nolint
  get_supported_filter_varnames(data$get_dataset())
}

#' @keywords internal
#' @export
get_supported_filter_varnames.MAEFilteredDataset <- function(data) { # nolint
  character(0)
}


#' @title Returns a `choices_labeled` object
#'
#' @param data (`data.frame`, `DFrame`, `list`)\cr
#'   where labels can be taken from in case when `varlabels` is not specified.
#'   `data` must be specified if `varlabels` is not specified.
#' @param choices (`character`)\cr
#'  the array of chosen variables
#' @param varlabels (`character`)\cr
#'  the labels of variables in data
#' @param keys (`character`)\cr
#'  the names of the key columns in data
#' @return `character(0)` if choices are empty; a `choices_labeled` object otherwise
#' @keywords internal
data_choices_labeled <- function(data, choices, varlabels = character(0), keys = character(0)) {
  if (length(choices) == 0) {
    return(character(0))
  }

  choice_labels <- if (identical(varlabels, character(0))) {
    vapply(
      X = data,
      FUN.VALUE = character(1),
      FUN = function(x) {
        label <- attr(x, "label")
        if (length(label) != 1) {
          ""
        } else {
          label
        }
      }
    )[choices]
  } else {
    varlabels
  }

  if (!identical(choice_labels, character(0))) {
    choice_labels[is.na(choice_labels) | choice_labels == ""] <- names(
      choice_labels[is.na(choice_labels) | choice_labels == ""]
    )
    choice_types <- setNames(variable_types(data = data, columns = choices), choices)
    choice_types[keys] <- "primary_key"

    choices_labeled(
      choices = choices,
      labels = unname(choice_labels[choices]),
      types = choice_types[choices]
    )
  } else {
    choices
  }
}
