#' @title `FilterStates` subclass for MultiAssayExperiments
#' @description Handles filter states in a `MultiAssayExperiment`
#' @keywords internal
#'
#'
MAEFilterStates <- R6::R6Class( # nolint
  classname = "MAEFilterStates",
  inherit = FilterStates,
  public = list(
    # public methods ----

    #' @description Initializes `MAEFilterStates` object
    #'
    #' Initialize `MAEFilterStates` object
    #'
    #' @param data (`MultiAssayExperiment`)\cr
    #'   the R object which `MultiAssayExperiment::subsetByColData` function is applied on.
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `MultiAssayExperiment` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #' @param keys (`character`)\cr
    #'   key columns names
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = "subjects",
                          varlabels = character(0),
                          keys = character(0)) {
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_class(data, "MultiAssayExperiment")
      data <- SummarizedExperiment::colData(data)
      data_reactive <- function(sid = character(0)) SummarizedExperiment::colData(data_reactive(sid = sid))
      super$initialize(data, data_reactive, dataname, datalabel)
      private$keys <- keys
      private$varlabels <- varlabels
      private$set_filterable_varnames(include_varnames = colnames(data))
      return(invisible(self))
    },

    #' @description
    #' Returns the formatted string representing this `MAEFilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before each line of the representation
    #' @return `character(1)` the formatted string
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      if (length(private$state_list_get()) > 0) {
        formatted_states <- sprintf("%sSubject filters:", format("", width = indent))
        for (state in private$state_list_get()) {
          formatted_states <- c(formatted_states, state$format(indent = indent * 2))
        }
        paste(formatted_states, collapse = "\n")
      }
    },

    # shiny modules ----

    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @return shiny.tag
    ui_add = function(id) {
      data <- private$data
      checkmate::assert_string(id)

      ns <- NS(id)

      if (ncol(data) == 0) {
        div("no sample variables available")
      } else if (nrow(data) == 0) {
        div("no samples available")
      } else {
        teal.widgets::optionalSelectInput(
          ns("var_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select subject variable"
          )
        )
      }
    }
  ),

  # private fields ----

  private = list(
    extract_type = "list",
    fun = quote(MultiAssayExperiment::subsetByColData),
    keys = character(0),
    varlabels = character(0)
  )
)
