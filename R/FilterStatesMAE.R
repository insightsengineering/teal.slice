#' @title `FilterStates` subclass for `MultiAssayExperiments`
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
    #' @param datalabel (`NULL` or `character(1)`)\cr
    #'   text label value
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #' @param keys (`character`)\cr
    #'   key columns names
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = "subjects",
                          keys = character(0)) {
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_class(data, "MultiAssayExperiment")
      data <- SummarizedExperiment::colData(data)
      data_reactive <- function(sid = "") SummarizedExperiment::colData(data_reactive(sid = sid))
      super$initialize(data, data_reactive, dataname, datalabel)
      private$keys <- keys
      private$set_filterable_varnames(include_varnames = colnames(data))
      return(invisible(self))
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
    fun = quote(MultiAssayExperiment::subsetByColData)
  )
)
