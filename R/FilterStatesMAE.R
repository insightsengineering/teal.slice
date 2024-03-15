# MAEFilterStates ------

#' @name MAEFilterStates
#' @docType class
#' @title `FilterStates` subclass for `MultiAssayExperiment`s
#' @description Handles filter states in a `MultiAssayExperiment`.
#' @keywords internal
#'
MAEFilterStates <- R6::R6Class( # nolint
  classname = "MAEFilterStates",
  inherit = FilterStates,
  # public methods ----
  public = list(
    #' @description
    #' Initialize `MAEFilterStates` object.
    #'
    #' @param data (`MultiAssayExperiment`)
    #'   the `R` object which `MultiAssayExperiment::subsetByColData` function is applied on.
    #' @param data_reactive (`function(sid)`)
    #'   should return a `MultiAssayExperiment` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)
    #'   name of the data used in the subset expression.
    #'   Passed to the function argument attached to this `FilterStates`.
    #' @param datalabel (`character(1)`) optional
    #'   text label.
    #' @param varlabels (`character`)
    #'   labels of the variables used in this object.
    #' @param keys (`character`)
    #'   key column names.
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

      invisible(self)
    }
  ),

  # private fields ----

  private = list(
    extract_type = "list",
    fun = quote(MultiAssayExperiment::subsetByColData)
  )
)
