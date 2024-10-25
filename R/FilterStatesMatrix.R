# MatrixFilterStates ------

#' @name MatrixFilterStates
#' @docType class
#' @title `FilterStates` subclass for matrices
#' @description Handles filter states in a `matrix`.
#' @keywords internal
#'
MatrixFilterStates <- R6::R6Class( # nolint
  classname = "MatrixFilterStates",
  inherit = FilterStates,

  # public methods ----
  public = list(
    #' @description
    #' Initialize `MatrixFilterStates` object.
    #'
    #' @param data (`matrix`)
    #'   the `R` object which `subset` function is applied on.
    #' @param data_reactive (`function(sid)`)
    #'   should return a `matrix` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated on a change in filters.
    #'   If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)
    #'   name of the data used in the subset expression.
    #'   Passed to the function argument attached to this `FilterStates`.
    #' @param datalabel (`character(1)`) optional
    #'   text label. Should be a name of experiment.
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = NULL) {
      checkmate::assert_matrix(data)
      super$initialize(data, data_reactive, dataname, datalabel)
      private$set_filterable_varnames(include_varnames = colnames(private$data))
      if (!is.null(datalabel)) {
        private$dataname_prefixed <- sprintf(
          "%s[['%s']]", private$dataname_prefixed, datalabel
        )
      }
    }
  ),
  private = list(
    extract_type = "matrix"
  )
)
