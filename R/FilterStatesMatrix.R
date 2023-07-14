#' @title `FilterStates` subclass for matrices
#' @description Handles filter states in a `matrix`
#' @keywords internal
#'
#'
MatrixFilterStates <- R6::R6Class( # nolint
  classname = "MatrixFilterStates",
  inherit = FilterStates,

  # public methods ----
  public = list(
    #' @description Initialize `MatrixFilterStates` object
    #'
    #' Initialize `MatrixFilterStates` object
    #'
    #' @param data (`matrix`)\cr
    #'   the R object which `subset` function is applied on.
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `matrix` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #' @param datalabel (`NULL` or `character(1)`)\cr
    #'   text label value. Should be a name of experiment.
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = NULL) {
      checkmate::assert_matrix(data)
      super$initialize(data, data_reactive, dataname, datalabel)
      private$set_filterable_varnames(include_varnames = colnames(private$data))
    }
  ),
  private = list(
    extract_type = "matrix"
  )
)
