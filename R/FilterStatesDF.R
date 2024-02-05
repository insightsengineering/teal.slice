# DFFilterStates ------

#' @name DFFilterStates
#' @docType class
#'
#' @title `FilterStates` subclass for data frames
#'
#' @description Handles filter states in a `data.frame`.
#'
#' @keywords internal
#'
DFFilterStates <- R6::R6Class( # nolint
  classname = "DFFilterStates",
  inherit = FilterStates,

  # public methods ----
  public = list(
    #' @description
    #' Initializes `DFFilterStates` object by setting `dataname`
    #'  and initializing `state_list` (`shiny::reactiveVal`).
    #' This class contains a single `state_list` with no specified name,
    #' which means that when calling the subset function associated with this class
    #' (`dplyr::filter`), a list of conditions is passed to unnamed arguments (`...`).
    #'
    #' @param data (`data.frame`)
    #'   the `R` object which `dplyr::filter` function will be applied on.
    #' @param data_reactive (`function(sid)`)
    #'   should return a `data.frame` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated on a change in filters.
    #'   If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character`)
    #'   name of the data used in the *subset expression*.
    #'   Passed to the function argument attached to this `FilterStates`.
    #' @param datalabel (`character(1)`)
    #'   optional text label.
    #' @param keys (`character`)
    #'   key column names.
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = NULL,
                          keys = character(0)) {
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_data_frame(data)
      super$initialize(data, data_reactive, dataname, datalabel)
      private$keys <- keys
      private$set_filterable_varnames(include_varnames = colnames(private$data))
    }
  ),

  # private members ----
  private = list(
    fun = quote(dplyr::filter)
  )
)
