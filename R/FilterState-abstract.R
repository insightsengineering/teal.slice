#' @name FilterState
#' @docType class
#'
#'
#' @title FilterState Abstract Class
#'
#' @description Abstract class to encapsulate filter states
#'
#' @details
#' This class is responsible for managing single filter item within
#' `FilteredData` class. Filter states depend on the variable type:
#' (`logical`, `integer`, `numeric`, `factor`, `character`, `Date`, `POSIXct`, `POSIXlt`)
#' and returns `FilterState` object with class corresponding to input variable.
#' Class controls single filter entry in `module_single_filter_item` and returns
#' code relevant to selected values.
#' - `factor`, `character`: `class = ChoicesFilterState`
#' - `numeric`: `class = RangeFilterState`
#' - `logical`: `class = LogicalFilterState`
#' - `Date`: `class = DateFilterState`
#' - `POSIXct`, `POSIXlt`: `class = DatetimeFilterState`
#' - all `NA` entries: `class: FilterState`, cannot be filtered
#' - default: `FilterState`, cannot be filtered
#' \cr
#' Each variable's filter state is an `R6` object which contains `choices`,
#' `selected`, `varname`, `dataname`, `labels`, `na_count`, `keep_na` and other
#' variable type specific fields (`keep_inf`, `inf_count`, `timezone`).
#' Object contains also shiny module (`ui` and `server`) which manages
#' state of the filter through reactive values `selected`, `keep_na`, `keep_inf`
#' which trigger `get_call()` and every R function call up in reactive
#' chain.
#' \cr
#' \cr
#' @section Modifying state:
#' Modifying a `FilterState` object is possible in three scenarios:
#' * In the interactive session by directly specifying values of `selected`,
#'   `keep_na` or `keep_inf` using `set_state` method (to update all at once),
#'   or using `set_selected`, `set_keep_na` or `set_keep_inf`
#' * In a running application by changing appropriate inputs
#' * In a running application by using [filter_state_api] which directly uses `set_state` method
#'  of the `FilterState` object.
#'
#' @keywords internal
FilterState <- R6::R6Class( # nolint
  "FilterState",

  # public methods ----
  public = list(
    #' @description
    #' Initialize a `FilterState` object
    #' @param x (`vector`)\cr
    #'   values of the variable used in filter
    #' @param varname (`character`)\cr
    #'   name of the variable
    #' @param varlabel (`character(1)`)\cr
    #'   label of the variable (optional).
    #' @param dataname (`character(1)`)\cr
    #'   name of dataset where `x` is taken from.
    #' @return self invisibly
    #'
    initialize = function(x,
                          varname,
                          varlabel = character(0),
                          dataname = NULL,
                          extract_type = character(0)) {
      checkmate::assert_string(varname)
      checkmate::assert_character(varlabel, max.len = 1, any.missing = FALSE)
      checkmate::assert_string(dataname, null.ok = TRUE)

      private$dataname <- dataname
      private$varname <- varname
      private$varlabel <- if (identical(varlabel, as.character(varname))) {
        # to not display duplicated label
        character(0)
      } else {
        varlabel
      }
      invisible(self)
    },

    #' @description
    #' Returns reproducible condition call for current selection relevant
    #' for selected variable type.
    #' Method is using internal reactive values which makes it reactive
    #' and must be executed in reactive or isolated context.
    #'
    get_call = function() {
      NULL
    }
  ),
  # private members ----
  private = list(
    dataname = character(0),
    varname = character(0),
    varlabel = character(0)
  )
)
