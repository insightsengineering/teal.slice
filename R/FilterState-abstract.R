#' @name FilterState
#' @docType class
#'
#'
#' @title FilterState Abstract Class
#'
#' @description Abstract class to encapsulate filter states
#'
#' @details
#' This abstract class to encapsulate [InteractiveFilterState]
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
    #' Returns reproducible condition call
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
