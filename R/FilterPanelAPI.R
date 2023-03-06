#' @name FilterPanelAPI
#' @docType class
#'
#' @title Class to encapsulate the API of the filter panel of a teal app
#'
#' @details
#'   The purpose of this class is to encapsulate the API of the filter panel in a new class `FilterPanelAPI` so
#'   that it can be passed and used in the `server` call of any module instead of passing the whole `FilteredData`
#'   object.
#'
#'   This class is supported by methods to set, get, remove filter states in the filter panel API.
#'
#' @export
#'
#' @examples
#' library(teal.slice)
#' fd <- teal.slice::init_filtered_data(list(iris = list(dataset = iris)))
#' fpa <- FilterPanelAPI$new(fd)
#'
#' # get the actual filter state --> empty named list
#' isolate(fpa$get_filter_state())
#'
#' # set a filter state
#' isolate(
#'   set_filter_state(
#'     fpa,
#'     list(iris = list(Species = list(selected = "setosa", keep_na = TRUE)))
#'   )
#' )
#'
#' # get the actual filter state --> named list with filters
#' isolate(fpa$get_filter_state())
#'
#' # remove all_filter_states
#' fpa$clear_filter_states()
#'
#' # get the actual filter state --> empty named list
#' isolate(fpa$get_filter_state())
#'
FilterPanelAPI <- R6::R6Class( # nolint
  "FilterPanelAPI",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initialize a `FilterPanelAPI` object
    #' @param datasets (`FilteredData`) object.
    initialize = function(datasets) {
      checkmate::assert_class(datasets, "FilteredData")
      private$filtered_data <- datasets
    },

    #' @description
    #' Gets the reactive values from the active `FilterState` objects of the `FilteredData` object.
    #'
    #' Gets all active filters in the form of a nested list.
    #' The output list is a compatible input to `set_filter_state`.
    #'
    #' @return `list` with named elements corresponding to `FilteredDataset` objects with active filters.
    get_filter_state = function() {
      private$filtered_data$get_filter_state()
    },

    #' @description
    #' Sets active filter states.
    #' @param filter (`named list`)\cr
    #' nested list of filter selections applied to datasets.
    #'
    #' @return `NULL`
    set_filter_state = function(filter) {
      if (private$filtered_data$get_filter_panel_active()) {
        private$filtered_data$set_filter_state(filter)
      } else {
        warning(private$deactivated_msg)
      }
      invisible(NULL)
    },

    #' @description
    #' Remove one or more `FilterState` of a `FilteredDataset` in the `FilteredData` object.
    #' @param filter (`named list`)\cr
    #'  nested list of filter selections applied to datasets.
    #'
    #' @return `NULL`
    remove_filter_state = function(filter) {
      if (private$filtered_data$get_filter_panel_active()) {
        private$filtered_data$remove_filter_state(filter)
      } else {
        warning(private$deactivated_msg)
      }
      invisible(NULL)
    },

    #' @description Remove all `FilterStates` of the `FilteredData` object.
    #'
    #' @param datanames (`character`)\cr
    #'  datanames to remove their `FilterStates`;
    #'  omit to remove all `FilterStates` in the `FilteredData` object
    #'
    #' @return `NULL`
    #'
    clear_filter_states = function(datanames) {
      if (private$filtered_data$get_filter_panel_active()) {
        datanames_to_remove <- if (missing(datanames)) private$filtered_data$datanames() else datanames
        private$filtered_data$clear_filter_states(datanames = datanames_to_remove)
      } else {
        warning(private$deactivated_msg)
      }
      invisible(NULL)
    },
    #' @description
    #' Toggle the state of the global Filter Panel button by running `javascript` code
    #' to click the toggle button with the `filter_panel_active` id suffix.
    #' The button id is prefixed with the Filter Panel shiny namespace.
    #' This button is observed in `srv_filter_panel` method that executes
    #' `filter_panel_enable()` or `filter_panel_disable()` method depending on the toggle state.
    #'
    #' @return `NULL`
    filter_panel_toggle = function() {
      shinyjs::runjs(
        sprintf(
          '$("#%s-filter_turn_onoff").click();',
          private$filtered_data$get_filter_panel_ui_id()
        )
      )
      invisible(NULL)
    }
  ),
  ## __Private Methods ====
  private = list(
    filtered_data = NULL,
    deactivated_msg = "Filter Panel is deactivated so the action can not be applied with api."
  )
)
