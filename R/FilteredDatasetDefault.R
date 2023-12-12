# DefaultFilteredDataset ----

#' @title The `DefaultFilteredDataset` R6 class
#' @description Stores any object as inert entity. Filtering is not supported.
#' @keywords internal
#' @examples
#' library(shiny)
#' ds <- teal.slice:::DefaultFilteredDataset$new(letters, "letters")
#' isolate(ds$get_filter_state())
#' isolate(ds$get_call())
DefaultFilteredDataset <- R6::R6Class( #nolint
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,

  ## Public Methods ----
  public = list(

    #' @description
    #' Initializes this `DefaultFilteredDataset` object
    #'
    #' @param dataset
    #'  Any type of object; will not be filtered.
    #' @param dataname (`character(1)`)\cr
    #'  Name given to the dataset; must not contain spaces.
    #' @param label (`character(1)`)\cr
    #'   Label to describe the dataset.
    initialize = function(dataset, dataname, label = character(0)) {
      super$initialize(dataset = dataset, dataname = dataname, label = label)
    },

    #' @description
    #' Returns a formatted string representing this `DefaultFilteredDataset` object.
    #'
    #' @param show_all (`logical(1)`) for method consistency, ignored
    #' @param trim_lines (`logical(1)`) flag specifying whether to trim lines if class names are too long
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(show_all, trim_lines = FALSE) {
      class_string <- toString(class(private$dataset))
      if (trim_lines) {
        trim_position <- 37L
        class_string <- strtrim(class_string, trim_position)
        substr(class_string, 35L, 37L) <- "..."
      }
      sprintf(" - unfiltered dataset:\t\"%s\":   %s", private$dataname, class_string)
    },

    #' @param sid (`character`) for method consistency, ignored
    #' @return `NULL`, invisibly.
    get_call = function(sid) {
      warning("DefaultFilterState does not create filter calls")
      invisible(NULL)
    },
    #' @return `NULL`, invisibly.
    get_filter_state = function() {
      warning("DefaultFilterState does not have state to return")
      invisible(NULL)
    },
    #' @param state (`teal_slices`) for method consistency, ignored
    #' @return `NULL`, invisibly.
    set_filter_state = function(state) {
      warning("DefaultFilterState cannot set state")
      invisible(NULL)
    },
    #' @param force (`logical`) for method consistency, ignored
    #' @return `NULL`, invisibly.
    clear_filter_states = function(force) {
      warning("DefaultFilterState does not have filter states to clear")
      invisible(NULL)
    },

    #' @return `NULL`, invisibly
    get_filter_overview = function() {
      invisible(NULL)
    }
  ),
  private = list(
    ## Private Methods ----
    add_filter_states = function(filter_states, id) {
      warning("DefaultFilterState cannnot add filter states")
    }

    ## Private Fields ----
  )
)
