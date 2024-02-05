# DefaultFilteredDataset ----

#' @name DefaultFilteredDataset
#' @docType class
#' @title `DefaultFilteredDataset` `R6` class
#'
#' @description Stores any object as inert entity. Filtering is not supported.
#'
#' @keywords internal
#'
DefaultFilteredDataset <- R6::R6Class( # nolint
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,

  # public methods ----
  public = list(

    #' @description
    #' Initializes this `DefaultFilteredDataset` object.
    #'
    #' @param dataset
    #'  any type of object; will not be filtered.
    #' @param dataname (`character(1)`)
    #'  syntactically valid name given to the dataset.
    #' @param label (`character(1)`)
    #'   label to describe the dataset.
    #'
    #' @return Object of class `DefaultFilteredDataset`, invisibly.
    #'
    initialize = function(dataset, dataname, label = character(0)) {
      super$initialize(dataset = dataset, dataname = dataname, label = label)
    },

    #' @description
    #' Returns a formatted string representing this `DefaultFilteredDataset` object.
    #'
    #' @param show_all (`logical(1)`) for method consistency, ignored.
    #' @param trim_lines (`logical(1)`) flag specifying whether to trim lines if class names are too long.
    #'
    #' @return The formatted string.
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

    #' @param sid (`character(1)`) for method consistency, ignored.
    #' @return `NULL`, invisibly.
    get_call = function(sid) {
      invisible(NULL)
    },
    #' @return `NULL`, invisibly.
    get_filter_state = function() {
      invisible(NULL)
    },
    #' @param state (`teal_slices`) for method consistency, ignored.
    #' @return `NULL`, invisibly.
    set_filter_state = function(state) {
      if (length(state) != 0L) {
        warning("DefaultFilterState cannot set state")
      }
      invisible(NULL)
    },
    #' @param force (`logical(1)`) for method consistency, ignored.
    #' @return `NULL`, invisibly.
    clear_filter_states = function(force) {
      invisible(NULL)
    },

    #' @description
    #' Creates row for filter overview in the form of \cr
    #' `dataname` - unsupported data class
    #' @return A `data.frame`.
    get_filter_overview = function() {
      data.frame(dataname = private$dataname, obs = NA, obs_filtered = NA)
    },

    # shiny modules ----

    #' @description
    #' Overwrites parent method.
    #' @details
    #' Blank UI module that would list active filter states for this dataset.
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #' @return An empty `div`.
    ui_active = function(id) {
      ns <- NS(id)
      div()
    },

    #' @description
    #' Overwrites parent method.
    #' @details
    #' Blank UI module that would list active filter states for this dataset.
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #' @return An empty `div`.
    ui_add = function(id) {
      ns <- NS(id)
      div()
    }
  ),
  private = list(
    # private methods ----
    # private fields ----
  )
)
