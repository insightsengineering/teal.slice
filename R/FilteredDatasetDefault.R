# DefaultFilteredDataset ----

#' @title `DefaultFilteredDataset` `R6` class
#' @description Stores any object as inert entity. Filtering is not supported.
#' @examples
#' # use non-exported function from teal.slice
#' DefaultFilteredDataset <- getFromNamespace("DefaultFilteredDataset", "teal.slice")
#'
#' ds <- DefaultFilteredDataset$new(letters, "letters")
#' isolate(ds$get_filter_state())
#' isolate(ds$get_call())
#' @keywords internal
DefaultFilteredDataset <- R6::R6Class( # nolint
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,

  ## Public Methods ----
  public = list(

    #' @description
    #' Initializes this `DefaultFilteredDataset` object.
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
      invisible(NULL)
    },
    #' @return `NULL`, invisibly.
    get_filter_state = function() {
      invisible(NULL)
    },
    #' @param state (`teal_slices`) for method consistency, ignored
    #' @return `NULL`, invisibly.
    set_filter_state = function(state) {
      if (length(state) != 0L) {
        warning("DefaultFilterState cannot set state")
      }
      invisible(NULL)
    },
    #' @param force (`logical`) for method consistency, ignored
    #' @return `NULL`, invisibly.
    clear_filter_states = function(force) {
      invisible(NULL)
    },

    #' @description
    #' Get `data.frame` to use in the filter-panel overview.
    #' Output of this function indicates unsupported dataset class.
    #' @return `list` containing character `#filtered/#not_filtered`
    get_filter_overview = function() {
      data.frame(dataname = private$dataname, obs = NA, obs_filtered = NA)
    },

    ### shiny modules ----

    #' @description
    #' Overwrites parent method.
    #' @details
    #' Blank module UI that would list active filter states for this dataset.
    #' @param id (`character(1)`)\cr
    #'  `shiny` module id
    #' @return empty `div`
    ui_active = function(id) {
      ns <- NS(id)
      div()
    },

    #' @description
    #' Overwrites parent method.
    #' @details
    #' Blank module UI that would list active filter states for this dataset.
    #' @param id (`character(1)`)\cr
    #'  `shiny` module id
    #' @return empty `div`
    ui_add = function(id) {
      ns <- NS(id)
      div()
    }
  ),
  private = list(
    ## Private Methods ----
    ## Private Fields ----
  )
)
