# DefaultFilteredDataset ------
#' @title The `DefaultFilteredDataset` R6 class
#' @keywords internal
#' @examples
#' library(shiny)
#' ds <- teal.slice:::DefaultFilteredDataset$new(iris, "iris")
#' isolate(
#'   ds$set_filter_state(
#'     state = list(
#'       Species = list(selected = "virginica"),
#'       Petal.Length = list(selected = c(2.0, 5))
#'     )
#'   )
#' )
#' isolate(ds$get_filter_state())
#' isolate(ds$get_call())
DefaultFilteredDataset <- R6::R6Class( # nolint
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,
  public = list(

    #' @description
    #' Initializes this `DefaultFilteredDataset` object
    #'
    #' @param dataset (`data.frame`)\cr
    #'  single data.frame for which filters are rendered
    #' @param dataname (`character`)\cr
    #'  A given name for the dataset it may not contain spaces
    #' @param keys optional, (`character`)\cr
    #'   Vector with primary keys
    #' @param label (`character`)\cr
    #'   Label to describe the dataset
    #' @param metadata (named `list` or `NULL`) \cr
    #'   Field containing metadata about the dataset. Each element of the list
    #'   should be atomic and length one.
    initialize = function(dataset, dataname, keys = character(0), label = character(0), metadata = NULL) {
      checkmate::assert_class(dataset, "data.frame")
      super$initialize(dataset, dataname, keys, label, metadata)
      dataname <- self$get_dataname()

      private$add_filter_states(
        filter_states = init_filter_states(
          data = self$get_dataset(),
          dataname = dataname,
          varlabels = self$get_varlabels(),
          keys = self$get_keys()
        ),
        id = "filter"
      )
      invisible(self)
    },

    #' @description
    #' Gets the filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains single `FilterStates`
    #' which contains single `state_list` and all `FilterState` objects
    #' applies to one argument (`...`) in `dplyr::filter` call.
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      filter_call <- Filter(
        f = Negate(is.null),
        x = lapply(
          self$get_filter_states(),
          function(x) x$get_call()
        )
      )
      if (length(filter_call) == 0) {
        return(NULL)
      }
      filter_call
    },

    #' @description
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Get all active filters from this dataset in form of the nested list.
    #' The output list is a compatible input to `self$set_filter_state`.
    #' @return `list` with named elements corresponding to `FilterState` objects
    #' (active filters).
    get_filter_state = function() {
      self$get_filter_states("filter")$get_filter_state()
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`named list`)\cr
    #'  containing values of the initial filter. Values should be relevant
    #'  to the referred column.
    #' @param ... Additional arguments. Note that this is currently not used
    #' @examples
    #' dataset <- teal.slice:::DefaultFilteredDataset$new(iris, "iris")
    #' fs <- list(
    #'   Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    #'   Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    #' )
    #' shiny::isolate(dataset$set_filter_state(state = fs))
    #' shiny::isolate(dataset$get_filter_state())
    #'
    #' @return `NULL`
    set_filter_state = function(state, ...) {
      checkmate::assert_list(state)
      logger::log_trace(
        sprintf(
          "DefaultFilteredDataset$set_filter_state setting up filters of variables %s, dataname: %s",
          paste(names(state), collapse = ", "),
          self$get_dataname()
        )
      )

      data <- self$get_dataset()
      fs <- self$get_filter_states()[[1]]
      fs$set_filter_state(state = state, data = data, ...)
      logger::log_trace(
        sprintf(
          "DefaultFilteredDataset$set_filter_state done setting up filters of variables %s, dataname: %s",
          paste(names(state), collapse = ", "),
          self$get_dataname()
        )
      )
      NULL
    },

    #' @description Remove one or more `FilterState` of a `FilteredDataset`
    #'
    #' @param state_id (`character`)\cr
    #'  Vector of character names of variables to remove their `FilterState`.
    #'
    #' @return `NULL`
    remove_filter_state = function(state_id) {
      logger::log_trace(
        sprintf(
          "DefaultFilteredDataset$remove_filter_state removing filters of variable %s, dataname: %s",
          state_id,
          self$get_dataname()
        )
      )

      fdata_filter_state <- self$get_filter_states()[[1]]
      for (element in state_id) {
        fdata_filter_state$remove_filter_state(element)
      }
      logger::log_trace(
        sprintf(
          "DefaultFilteredDataset$remove_filter_state done removing filters of variable %s, dataname: %s",
          state_id,
          self$get_dataname()
        )
      )
      invisible(NULL)
    },

    #' @description
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_add_filter_state = function(id) {
      ns <- NS(id)
      tagList(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        self$get_filter_states(id = "filter")$ui_add_filter_state(
          id = ns("filter"),
          data = self$get_dataset()
        )
      )
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add_filter_state` calls single module
    #' `srv_add_filter_state` from `FilterStates` (`DefaultFilteredDataset`
    #' contains single `FilterStates`)
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param ... other arguments passed on to child `FilterStates` methods.
    #'
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, ...) {
      check_ellipsis(..., stop = FALSE, allowed_args = "vars_include")
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "DefaultFilteredDataset$srv_add_filter_state initializing, dataname: { deparse1(self$get_dataname()) }"
          )
          data <- self$get_dataset()
          self$get_filter_states(id = "filter")$srv_add_filter_state(
            id = "filter",
            data = data,
            ...
          )
          logger::log_trace(
            "DefaultFilteredDataset$srv_add_filter_state initialized, dataname: { deparse1(self$get_dataname()) }"
          )
          NULL
        }
      )
    },

    #' @description
    #' Get number of observations based on given keys
    #' The output shows the comparison between `filtered_dataset`
    #' function parameter and the dataset inside self
    #' @param filtered_dataset comparison object, of the same class
    #' as `self$get_dataset()`, if `NULL` then `self$get_dataset()`
    #' is used.
    #' @param subject_keys (`character` or `NULL`) columns denoting unique subjects when
    #' calculating the filtering.
    #' @return `list` containing character `#filtered/#not_filtered`
    get_filter_overview_nsubjs = function(filtered_dataset = self$get_dataset(), subject_keys = NULL) {
      checkmate::assert_class(filtered_dataset, classes = class(self$get_dataset()))
      checkmate::assert_character(subject_keys, null.ok = TRUE, any.missing = FALSE)

      f_rows <- if (length(subject_keys) == 0) {
        dplyr::n_distinct(filtered_dataset)
      } else {
        dplyr::n_distinct(filtered_dataset[subject_keys])
      }

      nf_rows <- if (length(subject_keys) == 0) {
        dplyr::n_distinct(self$get_dataset())
      } else {
        dplyr::n_distinct(self$get_dataset()[subject_keys])
      }

      list(paste0(f_rows, "/", nf_rows))
    }
  ),
  private = list(
    # Gets filter overview observations number and returns a
    # list of the number of observations of filtered/non-filtered datasets
    get_filter_overview_nobs = function(filtered_dataset) {
      f_rows <- nrow(filtered_dataset)
      nf_rows <- nrow(self$get_dataset())
      list(
        paste0(f_rows, "/", nf_rows)
      )
    }
  )
)
