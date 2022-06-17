#' Initializes `FilteredDataset`
#'
#' @keywords internal
#' @examples
#' # DefaultFilteredDataset example
#' iris_fd <- init_filtered_dataset(iris, dataname = "iris", metadata = list(type = "teal"))
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     iris_fd$ui_add_filter_state(id = "add"),
#'     iris_fd$ui("dataset"),
#'     verbatimTextOutput("call"),
#'     verbatimTextOutput("metadata")
#'   ),
#'   server = function(input, output, session) {
#'     iris_fd$srv_add_filter_state(id = "add")
#'     iris_fd$server(id = "dataset")
#'
#'     output$metadata <- renderText({
#'       paste("Type =", iris_fd$get_metadata()$type)
#'     })
#'
#'     output$call <- renderText({
#'       paste(
#'         vapply(iris_fd$get_call(), deparse1, character(1), collapse = "\n"),
#'         collapse = "\n"
#'       )
#'     })
#'   }
#' )
#' }
#'
#' # MAEFilteredDataset example
#' library(MultiAssayExperiment)
#' data(miniACC)
#' MAE_fd <- init_filtered_dataset(miniACC, "MAE", metadata = list(type = "MAE"))
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     MAE_fd$ui_add_filter_state(id = "add"),
#'     MAE_fd$ui("dataset"),
#'     verbatimTextOutput("call"),
#'     verbatimTextOutput("metadata")
#'   ),
#'   server = function(input, output, session) {
#'     MAE_fd$srv_add_filter_state(id = "add")
#'     MAE_fd$server(id = "dataset")
#'     output$metadata <- renderText({
#'       paste("Type =", MAE_fd$get_metadata()$type)
#'     })
#'     output$call <- renderText({
#'       paste(
#'         vapply(MAE_fd$get_call(), deparse1, character(1), collapse = "\n"),
#'         collapse = "\n"
#'       )
#'     })
#'   }
#' )
#' }
#' @param dataset (`data.frame` or `MultiAssayExperiment`)\cr
#' @param dataname (`character`)\cr
#'  A given name for the dataset it may not contain spaces
#' @param keys optional, (`character`)\cr
#'   Vector with primary keys
#' @param label (`character`)\cr
#'   Label to describe the dataset
#' @param metadata (named `list` or `NULL`) \cr
#'   Field containing metadata about the dataset. Each element of the list
#'   should be atomic and length one.
init_filtered_dataset <- function(dataset, # nolint
                                  dataname,
                                  keys = character(0),
                                  label = attr(dataset, "label"),
                                  metadata = NULL) {
  UseMethod("init_filtered_dataset")
}

#' @keywords internal
#' @export
init_filtered_dataset.data.frame <- function(dataset, # nolint
                                             dataname,
                                             keys = character(0),
                                             label = attr(dataset, "label"),
                                             metadata = NULL) {
  DefaultFilteredDataset$new(dataset, dataname, keys, label, metadata)
}

#' @keywords internal
#' @export
init_filtered_dataset.MultiAssayExperiment <- function(dataset, # nolint
                                                       dataname,
                                                       keys = character(0),
                                                       label = attr(dataset, "label"),
                                                       metadata = NULL) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
  }
  MAEFilteredDataset$new(dataset, dataname, keys, label, metadata)
}

# FilteredDataset abstract --------
#' @title `FilterStates` R6 class
#' @description
#' `FilteredDataset` is a class which renders/controls `FilterStates`(s)
#' Each `FilteredDataset` contains `filter_states` field - a `list` which contains one
#' (`data.frame`) or multiple (`MultiAssayExperiment`) `FilterStates` objects.
#' Each `FilterStates` is responsible for one filter/subset expression applied for specific
#' components of the dataset.
#' @keywords internal
FilteredDataset <- R6::R6Class( # nolint
  "FilteredDataset",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initializes this `FilteredDataset` object
    #'
    #' @param dataset (`data.frame` or `MultiAssayExperiment`)\cr
    #'  single dataset for which filters are rendered
    #' @param dataname (`character`)\cr
    #'  A given name for the dataset it may not contain spaces
    #' @param keys optional, (`character`)\cr
    #'   Vector with primary keys
    #' @param label (`character`)\cr
    #'   Label to describe the dataset
    #' @param metadata (named `list` or `NULL`) \cr
    #'   Field containing metadata about the dataset. Each element of the list
    #'   should be atomic and length one.
    initialize = function(dataset, dataname, keys = character(0), label = attr(dataset, "label"), metadata = NULL) {
      # dataset assertion in child classes

      checkmate::assert_string(dataname)
      if (!checkmate::test_string(dataname, pattern = "^\\S+$")) {
        stop("dataname should not contain spaces")
      }

      checkmate::assert_character(keys, any.missing = FALSE)
      checkmate::assert_character(label, null.ok = TRUE)
      teal.data::validate_metadata(metadata)

      logger::log_trace("Instantiating { class(self)[1] }, dataname: { deparse1(dataname) }")
      private$dataset <- dataset
      private$dataname <- dataname
      private$keys <- keys
      private$label <- if (is.null(label)) character(0) else label
      private$metadata <- metadata
      private$calculate_hash()
      invisible(self)
    },


    #' @description
    #' Returns a string representation of the filter state in this `FilteredDataset`.
    #'
    #' @return `character(1)` the formatted string representing the filter state
    #'
    get_formatted_filter_state = function() {
      out <- paste0("Filters for dataset: ", self$get_dataname())
      for (states in self$get_filter_states()) out <- c(out, states$format(indent = 2))
      paste(out, collapse = "\n")
    },

    #' @description
    #' Removes all active filter items applied to this dataset
    #' @return NULL
    queues_empty = function() {
      logger::log_trace("Removing all filters from FilteredDataset: { deparse1(self$get_dataname()) }")
      lapply(
        self$get_filter_states(),
        function(queue) queue$queue_empty()
      )
      logger::log_trace("Removed all filters from FilteredDataset: { deparse1(self$get_dataname()) }")
      NULL
    },
    # managing filter states -----


    # getters ----
    #' @description
    #' Gets a filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      stop("Pure virtual method.")
    },

    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Get all active filters from this dataset in form of the nested list.
    #' The output list is a compatible input to `self$set_filter_state`.
    #' @return `list` with named elements corresponding to `FilterStates` objects
    #' with active filters.
    get_filter_state = function() {
      states <- lapply(self$get_filter_states(), function(x) x$get_filter_state())
      Filter(function(x) length(x) > 0, states)
    },

    #' @description
    #' Gets the active `FilterStates` objects.
    #' @param id (`character(1)`, `character(0)`)\cr
    #'   the id of the `private$filter_states` list element where `FilterStates` is kept.
    #' @return `FilterStates` or `list` of `FilterStates` objects.
    get_filter_states = function(id = character(0)) {
      if (length(id) == 0) {
        private$filter_states
      } else {
        private$filter_states[[id]]
      }
    },

    #' @description
    #' Get name of the dataset
    #'
    #' Get name of the dataset
    #' @return `character(1)` as a name of this dataset
    get_dataname = function() {
      private$dataname
    },

    #' @description
    #' Gets the dataset object in this `FilteredDataset`
    #' @return `data.frame` or `MultiAssayExperiment`
    get_dataset = function() {
      private$dataset
    },

    #' @description
    #' Gets the metadata for the dataset in this `FilteredDataset`
    #' @return named `list` or `NULL`
    get_metadata = function() {
      private$metadata
    },

    #' @description
    #' Get filter overview rows of a dataset
    #' The output shows the comparison between `filtered_dataset`
    #' function parameter and the dataset inside self
    #' @param filtered_dataset comparison object, of the same class
    #' as `self$get_dataset()`, if `NULL` then `self$get_dataset()`
    #' is used.
    #' @return (`matrix`) matrix of observations and subjects
    get_filter_overview_info = function(filtered_dataset = NULL) {

      checkmate::assert_class(filtered_dataset, classes = class(self$get_dataset()), null.ok = TRUE)

      if (is.null(filtered_dataset)) {
        filtered_dataset <- self$get_dataset()
      }
      df <- cbind(private$get_filter_overview_nobs(filtered_dataset), "")
      rownames(df) <- self$get_dataname()
      colnames(df) <- c("Obs", "Subjects")
      df
    },

    #' @description
    #' Returns the hash of the dataset
    #' @return (`character(1)`) the hash
    get_hash = function() {
      private$hash
    },

    #' @description
    #' Gets the keys for the dataset of this `FilteredDataset`
    #' @return (`character`) the keys of dataset
    get_keys = function() {
      private$keys
    },

    #' @description
    #' Gets labels of variables in the data
    #'
    #' Variables are the column names of the data.
    #' Either, all labels must have been provided for all variables
    #' in `set_data` or `NULL`.
    #'
    #' @param variables (`character` vector) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #' @return (`character` or `NULL`) variable labels, `NULL` if `column_labels`
    #'   attribute does not exist for the data
    get_varlabels = function(variables = NULL) {
      checkmate::assert_character(variables, null.ok = TRUE, any.missing = FALSE)

      labels <- formatters::var_labels(private$dataset, fill = FALSE)
      if (is.null(labels)) {
        return(NULL)
      }
      if (!is.null(variables)) labels <- labels[intersect(self$get_varnames(), variables)]
      labels
    },

    #' @description
    #' Gets the dataset label
    #' @return (`character`) the dataset label
    get_dataset_label = function() {
      private$label
    },

    #' @description
    #' Gets variable names from dataset
    #' @return `character` the variable names
    get_varnames = function() {
      colnames(self$get_dataset())
    },

    #' @description
    #' Gets the suffixed dataname
    #' Used when filtering the data to get `<dataname>_FILTERED`,
    #' `<dataname>_FILTERED_ALONE` or any other name.
    #' @param dataname (`character(1)`) dataname
    #' @param suffix (`character(1)`) string to be putted after dataname
    #' @return `character(1)`
    get_filtered_dataname = function(dataname = self$get_dataname(), suffix = "_FILTERED") {
      paste0(dataname, suffix)
    },

    #' @description
    #' Gets variable names for the filtering.
    #'
    #' @return (`character` vector) of variable names
    get_filterable_varnames = function() {
      get_filterable_varnames(self$get_dataset())
    },

    # modules ------
    #' @description
    #' UI module for dataset active filters
    #'
    #' UI module containing dataset active filters along with
    #' title and remove button.
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui = function(id) {
      dataname <- self$get_dataname()
      checkmate::assert_string(dataname)

      ns <- NS(id)
      if_multiple_filter_states <- length(self$get_filter_states()) > 1
      span(
        id = id,
        include_css_files("filter-panel"),
        div(
          id = ns("whole_ui"), # to hide it entirely
          fluidRow(
            column(
              width = 8,
              tags$span(dataname, class = "filter_panel_dataname")
            ),
            column(
              width = 4,
              actionLink(
                ns("remove_filters"),
                label = "",
                icon = icon("times-circle", lib = "font-awesome"),
                class = "remove pull-right"
              )
            )
          ),
          div(
            # id needed to insert and remove UI to filter single variable as needed
            # it is currently also used by the above module to entirely hide this panel
            id = ns("filters"),
            class = "parent-hideable-list-group",
            tagList(
              lapply(
                names(self$get_filter_states()),
                function(x) {
                  tagList(self$get_filter_states(id = x)$ui(id = ns(x)))
                }
              )
            )
          )
        )
      )
    },

    #' @description
    #' Server module for a dataset active filters
    #'
    #' Server module managing a  active filters.
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          dataname <- self$get_dataname()
          logger::log_trace("FilteredDataset$server initializing, dataname: { deparse1(dataname) }")
          checkmate::assert_string(dataname)
          shiny::setBookmarkExclude("remove_filters")
          lapply(
            names(self$get_filter_states()),
            function(x) {
              self$get_filter_states(id = x)$server(id = x)
            }
          )

          shiny::observeEvent(self$get_filter_state(), {
            if (length(self$get_filter_state()) == 0) {
              shinyjs::hide("remove_filters")
            } else {
              shinyjs::show("remove_filters")
            }
          })

          observeEvent(input$remove_filters, {
            logger::log_trace("FilteredDataset$server@1 removing filters, dataname: { deparse1(dataname) }")
            lapply(
              self$get_filter_states(),
              function(x) x$queue_empty()
            )
            logger::log_trace("FilteredDataset$server@1 removed filters, dataname: { deparse1(dataname) }")
          })

          logger::log_trace("FilteredDataset$initialized, dataname: { deparse1(dataname) }")
          NULL
        }
      )
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
      stop("Pure virtual method")
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param ... ignored
    #' @return `moduleServer` function.
    srv_add_filter_state = function(id, ...) {
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          stop("Pure virtual method")
        }
      )
    }
  ),
  ## __Private Fields ====
  private = list(
    dataset = NULL,
    filter_states = list(),
    dataname = character(0),
    keys = character(0),
    label = character(0),
    metadata = NULL,
    hash = character(0),

    # Calculates the MD5 hash of the data stored in this object.
    # @return NULL
    calculate_hash = function() {
      private$hash <- digest::digest(self$get_dataset(), algo = "md5")
      NULL
    },

    # Adds `FilterStates` to the `private$filter_states`.
    # `FilterStates` is added once for each element of the dataset.
    # @param filter_states (`FilterStates`)
    # @param id (`character(1)`)
    add_filter_states = function(filter_states, id) {
      stopifnot(is(filter_states, "FilterStates"))
      checkmate::assert_string(id)

      x <- setNames(list(filter_states), id)
      private$filter_states <- c(self$get_filter_states(), x)
    },

    # @description
    # Checks if the dataname exists and
    # (if provided) that varname is a valid column in the dataset
    #
    # Stops when this is not the case.
    #
    # @param varname (`character`) column within the dataset;
    #   if `NULL`, this check is not performed
    check_data_varname_exists = function(varname = NULL) {
      checkmate::assert_string(varname, null.ok = TRUE)

      isolate({
        if (!is.null(varname) && !(varname %in% self$get_varnames())) {
          stop(
            sprintf("variable '%s' does not exist in data '%s'", varname, dataname)
          )
        }
      })

      return(invisible(NULL))
    }
  )
)

# DefaultFilteredDataset ------
#' @title The `DefaultFilteredDataset` R6 class
#' @keywords internal
#' @examples
#' library(shiny)
#' ds <- teal.slice:::DefaultFilteredDataset$new(iris, "iris")
#' ds$get_data(filtered = FALSE)
#'
#' ds$set_filter_state(
#'   state = list(
#'     Species = list(selected = "virginica"),
#'     Petal.Length = list(selected = c(2.0, 5))
#'   )
#' )
#'
#' isolate(ds$get_filter_state())
#' isolate(ds$get_filter_overview_info())
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
          input_dataname = as.name(dataname),
          output_dataname = as.name(sprintf("%s_FILTERED", dataname)),
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
    #' which contains single `ReactiveQueue` and all `FilterState` objects
    #' applies to one argument (`...`) in `dplyr::filter` call.
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      Filter(
        f = Negate(is.null),
        x = lapply(
          self$get_filter_states(),
          function(x) x$get_call()
        )
      )
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
    #' dataset$set_filter_state(state = fs)
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
    #' @param element_id (`character`)\cr
    #'  Vector of character names of variables to remove their `FilterState`.
    #'
    #' @return `NULL`
    remove_filter_state = function(element_id) {
      logger::log_trace(
        sprintf(
          "DefaultFilteredDataset$remove_filter_state removing filters of variable %s, dataname: %s",
          element_id,
          self$get_dataname()
        )
      )

      fdata_filter_state <- self$get_filter_states()[[1]]
      for (element in element_id) {
        fdata_filter_state$remove_filter_state(element)
      }
      logger::log_trace(
        sprintf(
          "DefaultFilteredDataset$remove_filter_state done removing filters of variable %s, dataname: %s",
          element_id,
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
    #' @param `subject_keys` `character` or `NULL` columns to keep when
    #' calculating the filtering
    #' @return `list` containing character `#filtered/#not_filtered`
    get_filter_overview_nsubjs = function(filtered_dataset = NULL, subject_keys = NULL) {

      checkmate::assert_class(filtered_dataset, classes = class(self$get_dataset()), null.ok = TRUE)
      checkmate::assert_character(subject_keys, null.ok = TRUE, any.missing = FALSE)

      if (is.null(filtered_dataset)) {
        filtered_dataset <- self$get_dataset()
      }

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


# MAEFilteredDataset ------
#' @keywords internal
#' @title `MAEFilteredDataset` R6 class
MAEFilteredDataset <- R6::R6Class( # nolint
  classname = "MAEFilteredDataset",
  inherit = FilteredDataset,
  public = list(

    #' @description
    #' Initialize `MAEFilteredDataset` object
    #'
    #' @param dataset (`MulitiAssayExperiment`)\cr
    #'  single `MultiAssayExperiment` for which filters are rendered
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
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      checkmate::assert_class(dataset, "MultiAssayExperiment")
      super$initialize(dataset, dataname, keys, label, metadata)

      experiment_names <- names(dataset)

      # subsetting by subjects means subsetting by colData(MAE)
      private$add_filter_states(
        filter_states = init_filter_states(
          data = dataset,
          input_dataname = as.name(dataname),
          output_dataname = as.name(sprintf("%s_FILTERED", dataname)),
          varlabels = self$get_varlabels(),
          datalabel = "subjects",
          keys = self$get_keys()
        ),
        id = "subjects"
      )

      # elements of the list (experiments) are unknown
      # dispatch needed because we can't hardcode methods otherwise:
      #  if (matrix) else if (SummarizedExperiment) else if ...
      lapply(
        experiment_names,
        function(experiment_name) {
          input_dataname <- call_extract_list(
            sprintf("%s_FILTERED", dataname),
            experiment_name,
            dollar = FALSE
          )

          private$add_filter_states(
            filter_states = init_filter_states(
              data = dataset[[experiment_name]],
              input_dataname = input_dataname,
              output_dataname = input_dataname,
              datalabel = experiment_name
            ),
            id = experiment_name
          )
        }
      )
    },

    #' @description
    #' Get filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains multiple `FilterStates`:
    #' \itemize{
    #'   \item{`colData(dataset)`}{for this object single `MAEFilterStates`
    #'   which returns `subsetByColData` call}
    #'   \item{experiments}{for each experiment single `SEFilterStates` and
    #'   `FilterStates_matrix`, both returns `subset` call}
    #' }
    #' @return filter `call` or `list` of filter calls
    get_call = function() {
      Filter(
        f = Negate(is.null),
        x = lapply(
          self$get_filter_states(),
          function(x) x$get_call()
        )
      )
    },

    #' @description
    #' Gets labels of variables in the data
    #'
    #' Variables are the column names of the data.
    #' Either, all labels must have been provided for all variables
    #' in `set_data` or `NULL`.
    #'
    #' @param variables (`character` vector) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #' @return (`character` or `NULL`) variable labels, `NULL` if `column_labels`
    #'   attribute does not exist for the data
    get_varlabels = function(variables = NULL) {
      checkmate::assert_character(variables, null.ok = TRUE, any.missing = FALSE)

      labels <- vapply(
        X = SummarizedExperiment::colData(private$dataset),
        FUN.VALUE = character(1),
        FUN = function(x) {
          label <- attr(x, "label")
          if (length(label) != 1) {
            NA_character_
          } else {
            label
          }
        }
      )

      if (is.null(labels)) {
        return(NULL)
      }
      if (!is.null(variables)) labels <- labels[intersect(self$get_varnames(), variables)]
      labels
    },

    #' @description
    #' Get filter overview rows of a dataset
    #' TODO docs
    #' @return (`matrix`) matrix of observations and subjects
    get_filter_overview_info = function(filtered_dataset = NULL) {
      if (is.null(filtered_dataset)) {
        filtered_dataset <- self$get_dataset()
      }

      names_exps <- paste0("- ", names(self$get_dataset()))
      mae_and_exps <- c(self$get_dataname(), names_exps)

      df <- cbind(
        private$get_filter_overview_nobs(filtered_dataset),
        self$get_filter_overview_nsubjs(filtered_dataset)
      )

      rownames(df) <- mae_and_exps
      colnames(df) <- c("Obs", "Subjects")

      df
    },

    #' @description
    #' Gets variable names for the filtering.
    #'
    #' @return (`character(0)`)
    get_filterable_varnames = function() {
      character(0)
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`named list`)\cr
    #'  names of the list should correspond to the names of the initialized `FilterStates`
    #'  kept in `private$filter_states`. For this object they are `"subjects"` and
    #'  names of the experiments. Values of initial state should be relevant
    #'  to the referred column.
    #' @param ... ignored.
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #' dataset <- teal.slice:::MAEFilteredDataset$new(
    #'   miniACC, "MAE"
    #' )
    #' fs <- list(
    #'   subjects = list(
    #'     years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
    #'     vital_status = list(selected = "1", keep_na = FALSE),
    #'     gender = list(selected = "female", keep_na = TRUE)
    #'   ),
    #'   RPPAArray = list(
    #'     subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
    #'   )
    #' )
    #' dataset$set_filter_state(state = fs)
    #' shiny::isolate(dataset$get_filter_state())
    #' @return `NULL`
    set_filter_state = function(state, ...) {
      checkmate::assert_list(state)
      checkmate::assert_subset(names(state), c(names(self$get_filter_states())))

      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$set_filter_state setting up filters of variable %s, dataname: %s",
          paste(names(state), collapse = ", "),
          self$get_dataname()
        )
      )
      data <- self$get_dataset()
      for (fs_name in names(state)) {
        fs <- self$get_filter_states()[[fs_name]]
        fs$set_filter_state(
          state = state[[fs_name]],
          data = `if`(fs_name == "subjects", data, data[[fs_name]])
        )
      }

      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$set_filter_state done setting filters of variable %s, dataname: %s",
          paste(names(state), collapse = ", "),
          self$get_dataname()
        )
      )
      NULL
    },

    #' @description Remove one or more `FilterState` of a `MAEFilteredDataset`
    #'
    #' @param element_id (`list`)\cr
    #'  Named list of variables to remove their `FilterState`.
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(element_id) {
      checkmate::assert_list(element_id, names = "unique")
      checkmate::assert_subset(names(element_id), c(names(self$get_filter_states())))

      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$remove_filter_state removing filters of variable %s, dataname: %s",
          element_id,
          self$get_dataname()
        )
      )

      for (fs_name in names(element_id)) {
        fdata_filter_state <- self$get_filter_states()[[fs_name]]
        fdata_filter_state$remove_filter_state(
          `if`(fs_name == "subjects", element_id[[fs_name]][[1]], element_id[[fs_name]])
        )
      }
      logger::log_trace(
        sprintf(
          "MAEFilteredDataset$remove_filter_state done removing filters of variable %s, dataname: %s",
          element_id,
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
      data <- self$get_dataset()
      experiment_names <- names(data)

      div(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        br(),
        HTML("&#9658;"),
        tags$label("Add subjects filter"),
        self$get_filter_states("subjects")$ui_add_filter_state(
          id = ns("subjects"),
          data = data
        ),
        tagList(
          lapply(
            experiment_names,
            function(experiment_name) {
              tagList(
                HTML("&#9658;"),
                tags$label("Add", tags$code(experiment_name), "filter"),
                self$get_filter_states(experiment_name)$ui_add_filter_state(
                  id = ns(experiment_name),
                  data = data[[experiment_name]]
                )
              )
            }
          )
        )
      )
    },

    #' @description
    #' Server module to add filter variable for this dataset
    #'
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add_filter_state` calls multiple modules
    #' of the same name from `FilterStates` as `MAEFilteredDataset`
    #' contains one `FilterStates` object for `colData` and one for each
    #' experiment.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param ... ignored.
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, ...) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(paste(
            "MAEFilteredDataset$srv_add_filter_state initializing,",
            "dataname: { deparse1(self$get_dataname()) }"
          ))
          data <- self$get_dataset()
          self$get_filter_states("subjects")$srv_add_filter_state(
            id = "subjects",
            data = data # MultiAssayExperiment
            # ignoring vars_include
          )

          experiment_names <- names(data)
          lapply(
            experiment_names,
            function(experiment_name) {
              self$get_filter_states(experiment_name)$srv_add_filter_state(
                id = experiment_name,
                data = data[[experiment_name]] # SummarizedExperiment or matrix
                # ignoring vars_include
              )
            }
          )
          logger::log_trace(paste(
            "MAEFilteredDataset$srv_add_filter_state initialized,",
            "dataname: { deparse1(self$get_dataname()) }"
          ))
          NULL
        }
      )
    },

    # Gets filter overview subjects number and returns a list
    # of the number of subjects of filtered/non-filtered datasets
    get_filter_overview_nsubjs = function(filtered_dataset, subject_keys) {
      data_f <- filtered_dataset
      data_nf <- self$get_dataset()
      experiment_names <- names(data_nf)

      data_f_subjects_info <- nrow(SummarizedExperiment::colData(data_f))
      data_nf_subjects_info <- nrow(SummarizedExperiment::colData(data_nf))
      mae_total_subjects_info <- paste0(data_f_subjects_info, "/", data_nf_subjects_info)

      get_experiment_rows <- function(mae, experiment) {
        sample_subset <- subset(MultiAssayExperiment::sampleMap(mae), colname %in% colnames(experiment))
        length(unique(sample_subset$primary))
      }

      subjects_info <- lapply(
        experiment_names,
        function(experiment_name) {
          subjects_f_rows <- get_experiment_rows(data_f, data_f[[experiment_name]])
          subjects_nf_rows <- get_experiment_rows(data_nf, data_nf[[experiment_name]])

          subjects_info <- paste0(subjects_f_rows, "/", subjects_nf_rows)
          subjects_info
        }
      )

      append(
        list(mae_total_subjects_info),
        subjects_info
      )
    }
  ),
  private = list(
    # Gets filter overview observations number and returns a
    # list of the number of observations of filtered/non-filtered datasets
    get_filter_overview_nobs = function(filtered_dataset) {
      data_f <- filtered_dataset
      data_nf <- self$get_dataset()
      experiment_names <- names(data_nf)
      mae_total_data_info <- ""

      data_info <- lapply(
        experiment_names,
        function(experiment_name) {
          data_f_rows <- ncol(data_f[[experiment_name]])
          data_nf_rows <- ncol(data_nf[[experiment_name]])

          data_info <- paste0(data_f_rows, "/", data_nf_rows)
          data_info
        }
      )

      append(
        list(mae_total_data_info),
        data_info
      )
    }
  )
)
