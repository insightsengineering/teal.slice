#' Initializes `FilteredDataset`
#'
#' @keywords internal
#' @examples
#' # DefaultFilteredDataset example
#' iris_fd <- teal.slice:::init_filtered_dataset(
#'   iris,
#'   dataname = "iris",
#'   metadata = list(type = "teal")
#' )
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
#' MAE_fd <- teal.slice:::init_filtered_dataset(miniACC, "MAE", metadata = list(type = "MAE"))
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
#' @param parent_name (`character(1)`)\cr
#'   Name of the parent dataset
#' @param parent (`reactive`)\cr
#'   a `reactive` returning parent `data.frame`
#' @param join_keys (`character`)\cr
#'   Name of the columns in this dataset to join with `parent`
#'   dataset. If the column names are different if both datasets
#'   then the names of the vector define the `parent` columns.
#' @param label (`character`)\cr
#'   Label to describe the dataset
#' @param metadata (named `list` or `NULL`) \cr
#'   Field containing metadata about the dataset. Each element of the list
#'   should be atomic and length one.
#' @export
#' @note Although this function is exported for use in other packages, it may be changed or removed in a future release
#'   at which point any code which relies on this exported function will need to be changed.
init_filtered_dataset <- function(dataset, # nolint
                                  dataname,
                                  keys = character(0),
                                  parent_name = character(0),
                                  parent = reactive(dataset),
                                  join_keys = character(0),
                                  label = attr(dataset, "label"),
                                  metadata = NULL,
                                  ...) {
  UseMethod("init_filtered_dataset")
}

#' @keywords internal
#' @export
init_filtered_dataset.data.frame <- function(dataset, # nolint
                                             dataname,
                                             keys = character(0),
                                             parent_name = character(0),
                                             parent = NULL,
                                             join_keys = character(0),
                                             label = attr(dataset, "label"),
                                             metadata = NULL) {
  DefaultFilteredDataset$new(
    dataset = dataset,
    dataname = dataname,
    keys = keys,
    parent_name = parent_name,
    parent = parent,
    join_keys = join_keys,
    label = label,
    metadata = metadata
  )
}

#' @keywords internal
#' @export
init_filtered_dataset.MultiAssayExperiment <- function(dataset, # nolint
                                                       dataname,
                                                       keys = character(0),
                                                       label = attr(dataset, "label"),
                                                       parent_name = character(0),
                                                       parent = NULL,
                                                       join_keys = character(0),
                                                       metadata = NULL) {
  if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
    stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
  }
  MAEFilteredDataset$new(
    dataset = dataset,
    dataname = dataname,
    keys = keys,
    label = label,
    metadata = metadata
  )
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
    #' @param dataname (`character(1)`)\cr
    #'  A given name for the dataset it may not contain spaces
    #' @param keys optional, (`character`)\cr
    #'   Vector with primary keys
    #' @param label (`character(1)`)\cr
    #'   Label to describe the dataset
    #' @param metadata (named `list` or `NULL`) \cr
    #'   Field containing metadata about the dataset. Each element of the list
    #'   should be atomic and length one.
    initialize = function(dataset, dataname, keys = character(0), label = attr(dataset, "label"), metadata = NULL) {
      # dataset assertion in child classes
      check_simple_name(dataname)
      checkmate::assert_character(keys, any.missing = FALSE)
      checkmate::assert_character(label, null.ok = TRUE)
      teal.data::validate_metadata(metadata)

      logger::log_trace("Instantiating { class(self)[1] }, dataname: { deparse1(dataname) }")
      private$dataset <- dataset
      private$dataname <- dataname
      private$keys <- keys
      private$label <- if (is.null(label)) character(0) else label
      private$metadata <- metadata
      private$dataset_filtered <- reactive({
        env <- new.env(parent = parent.env(globalenv()))
        env[[dataname]] <- private$dataset
        filter_call <- self$get_call()
        eval_expr_with_msg(filter_call, env)
        get(x = dataname, envir = env)
      })
      invisible(self)
    },


    #' @description
    #' Returns a string representation of the filter state in this `FilteredDataset`.
    #'
    #' @return `character(1)` the formatted string representing the filter state or
    #' `NULL` if no filter state is present.
    #'
    get_formatted_filter_state = function() {
      out <- Filter(
        function(x) x != "",
        sapply(
          self$get_filter_states(),
          function(states) {
            states$format(indent = 2)
          }
        )
      )
      if (length(out) > 0) {
        header <- paste0("Filters for dataset: ", self$get_dataname())
        paste(c(header, out), collapse = "\n")
      }
    },

    #' @description
    #' Removes all active filter items applied to this dataset
    #' @return NULL
    state_lists_empty = function() {
      logger::log_trace("Removing all filters from FilteredDataset: { deparse1(self$get_dataname()) }")
      lapply(
        self$get_filter_states(),
        function(state_list) state_list$state_list_empty()
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
      filter_call <- Filter(
        f = Negate(is.null),
        x = lapply(self$get_filter_states(), function(x) x$get_call())
      )
      if (length(filter_call) == 0) {
        return(NULL)
      }
      filter_call
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
    #' Gets the number of active `FilterState` objects in all `FilterStates` in this `FilteredDataset`.
    #' @return `integer(1)`
    get_filter_count = function() {
      sum(vapply(private$filter_states,
                 function(state) state$get_filter_count(),
                 FUN.VALUE = integer(1)))
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
    #' @param filtered (`logical(1)`)\cr
    #'
    #' @return `data.frame` or `MultiAssayExperiment` as a raw data
    #'  or as a reactive with filter applied
    #'
    get_dataset = function(filtered = FALSE) {
      if (filtered) {
        private$dataset_filtered
      } else {
        private$dataset
      }
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
    get_filter_overview_info = function() {
      dataset <- self$get_dataset()
      dataset_filtered <- self$get_dataset(TRUE)

      df <- cbind(private$get_filter_overview_nobs(dataset, dataset_filtered), "")
      rownames(df) <- self$get_dataname()
      colnames(df) <- c("Obs", "Subjects")
      df
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
      dataset <- self$get_dataset()
      labels <- formatters::var_labels(dataset, fill = FALSE)
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
    #' Set the allowed filterable variables
    #' @param varnames (`character` or `NULL`) The variables which can be filtered
    #' See `self$get_filterable_varnames` for more details
    #'
    #' @details When retrieving the filtered variables only
    #' those which have filtering supported (i.e. are of the permitted types)
    #' are included.
    #'
    #' @return invisibly this `FilteredDataset`
    set_filterable_varnames = function(varnames) {
      checkmate::assert_character(varnames, any.missing = FALSE, null.ok = TRUE)
      private$filterable_varnames <- varnames
      return(invisible(self))
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
              tagList(
                actionLink(
                  ns("remove_filters"),
                  label = "",
                  icon = icon("circle-xmark", lib = "font-awesome"),
                  class = "remove pull-right"
                ),
                actionLink(
                  ns("collapse"),
                  label = "",
                  icon = icon("angle-down", lib = "font-awesome"),
                  class = "remove pull-right"
                )
              )
            )
          ),
          shinyjs::hidden(
            div(
              id = ns("filter_count_ui"),
              tagList(
                textOutput(ns("filter_count")),
                br()
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
          output$filter_count <- renderText(
            sprintf(
              "%d filter%s applied",
              self$get_filter_count(),
              if (self$get_filter_count() != 1) "s" else ""
            )
          )

          lapply(
            names(self$get_filter_states()),
            function(x) {
              self$get_filter_states(id = x)$server(id = x)
            }
          )

          shiny::observeEvent(self$get_filter_state(), {
            shinyjs::hide("filter_count_ui")
            shinyjs::show("filters")
            shinyjs::toggle("remove_filters", condition = length(self$get_filter_state()) != 0)
            shinyjs::toggle("collapse", condition = length(self$get_filter_state()) != 0)
          })

          shiny::observeEvent(input$collapse, {
            shinyjs::toggle("filter_count_ui")
            shinyjs::toggle("filters")
            toggle_icon(session$ns("collapse"), c("fa-angle-right", "fa-angle-down"))
          })

          observeEvent(input$remove_filters, {
            logger::log_trace("FilteredDataset$server@1 removing filters, dataname: { deparse1(dataname) }")
            lapply(
              self$get_filter_states(),
              function(x) x$state_list_empty()
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
    dataset_filtered = NULL,
    filter_states = list(),
    filterable_varnames = character(0),
    dataname = character(0),
    keys = character(0),
    parent = NULL, # reactive
    label = character(0),
    metadata = NULL,

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
