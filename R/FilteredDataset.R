# FilteredDataset abstract --------

#' @name FilteredDataset
#' @docType class
#'
#' @title `FilteredDataset` `R6` class
#' @description
#' `FilteredDataset` is a class which renders/controls `FilterStates`(s)
#' Each `FilteredDataset` contains `filter_states` field - a `list` which contains one
#' (`data.frame`) or multiple (`MultiAssayExperiment`) `FilterStates` objects.
#' Each `FilterStates` is responsible for one filter/subset expression applied for specific
#' components of the dataset.
#'
#' @keywords internal
FilteredDataset <- R6::R6Class( # nolint
  "FilteredDataset",
  # public methods ----
  public = list(
    #' @description
    #' Initializes this `FilteredDataset` object.
    #'
    #' @param dataset any object
    #' @param dataname (`character(1)`)
    #'  syntactically valid name given to the dataset.
    #' @param keys (`character`) optional
    #'   vector of primary key column names.
    #' @param label (`character(1)`)
    #'   label to describe the dataset.
    #'
    #' @return Object of class `FilteredDataset`, invisibly.
    #'
    initialize = function(dataset, dataname, keys = character(0), label = attr(dataset, "label", exact = TRUE)) {
      logger::log_trace("Instantiating { class(self)[1] }, dataname: { dataname }")

      # dataset assertion in child classes
      check_simple_name(dataname)
      checkmate::assert_character(keys, any.missing = FALSE)
      checkmate::assert_character(label, null.ok = TRUE)

      logger::log_trace("Instantiating { class(self)[1] }, dataname: { dataname }")
      private$dataset <- dataset
      private$dataname <- dataname
      private$keys <- keys
      private$label <- if (is.null(label)) character(0) else label

      # function executing reactive call and returning data
      private$data_filtered_fun <- function(sid = "") {
        checkmate::assert_character(sid)
        if (length(sid)) {
          logger::log_trace("filtering data dataname: { dataname }, sid: { sid }")
        } else {
          logger::log_trace("filtering data dataname: { private$dataname }")
        }
        env <- new.env(parent = parent.env(globalenv()))
        env[[dataname]] <- private$dataset
        filter_call <- self$get_call(sid)
        eval_expr_with_msg(filter_call, env)
        get(x = dataname, envir = env)
      }

      private$data_filtered <- reactive(private$data_filtered_fun())
      logger::log_trace("Instantiated { class(self)[1] }, dataname: { private$dataname }")
      invisible(self)
    },

    #' @description
    #' Returns a formatted string representing this `FilteredDataset` object.
    #'
    #' @param show_all (`logical(1)`) passed to `format.teal_slice`.
    #' @param trim_lines (`logical(1)`) passed to `format.teal_slice`.
    #'
    #' @return The formatted character string.
    #'
    format = function(show_all = FALSE, trim_lines = TRUE) {
      sprintf(
        "%s:\n%s",
        class(self)[1],
        format(self$get_filter_state(), show_all = show_all, trim_lines = trim_lines)
      )
    },

    #' @description
    #' Prints this `FilteredDataset` object.
    #'
    #' @param ... additional arguments passed to `format`.
    #'
    print = function(...) {
      cat(isolate(self$format(...)), "\n")
    },

    #' @description
    #' Removes all filter items applied to this dataset.
    #'
    #' @param force (`logical(1)`)
    #'   flag specifying whether to include anchored filter states.
    #'
    #' @return `NULL`.
    clear_filter_states = function(force = FALSE) {
      logger::log_trace("Removing filters from FilteredDataset: { deparse1(self$get_dataname()) }")
      lapply(
        private$get_filter_states(),
        function(filter_states) filter_states$clear_filter_states(force)
      )
      logger::log_trace("Removed filters from FilteredDataset: { deparse1(self$get_dataname()) }")
      NULL
    },

    # managing filter states -----

    # getters ----
    #' @description
    #' Gets a filter expression.
    #'
    #' This function returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #'
    #' @param sid (`character`)
    #'  when specified, the method returns code containing conditions calls of
    #'  `FilterState` objects with `sid` different to this `sid` argument.
    #'
    #' @return Either a `list` of filter `call`s, or `NULL`.
    get_call = function(sid = "") {
      filter_call <- Filter(
        f = Negate(is.null),
        x = lapply(private$get_filter_states(), function(x) x$get_call(sid))
      )
      if (length(filter_call) == 0) {
        return(NULL)
      }
      filter_call
    },

    #' @description
    #' Gets states of all contained `FilterState` objects.
    #'
    #' @return A `teal_slices` object.
    #'
    get_filter_state = function() {
      states <- unname(lapply(private$get_filter_states(), function(x) x$get_filter_state()))
      do.call(c, states)
    },

    #' @description
    #' Set filter state.
    #'
    #' @param state (`teal_slices`)
    #'
    #' @return Virtual method, returns nothing and raises error.
    #'
    set_filter_state = function(state) {
      stop("set_filter_state is an abstract class method.")
    },

    #' @description
    #' Gets the number of `FilterState` objects in all `FilterStates` in this `FilteredDataset`.
    #' @return `integer(1)`
    get_filter_count = function() {
      length(self$get_filter_state())
    },

    #' @description
    #' Gets the name of the dataset.
    #'
    #' @return A character string.
    get_dataname = function() {
      private$dataname
    },

    #' @description
    #' Gets the dataset object in this `FilteredDataset`.
    #'
    #' @param filtered (`logical(1)`)
    #'
    #' @return
    #' The stored dataset. If `data.frame` or `MultiAssayExperiment`,
    #' either raw or as a reactive with current filters applied (depending on `filtered`).
    #'
    get_dataset = function(filtered = FALSE) {
      if (filtered) {
        private$data_filtered
      } else {
        private$dataset
      }
    },

    #' @description
    #' Get filter overview of a dataset.
    #' @return Virtual method, returns nothing and raises an error.
    get_filter_overview = function() {
      stop("get_filter_overview is an abstract class method")
    },

    #' @description
    #' Gets the key columns for this dataset.
    #' @return Character vector of variable names
    get_keys = function() {
      private$keys
    },

    #' @description
    #' Gets the dataset label.
    #' @return Character string.
    get_dataset_label = function() {
      private$label
    },

    # modules ------
    #' @description
    #' `shiny` module containing active filters for a dataset, along with a title and a remove button.
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #'
    #' @return `shiny.tag`
    ui_active = function(id) {
      dataname <- self$get_dataname()
      checkmate::assert_string(dataname)

      ns <- NS(id)
      if_multiple_filter_states <- length(private$get_filter_states()) > 1
      tags$span(
        id = id,
        include_css_files("filter-panel"),
        tags$div(
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
            tags$div(
              id = ns("filter_count_ui"),
              tagList(
                textOutput(ns("filter_count")),
                tags$br()
              )
            )
          ),
          tags$div(
            # id needed to insert and remove UI to filter single variable as needed
            # it is currently also used by the above module to entirely hide this panel
            id = ns("filters"),
            class = "parent-hideable-list-group",
            tagList(
              lapply(
                names(private$get_filter_states()),
                function(x) {
                  tagList(private$get_filter_states()[[x]]$ui_active(id = ns(x)))
                }
              )
            )
          )
        )
      )
    },

    #' @description
    #' Server module for a dataset active filters.
    #'
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #' @return `NULL`.
    srv_active = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          dataname <- self$get_dataname()
          logger::log_trace("FilteredDataset$srv_active initializing, dataname: { dataname }")
          checkmate::assert_string(dataname)
          output$filter_count <- renderText(
            sprintf(
              "%d filter%s applied",
              self$get_filter_count(),
              if (self$get_filter_count() != 1) "s" else ""
            )
          )

          lapply(
            names(private$get_filter_states()),
            function(x) {
              private$get_filter_states()[[x]]$srv_active(id = x)
            }
          )

          observeEvent(self$get_filter_state(), {
            shinyjs::hide("filter_count_ui")
            shinyjs::show("filters")
            shinyjs::toggle("remove_filters", condition = length(self$get_filter_state()) != 0)
            shinyjs::toggle("collapse", condition = length(self$get_filter_state()) != 0)
          })

          observeEvent(input$collapse, {
            shinyjs::toggle("filter_count_ui")
            shinyjs::toggle("filters")
            toggle_icon(session$ns("collapse"), c("fa-angle-right", "fa-angle-down"))
          })

          observeEvent(input$remove_filters, {
            logger::log_trace("FilteredDataset$srv_active@1 removing all non-anchored filters, dataname: { dataname }")
            self$clear_filter_states()
            logger::log_trace("FilteredDataset$srv_active@1 removed all non-anchored filters, dataname: { dataname }")
          })

          logger::log_trace("FilteredDataset$initialized, dataname: { dataname }")

          NULL
        }
      )
    },

    #' @description
    #' UI module to add filter variable for this dataset.
    #'
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #'
    #' @return Virtual method, returns nothing and raises error.
    ui_add = function(id) {
      stop("Pure virtual method")
    },

    #' @description
    #' Server module to add filter variable for this dataset.
    #' For this class `srv_add` calls multiple modules
    #' of the same name from `FilterStates` as `MAEFilteredDataset`
    #' contains one `FilterStates` object for `colData` and one for each experiment.
    #'
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #'
    #' @return `NULL`.
    srv_add = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("MAEFilteredDataset$srv_add initializing, dataname: { deparse1(self$get_dataname()) }")
          elems <- private$get_filter_states()
          elem_names <- names(private$get_filter_states())
          lapply(
            elem_names,
            function(elem_name) elems[[elem_name]]$srv_add(elem_name)
          )
          logger::log_trace("MAEFilteredDataset$srv_add initialized, dataname: { deparse1(self$get_dataname()) }")
          NULL
        }
      )
    }
  ),
  # private fields ----
  private = list(
    dataset = NULL, # data.frame or MultiAssayExperiment
    data_filtered = NULL,
    data_filtered_fun = NULL, # function
    filter_states = list(),
    dataname = character(0),
    keys = character(0),
    label = character(0),

    # Adds `FilterStates` to the `private$filter_states`.
    # `FilterStates` is added once for each element of the dataset.
    # @param filter_states (`FilterStates`)
    # @param id (`character(1)`)
    add_filter_states = function(filter_states, id) {
      checkmate::assert_class(filter_states, "FilterStates")
      checkmate::assert_string(id)
      x <- stats::setNames(list(filter_states), id)
      private$filter_states <- c(private$get_filter_states(), x)
    },

    # @description
    # Gets `FilterStates` objects in this `FilteredDataset`.
    # @return list of `FilterStates` objects.
    get_filter_states = function() {
      private$filter_states
    }
  )
)
