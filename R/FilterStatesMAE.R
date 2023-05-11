#' @title `FilterStates` subclass for MultiAssayExperiments
#' @description Handles filter states in a `MultiAssayExperiment`
#' @keywords internal
#'
#'
MAEFilterStates <- R6::R6Class( # nolint
  classname = "MAEFilterStates",
  inherit = FilterStates,
  public = list(
    # public methods ----

    #' @description Initializes `MAEFilterStates` object
    #'
    #' Initialize `MAEFilterStates` object
    #'
    #' @param data (`MultiAssayExperiment`)\cr
    #'   the R object which `MultiAssayExperiment::subsetByColData` function is applied on.
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `MultiAssayExperiment` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #' @param datalabel (`NULL` or `character(1)`)\cr
    #'   text label value
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #' @param keys (`character`)\cr
    #'   key columns names
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = "subjects",
                          keys = character(0)) {
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_class(data, "MultiAssayExperiment")
      data <- SummarizedExperiment::colData(data)
      data_reactive <- function(sid = "") SummarizedExperiment::colData(data_reactive(sid = sid))
      super$initialize(data, data_reactive, dataname, datalabel)
      private$keys <- keys
      private$set_filterable_varnames(include_varnames = colnames(data))
      return(invisible(self))
    },

    #' @description
    #' Returns the formatted string representing this `MAEFilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before each line of the representation
    #' @return `character(1)` the formatted string
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      if (length(private$state_list_get()) > 0) {
        formatted_states <- sprintf("%sSubject filters:", format("", width = indent))
        for (state in private$state_list_get()) {
          formatted_states <- c(formatted_states, state$format(indent = indent * 2))
        }
        paste(formatted_states, collapse = "\n")
      }
    },

    # shiny modules ----

    #' @description
    #' Server module
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    srv_active = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state <- reactiveVal(isolate(private$state_list_get("y")))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(private$state_list_get("y"), {
            added_state_name(setdiff(names(private$state_list_get("y")), names(previous_state())))
            removed_state_name(setdiff(names(previous_state()), names(private$state_list_get("y"))))

            previous_state(private$state_list_get("y"))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- private$state_list_get("y")
            html_ids <- private$map_vars_to_html_ids(names(fstates))
            for (fname in added_state_name()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                "y",
                state_id = fname
              )
            }
            added_state_name(character(0))
          })

          observeEvent(removed_state_name(), ignoreNULL = TRUE, {
            for (fname in removed_state_name()) {
              private$remove_filter_state_ui("y", fname, .input = input)
            }
            removed_state_name(character(0))
          })
          NULL
        }
      )
    },

    #' @description
    #' Returns active `FilterState` objects.
    #'
    #' Gets all active filters from this dataset in form of the nested list.
    #' The output list can be used as input to `self$set_filter_state`.
    #'
    #' @return `list` with elements number equal number of `FilterStates`.
    get_filter_state = function() {
      lapply(private$state_list_get("y"), function(x) x$get_state())
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `colData(data)`.
    #' @return `NULL`
    set_filter_state = function(state) {
      logger::log_trace("MAEFilterState$set_filter_state initializing, dataname: { private$dataname }")
      checkmate::assert_list(state, null.ok = TRUE, names = "named")

      data <- private$data
      data_reactive <- private$data_reactive

      # excluding not supported variables
      state_varnames <- names(state)
      filterable_varnames <- get_supported_filter_varnames(SummarizedExperiment::colData(data))
      excluded_varnames <- setdiff(state_varnames, filterable_varnames)
      if (length(excluded_varnames) > 0) {
        excluded_varnames_str <- toString(excluded_varnames)
        warning(
          "These columns filters were excluded: ",
          excluded_varnames_str,
          " from dataset ",
          private$dataname
        )
        logger::log_warn("Columns filters { excluded_varnames_str } were excluded from { private$dataname }")
        state <- state[state_varnames %in% filterable_varnames]
      }

      private$set_filter_state_impl(
        state = state,
        state_list_index = "y",
        data = SummarizedExperiment::colData(data),
        data_reactive = function(sid) SummarizedExperiment::colData(data_reactive(sid)),
        extract_type = "list",
        na_rm = TRUE
      )

      logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")
      NULL
    },

    #' @description
    #' Removes a variable from the `ReactiveQueue` and its corresponding UI element.
    #'
    #' @param state_id (`character(1)`)\cr name of `ReactiveQueue` element.
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(state_id) {
      logger::log_trace(
        sprintf(
          "%s$remove_filter_state for %s called, dataname: %s",
          class(self)[1],
          state_id,
          private$dataname
        )
      )

      if (!state_id %in% names(isolate(private$state_list_get("y")))) {
        msg <- sprintf(
          "%s is not an active 'patient' filter of dataset: %s and can't be removed.",
          state_id,
          private$dataname
        )
        warning(msg)
        logger::log_warn(msg)
      } else {
        private$state_list_remove("y", state_id = state_id)
        logger::log_trace(
          sprintf(
            "%s$remove_filter_state for variable %s done, dataname: %s",
            class(self)[1],
            state_id,
            private$dataname
          )
        )
      }
    },

    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @return shiny.tag
    ui_add = function(id) {
      data <- private$data
      checkmate::assert_string(id)

      ns <- NS(id)

      if (ncol(data) == 0) {
        div("no sample variables available")
      } else if (nrow(data) == 0) {
        div("no samples available")
      } else {
        teal.widgets::optionalSelectInput(
          ns("var_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select subject variable"
          )
        )
      }
    }
  ),

  # private fields ----

  private = list(
    extract_type = "list",
    fun = quote(MultiAssayExperiment::subsetByColData)
  )
)
