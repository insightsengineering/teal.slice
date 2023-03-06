#' @title `FilterStates` subclass for MultiAssayExperiments
#' @description Handles filter states in a `MultiAssayExperiment`
#' @keywords internal
#'
#'
MAEFilterStates <- R6::R6Class( # nolint
  classname = "MAEFilterStates",
  inherit = FilterStates,
  public = list(
    #' @description Initializes `MAEFilterStates` object
    #'
    #' Initialize `MAEFilterStates` object
    #'
    #' @param data (`MultiAssayExperiment`)\cr
    #'   the R object which `MultiAssayExperiment::subsetByColData` function is applied on.
    #'
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `MultiAssayExperiment` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #'
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #'
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #'
    #' @param keys (`character`)\cr
    #'   key columns names
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = "subjects",
                          varlabels = character(0),
                          keys = character(0)) {
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_class(data, "MultiAssayExperiment")
      super$initialize(data, data_reactive, dataname, datalabel)
      private$keys <- keys
      private$varlabels <- varlabels
      private$state_list <- list(
        y = reactiveVal()
      )
      return(invisible(self))
    },

    #' @description
    #' Returns the formatted string representing this `MAEFilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before each line of the representation
    #' @return `character(1)` the formatted string
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      if (length(private$state_list_get(1L)) > 0) {
        formatted_states <- sprintf("%sSubject filters:", format("", width = indent))
        for (state in private$state_list_get(1L)) {
          formatted_states <- c(formatted_states, state$format(indent = indent + 2))
        }
        paste(formatted_states, collapse = "\n")
      }
    },

    #' @description
    #' Returns function name used to create filter call.
    #' For `MAEFilterStates` `MultiAssayExperiment::subsetByColData` is used.
    #' @return `character(1)`
    get_fun = function() {
      "MultiAssayExperiment::subsetByColData"
    },

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

      if (ncol(SummarizedExperiment::colData(data)) == 0) {
        div("no sample variables available")
      } else if (nrow(SummarizedExperiment::colData(data)) == 0) {
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
    },

    #' @description
    #' Shiny server module to add filter variable.
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    srv_add = function(id) {
      data <- SummarizedExperiment::colData(private$data)

      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("MAEFilterState$srv_add initializing, dataname: { private$dataname }")
          active_filter_vars <- reactive({
            vapply(
              X = private$state_list_get("y"),
              FUN.VALUE = character(1),
              FUN = function(x) x$get_varname()
            )
          })

          # available choices to display
          avail_column_choices <- reactive({
            choices <- setdiff(get_supported_filter_varnames(data = data), active_filter_vars())
            varlabels <- vapply(
              colnames(data),
              FUN = function(x) {
                label <- attr(data[[x]], "label")
                if (is.null(label)) {
                  x
                } else {
                  label
                }
              },
              FUN.VALUE = character(1)
            )
            data_choices_labeled(
              data = data,
              choices = choices,
              varlabels = varlabels,
              keys = private$keys
            )
          })
          observeEvent(
            avail_column_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "MAEFilterStates$srv_add@1 updating available column choices,",
                "dataname: { private$dataname }"
              ))
              if (is.null(avail_column_choices())) {
                shinyjs::hide("var_to_add")
              } else {
                shinyjs::show("var_to_add")
              }
              teal.widgets::updateOptionalSelectInput(
                session,
                "var_to_add",
                choices = avail_column_choices()
              )
              logger::log_trace(paste(
                "MAEFilterStates$srv_add@1 updated available column choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "MAEFilterStates$srv_add@2 adding FilterState of variable %s, dataname: %s",
                  deparse1(input$var_to_add),
                  private$dataname
                )
              )
              varname <- input$var_to_add
              self$set_filter_state(setNames(list(list()), varname))
              logger::log_trace(
                sprintf(
                  "MAEFilterStates$srv_add@2 added FilterState of variable %s, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )

          logger::log_trace(
            "MAEFilterState$srv_add initialized, dataname: { private$dataname }"
          )
          NULL
        }
      )
    }
  ),
  private = list(
    varlabels = character(0),
    keys = character(0)
  )
)
