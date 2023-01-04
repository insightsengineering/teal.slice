#' @title `DFFFilterStates`
#' @description Specialization of `FilterStates` for a base `data.frame`.
#'
#' @keywords internal
DFFilterStates <- R6::R6Class( # nolint
  classname = "DFFilterStates",
  inherit = FilterStates,
  public = list(
    #' @description Initializes `DFFilterStates` object
    #'
    #' Initializes `DFFilterStates` object by setting `input_dataname`,
    #' `output_dataname` and initializing `ReactiveQueue`. This class contains a
    #' single `ReactiveQueue` with no specified name which means that
    #' when calling the function associated to this class (`dplyr::filter`), a list of
    #' conditions are passed to unnamed arguments (`...`).
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on lhs of the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the lhs of the assignment expression.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #'
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #'
    #' @param keys (`character`)\cr
    #'   key columns names
    initialize = function(input_dataname, output_dataname, datalabel, varlabels, keys) {
      super$initialize(input_dataname, output_dataname, datalabel)
      private$varlabels <- varlabels
      private$keys <- keys

      self$queue_initialize(
        list(
          ReactiveQueue$new()
        )
      )
    },

    #' @description
    #' Returns the formatted string representing this `FilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before each line of the representation
    #' @return `character(1)` the formatted string
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      formatted_states <- vapply(
        self$queue_get(1L), function(state) state$format(indent = indent),
        USE.NAMES = FALSE, FUN.VALUE = character(1)
      )
      paste(formatted_states, collapse = "\n")
    },

    #' @description
    #' Get function name
    #'
    #' Get function name used to create filter call.
    #' For `DFFilterStates` `dplyr::filter` is used
    #' @return `character(1)`
    get_fun = function() {
      return("dplyr::filter")
    },

    #' @description
    #' Server module
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state <- reactiveVal(isolate(self$queue_get(1L)))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$queue_get(1L), {
            added_state_name(setdiff(names(self$queue_get(1L)), names(previous_state())))
            removed_state_name(setdiff(names(previous_state()), names(self$queue_get(1L))))
            previous_state(self$queue_get(1L))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- self$queue_get(1L)
            html_ids <- private$map_vars_to_html_ids(names(fstates))
            for (fname in added_state_name()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                queue_index = 1L,
                element_id = fname
              )
            }
            added_state_name(character(0))
          })

          observeEvent(removed_state_name(), {
            req(removed_state_name())
            for (fname in removed_state_name()) {
              private$remove_filter_state_ui(1L, fname)
            }
            removed_state_name(character(0))
          })
          NULL
        }
      )
    },

    #' @description
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Get active filter state from the `FilterState` objects kept in `ReactiveQueue`.
    #' The output list is a compatible input to `self$set_filter_state`.
    #' @return `list` with named elements corresponding to `FilterState` in the `ReactiveQueue`.
    get_filter_state = function() {
      lapply(self$queue_get(1L), function(x) x$get_state())
    },

    #' @description
    #' Set filter state
    #'
    #' @param data (`data.frame`)\cr
    #'   data which are supposed to be filtered.
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    #' @param vars_include (`character(n)`)\cr
    #'  optional, vector of column names to be included.
    #' @param ... ignored.
    #' @examples
    #' dffs <- teal.slice:::DFFilterStates$new(
    #'   input_dataname = "iris",
    #'   output_dataname = "iris_filtered",
    #'   datalabel = character(0),
    #'   varlabels = character(0),
    #'   keys = character(0)
    #' )
    #' fs <- list(
    #'   Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = TRUE),
    #'   Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    #' )
    #' dffs$set_filter_state(state = fs, data = iris)
    #' shiny::isolate(dffs$get_filter_state())
    #'
    #' @return `NULL`
    set_filter_state = function(data, state, vars_include = get_supported_filter_varnames(data = data), ...) {
      checkmate::assert_data_frame(data)
      checkmate::assert(
        checkmate::check_subset(names(state), names(data)),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      logger::log_trace(
        "{ class(self)[1] }$set_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
      )

      filter_states <- self$queue_get(1L)
      state_names <- names(state)
      excluded_vars <- setdiff(state_names, vars_include)
      if (length(excluded_vars) > 0) {
        warning(
          paste(
            "These columns filters were excluded:",
            paste(excluded_vars, collapse = ", "),
            "from dataset",
            private$input_dataname
          )
        )
        logger::log_warn(
          paste(
            "Columns filters { paste(excluded_vars, collapse = ', ') } were excluded",
            "from { deparse1(private$input_dataname) }"
          )
        )
      }

      filters_to_apply <- state_names[state_names %in% vars_include]

      for (varname in filters_to_apply) {
        value <- resolve_state(state[[varname]])
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          set_state(x = fstate, value = value)
        } else {
          fstate <- init_filter_state(
            data[[varname]],
            varname = as.name(varname),
            varlabel = private$get_varlabels(varname),
            input_dataname = private$input_dataname
          )
          set_state(x = fstate, value = value, is_reactive = FALSE)
          self$queue_push(x = fstate, queue_index = 1L, element_id = varname)
        }
      }
      logger::log_trace(
        "{ class(self)[1] }$set_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
      )
      NULL
    },

    #' @description Remove a `FilterState` from the `ReactiveQueue`.
    #'
    #' @param element_id (`character(1)`)\cr name of `ReactiveQueue` element.
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(element_id) {
      logger::log_trace(
        sprintf(
          "%s$remove_filter_state for variable %s called, dataname: %s",
          class(self)[1],
          element_id,
          deparse1(private$input_dataname)
        )
      )

      if (!element_id %in% names(self$queue_get(1L))) {
        warning(paste(
          "Variable:", element_id,
          "is not present in the actual active filters of dataset: { deparse1(private$input_dataname) }",
          "therefore no changes are applied."
        ))
        logger::log_warn(
          paste(
            "Variable:", element_id, "is not present in the actual active filters of dataset:",
            "{ deparse1(private$input_dataname) } therefore no changes are applied."
          )
        )
      } else {
        self$queue_remove(queue_index = 1L, element_id = element_id)
        logger::log_trace(
          sprintf(
            "%s$remove_filter_state for variable %s done, dataname: %s",
            class(self)[1],
            element_id,
            deparse1(private$input_dataname)
          )
        )
      }
    },

    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`data.frame`)\cr
    #'  object which columns are used to choose filter variables.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      checkmate::assert_string(id)
      stopifnot(is.data.frame(data))

      ns <- NS(id)

      if (ncol(data) == 0) {
        div("no sample variables available")
      } else if (nrow(data) == 0) {
        div("no samples available")
      } else {
        div(
          teal.widgets::optionalSelectInput(
            ns("var_to_add"),
            choices = NULL,
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              noneSelectedText = "Select variable to filter"
            )
          )
        )
      }
    },

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param data (`data.frame`)\cr
    #'  object which columns are used to choose filter variables.
    #' @param vars_include (`character(n)`)\cr
    #'  optional, vector of column names to be included.
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, data, vars_include = get_supported_filter_varnames(data = data), ...) {
      stopifnot(is.data.frame(data))
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "DFFilterStates$srv_add_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
          )
          shiny::setBookmarkExclude(c("var_to_add"))
          active_filter_vars <- reactive({
            vapply(
              X = self$queue_get(queue_index = 1L),
              FUN.VALUE = character(1),
              FUN = function(x) x$get_varname(deparse = TRUE)
            )
          })

          # available choices to display
          avail_column_choices <- reactive({
            choices <- setdiff(vars_include, active_filter_vars())

            data_choices_labeled(
              data = data,
              choices = choices,
              varlabels = private$get_varlabels(choices),
              keys = private$keys
            )
          })
          observeEvent(
            avail_column_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "DFFilterStates$srv_add_filter_state@1 updating available column choices,",
                "dataname: { deparse1(private$input_dataname) }"
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
                "DFFilterStates$srv_add_filter_state@1 updated available column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "DFFilterStates$srv_add_filter_state@2 adding FilterState of variable %s, dataname: %s",
                  input$var_to_add,
                  deparse1(private$input_dataname)
                )
              )
              self$queue_push(
                x = init_filter_state(
                  data[[input$var_to_add]],
                  varname = as.name(input$var_to_add),
                  varlabel = private$get_varlabels(input$var_to_add),
                  input_dataname = private$input_dataname
                ),
                queue_index = 1L,
                element_id = input$var_to_add
              )
              logger::log_trace(
                sprintf(
                  "DFFilterStates$srv_add_filter_state@2 added FilterState of variable %s, dataname: %s",
                  input$var_to_add,
                  deparse1(private$input_dataname)
                )
              )
            }
          )

          logger::log_trace(
            "DFFilterStates$srv_add_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
          )
          NULL
        }
      )
    }
  ),
  private = list(
    varlabels = character(0),
    keys = character(0),
    #' description
    #' Get label of specific variable. In case when variable label is missing
    #' name of the variable is returned.
    #' parameter variable (`character`)\cr
    #'  name of the variable for which label should be returned
    #' return `character`
    get_varlabels = function(variables = character(0)) {
      checkmate::assert_character(variables)
      if (identical(variables, character(0))) {
        private$varlabels
      } else {
        varlabels <- private$varlabels[variables]
        missing_labels <- is.na(varlabels) | varlabels == ""
        varlabels[missing_labels] <- variables[missing_labels]
        varlabels
      }
    }
  )
)
