#' @title `MAEFilterStates`
#' @description Specialization of `FilterStates` for `MultiAssayExperiment`.
#' @keywords internal
MAEFilterStates <- R6::R6Class( # nolint
  classname = "MAEFilterStates",
  inherit = FilterStates,
  public = list(
    #' @description Initializes `MAEFilterStates` object
    #'
    #' Initialize `MAEFilterStates` object
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
    initialize = function(data, data_filtered, input_dataname, output_dataname, datalabel, varlabels, keys) {
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      super$initialize(data, data_filtered, input_dataname, output_dataname, datalabel)
      private$keys <- keys
      private$varlabels <- varlabels

      self$state_list_initialize(
        list(
          y = shiny::reactiveVal()
        )
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

      if (length(self$state_list_get(1L)) > 0) {
        formatted_states <- sprintf("%sSubject filters:", format("", width = indent))
        for (state in self$state_list_get(1L)) formatted_states <- c(formatted_states, state$format(indent = indent + 2))
        paste(formatted_states, collapse = "\n")
      }
    },

    #' @description
    #' Returns function name used to create filter call.
    #' For `MAEFilterStates` `MultiAssayExperiment::subsetByColData` is used.
    #' @return `character(1)`
    get_fun = function() {
      return("MultiAssayExperiment::subsetByColData")
    },

    #' @description
    #' Server module
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          previous_state <- reactiveVal(isolate(self$state_list_get("y")))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$state_list_get("y"), {
            added_state_name(setdiff(names(self$state_list_get("y")), names(previous_state())))
            removed_state_name(setdiff(names(previous_state()), names(self$state_list_get("y"))))

            previous_state(self$state_list_get("y"))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- self$state_list_get("y")
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
      lapply(self$state_list_get("y"), function(x) x$get_state())
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `colData(data)`.
    #' @param ... ignored.
    #' @return `NULL`
    set_filter_state = function(state, ...) {
      data <- private$data
      data_filtered <- private$data_filtered

      checkmate::assert_class(data, "MultiAssayExperiment")
      checkmate::assert(
        checkmate::check_subset(names(state), names(SummarizedExperiment::colData(data))),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      logger::log_trace("MAEFilterState$set_filter_state initializing, dataname: { deparse1(private$input_dataname) }")
      filter_states <- self$state_list_get("y")
      lapply(names(state), function(varname) {
        value <- resolve_state(state[[varname]])
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          fstate$set_state(value)
        } else {
          fstate <- init_filter_state(
            x = SummarizedExperiment::colData(data)[[varname]],
            x_filtered = reactive(SummarizedExperiment::colData(data_filtered())[[varname]]),
            varname = as.name(varname),
            varlabel = private$get_varlabels(varname),
            input_dataname = private$input_dataname,
            extract_type = "list"
          )
          fstate$set_state(value)
          fstate$set_na_rm(TRUE)
          self$state_list_push(
            x = fstate,
            "y",
            state_id = varname
          )
        }
      })
      logger::log_trace("MAEFilterState$set_filter_state initialized, dataname: { deparse1(private$input_dataname) }")
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
          deparse1(private$input_dataname)
        )
      )

      if (!state_id %in% names(self$state_list_get("y"))) {
        warning(paste(
          "Variable:", state_id,
          "is not present in the actual active filters of dataset: { deparse1(private$input_dataname) }",
          "therefore no changes are applied."
        ))
        logger::log_warn(
          paste(
            "Variable:", state_id, "is not present in the actual active filters of dataset:",
            "{ deparse1(private$input_dataname) } therefore no changes are applied."
          )
        )
      } else {
        self$state_list_remove("y", state_id = state_id)
        logger::log_trace(
          sprintf(
            "%s$remove_filter_state for variable %s done, dataname: %s",
            class(self)[1],
            state_id,
            deparse1(private$input_dataname)
          )
        )
      }
    },

    #' @description
    #' Shiny UI module to add filter variable
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @return shiny.tag
    ui_add_filter_state = function(id) {
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
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, ...) {
      data <- private$data
      data_filtered <- private$data_filtered
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "MAEFilterState$srv_add_filter_state initializing, dataname: { deparse1(private$input_dataname) }"
          )
          active_filter_vars <- reactive({
            vapply(
              X = self$state_list_get("y"),
              FUN.VALUE = character(1),
              FUN = function(x) x$get_varname(deparse = TRUE)
            )
          })

          # available choices to display
          avail_column_choices <- reactive({
            choices <- setdiff(
              get_supported_filter_varnames(data = SummarizedExperiment::colData(data)),
              active_filter_vars()
            )
            data_choices_labeled(
              data = SummarizedExperiment::colData(data),
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
                "MAEFilterStates$srv_add_filter_state@1 updating available column choices,",
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
                "MAEFilterStates$srv_add_filter_state@1 updated available column choices,",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "MAEFilterStates$srv_add_filter_state@2 adding FilterState of variable %s, dataname: %s",
                  deparse1(input$var_to_add),
                  deparse1(private$input_dataname)
                )
              )
              varname <- input$var_to_add
              self$set_filter_state(setNames(list(list()), varname))
              logger::log_trace(
                sprintf(
                  "MAEFilterStates$srv_add_filter_state@2 added FilterState of variable %s, dataname: %s",
                  deparse1(varname),
                  deparse1(private$input_dataname)
                )
              )
            }
          )

          logger::log_trace(
            "MAEFilterState$srv_add_filter_state initialized, dataname: { deparse1(private$input_dataname) }"
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
