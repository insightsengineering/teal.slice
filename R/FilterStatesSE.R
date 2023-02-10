#' @title `SEFilterStates`
#' @description Specialization of `FilterStates` for `SummaryExperiment`.
#' @keywords internal
SEFilterStates <- R6::R6Class( # nolint
  classname = "SEFilterStates",
  inherit = FilterStates,

  # public methods ----
  public = list(
    #' @description Initialize `SEFilterStates` object
    #'
    #' Initialize `SEFilterStates` object
    #'
    #' @param data (`SummarizedExperiment`)\cr
    #'   the R object which `subset` function is applied on.
    #'
    #' @param data_reactive (`reactive`)\cr
    #'   should return a `SummarizedExperiment` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If `reactive(NULL)` then filtered counts are not shown.
    #'
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    initialize = function(data, data_reactive, dataname, datalabel) {
      if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("Cannot load SummarizedExperiment - please install the package or restart your session.")
      }
      super$initialize(data, data_reactive, dataname, datalabel)
      private$state_list <- list(
        subset = reactiveVal(),
        select = reactiveVal()
      )
    },

    #' @description
    #' Returns the formatted string representing this `MAEFilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before each line of the representation
    #' @return `character(1)` the formatted string
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      whitespace_indent <- format("", width = indent)
      formatted_states <- c()
      if (!is.null(self$state_list_get(state_list_index = "subset"))) {
        formatted_states <- c(formatted_states, paste0(whitespace_indent, "  Subsetting:"))
        for (state in self$state_list_get(state_list_index = "subset")) {
          formatted_states <- c(formatted_states, state$format(indent = indent + 4))
        }
      }

      if (!is.null(self$state_list_get(state_list_index = "select"))) {
        formatted_states <- c(formatted_states, paste0(whitespace_indent, "  Selecting:"))
        for (state in self$state_list_get(state_list_index = "select")) {
          formatted_states <- c(formatted_states, state$format(indent = indent + 4))
        }
      }

      if (length(formatted_states) > 0) {
        formatted_states <- c(paste0(whitespace_indent, "Assay ", self$get_datalabel(), " filters:"), formatted_states)
        paste(formatted_states, collapse = "\n")
      }
    },

    #' @description
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Gets all active filters from this dataset in form of the nested list.
    #' The output list is a compatible input to `self$set_filter_state`.
    #'
    #' @return `list` containing one or two lists  depending on the number of
    #' `state_list` object (I.e. if `rowData` and `colData` exist). Each
    #' `list` contains elements number equal to number of active filter variables.
    get_filter_state = function() {
      states <- sapply(
        X = names(private$state_list),
        simplify = FALSE,
        function(x) {
          lapply(self$state_list_get(state_list_index = x), function(xx) xx$get_state())
        }
      )
      Filter(function(x) length(x) > 0, states)
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`named list`)\cr
    #'   this list should contain `subset` and `select` element where
    #'   each should be a named list containing values as a selection in the `FilterState`.
    #'   Names of each the `list` element in `subset` and `select` should correspond to
    #'   the name of the column in `rowData(data)` and `colData(data)`.
    #' @return `NULL`
    set_filter_state = function(state) {
      data <- private$data
      data_reactive <- private$data_reactive

      checkmate::assert_class(data, "SummarizedExperiment")
      checkmate::assert_class(state, "list")

      checkmate::assert(
        checkmate::check_subset(names(state), c("subset", "select")),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      checkmate::assert(
        checkmate::test_null(state$subset),
        checkmate::assert(
          checkmate::check_class(state$subset, "list"),
          checkmate::check_subset(names(state$subset), names(SummarizedExperiment::rowData(data))),
          combine = "and"
        ),
        combine = "or"
      )
      checkmate::assert(
        checkmate::test_null(state$select),
        checkmate::assert(
          checkmate::check_class(state$select, "list"),
          checkmate::check_subset(names(state$select), names(SummarizedExperiment::colData(data))),
          combine = "and"
        ),
        combine = "or"
      )

      filter_states <- self$state_list_get("subset")
      lapply(names(state$subset), function(varname) {
        value <- resolve_state(state$subset[[varname]])
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          fstate$set_state(value)
        } else {
          fstate <- init_filter_state(
            x = SummarizedExperiment::rowData(data)[[varname]],
            x_reactive = reactive(
              if (!is.null(data_reactive())) SummarizedExperiment::rowData(data_reactive())[[varname]]
            ),
            varname = varname,
            dataname = private$dataname
          )
          fstate$set_state(value)
          self$state_list_push(
            x = fstate,
            state_list_index = "subset",
            state_id = varname
          )
        }
      })

      filter_states <- self$state_list_get("select")
      lapply(names(state$select), function(varname) {
        value <- resolve_state(state$select[[varname]])
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          fstate$set_state(value)
        } else {
          fstate <- init_filter_state(
            x = SummarizedExperiment::colData(data)[[varname]],
            x_reactive = reactive(
              if (!is.null(data_reactive())) SummarizedExperiment::colData(data_reactive())[[varname]]
            ),
            varname = varname,
            dataname = private$dataname
          )
          fstate$set_state(value)
          self$state_list_push(
            x = fstate,
            state_list_index = "select",
            state_id = varname
          )
        }
      })
      logger::log_trace(paste(
        "SEFilterState$set_filter_state initialized,",
        "dataname: { private$dataname }"
      ))
      NULL
    },

    #' @description Remove a variable from the `state_list` and its corresponding UI element.
    #'
    #' @param state_id (`character(1)`)\cr name of `state_list` element.
    #'
    #' @return `NULL`
    remove_filter_state = function(state_id) {
      logger::log_trace(
        sprintf(
          "%s$remove_filter_state called, dataname: %s",
          class(self)[1],
          private$dataname
        )
      )

      checkmate::assert(
        !checkmate::test_null(names(state_id)),
        checkmate::check_subset(names(state_id), c("subset", "select")),
        combine = "and"
      )
      for (varname in state_id$subset) {
        if (!all(unlist(state_id$subset) %in% names(self$state_list_get("subset")))) {
          warning(paste(
            "Variable:", state_id, "is not present in the actual active subset filters of dataset:",
            "{ private$dataname } therefore no changes are applied."
          ))
          logger::log_warn(
            paste(
              "Variable:", state_id, "is not present in the actual active subset filters of dataset:",
              "{ private$dataname } therefore no changes are applied."
            )
          )
        } else {
          self$state_list_remove(state_list_index = "subset", state_id = varname)
          logger::log_trace(
            sprintf(
              "%s$remove_filter_state for subset variable %s done, dataname: %s",
              class(self)[1],
              varname,
              private$dataname
            )
          )
        }
      }

      for (varname in state_id$select) {
        if (!all(unlist(state_id$select) %in% names(self$state_list_get("select")))) {
          warning(paste(
            "Variable:", state_id, "is not present in the actual active select filters of dataset:",
            "{ private$dataname } therefore no changes are applied."
          ))
          logger::log_warn(
            paste(
              "Variable:", state_id, "is not present in the actual active select filters of dataset:",
              "{ private$dataname } therefore no changes are applied."
            )
          )
        } else {
          self$state_list_remove(state_list_index = "select", state_id = varname)
          sprintf(
            "%s$remove_filter_state for select variable %s done, dataname: %s",
            class(self)[1],
            varname,
            private$dataname
          )
        }
      }
    },

    # shiny modules ----
    #' @description
    #' Shiny module UI
    #'
    #' Shiny UI element that stores `FilterState` UI elements.
    #' Populated with elements created with `renderUI` in the module server.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny element (module instance) id
    #'
    #' @return `shiny.tag`
    #'
    ui = function(id) {
      ns <- NS(id)
      datalabel <- self$get_datalabel()
      tags$div(
        class = "list-group hideable-list-group",
        `data-label` = ifelse(datalabel == "", "", datalabel), # todo: labels are not displayed for MAE - see filter-panel.css
        shiny::tagList(uiOutput(ns("genes"))),
        shiny::tagList(uiOutput(ns("samples")))
      )
    },

    #' @description
    #' Shiny server module.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny module instance id
    #'
    #' @return `moduleServer` function which returns `NULL`
    #'
    server = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          genes_previous_state <- reactiveVal(character(0))
          genes_added_state_name <- reactiveVal(character(0))

          observeEvent(self$state_list_get(1L), {
            genes_added_state_name(setdiff(names(self$state_list_get(1L)), names(genes_previous_state())))
            genes_previous_state(self$state_list_get(1L))
          })

          observeEvent(
            genes_added_state_name(), # we want to call FilterState module only once when it's added
            ignoreNULL = TRUE,
            {
              fstates <- self$state_list_get(1L)
              lapply(genes_added_state_name(), function(fname) {
                id <- sprintf("genes-%s", fname)
                private$srv_card_module(id = id, state_list_index = 1L, element_id = fname, fs = fstates[[fname]])
              })
              genes_added_state_name(character(0))
            }
          )

          output[["genes"]] <- shiny::renderUI({
            fstates <- self$state_list_get(1L) # rerenders when queue changes / not when the state changes
            lapply(names(fstates), function(fname) {
              id <- sprintf("genes-%s", fname)
              private$ui_card_module(id = session$ns(id), fstates[[fname]])
            })
          })


          samples_previous_state <- reactiveVal(character(0))
          samples_added_state_name <- reactiveVal(character(0))

          observeEvent(self$state_list_get(2L), {
            samples_added_state_name(setdiff(names(self$state_list_get(2L)), names(samples_previous_state())))
            samples_previous_state(self$state_list_get(2L))
          })

          observeEvent(
            samples_added_state_name(), # we want to call FilterState module only once when it's added
            ignoreNULL = TRUE,
            {
              fstates <- self$state_list_get(2L)
              lapply(samples_added_state_name(), function(fname) {
                id <- sprintf("samples-%s", fname)
                private$srv_card_module(id = id, state_list_index = 2L, element_id = fname, fs = fstates[[fname]])
              })
              samples_added_state_name(character(0))
            }
          )

          output[["samples"]] <- shiny::renderUI({
            fstates <- self$state_list_get(2L) # rerenders when queue changes / not when the state changes
            lapply(names(fstates), function(fname) {
              id <- sprintf("samples-%s", fname)
              private$ui_card_module(id = session$ns(id), fstates[[fname]])
            })
          })

          NULL
        }
      )
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

      row_input <- if (ncol(SummarizedExperiment::rowData(data)) == 0) {
        div("no sample variables available")
      } else if (nrow(SummarizedExperiment::rowData(data)) == 0) {
        div("no samples available")
      } else {
        teal.widgets::optionalSelectInput(
          ns("row_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select gene variable"
          )
        )
      }

      col_input <- if (ncol(SummarizedExperiment::colData(data)) == 0) {
        div("no sample variables available")
      } else if (nrow(SummarizedExperiment::colData(data)) == 0) {
        div("no samples available")
      } else {
        teal.widgets::optionalSelectInput(
          ns("col_to_add"),
          choices = NULL,
          options = shinyWidgets::pickerOptions(
            liveSearch = TRUE,
            noneSelectedText = "Select sample variable"
          )
        )
      }

      div(
        row_input,
        col_input
      )
    },

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #' This module unlike other `FilterStates` classes manages two
    #' sets of filter variables - one for `colData` and another for
    #' `rowData`.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id) {
      data <- private$data
      data_reactive <- private$data_reactive
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "SEFilterState$srv_add_filter_state initializing, dataname: { private$dataname }"
          )



          row_data <- SummarizedExperiment::rowData(data)
          col_data <- SummarizedExperiment::colData(data)

          # available choices to display
          avail_row_data_choices <- reactive({
            active_filter_row_vars <- vapply(
              X = self$state_list_get(state_list_index = "subset"),
              FUN.VALUE = character(1),
              FUN = function(x) x$get_varname(deparse = TRUE)
            )
            choices <- setdiff(
              get_supported_filter_varnames(data = row_data),
              active_filter_row_vars
            )

            data_choices_labeled(
              data = row_data,
              choices = choices,
              varlabels = character(0),
              keys = NULL
            )
          })

          avail_col_data_choices <- reactive({
            active_filter_col_vars <- vapply(
              X = self$state_list_get(state_list_index = "select"),
              FUN.VALUE = character(1),
              FUN = function(x) x$get_varname(deparse = TRUE)
            )
            choices <- setdiff(
              get_supported_filter_varnames(data = col_data),
              active_filter_col_vars
            )

            data_choices_labeled(
              data = col_data,
              choices = choices,
              varlabels = character(0),
              keys = NULL
            )
          })

          observeEvent(
            avail_row_data_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@1 updating available row data choices,",
                "dataname: { private$dataname }"
              ))
              if (is.null(avail_row_data_choices())) {
                shinyjs::hide("row_to_add")
              } else {
                shinyjs::show("row_to_add")
              }
              teal.widgets::updateOptionalSelectInput(
                session,
                "row_to_add",
                choices = avail_row_data_choices()
              )
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@1 updated available row data choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            avail_col_data_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@2 updating available col data choices,",
                "dataname: { private$dataname }"
              ))
              if (is.null(avail_col_data_choices())) {
                shinyjs::hide("col_to_add")
              } else {
                shinyjs::show("col_to_add")
              }
              teal.widgets::updateOptionalSelectInput(
                session,
                "col_to_add",
                choices = avail_col_data_choices()
              )
              logger::log_trace(paste(
                "SEFilterStates$srv_add_filter_state@2 updated available col data choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$col_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add_filter_state@3 adding FilterState of column %s to col data, dataname: %s",
                  deparse1(input$col_to_add),
                  private$dataname
                )
              )
              varname <- input$col_to_add
              self$set_filter_state(list(select = setNames(list(list()), varname)))
              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add_filter_state@3 added FilterState of column %s to col data, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )


          observeEvent(
            eventExpr = input$row_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add_filter_state@4 adding FilterState of variable %s to row data, dataname: %s",
                  deparse1(input$row_to_add),
                  private$dataname
                )
              )
              varname <- input$row_to_add
              self$set_filter_state(list(subset = setNames(list(list()), varname)))
              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add_filter_state@4 added FilterState of variable %s to row data, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )

          logger::log_trace(
            "SEFilterState$srv_add_filter_state initialized, dataname: { private$dataname }"
          )
          NULL
        }
      )
    }
  )
)
