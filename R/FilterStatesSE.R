#' @title `FilterStates` subclass for SummarizedExperiments
#' @description Handles filter states in a `SummaryExperiment`
#' @keywords internal
#'
#'
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
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `SummarizedExperiment` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #' @param excluded_varnames (`character`)\cr
    #'   names of variables that can \strong{not} be filtered on.
    #' @param count_type `character(1)`\cr
    #'   specifying how observations are tallied
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = character(0),
                          excluded_varnames = character(0),
                          count_type = c("all", "none")) {
      if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("Cannot load SummarizedExperiment - please install the package or restart your session.")
      }
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_class(data, "SummarizedExperiment")
      super$initialize(data, data_reactive, dataname, datalabel, excluded_varnames, count_type)
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

      formatted_states <- c()
      if (!is.null(private$state_list_get(state_list_index = "subset"))) {
        formatted_states <- c(formatted_states, sprintf("%sSubsetting:", format("", width = indent * 2)))
        for (state in private$state_list_get(state_list_index = "subset")) {
          formatted_states <- c(formatted_states, state$format(indent = indent * 2))
        }
      }

      if (!is.null(private$state_list_get(state_list_index = "select"))) {
        formatted_states <- c(formatted_states, sprintf("%sSelecting:", format("", width = indent * 2)))
        for (state in private$state_list_get(state_list_index = "select")) {
          formatted_states <- c(formatted_states, state$format(indent = indent * 2))
        }
      }

      if (length(formatted_states) > 0) {
        formatted_states <- c(
          sprintf("%sAssay %s filters:", format("", width = indent), self$get_datalabel()),
          formatted_states
        )
        paste(formatted_states, collapse = "\n")
      }
    },

    #' @description
    #' Returns active `FilterState` objects.
    #'
    #' Gets all filter state information from this dataset.
    #'
    #' @return `teal_slices`
    #'
    get_filter_state = function() {
      slices_subset <- lapply(private$state_list$subset(), function(x) x$get_state())
      slices_select <- lapply(private$state_list$select(), function(x) x$get_state())
      slices <- c(slices_subset, slices_select)
      excluded_varnames <- structure(
        list(setdiff(colnames(private$data), private$filterable_varnames)),
        names = private$dataname
      )
      excluded_varnames <- Filter(function(x) !identical(x, character(0)), excluded_varnames)

      do.call(filter_settings, c(slices, list(exclude = excluded_varnames, count_type = private$count_type)))
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`teal_slices`)\cr
    #'   `teal_slice` objects targeting `rowData(data)` should contain the field `target = "subset"`\cr
    #'   `teal_slice` objects targeting `colData(data)` should contain the field `target = "select"`
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      checkmate::assert_class(state, "teal_slices")
      lapply(state, function(x) {
        checkmate::assert_true(x$dataname == private$dataname, .var.name = "dataname matches private$dataname")
      })

      logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")

      private$set_filter_state_impl(
        state = slices_which(state, "target == \"subset\""),
        state_list_index = "subset",
        data = SummarizedExperiment::rowData(private$data),
        data_reactive = function(sid) SummarizedExperiment::rowData(private$data_reactive(sid))
      )
      private$set_filter_state_impl(
        state = slices_which(state, "target == \"select\""),
        state_list_index = "select",
        data = SummarizedExperiment::colData(private$data),
        data_reactive = function(sid) SummarizedExperiment::colData(private$data_reactive(sid))
      )

      logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")

      invisible(NULL)
    },

    #' @description
    #' Remove one or more `FilterState`s from `state_list`s along with their corresponding UI elements.
    #'
    #' @param state (`teal_slices`)\cr
    #'   specifying `FilterState` objects to remove;
    #'   `teal_slice`s may contain only `dataname` and `varname`, other elements are ignored
    #'
    #' @return `NULL` invisibly
    #'
    remove_filter_state = function(state) {
      checkmate::assert_class(state, "teal_slices")
      slices_for_subset <- slices_which(
        state,
        sprintf("varname %%in%% c(%s)", dQuote(toString(names(private$state_list$subset())), q = FALSE))
      )
      slices_for_select <- slices_which(
        state,
        sprintf("varname %%in%% c(%s)", dQuote(toString(names(private$state_list$select())), q = FALSE))
      )

      if ((length(slices_for_subset) + length(slices_for_select) == 0L)) {
        warning("SEFilterStates$remove_filter_state: no filter states to remove")
      }

      lapply(slices_for_subset, function(x) {
        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removing filter, dataname: { x$dataname }, varname: { x$varname }"
        )

        private$state_list_remove(state_list_index = "subset", state_id = x$varname)

        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removed filter, dataname: { x$dataname }, varname: { x$varname }"
        )
      })

      lapply(slices_for_select, function(x) {
        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removing filter, dataname: { x$dataname }, varname: { x$varname }"
        )

        private$state_list_remove(state_list_index = "select", state_id = x$varname)

        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removed filter, dataname: { x$dataname }, varname: { x$varname }"
        )
      })

      invisible(NULL)
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
          previous_state_subset <- reactiveVal(isolate(private$state_list_get("subset")))
          added_state_name_subset <- reactiveVal(character(0))
          removed_state_name_subset <- reactiveVal(character(0))

          observeEvent(private$state_list_get("subset"), {
            added_state_name_subset(
              setdiff(names(private$state_list_get("subset")), names(previous_state_subset()))
            )
            removed_state_name_subset(
              setdiff(names(previous_state_subset()), names(private$state_list_get("subset")))
            )
            previous_state_subset(private$state_list_get("subset"))
          })

          observeEvent(added_state_name_subset(), ignoreNULL = TRUE, {
            fstates <- private$state_list_get("subset")
            html_ids <- private$map_vars_to_html_ids(keys = names(fstates), prefix = "rowData")
            for (fname in added_state_name_subset()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                state_list_index = "subset",
                state_id = fname
              )
            }
            added_state_name_subset(character(0))
          })

          observeEvent(removed_state_name_subset(), ignoreNULL = TRUE, {
            for (fname in removed_state_name_subset()) {
              private$remove_filter_state_ui("subset", fname, .input = input)
            }
            removed_state_name_subset(character(0))
          })

          # select
          previous_state_select <- reactiveVal(isolate(private$state_list_get("select")))
          added_state_name_select <- reactiveVal(character(0))
          removed_state_name_select <- reactiveVal(character(0))

          observeEvent(private$state_list_get("select"), {
            # find what has been added or removed
            added_state_name_select(
              setdiff(names(private$state_list_get("select")), names(previous_state_select()))
            )
            removed_state_name_select(
              setdiff(names(previous_state_select()), names(private$state_list_get("select")))
            )
            previous_state_select(private$state_list_get("select"))
          })

          observeEvent(added_state_name_select(), ignoreNULL = TRUE, {
            fstates <- private$state_list_get("select")
            html_ids <- private$map_vars_to_html_ids(keys = names(fstates), prefix = "colData")
            for (fname in added_state_name_select()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                state_list_index = "select",
                state_id = fname
              )
            }
            added_state_name_select(character(0))
          })

          observeEvent(removed_state_name_select(), ignoreNULL = TRUE, {
            for (fname in removed_state_name_select()) {
              private$remove_filter_state_ui("select", fname, .input = input)
            }
            removed_state_name_select(character(0))
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
    ui_add = function(id) {
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
    srv_add = function(id) {
      data <- private$data
      data_reactive <- private$data_reactive
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "SEFilterState$srv_add initializing, dataname: { private$dataname }"
          )
          row_data <- SummarizedExperiment::rowData(data)
          col_data <- SummarizedExperiment::colData(data)

          # available choices to display
          avail_row_data_choices <- reactive({
            slices_for_subset <- slices_which(self$get_filter_state(), "target == \"subset\"")
            active_filter_row_vars <- slices_field(slices_for_subset, "varname")

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
            slices_for_select <- slices_which(self$get_filter_state(), "target == \"select\"")
            active_filter_col_vars <- slices_field(slices_for_select, "varname")

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
                "SEFilterStates$srv_add@1 updating available row data choices,",
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
                "SEFilterStates$srv_add@1 updated available row data choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            avail_col_data_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "SEFilterStates$srv_add@2 updating available col data choices,",
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
                "SEFilterStates$srv_add@2 updated available col data choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$col_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add@3 adding FilterState of column %s to col data, dataname: %s",
                  deparse1(input$col_to_add),
                  private$dataname
                )
              )
              varname <- input$col_to_add
              # self$set_filter_state(list(select = setNames(list(list()), varname)))
              self$set_filter_state(filter_settings(filter_var(private$dataname, varname, target = "select")))

              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add@3 added FilterState of column %s to col data, dataname: %s",
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
                  "SEFilterStates$srv_add@4 adding FilterState of variable %s to row data, dataname: %s",
                  deparse1(input$row_to_add),
                  private$dataname
                )
              )
              varname <- input$row_to_add
              self$set_filter_state(filter_settings(filter_var(private$dataname, varname, target = "subset")))

              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add@4 added FilterState of variable %s to row data, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )

          logger::log_trace(
            "SEFilterState$srv_add initialized, dataname: { private$dataname }"
          )
          NULL
        }
      )
    }
  ),

  # private methods ----
  # private methods ----
  private = list(
    get_dataname_prefixed = function() {
      if (identical(private$datalabel, character(0))) {
        private$dataname
      } else {
        sprintf('%s[["%s"]]', private$dataname, private$datalabel)
      }
    }
  )
)
