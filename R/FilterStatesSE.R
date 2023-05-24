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
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = NULL) {
      if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("Cannot load SummarizedExperiment - please install the package or restart your session.")
      }
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_class(data, "SummarizedExperiment")
      super$initialize(data, data_reactive, dataname, datalabel)
      if (!is.null(datalabel)) {
        private$dataname_prefixed <- sprintf("%s[['%s']]", dataname, datalabel)
      }
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`teal_slices`)\cr
    #'    `teal_slice` objects should contain the field `arg %in% c("subset", "select")`
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")
      checkmate::assert_class(state, "teal_slices")
      lapply(state, function(x) {
        checkmate::assert_choice(x$arg, choices = c("subset", "select"), null.ok = TRUE, .var.name = "teal_slice$arg")
      })
      count_type <- attr(state, "count_type")
      if (length(count_type)) {
        private$count_type <- count_type
      }

      subset_states <- Filter(function(x) x$arg == "subset", state)
      private$set_filter_state_impl(
        state = subset_states,
        data = SummarizedExperiment::rowData(private$data),
        data_reactive = function(sid = "") SummarizedExperiment::rowData(private$data_reactive())
      )

      select_states <- Filter(function(x) x$arg == "select", state)
      private$set_filter_state_impl(
        state = select_states,
        data = SummarizedExperiment::colData(private$data),
        data_reactive = function(sid = "") SummarizedExperiment::colData(private$data_reactive())
      )

      logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")
      invisible(NULL)
    },

    # shiny modules ----

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
          logger::log_trace("SEFilterState$srv_add initializing, dataname: { private$dataname }")
          row_data <- SummarizedExperiment::rowData(data)
          col_data <- SummarizedExperiment::colData(data)

          # available choices to display
          avail_row_data_choices <- reactive({
            slices_for_subset <- Filter(function(x) x$arg == "subset", self$get_filter_state())
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
            slices_for_select <- Filter(function(x) x$arg == "select", self$get_filter_state())
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
              self$set_filter_state(filter_settings(filter_var(private$dataname, varname, arg = "select")))

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
              self$set_filter_state(filter_settings(filter_var(private$dataname, varname, arg = "subset")))

              logger::log_trace(
                sprintf(
                  "SEFilterStates$srv_add@4 added FilterState of variable %s to row data, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )

          logger::log_trace("SEFilterState$srv_add initialized, dataname: { private$dataname }")
          NULL
        }
      )
    }
  )
)
