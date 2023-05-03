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
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #' @param keys (`character`)\cr
    #'   key columns names
    #'
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
      data <- colData(data)
      data_reactive <- function(sid = character(0)) SummarizedExperiment::colData(data_reactive())
      super$initialize(data, data_reactive, dataname, datalabel)
      private$keys <- keys
      private$varlabels <- varlabels
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

    #' @description
    #' Set filter state
    #'
    #' @param state (`teal_slices`)\cr
    #'    `teal_slice` objects should contain the field `arg = "y"`
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")
      checkmate::assert_class(state, "teal_slices")
      lapply(state, function(x) checkmate::assert_true(x$arg == "y", .var.name = "teal_slice"))

      super$set_filter_state(state)

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
      data <- private$data

      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("MAEFilterState$srv_add initializing, dataname: { private$dataname }")

          # available choices to display
          avail_column_choices <- reactive({
            vars_include <- private$get_filterable_varnames()
            active_filter_vars <- slices_field(self$get_filter_state(), "varname")
            choices <- setdiff(vars_include, active_filter_vars)
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
              self$set_filter_state(filter_settings(
                filter_var(dataname = private$dataname, varname = varname, datalabel = "subjects")
              ))

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

  # private fields ----

  private = list(
    extract_type = "list",
    fun = quote(MultiAssayExperiment::subsetByColData),
    keys = character(0),
    varlabels = character(0)
  )
)
