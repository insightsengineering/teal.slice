#' @title `FilterStates` subclass for matrices
#' @description Handles filter states in a `matrix`
#' @keywords internal
#'
#'
MatrixFilterStates <- R6::R6Class( # nolint
  classname = "MatrixFilterStates",
  inherit = FilterStates,

  # public methods ----
  public = list(
    #' @description Initialize `MatrixFilterStates` object
    #'
    #' Initialize `MatrixFilterStates` object
    #'
    #' @param data (`matrix`)\cr
    #'   the R object which `subset` function is applied on.
    #'
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `matrix` object or `NULL`.
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
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = character(0)) {
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_matrix(data)
      super$initialize(data, data_reactive, dataname, datalabel)
      private$state_list <- list(
        subset = reactiveVal()
      )
    },

    #' @description
    #' Returns the formatted string representing this `MatrixFilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before each line of the representation
    #' @return `character(1)` the formatted string
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      formatted_states <- c()
      for (state in self$state_list_get(state_list_index = "subset")) {
        formatted_states <- c(formatted_states, state$format(indent = indent + 2))
      }
      paste(formatted_states, collapse = "\n")
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
          previous_state <- reactiveVal(isolate(self$state_list_get("subset")))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$state_list_get("subset"), {
            added_state_name(
              setdiff(names(self$state_list_get("subset")), names(previous_state()))
            )
            removed_state_name(
              setdiff(names(previous_state()), names(self$state_list_get("subset")))
            )
            previous_state(self$state_list_get("subset"))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- self$state_list_get("subset")
            html_ids <- private$map_vars_to_html_ids(keys = names(fstates))
            for (fname in added_state_name()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                state_list_index = "subset",
                state_id = fname
              )
            }
            added_state_name(character(0))
          })

          observeEvent(removed_state_name(), ignoreNULL = TRUE, {
            for (fname in removed_state_name()) {
              private$remove_filter_state_ui("subset", fname, .input = input)
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
    #' @return `list` containing `list` with selected values for each `FilterState`.
    get_filter_state = function() {
      lapply(self$state_list_get(state_list_index = "subset"), function(x) x$get_state())
    },

    #' @description
    #' Sets a filter state
    #'
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    #' @return `NULL`
    set_filter_state = function(state) {
      logger::log_trace("MatrixFilterState$set_filter_state initializing, dataname: { private$dataname }")
      checkmate::assert_list(state, null.ok = TRUE, names = "named")

      data <- private$data
      data_reactive <- private$data_reactive

      private$set_filter_state_impl(
        state = state,
        state_list_index = "subset",
        data = data,
        data_reactive = data_reactive
      )

      logger::log_trace("MatrixFilterState$set_filter_state initialized, dataname: { private$dataname }")
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
          "%s$remove_filter_state of variable %s, dataname: %s",
          class(self)[1],
          state_id,
          private$dataname
        )
      )

      if (!state_id %in% names(shiny::isolate(self$state_list_get("subset")))) {
        msg <- sprintf(
          "%s is not an active filter of dataset: %s and can't be removed.",
          state_id,
          private$dataname
        )
        warning(msg)
        logger::log_warn(msg)
      } else {
        self$state_list_remove(state_list_index = "subset", state_id = state_id)
        logger::log_trace(
          sprintf(
            "%s$remove_filter_state of variable %s done, dataname: %s",
            class(self)[1],
            state_id,
            private$dataname
          )
        )
      }
    },

    # shiny modules ----

    #' @description
    #' Shiny UI module to add filter variable.
    #'
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @return shiny.tag
    #'
    ui_add_filter_state = function(id) {
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
            noneSelectedText = "Select variable to filter"
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
    #'   shiny module instance id
    #'
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id) {
      data <- private$data
      data_reactive <- private$data_reactive
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "MatrixFilterStates$srv_add_filter_state initializing, dataname: { private$dataname }"
          )
          active_filter_vars <- reactive({
            vapply(
              X = self$state_list_get(state_list_index = "subset"),
              FUN.VALUE = character(1),
              FUN = function(x) x$get_varname()
            )
          })

          # available choices to display
          avail_column_choices <- reactive({
            choices <- setdiff(
              get_supported_filter_varnames(data = data),
              active_filter_vars()
            )
            data_choices_labeled(
              data = data,
              choices = choices,
              varlabels = character(0),
              keys = NULL
            )
          })
          observeEvent(
            avail_column_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "MatrixFilterStates$srv_add_filter_state@1 updating column choices,",
                "dataname: { private$dataname }"
              ))
              if (length(avail_column_choices()) < 0) {
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
                "MatrixFilterStates$srv_add_filter_state@1 updated column choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "MatrixFilterState$srv_add_filter_state@2 adding FilterState of variable %s, dataname: %s",
                  deparse1(input$var_to_add),
                  private$dataname
                )
              )
              varname <- input$var_to_add
              self$set_filter_state(setNames(list(list()), varname))
              logger::log_trace(
                sprintf(
                  "MatrixFilterState$srv_add_filter_state@2 added FilterState of variable %s, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )

          logger::log_trace(
            "MatrixFilterStates$srv_add_filter_state initialized, dataname: { private$dataname }"
          )
          NULL
        }
      )
    }
  )
)
