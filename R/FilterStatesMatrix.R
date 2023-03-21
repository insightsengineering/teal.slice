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
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `matrix` object or `NULL`.
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
    #'   specifying how observations are tallied.
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = character(0),
                          excluded_varnames = character(0),
                          count_type = c("none", "all", "hierarchical")) {
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_matrix(data)
      super$initialize(data, data_reactive, dataname, datalabel, excluded_varnames, count_type)
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
      for (state in private$state_list_get(state_list_index = "subset")) {
        formatted_states <- c(formatted_states, state$format(indent = indent + 2))
      }
      paste(formatted_states, collapse = "\n")
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
          previous_state <- reactiveVal(isolate(private$state_list_get("subset")))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(private$state_list_get("subset"), {
            added_state_name(
              setdiff(names(private$state_list_get("subset")), names(previous_state()))
            )
            removed_state_name(
              setdiff(names(previous_state()), names(private$state_list_get("subset")))
            )
            previous_state(private$state_list_get("subset"))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- private$state_list_get("subset")
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
    #' Gets all filter state information from this dataset.
    #'
    #' @return `teal_slices`
    #'
    get_filter_state = function() {
      slices <- lapply(private$state_list_get("subset"), function(x) x$get_state())
      excluded_varnames <- structure(
        list(setdiff(colnames(private$data), private$filterable_varnames)),
        names = private$dataname)
      excluded_varnames <- Filter(function(x) !identical(x, character(0)), excluded_varnames)

      do.call(filter_settings, c(slices, list(exclude = excluded_varnames, count_type = private$count_type)))

    },

    #' @description
    #' Sets a filter state
    #'
    #' @param state (`teal_slices`)
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      checkmate::assert_class(state, "teal_slices")
      lapply(state, function(x) {
        checkmate::assert_true(x$dataname == private$dataname, .var_name = "dataname mathces private$dataname")
      })

      logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")

      private$set_filter_state_impl(
        state = state,
        state_list_index = "subset",
        data = private$data,
        data_reactive = private$data_reactive,
        extract_type = "matrix"
      )

      logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")

      invisible(NULL)
    },

    #' @description
    #' Remove one or more `FilterState`s from the `state_list` along with their corresponding UI elements.
    #'
    #' @param state (`teal_slices`)\cr
    #'   specifying `FilterState` objects to remove;
    #'   `teal_slice`s may contain only `dataname` and `varname`, other elements are ignored
    #'
    #' @return `NULL` invisibly
    #'
    remove_filter_state = function(state) {
      checkmate::assert_class(state, "teal_slices")

      lapply(state, function(x) {
        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removing filter, dataname: { x$dataname }, varname: { x$varname }"
        )

        private$state_list_remove(state_list_index = "subset", state_id = x$varname)

        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removed filter, dataname: { x$dataname }, varname: { x$varname }"
        )
      })

      invisible(NULL)
    },

    # shiny modules ----

    #' @description
    #' Shiny UI module to add filter variable.
    #'
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @return shiny.tag
    #'
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
    srv_add = function(id) {
      data <- private$data
      data_reactive <- private$data_reactive
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "MatrixFilterStates$srv_add initializing, dataname: { private$dataname }"
          )

          # available choices to display
          avail_column_choices <- reactive({
            active_filter_vars <- unique(unlist(extract_feat(self$get_filter_state(), "varname")))
            choices <- setdiff(
              get_supported_filter_varnames(data = data),
              active_filter_vars
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
                "MatrixFilterStates$srv_add@1 updating column choices,",
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
                "MatrixFilterStates$srv_add@1 updated column choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "MatrixFilterState$srv_add@2 adding FilterState of variable %s, dataname: %s",
                  deparse1(input$var_to_add),
                  private$dataname
                )
              )
              varname <- input$var_to_add
              self$set_filter_state(filter_settings(filter_var(private$dataname, varname)))

              logger::log_trace(
                sprintf(
                  "MatrixFilterState$srv_add@2 added FilterState of variable %s, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )

          logger::log_trace(
            "MatrixFilterStates$srv_add initialized, dataname: { private$dataname }"
          )
          NULL
        }
      )
    }
  )
)
