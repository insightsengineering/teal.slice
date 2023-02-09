#' @title `DFFFilterStates`
#'
#' @description Specialization of `FilterStates` for a base `data.frame`.
#'
#' @keywords internal
#'
DFFilterStates <- R6::R6Class( # nolint
  classname = "DFFilterStates",
  inherit = FilterStates,

  # public methods ----
  public = list(
    #' @description Initializes `DFFilterStates` object.
    #'
    #' Initializes `DFFilterStates` object by setting `dataname`
    #'  and initializing `state_list` (`shiny::reactiveVal`).
    #' This class contains a single `state_list` with no specified name,
    #' which means that when calling the subset function associated with this class
    #' (`dplyr::filter`), a list of conditions is passed to unnamed arguments (`...`).
    #'
    #' @param data (`data.frame`)\cr
    #'   the R object which `dplyr::filter` function is applied on.
    #'
    #' @param data_reactive (`reactive`)\cr
    #'   should a `data.frame` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If `reactive(NULL)` then filtered counts are not shown.
    #'
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the \emph{subset expression}
    #'   specified to the function argument attached to this `FilterStates`
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value
    #' @param varlabels (`character`)\cr
    #'   labels of the variables used in this object
    #' @param keys (`character`)\cr
    #'   key columns names
    #'
    initialize = function(data, data_reactive, dataname, datalabel, varlabels, keys) {
      super$initialize(data, data_reactive, dataname, datalabel)
      private$varlabels <- varlabels
      private$keys <- keys
      self$set_filterable_varnames(colnames(data))
      private$state_list <- list(
        reactiveVal()
      )
    },

    #' @description
    #' Returns a formatted string representing this `FilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces prepended to each line of the output
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(indent = 0) {
      checkmate::assert_number(indent, finite = TRUE, lower = 0)

      formatted_states <- vapply(
        self$state_list_get(1L), function(state) state$format(indent = indent),
        USE.NAMES = FALSE, FUN.VALUE = character(1)
      )
      paste(formatted_states, collapse = "\n")
    },

    #' @description
    #' Gets the name of the function used to filter the data in this `FilterStates`.
    #'
    #' Get name of  function used to create the \emph{subset expression}.
    #' For `DFFilterStates` this is `dplyr::filter`.
    #'
    #' @return `character(1)`
    get_fun = function() {
      return("dplyr::filter")
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
          previous_state <- reactiveVal(character(0))
          added_state_name <- reactiveVal(character(0))
          removed_state_name <- reactiveVal(character(0))

          observeEvent(self$state_list_get(1L), {
            added_state_name(setdiff(names(self$state_list_get(1L)), names(previous_state())))
            removed_state_name(setdiff(names(previous_state()), names(self$state_list_get(1L))))
            previous_state(self$state_list_get(1L))
          })

          observeEvent(added_state_name(), ignoreNULL = TRUE, {
            fstates <- self$state_list_get(1L)
            html_ids <- private$map_vars_to_html_ids(names(fstates))
            for (fname in added_state_name()) {
              private$insert_filter_state_ui(
                id = html_ids[fname],
                filter_state = fstates[[fname]],
                state_list_index = 1L,
                state_id = fname
              )
            }
            added_state_name(character(0))
          })

          observeEvent(removed_state_name(), ignoreNULL = TRUE, {
            for (fname in removed_state_name()) {
              private$remove_filter_state_ui(1L, fname, .input = input)
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
    #' Get active filter state from the `FilterState` objects kept in `state_list`.
    #' The output list is a compatible input to `self$set_filter_state`.
    #'
    #' @return `list` with named elements corresponding to `FilterState` in the `state_list`.
    #'
    get_filter_state = function() {
      lapply(self$state_list_get(1L), function(x) x$get_state())
    },

    #' @description
    #' Set filter state.
    #'
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the columns.
    #' @examples
    #' df <- data.frame(
    #'   character = letters,
    #'   numeric = seq_along(letters),
    #'   date = seq(Sys.Date(), length.out = length(letters), by = "1 day"),
    #'   datetime = seq(Sys.time(), length.out = length(letters), by = "33.33 hours")
    #' )
    #' filter_states <- teal.slice:::DFFilterStates$new(
    #'   data = df,
    #'   data_reactive = reactive(data),
    #'   dataname = "data",
    #'   output_dataname = "data_filtered",
    #'   varlabels = c(x = "x variable", SEX = "Sex"),
    #'   datalabel = character(0),
    #'   keys = character(0)
    #' )
    #' isolate(filter_states$set_filter_state(list(character = list("a"))))
    #' isolate(filter_states$get_call())
    #'
    #' @return `NULL`
    set_filter_state = function(state) {
      data <- private$data
      data_reactive <- private$data_reactive

      checkmate::assert(
        checkmate::check_subset(names(state), names(data)),
        checkmate::check_class(state, "default_filter"),
        combine = "or"
      )
      logger::log_trace(
        "{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }"
      )
      vars_include <- get_supported_filter_varnames(data = data)

      filter_states <- self$state_list_get(1L)
      state_names <- names(state)
      excluded_vars <- setdiff(state_names, vars_include)
      if (length(excluded_vars) > 0) {
        warning(
          paste(
            "These columns filters were excluded:",
            paste(excluded_vars, collapse = ", "),
            "from dataset",
            private$dataname
          )
        )
        logger::log_warn(
          paste(
            "Columns filters { paste(excluded_vars, collapse = ', ') } were excluded",
            "from { private$dataname }"
          )
        )
      }

      filters_to_apply <- state_names[state_names %in% vars_include]

      lapply(filters_to_apply, function(varname) {
        value <- resolve_state(state[[varname]])
        if (varname %in% names(filter_states)) {
          fstate <- filter_states[[varname]]
          fstate$set_state(value)
        } else {
          fstate <- init_filter_state(
            x = data[[varname]],
            x_reactive = reactive(data_reactive()[[varname]]),
            varname = varname,
            varlabel = private$get_varlabels(varname),
            dataname = private$dataname
          )
          fstate$set_state(value)
          self$state_list_push(x = fstate, state_list_index = 1L, state_id = varname)
        }
      })
      logger::log_trace(
        "{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }"
      )
      NULL
    },

    #' @description Remove a `FilterState` from the `state_list`.
    #'
    #' @param state_id (`character(1)`)\cr name of `state_list` element
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(state_id) {
      logger::log_trace(
        sprintf(
          "%s$remove_filter_state for variable %s called, dataname: %s",
          class(self)[1],
          state_id,
          private$dataname
        )
      )

      if (!state_id %in% names(self$state_list_get(1L))) {
        warning(paste(
          "Variable:", state_id,
          "is not present in the actual active filters of dataset: { private$dataname }",
          "therefore no changes are applied."
        ))
        logger::log_warn(
          paste(
            "Variable:", state_id, "is not present in the actual active filters of dataset:",
            "{ private$dataname } therefore no changes are applied."
          )
        )
      } else {
        self$state_list_remove(state_list_index = 1L, state_id = state_id)
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

    # shiny modules ----

    #' @description
    #' Set the allowed filterable variables
    #' @param varnames (`character` or `NULL`) The variables which can be filtered
    #' See `self$get_filterable_varnames` for more details
    #'
    #' @details When retrieving the filtered variables only
    #' those which have filtering supported (i.e. are of the permitted types)
    #' are included.
    #'
    #' @return invisibly this `FilteredDataset`
    set_filterable_varnames = function(varnames) {
      checkmate::assert_character(varnames, any.missing = FALSE, null.ok = TRUE)
      supported_vars <- get_supported_filter_varnames(private$data)
      private$filterable_varnames <- intersect(varnames, supported_vars)
      return(invisible(self))
    },

    #' @description
    #' Shiny UI module to add filter variable.
    #'
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`data.frame`)\cr
    #'  object which columns are used to choose filter variables.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      checkmate::assert_string(id)
      data <- private$data

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
    #' Shiny server module to add filter variable.
    #'
    #' This module controls available choices to select as a filter variable.
    #' Once selected, a variable is removed from available choices.
    #' Removing a filter variable adds it back to available choices.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #'
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          data <- private$data
          data_reactive <- private$data_reactive
          vars_include <- private$filterable_varnames
          logger::log_trace(
            "DFFilterStates$srv_add_filter_state initializing, dataname: { private$dataname }"
          )
          active_filter_vars <- reactive({
            vapply(
              X = self$state_list_get(state_list_index = 1L),
              FUN.VALUE = character(1),
              FUN = function(x) x$get_varname()
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
                "DFFilterStates$srv_add_filter_state@1 updated available column choices,",
                "dataname: { private$dataname }"
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
                  private$dataname
                )
              )
              varname <- input$var_to_add
              self$set_filter_state(state = setNames(list(list()), varname))
              logger::log_trace(
                sprintf(
                  "DFFilterStates$srv_add_filter_state@2 added FilterState of variable %s, dataname: %s",
                  varname,
                  private$dataname
                )
              )
            }
          )

          logger::log_trace(
            "DFFilterStates$srv_add_filter_state initialized, dataname: { private$dataname }"
          )
          NULL
        }
      )
    }
  ),

  # private members ----
  private = list(
    varlabels = character(0),
    keys = character(0),
    # @description
    # Get label of specific variable. If variable label is missing, variable name is returned.
    #
    # @para variable (`character`)\cr
    #   name of variable for which label should be returned
    #
    # @return `character`
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
