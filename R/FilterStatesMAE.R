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
    #' @param excluded_varnames `named list` containing one character vector
    #'   of names of variables that can be filtered;
    #'   names of the list must match `dataname`
    #' @param count_type `character(1)`\cr
    #'   specifying how observations are tallied
    #' @param keys (`character`)\cr
    #'   key columns names
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = "subjects",
                          varlabels = character(0),
                          excluded_varnames = character(0),
                          count_type = c("all", "none"),
                          keys = character(0)) {
      if (!requireNamespace("MultiAssayExperiment", quietly = TRUE)) {
        stop("Cannot load MultiAssayExperiment - please install the package or restart your session.")
      }
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_class(data, "MultiAssayExperiment")
      super$initialize(data, data_reactive, dataname, datalabel, excluded_varnames, count_type)
      private$keys <- keys
      private$varlabels <- varlabels
      private$filterable_varnames <- setdiff(colnames(SummarizedExperiment::colData(data)), excluded_varnames)
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
          formatted_states <- c(formatted_states, state$format(indent = indent * 2))
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
    #' Returns active `FilterState` objects.
    #'
    #' Gets all filter state information from this dataset.
    #'
    #' @return `teal_slices`
    #'
    get_filter_state = function() {
      slices <- lapply(private$state_list$y(), function(x) x$get_state())
      excluded_varnames <- structure(
        list(setdiff(colnames(SummarizedExperiment::colData(private$data)), private$filterable_varnames)),
        names = private$dataname
      )
      excluded_varnames <- Filter(function(x) !identical(x, character(0)), excluded_varnames)

      do.call(filter_settings, c(slices, list(exclude = excluded_varnames, count_type = private$count_type)))
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`teal_slices`)\cr
    #'    `teal_slice` objects should contain the field `target = "y"`
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      checkmate::assert_class(state, "teal_slices")
      lapply(state, function(x) {
        checkmate::assert_true(x$dataname == private$dataname, .var.name = "dataname matches private$dataname")
      })
      checkmate::assert_true(
        all(vapply(state, function(x) identical(x$target, "y"), logical(1L))),
        .var.name = "FilterStatesMAE$set_filter_state: all slices in state must have target = \"y\""
      )

      logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")

      # Drop teal_slices that refer to excluded variables.
      varnames <- slices_field(state, "varname")
      filterable <- get_supported_filter_varnames(SummarizedExperiment::colData(private$data))
      if (!all(varnames %in% filterable)) {
        excluded_varnames <- toString(dQuote(setdiff(varnames, filterable), q = FALSE))
        state <- slices_which(
          state,
          sprintf("!varname %%in%% c(%s)", excluded_varnames)
        )
        logger::log_warn("filters for columns: { excluded_varnames } excluded from { private$dataname }")
      }

      private$set_filter_state_impl(
        state = slices_which(state, "target == \"y\""),
        state_list_index = "y",
        data = SummarizedExperiment::colData(private$data),
        data_reactive = function(sid) SummarizedExperiment::colData(private$data_reactive(sid)),
        extract_type = "list"
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

        private$state_list_remove(state_list_index = "y", state_id = x$varname)

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

          # available choices to display
          avail_column_choices <- reactive({
            active_filter_vars <- slices_field(self$get_filter_state(), "varname")
            choices <- setdiff(get_supported_filter_varnames(data = data), active_filter_vars)
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
              self$set_filter_state(filter_settings(filter_var(private$dataname, varname, target = "y")))

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
    varlabels = character(0),
    keys = character(0)
  )
)
