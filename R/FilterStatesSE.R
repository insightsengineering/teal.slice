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
    #'   should return a `SummarizedExperiment` object.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters.
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
    initialize = function(data, data_reactive, input_dataname, output_dataname, datalabel) {
      if (!requireNamespace("SummarizedExperiment", quietly = TRUE)) {
        stop("Cannot load SummarizedExperiment - please install the package or restart your session.")
      }
      super$initialize(data, data_reactive, input_dataname, output_dataname, datalabel)
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
            x_reactive = reactive(SummarizedExperiment::rowData(data_reactive())[[varname]]),
            varname = as.name(varname),
            input_dataname = private$input_dataname
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
            x_reactive = reactive(SummarizedExperiment::colData(data_reactive())[[varname]]),
            varname = as.name(varname),
            input_dataname = private$input_dataname
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
        "dataname: { deparse1(private$input_dataname) }"
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
          deparse1(private$input_dataname)
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
            "{ deparse1(private$input_dataname) } therefore no changes are applied."
          ))
          logger::log_warn(
            paste(
              "Variable:", state_id, "is not present in the actual active subset filters of dataset:",
              "{ deparse1(private$input_dataname) } therefore no changes are applied."
            )
          )
        } else {
          self$state_list_remove(state_list_index = "subset", state_id = varname)
          logger::log_trace(
            sprintf(
              "%s$remove_filter_state for subset variable %s done, dataname: %s",
              class(self)[1],
              varname,
              deparse1(private$input_dataname)
            )
          )
        }
      }

      for (varname in state_id$select) {
        if (!all(unlist(state_id$select) %in% names(self$state_list_get("select")))) {
          warning(paste(
            "Variable:", state_id, "is not present in the actual active select filters of dataset:",
            "{ deparse1(private$input_dataname) } therefore no changes are applied."
          ))
          logger::log_warn(
            paste(
              "Variable:", state_id, "is not present in the actual active select filters of dataset:",
              "{ deparse1(private$input_dataname) } therefore no changes are applied."
            )
          )
        } else {
          self$state_list_remove(state_list_index = "select", state_id = varname)
          sprintf(
            "%s$remove_filter_state for select variable %s done, dataname: %s",
            class(self)[1],
            varname,
            deparse1(private$input_dataname)
          )
        }
      }
    }
  )
)
