#' @name FilteredData
#' @docType class
#'
#' @title Class to encapsulate filtered datasets
#'
#' @details
#' The main purpose of this class is to provide a collection of reactive datasets,
#' each dataset having a filter state that determines how it is filtered.
#'
#' For each dataset, `get_filter_expr` returns the call to filter the dataset according
#' to the filter state. The data itself can be obtained through `get_data`.
#'
#' The datasets are filtered lazily, i.e. only when requested / needed in a Shiny app.
#'
#' By design, any `dataname` set through `set_dataset` cannot be removed because
#' other code may already depend on it. As a workaround, the underlying
#' data can be set to `NULL`.
#'
#' The class currently supports variables of the following types within datasets:
#' - `choices`: variable of type `factor`, e.g. `ADSL$COUNTRY`, `iris$Species`
#'      zero or more options can be selected, when the variable is a factor
#' - `logical`: variable of type `logical`, e.g. `ADSL$TRT_FLAG`
#'      exactly one option must be selected, `TRUE` or `FALSE`
#' - `ranges`: variable of type `numeric`, e.g. `ADSL$AGE`, `iris$Sepal.Length`
#'      numerical range, a range within this range can be selected
#' - `dates`: variable of type `Date`, `POSIXlt`
#' Other variables cannot be used for filtering the data in this class.
#'
#' Common arguments are:
#' 1. `filtered`: whether to return a filtered result or not
#' 2. `dataname`: the name of one of the datasets in this `FilteredData`
#' 3. `varname`: one of the columns in a dataset
#'
#' @keywords internal
#'
#' @examples
#' library(shiny)
#' datasets <- teal.slice:::FilteredData$new()
#' datasets$set_dataset("iris", iris)
#' datasets$set_dataset("mtcars", mtcars)
#'
#' # get datanames
#' isolate(datasets$datanames())
#'
#' datasets$set_filter_state(
#'   teal_slices(teal_slice(dataname = "iris", varname = "Species", selected = "virginica"))
#' )
#' isolate(datasets$get_call("iris"))
#'
#' datasets$set_filter_state(
#'   teal_slices(teal_slice(dataname = "mtcars", varname = "mpg", selected = c(15, 20)))
#' )
#'
#' isolate(datasets$get_filter_state())
#' isolate(datasets$get_call("iris"))
#' isolate(datasets$get_call("mtcars"))
#'
FilteredData <- R6::R6Class( # nolint
  "FilteredData",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initialize a `FilteredData` object
    #' @param data_objects (`list`)
    #'   Named list of data objects.
    #'   Names of the list will serve as `dataname`.
    #' @param join_keys (`join_keys` or NULL) see [`teal.data::join_keys()`].
    #'
    initialize = function() {
      private$datasets <- list()
      private$datasets_filtered <- list()
      private$state_list <- reactiveVal(NULL)
      self$set_available_teal_slices(x = reactive(NULL))
      invisible(self)
    },
    # teal_slices API ------
    #' @description
    #' Gets reactive values from active `FilterState` objects.
    #'
    #' Get active filter state from `FilterState` objects stored in `state_list`(s).
    #' The output is a list compatible with input to `self$set_filter_state`.
    #'
    #' @return `list` containing `list` per `FilterState` in the `state_list`
    #'
    get_filter_state = function() {
      slices <- unname(lapply(self$state_list_get(), function(x) x$get_state()))
      slices <- do.call(teal_slices, slices)
      attributes(slices) <- modifyList(attributes(slices), private$teal_slice_attrs)
      slices
    },

    #' @description
    #' Sets active filter states.
    #'
    #' @param state (`teal_slices`) object
    #'
    #' @return `NULL` invisibly
    #'
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #'
    #' datasets <- teal.slice:::FilteredData$new()
    #' datasets$set_dataset("iris", iris)
    #' datasets$set_dataset("mae", miniACC)
    #'
    #' fs <-
    #'   teal_slices(
    #'     teal_slice(dataname = "iris", varname = "Sepal.Length", selected = c(5.1, 6.4),
    #'                keep_na = TRUE, keep_inf = FALSE),
    #'     teal_slice(dataname = "iris", varname = "Species", selected = c("setosa", "versicolor"),
    #'                keep_na = FALSE),
    #'     teal_slice(dataname = "mae", varname = "years_to_birth", selected = c(30, 50),
    #'                keep_na = TRUE, keep_inf = FALSE),
    #'     teal_slice(dataname = "mae", varname = "vital_status", selected = "1", keep_na = FALSE),
    #'     teal_slice(dataname = "mae", varname = "gender", selected = "female", keep_na = TRUE),
    #'     teal_slice(dataname = "mae", varname = "ARRAY_TYPE",
    #'                selected = "", keep_na = TRUE, datalabel = "RPPAArray", arg = "subset")
    #'   )
    #' datasets$set_filter_state(state = fs)
    #' shiny::isolate(datasets$get_filter_state())
    #'
    set_filter_state = function(state) {
      shiny::isolate({
        # todo: asserts!
        checkmate::assert_class(state, "teal_slices")
        teal_slice_attrs <- attributes(state)
        private$teal_slice_attrs <- modifyList(
          private$teal_slice_attrs,
          teal_slice_attrs[!names(teal_slice_attrs) %in% c("names", "class")]
        )

        if (length(state) == 0L) {
          return(invisible(NULL))
        }

        slices_hashed <- vapply(state, `[[`, character(1L), "id")
        if (any(duplicated(slices_hashed))) {
          stop(
            "Some of the teal_slice objects refer to the same filter. ",
            "Please specify different 'id' when calling teal_slice"
          )
        }

        state_list <- self$state_list_get()
        lapply(state, function(slice) {
          state_id <- slice$id

          if (state_id %in% names(state_list)) {
            # Modify existing filter states.
            state_list[[state_id]]$set_state(slice)
          } else {
            if (inherits(slice, "teal_slice_expr")) {
              # create a new FilterStateExpr
              fstate <- init_filter_state_expr(slice)
            } else {
              dataname <- slice$dataname
              data <- self$get_data(dataname, filtered = FALSE)

              reactive_env <- reactive({
                env <- new.env()
                env[[dataname]] <- self$get_data(dataname, filtered = FALSE)
                for (ancestor in get_ancestors(self$get_join_keys(), dataname)) {
                  env[[ancestor]] <- self$get_data(ancestor, filtered = TRUE)
                }
                env
              })

              reactive_env_addrs <- reactive(
                vapply(reactive_env(), digest::digest, character(1))
              )

              # create a new FilterState
              fstate <- init_filter_state(
                x = get_slice_variable(data, slice),
                x_reactive = if (identical(private$count_type, "all")) {
                  bindCache(
                    reactive({
                      logger::log_trace(
                        "FilteredData$set_filter_state recalculating x_reactive for visible filter counts: { state_id }"
                      )
                      env <- reactive_env()
                      filter_call <- self$get_call(dataname, sid = state_id)
                      eval_expr_with_msg(filter_call, env)
                      get_slice_variable(env[[dataname]], slice)
                    }),
                    # call have to be cached because underneath $get_call uses the whole state_list
                    # so it reacts to change in any filter state (in all datasets). Therefore we bind
                    # reactive on a change of THIS dataset call
                    self$get_call(dataname, sid = state_id),
                    reactive_env_addrs()
                  )
                } else {
                  reactive(NULL)
                },
                slice = slice
              )
            }
            self$state_list_push(x = fstate)
          }
        })
      })

      invisible(NULL)
    },
    #' @description
    #' Removes one or more `FilterState` from a `FilteredData` object.
    #'
    #' @param state (`teal_slices`)\cr
    #'   specifying `FilterState` objects to remove;
    #'   `teal_slice`s may contain only `dataname` and `varname`, other elements are ignored
    #'
    #' @return `NULL` invisibly
    #'
    remove_filter_state = function(state) {
      shiny::isolate({
        checkmate::assert_class(state, "teal_slices")
        state_ids <- vapply(state, `[[`, character(1), "id")
        logger::log_trace("FilteredData$remove_filter_state removing filters, state_id: { toString(state_ids) }")
        self$state_list_remove(state_ids)
      })
      invisible(NULL)
    },

    #' @description
    #' Remove all `FilterState`(s) from the `FilteredData` object.
    #'
    #' @param datanames (`character`)\cr
    #'  `datanames` to remove their `FilterState`(s). By default all datasets are cleared.
    #' @param force (`logical(1)`)\cr
    #'   to force removal of anchored filters.
    #'
    #' @return `NULL` invisibly
    #'
    clear_filter_states = function(datanames = self$datanames(), force = FALSE) {
      isolate({
        state_list <- Filter(
          function(x) x$get_state()$dataname %in% datanames && (force || !isTRUE(x$get_state()$locked)),
          self$state_list_get()
        )

        if (length(state_list)) {
          state_ids <- vapply(state_list, function(x) x$get_state()$id, character(1))
          self$state_list_remove(state_ids, force)
        }
      })
    },

    #' @description Get list of filter states available for this object.
    #'
    #' All `teal_slice` objects that have been created since the beginning of the app session
    #' are stored in one `teal_slices` object. This returns a subset of that `teal_slices`,
    #' describing filter states that can be set for this object.
    #' @return `reactive` that returns `teal_slices`
    get_available_teal_slices = function() {
      private$available_teal_slices
    },

    #' @description Set list of external filter states available for activation.
    #'
    #' Unlike adding new filter from the column, these filters can come with some prespecified settings.
    #' `teal_slices` are wrapped in a `reactive` so they can be updated from elsewhere in the app.
    #' Filters passed in `x` are limited to those that can be set for this `FilteredData`,
    #' i.e. they have the correct `dataname` and `varname` (waived `teal_slice_fixed` as they do not have `varname`).
    #' List is accessible in `ui/srv_active` through `ui/srv_available_filters`.
    #' @param x (`reactive`)\cr
    #'  should return `teal_slices`
    #' @return invisible `NULL`
    set_available_teal_slices = function(x) {
      checkmate::assert_class(x, "reactive")
      private$available_teal_slices <- reactive({
        # Available filters should be limited to the ones relevant for this FilteredData.
        current_state <- isolate(self$get_filter_state())
        allowed <- attr(current_state, "include_varnames")
        forbidden <- attr(current_state, "exclude_varnames")
        foo <- function(slice) {
          if (slice$dataname %in% self$datanames()) {
            if (slice$fixed) {
              TRUE
            } else {
              isTRUE(slice$varname %in% allowed[[slice$dataname]]) ||
                isFALSE(slice$varname %in% forbidden[[slice$dataname]])
            }
          } else {
            FALSE
          }
        }
        Filter(foo, x())
      })
      invisible(NULL)
    },

    # filtering datasets ------
    #' @description
    #' Gets `datanames`
    #'
    #' The `datanames` are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of `datanames`
    datanames = function() {
      datanames <- names(private$datasets)
      sorted <- unlist(topological_sort(teal.data::parents(self$get_join_keys()[datanames])))
      union(sorted, datanames)
    },
    #' @description
    #' Adds a dataset to this `FilteredData`.
    #'
    #' @details
    #' `set_dataset` adds a dataset to this `FilteredData` object and sets reactive components
    #' for datasets' filtering. Data in the filter panel is filtered when:
    #' - filter call changes. Filter call is a product of all active `FilterState`(s).
    #' - When ancestors filtered data changes.
    #'
    #' @param dataset (`data.frame`, `MultiAssayExperiment`)\cr
    #'   data to be filtered.
    #'
    #' @param dataname (`string`)\cr
    #'   the name of the `dataset` to be added to this object
    #'
    #' @return (`self`) invisibly this `FilteredData`
    #'
    set_dataset = function(dataname, dataset) {
      shiny::isolate({
        # the UI also uses `datanames` in ids, so no whitespaces allowed
        check_simple_name(dataname)
        reactive_env <- reactive({
          data <- list()
          data[[dataname]] <- self$get_data(dataname, filtered = FALSE)
          for (ancestor in get_ancestors(self$get_join_keys(), dataname)) {
            data[[ancestor]] <- self$get_data(ancestor, filtered = TRUE)
          }
          data
        })

        reactive_env_addrs <- reactive(
          vapply(reactive_env(), digest::digest, character(1))
        )

        if (!dataname %in% self$datanames()) {
          # prepare slot for reactive dataset when initialized for the first time
          private$datasets[[dataname]] <- reactiveVal(dataset)
          private$datasets_filtered[[dataname]] <- bindCache(
            reactive({
              logger::log_trace("FilteredData$set_dataset@1 filtering dataset: { dataname }")
              env <- list2env(reactive_env())
              filter_call <- self$get_call(dataname)
              eval_expr_with_msg(filter_call, env)
              env[[dataname]]
            }),
            # call have to be cached because underneath $get_call uses the whole state_list
            # so it reacts to change in any filter state (in all datasets). Therefore we bind
            # reactive on a change of THIS dataset call
            self$get_call(dataname),
            reactive_env_addrs()
          )
        } else {
          isolate(private$datasets[[dataname]](dataset))
        }

        invisible(self)
      })
    },

    #' @description
    #' Gets filtered or unfiltered dataset.
    #'
    #' For `filtered = FALSE`, the original data set with
    #' `set_data` is returned including all attributes.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param filtered (`logical`) whether to return a filtered or unfiltered dataset
    #'
    get_data = function(dataname, filtered = TRUE) {
      if (filtered) {
        private$datasets_filtered[[dataname]]()
      } else {
        private$datasets[[dataname]]()
      }
    },

    #' @description
    #' Gets a `call` to filter the dataset according to the filter state.
    #'
    #' It returns a `call` to filter the dataset only, assuming the
    #' other (filtered) datasets it depends on are available.
    #'
    #' Together with `self$datanames()` which returns the datasets in the correct
    #' evaluation order, this generates the whole filter code, see the function
    #' `FilteredData$get_filter_code`.
    #'
    #' For the return type, note that `rlang::is_expression` returns `TRUE` on the
    #' return type, both for base R expressions and calls (single expression,
    #' capturing a function call).
    #'
    #' The filtered dataset has the name given by `self$filtered_dataname(dataname)`
    #'
    #' This can be used for the `Show R Code` generation.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param sid (`character(1)`) id of the filter state to exclude from the call
    #' @return (`call` or `list` of calls) to filter dataset calls
    #'
    get_call = function(dataname, sid = character(0)) {
      states_list <- Filter(
        function(x) {
          x$get_state()$dataname == dataname && !identical(x$get_state()$id, sid)
        },
        self$state_list_get() # this triggers reactive filtering for all datasets
      )
      data <- isolate(self$get_data(dataname, filtered = FALSE))
      filter_call <- get_filter_call(data, states_list)
      merge_call <- get_merge_call(self$get_join_keys()[dataname, ])
      c(filter_call, merge_call)
    },

    # join keys ------
    #' @description
    #' Set the `join_keys`.
    #'
    #' @param join_keys (`join_keys`) join_key (converted to a nested list)
    #'
    #' @return (`self`) invisibly this `FilteredData`
    #'
    set_join_keys = function(join_keys) {
      private$join_keys <- join_keys
    },

    #' @description
    #' Get join keys between two datasets.
    #'
    #' @return (`join_keys`)
    #'
    get_join_keys = function() {
      private$join_keys
    },

    #' @description
    #' Get keys for the dataset.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #'
    #' @return (`character`) keys of dataset
    #'
    get_keys = function(dataname) {
      keys <- self$get_join_keys()[dataname, dataname]
      if (length(keys) == 0) {
        character(0)
      } else {
        keys
      }
    },

    # state list ------
    #' @description
    #' Returns a list of currently active `FilterState` objects.
    #'
    #' @return `list` of `FilterState` objects
    #'
    state_list_get = function() {
      private$state_list()
    },

    #' @description
    #' Adds a new `FilterState` object to this `FilterStates`.\cr
    #' Raises error if the length of `x` does not match the length of `state_id`.
    #'
    #' @param x (`FilterState`)\cr
    #'   object to be added to filter state list
    #'
    #' @return NULL
    #'
    state_list_push = function(x) {
      shiny::isolate({
        logger::log_trace("FilteredData pushing into state_list, state_id: { x$get_state()$id }")
        checkmate::assert_multi_class(x, c("FilterState", "FilterStateExpr"))
        state <- stats::setNames(list(x), x$get_state()$id)
        new_state_list <- c(self$state_list_get(), state)
        private$state_list(new_state_list)
      })
      invisible(NULL)
    },

    #' @description
    #' Removes a single filter state with all associated shiny elements:\cr
    #' * specified `FilterState` from `private$state_list`
    #' * UI card created for this filter
    #' * observers tracking the selection and remove button
    #'
    #' @param state_id (`character`)\cr
    #'   names of element in a filter state (which is a `reactiveVal` containing a list)
    #' @param force (`logical(1)`)\cr
    #'   include locked filter states
    #'
    #' @return NULL
    #'
    state_list_remove = function(state_id, force = FALSE) {
      shiny::isolate({
        checkmate::assert_character(state_id)
        logger::log_trace("FilteredData removing a filter, state_id: { toString(state_id) }")

        current_state_ids <- vapply(self$state_list_get(), function(x) x$get_state()$id, character(1))
        to_remove <- state_id %in% current_state_ids
        if (any(to_remove)) {
          new_state_list <- Filter(
            function(state) {
              if (state$get_state()$id %in% state_id) {
                if (state$get_state()$anchored && !force) {
                  return(TRUE)
                } else {
                  state$destroy_observers()
                  FALSE
                }
              } else {
                TRUE
              }
            },
            self$state_list_get()
          )
          private$state_list(new_state_list)
        } else {
          warning(sprintf("\"%s\" not found in state list", state_id))
        }
      })

      invisible(NULL)
    }
  ),
  ## __Private Methods ====
  private = list(
    available_teal_slices = NULL, # reactive
    datasets = NULL, # list of reactiveVal
    datasets_filtered = NULL, # list of reactiveVal
    join_keys = teal.data::join_keys(),
    teal_slice_attrs = list(),
    state_list = NULL # reactiveValues
  )
)
