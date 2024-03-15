# DataframeFilteredDataset ------

#' @name DataframeFilteredDataset
#' @docType class
#' @title The `DataframeFilteredDataset` `R6` class
#'
#' @examples
#' # use non-exported function from teal.slice
#' DataframeFilteredDataset <- getFromNamespace("DataframeFilteredDataset", "teal.slice")
#'
#' library(shiny)
#'
#' ds <- DataframeFilteredDataset$new(iris, "iris")
#' ds$set_filter_state(
#'   teal_slices(
#'     teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
#'     teal_slice(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5))
#'   )
#' )
#' isolate(ds$get_filter_state())
#' isolate(ds$get_call())
#'
#' ## set_filter_state
#' dataset <- DataframeFilteredDataset$new(iris, "iris")
#' fs <- teal_slices(
#'   teal_slice(dataname = "iris", varname = "Species", selected = "virginica"),
#'   teal_slice(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5))
#' )
#' dataset$set_filter_state(state = fs)
#' isolate(dataset$get_filter_state())
#'
#' @keywords internal
#'
DataframeFilteredDataset <- R6::R6Class( # nolint
  classname = "DataframeFilteredDataset",
  inherit = FilteredDataset,

  # public fields ----
  public = list(

    #' @description
    #' Initializes this `DataframeFilteredDataset` object.
    #'
    #' @param dataset (`data.frame`)
    #'  single `data.frame` for which filters are rendered.
    #' @param dataname (`character(1)`)
    #'  syntactically valid name given to the dataset.
    #' @param keys (`character`) optional
    #'  vector of primary key column names.
    #' @param parent_name (`character(1)`)
    #'   name of the parent dataset.
    #' @param parent (`reactive`)
    #'   that returns a filtered `data.frame` from other `FilteredDataset` named `parent_name`.
    #'   Passing `parent` results in a `reactive` link that causes re-filtering of this `dataset`
    #'   based on the changes in `parent`.
    #' @param join_keys (`character`)
    #'   vector of names of columns in this dataset to join with `parent` dataset.
    #'   If column names in the parent do not match these, they should be given as the names of this vector.
    #' @param label (`character(1)`)
    #'   label to describe the dataset.
    #'
    #' @return Object of class `DataframeFilteredDataset`, invisibly.
    #'
    initialize = function(dataset,
                          dataname,
                          keys = character(0),
                          parent_name = character(0),
                          parent = NULL,
                          join_keys = character(0),
                          label = character(0)) {
      checkmate::assert_data_frame(dataset)
      super$initialize(dataset, dataname, keys, label)

      # overwrite filtered_data if there is relationship with parent dataset
      if (!is.null(parent)) {
        checkmate::assert_character(parent_name, len = 1)
        checkmate::assert_character(join_keys, min.len = 1)

        private$parent_name <- parent_name
        private$join_keys <- join_keys

        private$data_filtered_fun <- function(sid = "") {
          checkmate::assert_character(sid)
          if (length(sid)) {
            logger::log_trace("filtering data dataname: { dataname }, sid: { sid }")
          } else {
            logger::log_trace("filtering data dataname: { private$dataname }")
          }
          env <- new.env(parent = parent.env(globalenv()))
          env[[dataname]] <- private$dataset
          env[[parent_name]] <- parent()
          filter_call <- self$get_call(sid)
          eval_expr_with_msg(filter_call, env)
          get(x = dataname, envir = env)
        }
      }

      private$add_filter_states(
        filter_states = init_filter_states(
          data = dataset,
          data_reactive = private$data_filtered_fun,
          dataname = dataname,
          keys = self$get_keys()
        ),
        id = "filter"
      )

      # todo: Should we make these defaults? It could be handled by the app developer
      if (!is.null(parent)) {
        fs <- teal_slices(
          exclude_varnames = structure(
            list(intersect(colnames(dataset), colnames(isolate(parent())))),
            names = private$dataname
          )
        )
        self$set_filter_state(fs)
      }

      invisible(self)
    },

    #' @description
    #' Gets the subset expression.
    #'
    #' This function returns subset expressions equivalent to selected items
    #' within each of `filter_states`. Configuration of the expressions is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains single `FilterStates` which contains single `state_list`
    #' and all `FilterState` objects apply to one argument (`...`) in a `dplyr::filter` call.
    #'
    #' @param sid (`character`)
    #'  when specified, the method returns code containing conditions calls of
    #'  `FilterState` objects with `sid` different to that of this `sid` argument.
    #'
    #' @return Either a `list` of length 1 containing a filter `call`, or `NULL`.
    get_call = function(sid = "") {
      logger::log_trace("DataframeFilteredDataset$get_call initializing for dataname: { private$dataname }")
      filter_call <- super$get_call(sid)
      dataname <- private$dataname
      parent_dataname <- private$parent_name

      if (!identical(parent_dataname, character(0))) {
        join_keys <- private$join_keys
        parent_keys <- unname(join_keys)
        dataset_keys <- names(join_keys)

        y_arg <- if (length(parent_keys) == 0L) {
          parent_dataname
        } else {
          sprintf(
            "%s[, c(%s), drop = FALSE]",
            parent_dataname,
            toString(dQuote(parent_keys, q = FALSE))
          )
        }

        more_args <- if (length(parent_keys) == 0 || length(dataset_keys) == 0) {
          list()
        } else if (identical(parent_keys, dataset_keys)) {
          list(by = parent_keys)
        } else {
          list(by = stats::setNames(parent_keys, dataset_keys))
        }

        merge_call <- call(
          "<-",
          as.name(dataname),
          as.call(
            c(
              str2lang("dplyr::inner_join"),
              x = as.name(dataname),
              y = str2lang(y_arg),
              more_args
            )
          )
        )

        filter_call <- c(filter_call, merge_call)
      }
      logger::log_trace("DataframeFilteredDataset$get_call initializing for dataname: { private$dataname }")
      filter_call
    },

    #' @description
    #' Set filter state.
    #'
    #' @param state (`teal_slices`)
    #' @return `NULL`, invisibly.
    #'
    set_filter_state = function(state) {
      isolate({
        logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")
        checkmate::assert_class(state, "teal_slices")
        lapply(state, function(slice) {
          checkmate::assert_true(slice$dataname == private$dataname)
        })
        private$get_filter_states()[[1L]]$set_filter_state(state = state)
        invisible(NULL)
      })
    },

    #' @description
    #' Remove one or more `FilterState` form a `FilteredDataset`.
    #'
    #' @param state (`teal_slices`)
    #'   specifying `FilterState` objects to remove;
    #'   `teal_slice`s may contain only `dataname` and `varname`, other elements are ignored
    #'
    #' @return `NULL`, invisibly.
    #'
    remove_filter_state = function(state) {
      checkmate::assert_class(state, "teal_slices")

      isolate({
        logger::log_trace("{ class(self)[1] }$remove_filter_state removing filter(s), dataname: { private$dataname }")

        varnames <- unique(unlist(lapply(state, "[[", "varname")))
        private$get_filter_states()[[1]]$remove_filter_state(state)

        logger::log_trace("{ class(self)[1] }$remove_filter_state removed filter(s), dataname: { private$dataname }")
      })

      invisible(NULL)
    },

    #' @description
    #' UI module to add filter variable for this dataset.
    #'
    #' @param id (`character(1)`)
    #'   `shiny` module instance id.
    #'
    #' @return `shiny.tag`
    ui_add = function(id) {
      ns <- NS(id)
      tagList(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        private$get_filter_states()[["filter"]]$ui_add(id = ns("filter"))
      )
    },

    #' @description
    #' Creates row for filter overview in the form of \cr
    #' `dataname -- observations (remaining/total)` - data.frame
    #' @return A `data.frame`.
    get_filter_overview = function() {
      logger::log_trace("FilteredDataset$srv_filter_overview initialized")
      # Gets filter overview subjects number and returns a list
      # of the number of subjects of filtered/non-filtered datasets
      subject_keys <- if (length(private$parent_name) > 0) {
        names(private$join_keys)
      } else {
        self$get_keys()
      }
      dataset <- self$get_dataset()
      data_filtered <- self$get_dataset(TRUE)
      if (length(subject_keys) == 0) {
        data.frame(
          dataname = private$dataname,
          obs = nrow(dataset),
          obs_filtered = nrow(data_filtered())
        )
      } else {
        data.frame(
          dataname = private$dataname,
          obs = nrow(dataset),
          obs_filtered = nrow(data_filtered()),
          subjects = nrow(unique(dataset[subject_keys])),
          subjects_filtered = nrow(unique(data_filtered()[subject_keys]))
        )
      }
    }
  ),

  # private fields ----
  private = list(
    parent_name = character(0),
    join_keys = character(0)
  )
)
