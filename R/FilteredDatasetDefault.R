# DefaultFilteredDataset ------
#' @title The `DefaultFilteredDataset` R6 class
#' @keywords internal
#' @examples
#' library(shiny)
#' ds <- teal.slice:::DefaultFilteredDataset$new(iris, "iris")
#' ds$set_filter_state(
#'   filter_settings(
#'     filter_var(dataname = "iris", varname = "Species", selected = "virginica"),
#'     filter_var(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5))
#'   )
#' )
#' isolate(ds$get_filter_state())
#' isolate(ds$get_call())
DefaultFilteredDataset <- R6::R6Class( # nolint
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,
  public = list(

    #' @description
    #' Initializes this `DefaultFilteredDataset` object
    #'
    #' @param dataset (`data.frame`)\cr
    #'  single data.frame for which filters are rendered
    #' @param dataname (`character`)\cr
    #'  A given name for the dataset it may not contain spaces
    #' @param keys optional, (`character`)\cr
    #'   Vector with primary keys
    #' @param parent_name (`character(1)`)\cr
    #'   Name of the parent dataset
    #' @param parent (`reactive`)\cr
    #'   object returned by this reactive is a filtered `data.frame` from other `FilteredDataset`
    #'   named `parent_name`. Consequence of passing `parent` is a `reactive` link which causes
    #'   causing re-filtering of this `dataset` based on the changes in `parent`.
    #' @param join_keys (`character`)\cr
    #'   Name of the columns in this dataset to join with `parent`
    #'   dataset. If the column names are different if both datasets
    #'   then the names of the vector define the `parent` columns.
    #'
    #' @param label (`character`)\cr
    #'   Label to describe the dataset
    #' @param metadata (named `list` or `NULL`) \cr
    #'   Field containing metadata about the dataset. Each element of the list
    #'   should be atomic and length one.
    initialize = function(dataset,
                          dataname,
                          keys = character(0),
                          parent_name = character(0),
                          parent = NULL,
                          join_keys = character(0),
                          label = character(0),
                          metadata = NULL) {
      checkmate::assert_data_frame(dataset)
      super$initialize(dataset, dataname, keys, label, metadata)

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
        fs <- filter_settings(
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
    #' Gets the filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains single `FilterStates`
    #' which contains single `state_list` and all `FilterState` objects
    #' applies to one argument (`...`) in `dplyr::filter` call.
    #'
    #' @param sid (`character`)\cr
    #'  when specified then method returns code containing filter conditions of
    #'  `FilterState` objects which `"sid"` attribute is different than this `sid` argument.
    #'
    #' @return filter `call` or `list` of filter calls
    get_call = function(sid = "") {
      logger::log_trace("FilteredDatasetDefault$get_call initializing for dataname: { private$dataname }")
      filter_call <- super$get_call(sid)
      dataname <- private$dataname
      parent_dataname <- private$parent_name

      if (!identical(parent_dataname, character(0))) {
        join_keys <- private$join_keys
        parent_keys <- names(join_keys)
        dataset_keys <- unname(join_keys)

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
      logger::log_trace("FilteredDatasetDefault$get_call initializing for dataname: { private$dataname }")
      filter_call
    },

    #' @description
    #' Set filter state
    #'
    #' @param state (`teal_slice`) object
    #'
    #' @examples
    #' dataset <- teal.slice:::DefaultFilteredDataset$new(iris, "iris")
    #' fs <- filter_settings(
    #'   filter_var(dataname = "iris", varname = "Species", selected = "virginica"),
    #'   filter_var(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5))
    #' )
    #' dataset$set_filter_state(state = fs)
    #' shiny::isolate(dataset$get_filter_state())
    #'
    #' @return `NULL` invisibly
    #'
    set_filter_state = function(state) {
      shiny::isolate({
        logger::log_trace("{ class(self)[1] }$set_filter_state initializing, dataname: { private$dataname }")
        checkmate::assert_class(state, "teal_slices")
        lapply(state, function(x) {
          checkmate::assert_true(
            shiny::isolate(x$dataname) == private$dataname,
            .var.name = "dataname matches private$dataname"
          )
          private$get_filter_states()[[1L]]$set_filter_state(state = state)
          logger::log_trace("{ class(self)[1] }$set_filter_state initialized, dataname: { private$dataname }")
        })
        invisible(NULL)
      })
    },

    #' @description
    #' Remove one or more `FilterState` form a `FilteredDataset`
    #'
    #' @param state (`teal_slices`)\cr
    #'   specifying `FilterState` objects to remove;
    #'   `teal_slice`s may contain only `dataname` and `varname`, other elements are ignored
    #'
    #' @return `NULL` invisibly
    #'
    remove_filter_state = function(state) {
      shiny::isolate({
        logger::log_trace("{ class(self)[1] }$remove_filter_state removing filter(s), dataname: { private$dataname }")
        checkmate::assert_class(state, "teal_slices")

        varnames <- slices_field(state, "varname")
        private$get_filter_states()[[1]]$remove_filter_state(state)

        logger::log_trace("{ class(self)[1] }$remove_filter_state removed filter(s), dataname: { private$dataname }")

        invisible(NULL)
      })
    },

    #' @description
    #' UI module to add filter variable for this dataset
    #'
    #' UI module to add filter variable for this dataset
    #' @param id (`character(1)`)\cr
    #'  identifier of the element - preferably containing dataset name
    #'
    #' @return function - shiny UI module
    ui_add = function(id) {
      ns <- NS(id)
      tagList(
        tags$label("Add", tags$code(self$get_dataname()), "filter"),
        private$get_filter_states()[["filter"]]$ui_add(id = ns("filter"))
      )
    },

    #' @description
    #' Get number of observations based on given keys
    #' The output shows the comparison between `filtered_dataset`
    #' function parameter and the dataset inside self
    #' @return `list` containing character `#filtered/#not_filtered`
    get_filter_overview = function() {
      logger::log_trace("FilteredDataset$srv_filter_overview initialized")
      # Gets filter overview subjects number and returns a list
      # of the number of subjects of filtered/non-filtered datasets
      subject_keys <- if (length(private$parent_name) > 0) {
        private$join_keys
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
  private = list(
    parent_name = character(0),
    join_keys = character(0)
  )
)
