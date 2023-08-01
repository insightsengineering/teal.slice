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
#' datasets <- teal.slice:::FilteredData$new(
#'   list(
#'     iris = list(dataset = iris),
#'     mtcars = list(dataset = mtcars)
#'   )
#' )
#'
#' # get datanames
#' datasets$datanames()
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
    #'   should named elements containing `data.frame` or `MultiAssayExperiment`.
    #'   Names of the list will serve as `dataname`.
    #' @param join_keys (`JoinKeys` or NULL) see [`teal.data::join_keys()`].
    #' @param code (`CodeClass` or `NULL`) see [`teal.data::CodeClass`].
    #' @param check (`logical(1)`) whether data has been check against reproducibility.
    #'
    initialize = function(data_objects, join_keys = teal.data::join_keys(), code = NULL, check = FALSE) {
      checkmate::assert_list(data_objects, any.missing = FALSE, min.len = 0, names = "unique")
      # Note the internals of data_objects are checked in set_dataset
      checkmate::assert_class(join_keys, "JoinKeys")
      checkmate::assert_class(code, "CodeClass", null.ok = TRUE)
      checkmate::assert_flag(check)

      self$set_check(check)
      if (!is.null(code)) {
        self$set_code(code)
      }

      self$set_join_keys(join_keys)

      child_parent <- sapply(
        names(data_objects),
        function(i) join_keys$get_parent(i),
        USE.NAMES = TRUE,
        simplify = FALSE
      )
      ordered_datanames <- topological_sort(child_parent)

      for (dataname in ordered_datanames) {
        ds_object <- data_objects[[dataname]]
        validate_dataset_args(ds_object, dataname)
        if (inherits(ds_object, c("data.frame", "MultiAssayExperiment"))) {
          self$set_dataset(
            data = ds_object,
            dataname = dataname
          )
        } else {
          # custom support for TealData object which pass metadata and label also
          # see init_filtered_data.TealData
          self$set_dataset(
            data = ds_object$dataset,
            dataname = dataname,
            metadata = ds_object$metadata,
            label = ds_object$label
          )
        }
      }

      self$set_available_teal_slices(x = reactive(NULL))

      invisible(self)
    },

    #' @description
    #' Gets `datanames`
    #'
    #' The `datanames` are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of `datanames`
    datanames = function() {
      names(private$filtered_datasets)
    },

    #' Gets data label for the dataset
    #'
    #' Useful to display in `Show R Code`.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @return (`character`) keys of dataset
    get_datalabel = function(dataname) {
      private$get_filtered_dataset(dataname)$get_dataset_label()
    },

    #' Set list of external filter states available for activation.
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

    #' Get list of filter states available for this object.
    #'
    #' All `teal_slice` objects that have been created since the beginning of the app session
    #' are stored in one `teal_slices` object. This returns a subset of that `teal_slices`,
    #' describing filter states that can be set for this object.
    #' @return `reactive` that returns `teal_slices`
    get_available_teal_slices = function() {
      private$available_teal_slices
    },

    # datasets methods ----

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
    #'
    #' @return (`call` or `list` of calls) to filter dataset calls
    #'
    get_call = function(dataname) {
      checkmate::assert_subset(dataname, self$datanames())
      private$get_filtered_dataset(dataname)$get_call()
    },

    #' @description
    #' Gets the R preprocessing code string that generates the unfiltered datasets.
    #'
    #' @param dataname (`character(1)`) name(s) of dataset(s)
    #'
    #' @return (`character(1)`) deparsed code
    #'
    get_code = function(dataname = self$datanames()) {
      if (!is.null(private$code)) {
        paste0(private$code$get_code(dataname), collapse = "\n")
      } else {
        paste0("# No pre-processing code provided")
      }
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
      checkmate::assert_subset(dataname, self$datanames())
      checkmate::assert_flag(filtered)
      data <- private$get_filtered_dataset(dataname)$get_dataset(filtered)
      if (filtered) data() else data
    },

    #' @description
    #' Returns whether the datasets in the object has undergone a reproducibility check.
    #'
    #' @return `logical`
    #'
    get_check = function() {
      private$.check
    },

    #' @description
    #' Gets metadata for a given dataset.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #'
    #' @return value of metadata for given data (or `NULL` if it does not exist)
    #'
    get_metadata = function(dataname) {
      checkmate::assert_subset(dataname, self$datanames())
      private$get_filtered_dataset(dataname)$get_metadata()
    },

    #' @description
    #' Get join keys between two datasets.
    #'
    #' @return (`JoinKeys`)
    #'
    get_join_keys = function() {
      return(private$join_keys)
    },

    #' @description
    #' Get filter overview table in form of X (filtered) / Y (non-filtered).
    #'
    #' This is intended to be presented in the application.
    #' The content for each of the data names is defined in `get_filter_overview_info` method.
    #'
    #' @param datanames (`character` vector) names of the dataset
    #'
    #' @return (`matrix`) matrix of observations and subjects of all datasets
    #'
    get_filter_overview = function(datanames) {
      rows <- lapply(
        datanames,
        function(dataname) {
          private$get_filtered_dataset(dataname)$get_filter_overview()
        }
      )
      dplyr::bind_rows(rows)
    },

    #' @description
    #' Get keys for the dataset.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #'
    #' @return (`character`) keys of dataset
    #'
    get_keys = function(dataname) {
      private$get_filtered_dataset(dataname)$get_keys()
    },

    #' @description
    #' Adds a dataset to this `FilteredData`.
    #'
    #' @details
    #' `set_dataset` creates a `FilteredDataset` object which keeps `dataset` for the filtering purpose.
    #' If this data has a parent specified in the `JoinKeys` object stored in `private$join_keys`
    #' then created `FilteredDataset` (child) gets linked with other `FilteredDataset` (parent).
    #' "Child" dataset return filtered data then dependent on the reactive filtered data of the
    #' "parent". See more in documentation of `parent` argument in `FilteredDatasetDefault` constructor.
    #'
    #' @param data (`data.frame`, `MultiAssayExperiment`)\cr
    #'   data to be filtered.
    #'
    #' @param dataname (`string`)\cr
    #'   the name of the `dataset` to be added to this object
    #'
    #' @param metadata (named `list` or `NULL`) \cr
    #'   Field containing metadata about the dataset. Each element of the list
    #'   should be atomic and length one.
    #'
    #' @param label (`character(1)`)\cr
    #'   Label to describe the dataset
    #' @return (`self`) invisibly this `FilteredData`
    #'
    set_dataset = function(data, dataname, metadata, label) {
      logger::log_trace("FilteredData$set_dataset setting dataset, name: { dataname }")
      # to include it nicely in the Show R Code;
      # the UI also uses `datanames` in ids, so no whitespaces allowed
      check_simple_name(dataname)

      join_keys <- self$get_join_keys()
      parent_dataname <- join_keys$get_parent(dataname)
      if (length(parent_dataname) == 0) {
        private$filtered_datasets[[dataname]] <- init_filtered_dataset(
          dataset = data,
          dataname = dataname,
          metadata = metadata,
          label = label,
          keys = self$get_join_keys()$get(dataname, dataname)
        )
      } else {
        private$filtered_datasets[[dataname]] <- init_filtered_dataset(
          dataset = data,
          dataname = dataname,
          keys = join_keys$get(dataname, dataname),
          parent_name = parent_dataname,
          parent = reactive(self$get_data(parent_dataname, filtered = TRUE)),
          join_keys = self$get_join_keys()$get(dataname, parent_dataname),
          label = label,
          metadata = metadata
        )
      }

      invisible(self)
    },

    #' @description
    #' Set the `join_keys`.
    #'
    #' @param join_keys (`JoinKeys`) join_key (converted to a nested list)
    #'
    #' @return (`self`) invisibly this `FilteredData`
    #'
    set_join_keys = function(join_keys) {
      checkmate::assert_class(join_keys, "JoinKeys")
      private$join_keys <- join_keys
      invisible(self)
    },

    #' @description
    #' Sets whether the datasets in the object have undergone a reproducibility check.
    #'
    #' @param check (`logical`) whether datasets have undergone reproducibility check
    #'
    #' @return (`self`)
    #'
    set_check = function(check) {
      checkmate::assert_flag(check)
      private$.check <- check
      invisible(self)
    },

    #' @description
    #' Sets the R preprocessing code for single dataset.
    #'
    #' @param code (`CodeClass`)\cr
    #'   preprocessing code that can be parsed to generate the unfiltered datasets
    #'
    #' @return (`self`)
    #'
    set_code = function(code) {
      checkmate::assert_class(code, "CodeClass")
      logger::log_trace("FilteredData$set_code setting code")
      private$code <- code
      invisible(self)
    },

    # Functions useful for restoring from another dataset ----

    #' @description
    #' Gets states of all active `FilterState` objects.
    #'
    #' @return A `teal_slices` object.
    #'
    get_filter_state = function() {
      states <- unname(lapply(private$filtered_datasets, function(x) x$get_filter_state()))
      slices <- Filter(Negate(is.null), states)
      slices <- do.call(c, slices)
      if (!is.null(slices)) {
        attr(slices, "allow_add") <- private$allow_add
      }
      slices
    },

    #' @description
    #' Returns a formatted string representing this `FilteredData` object.
    #'
    #' @param show_all `logical(1)` passed to `format.teal_slice`
    #' @param trim_lines `logical(1)` passed to `format.teal_slice`
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(show_all = FALSE, trim_lines = TRUE) {
      sprintf(
        "%s:\n%s",
        class(self)[1],
        format(self$get_filter_state(), show_all = show_all, trim_lines = trim_lines)
      )
    },

    #' @description
    #' Prints this `FilteredData` object.
    #'
    #' @param ... additional arguments
    #'
    print = function(...) {
      cat(shiny::isolate(self$format(...)), "\n")
    },

    #' @description
    #' Sets active filter states.
    #'
    #' @param state either a `named list` list of filter selections
    #'              or a `teal_slices` object\cr
    #'              specification by list will be deprecated soon
    #'
    #' @return `NULL` invisibly
    #'
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #'
    #' datasets <- teal.slice:::FilteredData$new(
    #'   list(iris = list(dataset = iris),
    #'        mae = list(dataset = miniACC)
    #'   )
    #' )
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
        logger::log_trace("{ class(self)[1] }$set_filter_state initializing")
        if (!is.teal_slices(state)) {
          warning(
            paste(
              "From FilteredData$set_filter_state:",
              "Specifying filters as lists is obsolete and will be deprecated in the next release.",
              "Please see ?set_filter_state and ?teal_slices for details."
            ),
            call. = FALSE
          )
          state <- list_to_teal_slices(state)
        }

        checkmate::assert_class(state, "teal_slices")
        allow_add <- attr(state, "allow_add")
        if (!is.null(allow_add)) {
          private$allow_add <- allow_add
        }

        lapply(self$datanames(), function(dataname) {
          states <- Filter(function(x) identical(x$dataname, dataname), state)
          private$get_filtered_dataset(dataname)$set_filter_state(states)
        })

        logger::log_trace("{ class(self)[1] }$set_filter_state initialized")
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
        if (!is.teal_slices(state)) {
          warning(
            paste(
              "From FilteredData$remove_filter_state:",
              "Specifying filters as lists is obsolete and will be deprecated in the next release.",
              "Please see ?set_filter_state and ?teal_slices for details."
            ),
            call. = FALSE
          )
          state <- list_to_teal_slices(state)
        }

        checkmate::assert_class(state, "teal_slices")
        datanames <- unique(vapply(state, "[[", character(1L), "dataname"))
        checkmate::assert_subset(datanames, self$datanames())

        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removing filter(s), dataname: { private$dataname }"
        )

        lapply(datanames, function(dataname) {
          slices <- Filter(function(x) identical(x$dataname, dataname), state)
          private$get_filtered_dataset(dataname)$remove_filter_state(slices)
        })

        logger::log_trace(
          "{ class(self)[1] }$remove_filter_state removed filter(s), dataname: { private$dataname }"
        )
      })

      invisible(NULL)
    },

    #' @description
    #' Remove all `FilterStates` of a `FilteredDataset` or all `FilterStates`
    #' of a `FilteredData` object.
    #'
    #' @param datanames (`character`)\cr
    #'   `datanames` to remove their `FilterStates` or empty which removes
    #'   all `FilterStates` in the `FilteredData` object
    #' @param force (`logical(1)`)\cr
    #'   include locked filter states
    #'
    #' @return `NULL` invisibly
    #'
    clear_filter_states = function(datanames = self$datanames(), force = FALSE) {
      logger::log_trace(
        "FilteredData$clear_filter_states called, datanames: { toString(datanames) }"
      )

      for (dataname in datanames) {
        fdataset <- private$get_filtered_dataset(dataname = dataname)
        fdataset$clear_filter_states(force)
      }

      logger::log_trace(
        paste(
          "FilteredData$clear_filter_states removed all non-anchored FilterStates,",
          "datanames: { toString(datanames) }"
        )
      )

      invisible(NULL)
    },


    # shiny modules -----

    #' Module for the right filter panel in the teal app
    #' with a filter overview panel and a filter variable panel.
    #'
    #' This panel contains info about the number of observations left in
    #' the (active) datasets and allows to filter the datasets.
    #'
    #' @param id (`character(1)`)\cr
    #'   module id
    #' @return `shiny.tag`
    ui_filter_panel = function(id) {
      ns <- NS(id)
      div(
        id = ns(NULL), # used for hiding / showing
        include_css_files(pattern = "filter-panel"),
        self$ui_overview(ns("overview")),
        self$ui_active(ns("active")),
        if (private$allow_add) {
          self$ui_add(ns("add"))
        }
      )
    },

    #' Server function for filter panel
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param active_datanames `function / reactive` returning `datanames` that
    #'   should be shown on the filter panel,
    #'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
    #'   if the function returns `NULL` (as opposed to `character(0)`), the filter
    #'   panel will be hidden
    #' @return `moduleServer` function which returns `NULL`
    srv_filter_panel = function(id, active_datanames = self$datanames) {
      checkmate::assert_function(active_datanames)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("FilteredData$srv_filter_panel initializing")

          active_datanames_resolved <- reactive({
            checkmate::assert_subset(active_datanames(), self$datanames())
            active_datanames()
          })

          self$srv_overview("overview", active_datanames_resolved)
          self$srv_active("active", active_datanames_resolved)
          if (private$allow_add) {
            self$srv_add("add", active_datanames_resolved)
          }

          logger::log_trace("FilteredData$srv_filter_panel initialized")
          NULL
        }
      )
    },

    #' @description
    #' Server module responsible for displaying active filters.
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `shiny.tag`
    ui_active = function(id) {
      ns <- NS(id)
      div(
        id = id, # not used, can be used to customize CSS behavior
        class = "well",
        tags$div(
          class = "filter-panel-active-header",
          tags$span("Active Filter Variables", class = "text-primary mb-4"),
          private$ui_available_filters(ns("available_filters")),
          actionLink(
            inputId = ns("minimise_filter_active"),
            label = NULL,
            icon = icon("angle-down", lib = "font-awesome"),
            title = "Minimise panel",
            class = "remove_all pull-right"
          ),
          actionLink(
            inputId = ns("remove_all_filters"),
            label = "",
            icon("circle-xmark", lib = "font-awesome"),
            title = "Remove active filters",
            class = "remove_all pull-right"
          )
        ),
        div(
          id = ns("filter_active_vars_contents"),
          tagList(
            lapply(
              self$datanames(),
              function(dataname) {
                fdataset <- private$get_filtered_dataset(dataname)
                fdataset$ui_active(id = ns(dataname))
              }
            )
          )
        ),
        shinyjs::hidden(
          div(
            id = ns("filters_active_count"),
            textOutput(ns("teal_filters_count"))
          )
        )
      )
    },

    #' @description
    #' Server module responsible for displaying active filters.
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param active_datanames (`reactive`)\cr
    #'   defining subset of `self$datanames()` to be displayed.
    #' @return `moduleServer` returning `NULL`
    srv_active = function(id, active_datanames = self$datanames) {
      checkmate::assert_function(active_datanames)
      shiny::moduleServer(id, function(input, output, session) {
        logger::log_trace("FilteredData$srv_active initializing")

        private$srv_available_filters("available_filters")

        observeEvent(input$minimise_filter_active, {
          shinyjs::toggle("filter_active_vars_contents")
          shinyjs::toggle("filters_active_count")
          toggle_icon(session$ns("minimise_filter_active"), c("fa-angle-right", "fa-angle-down"))
          toggle_title(session$ns("minimise_filter_active"), c("Restore panel", "Minimise Panel"))
        })

        observeEvent(private$get_filter_count(), {
          shinyjs::toggle("remove_all_filters", condition = private$get_filter_count() != 0)
          shinyjs::show("filter_active_vars_contents")
          shinyjs::hide("filters_active_count")
          toggle_icon(session$ns("minimise_filter_active"), c("fa-angle-right", "fa-angle-down"), TRUE)
          toggle_title(session$ns("minimise_filter_active"), c("Restore panel", "Minimise Panel"), TRUE)
        })

        observeEvent(active_datanames(), {
          lapply(self$datanames(), function(dataname) {
            if (dataname %in% active_datanames()) {
              shinyjs::show(dataname)
            } else {
              shinyjs::hide(dataname)
            }
          })
        })

        # should not use for-loop as variables are otherwise only bound by reference
        # and last dataname would be used
        lapply(
          self$datanames(),
          function(dataname) {
            fdataset <- private$get_filtered_dataset(dataname)
            fdataset$srv_active(id = dataname)
          }
        )

        output$teal_filters_count <- shiny::renderText({
          n_filters_active <- private$get_filter_count()
          shiny::req(n_filters_active > 0L)
          sprintf(
            "%s filter%s applied across datasets",
            n_filters_active,
            ifelse(n_filters_active == 1, "", "s")
          )
        })

        observeEvent(input$remove_all_filters, {
          logger::log_trace("FilteredData$srv_filter_panel@1 removing all non-anchored filters")
          self$clear_filter_states()
          logger::log_trace("FilteredData$srv_filter_panel@1 removed all non-anchored filters")
        })
        logger::log_trace("FilteredData$srv_active initialized")
        NULL
      })
    },

    #' @description
    #' Server module responsible for displaying drop-downs with variables to add a filter.
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @return `shiny.tag`
    ui_add = function(id) {
      ns <- NS(id)
      div(
        id = id, # not used, can be used to customize CSS behavior
        class = "well",
        tags$div(
          class = "row",
          tags$div(
            class = "col-sm-9",
            tags$label("Add Filter Variables", class = "text-primary mb-4")
          ),
          tags$div(
            class = "col-sm-3",
            actionLink(
              ns("minimise_filter_add_vars"),
              label = NULL,
              icon = icon("angle-down", lib = "font-awesome"),
              title = "Minimise panel",
              class = "remove pull-right"
            )
          )
        ),
        div(
          id = ns("filter_add_vars_contents"),
          tagList(
            lapply(
              self$datanames(),
              function(dataname) {
                fdataset <- private$get_filtered_dataset(dataname)
                span(id = ns(dataname), fdataset$ui_add(ns(dataname)))
              }
            )
          )
        )
      )
    },

    #' @description
    #' Server module responsible for displaying drop-downs with variables to add a filter.
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param active_datanames (`reactive`)\cr
    #'   defining subset of `self$datanames()` to be displayed.
    #' @return `moduleServer` returning `NULL`
    srv_add = function(id, active_datanames = reactive(self$datanames())) {
      checkmate::assert_class(active_datanames, "reactive")
      moduleServer(id, function(input, output, session) {
        logger::log_trace("FilteredData$srv_add initializing")
        shiny::observeEvent(input$minimise_filter_add_vars, {
          shinyjs::toggle("filter_add_vars_contents")
          toggle_icon(session$ns("minimise_filter_add_vars"), c("fa-angle-right", "fa-angle-down"))
          toggle_title(session$ns("minimise_filter_add_vars"), c("Restore panel", "Minimise Panel"))
        })

        observeEvent(active_datanames(), {
          lapply(self$datanames(), function(dataname) {
            if (dataname %in% active_datanames()) {
              shinyjs::show(dataname)
            } else {
              shinyjs::hide(dataname)
            }
          })
        })

        # should not use for-loop as variables are otherwise only bound by reference
        # and last dataname would be used
        lapply(
          self$datanames(),
          function(dataname) {
            fdataset <- private$get_filtered_dataset(dataname)
            fdataset$srv_add(id = dataname)
          }
        )
        logger::log_trace("FilteredData$srv_filter_panel initialized")
        NULL
      })
    },

    #' Creates the UI for the module showing counts for each dataset
    #' contrasting the filtered to the full unfiltered dataset
    #'
    #' Per dataset, it displays
    #' the number of rows/observations in each dataset,
    #' the number of unique subjects.
    #'
    #' @param id module id
    ui_overview = function(id) {
      ns <- NS(id)
      div(
        id = id, # not used, can be used to customize CSS behavior
        class = "well",
        tags$div(
          class = "row",
          tags$div(
            class = "col-sm-9",
            tags$label("Active Filter Summary", class = "text-primary mb-4")
          ),
          tags$div(
            class = "col-sm-3",
            actionLink(
              ns("minimise_filter_overview"),
              label = NULL,
              icon = icon("angle-down", lib = "font-awesome"),
              title = "Minimise panel",
              class = "remove pull-right"
            )
          )
        ),
        tags$br(),
        div(
          id = ns("filters_overview_contents"),
          div(
            class = "teal_active_summary_filter_panel",
            tableOutput(ns("table"))
          )
        )
      )
    },

    #' Server function to display the number of records in the filtered and unfiltered
    #' data
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param active_datanames (`reactive`)\cr
    #'   returning `datanames` that should be shown on the filter panel,
    #'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
    #'   if the function returns `NULL` (as opposed to `character(0)`), the filter
    #'   panel will be hidden.
    #' @return `moduleServer` function which returns `NULL`
    srv_overview = function(id, active_datanames = self$datanames) {
      checkmate::assert_class(active_datanames, "reactive")
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("FilteredData$srv_filter_overview initializing")

          shiny::observeEvent(input$minimise_filter_overview, {
            shinyjs::toggle("filters_overview_contents")
            toggle_icon(session$ns("minimise_filter_overview"), c("fa-angle-right", "fa-angle-down"))
            toggle_title(session$ns("minimise_filter_overview"), c("Restore panel", "Minimise Panel"))
          })

          output$table <- renderUI({
            logger::log_trace("FilteredData$srv_filter_overview@1 updating counts")
            if (length(active_datanames()) == 0) {
              return(NULL)
            }

            datasets_df <- self$get_filter_overview(datanames = active_datanames())

            if (!is.null(datasets_df$obs)) {
              # some datasets (MAE colData) doesn't return obs column
              datasets_df <- transform(
                datasets_df,
                Obs = ifelse(
                  !is.na(obs),
                  sprintf("%s/%s", obs_filtered, obs),
                  ""
                )
              )
            }


            if (!is.null(datasets_df$subjects)) {
              # some datasets (without keys) doesn't return subjects
              datasets_df <- transform(
                datasets_df,
                Subjects = ifelse(
                  !is.na(subjects),
                  sprintf("%s/%s", subjects_filtered, subjects),
                  ""
                )
              )
            }
            datasets_df <- datasets_df[, colnames(datasets_df) %in% c("dataname", "Obs", "Subjects")]

            body_html <- apply(
              datasets_df,
              1,
              function(x) {
                tags$tr(
                  tagList(
                    lapply(x, tags$td)
                  )
                )
              }
            )

            header_html <- tags$tr(
              tagList(
                lapply(colnames(datasets_df), tags$td)
              )
            )

            table_html <- tags$table(
              class = "table custom-table",
              tags$thead(header_html),
              tags$tbody(body_html)
            )
            logger::log_trace("FilteredData$srv_filter_overview@1 updated counts")
            table_html
          })
          logger::log_trace("FilteredData$srv_filter_overview initialized")
          NULL
        }
      )
    },

    # deprecated - to remove after release --------------------------------------

    #' @description
    #' Method is deprecated. Provide resolved `active_datanames` to `srv_filter_panel`
    #'
    #' @param datanames `character vector` `datanames` to pick
    #'
    #' @return the intersection of `self$datanames()` and `datanames`
    #'
    handle_active_datanames = function(datanames) {
      stop("Deprecated with teal.slice 0.4.0")
    },

    #' @description
    #' Method is deprecated. Please extract column labels directly from the data.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param variables (`character`) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #'
    get_varlabels = function(dataname, variables = NULL) {
      stop("Deprecated with 0.4.0 - please extract column labels directly from the data.")
    },

    #' @description
    #' Method is deprecated, Please extract variable names directly from the data instead
    #'
    #' @param dataname (`character`) the name of the dataset
    #'
    get_varnames = function(dataname) {
      stop("Deprecated with 0.4.0 - please extract varniable names directly from the data")
    },

    #' @description
    #' Method is deprecated, please use `self$datanames()` instead
    #'
    #' @param dataname (`character` vector) names of the dataset
    #'
    get_filterable_datanames = function() {
      stop("Deprecated with 0.4.0 - please use self$datanames() instead")
    },

    #' @description
    #' Method is deprecated, please use `self$get_filter_state()` and retain `attr(, "filterable_varnames")` instead.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #'
    get_filterable_varnames = function(dataname) {
      stop("Deprecated with teal.slice 0.4.0 - see help(teal_slices) and description of include_varnames argument.")
    },

    #' @description
    #' Method is deprecated, please use `self$set_filter_state` and [teal_slices()] with `include_varnames` instead.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param varnames (`character` or `NULL`)
    #'   variables which users can choose to filter the data;
    #'   see `self$get_filterable_varnames` for more details
    #'
    #'
    set_filterable_varnames = function(dataname, varnames) {
      stop("Deprecated with teal.slice 0.4.0 - see help(teal_slices) and description of include_varnames argument.")
    },

    #' @description
    #' Method is deprecated, please use `format.teal_slices` on object returned from `self$get_filter_state()`
    #'
    get_formatted_filter_state = function() {
      stop("Deprecated with teal.slice 0.4.0 - get_filter_state returns teal_slice which has dedicated format method")
    },

    #' @description
    #' Deprecated - please use `clear_filter_states` method.
    #'
    #' @param datanames (`character`)
    #'
    #' @return `NULL` invisibly
    #'
    remove_all_filter_states = function(datanames) {
      warning("FilteredData$remove_all_filter_states is deprecated, please use FilteredData$clear_filter_states.")
      self$clear_filter_states(dataname)
    }
  ),

  ## __Private Members ====
  private = list(
    # selectively hide / show to only show `active_datanames` out of all datanames

    # private attributes ----
    filtered_datasets = list(),

    # activate/deactivate filter panel
    filter_panel_active = TRUE,

    # whether the datasets had a reproducibility check
    .check = FALSE,

    # preprocessing code used to generate the unfiltered datasets as a string
    code = NULL,

    # `reactive` containing teal_slices that can be selected; only active in module-specific mode
    available_teal_slices = NULL,

    # keys used for joining/filtering data a JoinKeys object (see teal.data)
    join_keys = NULL,

    # flag specifying whether the user may add filters
    allow_add = TRUE,

    # private methods ----

    # @description
    # Gets `FilteredDataset` object which contains all information
    # pertaining to the specified dataset.
    #
    # @param dataname (`character(1)`)\cr
    #   name of the dataset
    #
    # @return `FilteredDataset` object or list of `FilteredDataset`s
    #
    get_filtered_dataset = function(dataname = character(0)) {
      if (length(dataname) == 0) {
        private$filtered_datasets
      } else {
        private$filtered_datasets[[dataname]]
      }
    },

    # we implement these functions as checks rather than returning logicals so they can
    # give informative error messages immediately

    # @description
    # Gets the number of active `FilterState` objects in all `FilterStates`
    # in all `FilteredDataset`s in this `FilteredData` object.
    # @return `integer(1)`
    get_filter_count = function() {
      length(self$get_filter_state())
    },

    # @description
    # Activate available filters.
    # Module is composed from plus button and dropdown menu. Menu is shown when
    # the button is clicked. Menu contains available/active filters list
    # passed via `set_available_teal_slice`.
    ui_available_filters = function(id) {
      ns <- NS(id)

      active_slices_id <- shiny::isolate(vapply(self$get_filter_state(), `[[`, character(1), "id"))
      div(
        id = ns("available_menu"),
        shinyWidgets::dropMenu(
          actionLink(
            ns("show"),
            label = NULL,
            icon = icon("plus", lib = "font-awesome"),
            title = "Available filters",
            class = "remove pull-right"
          ),
          div(
            class = "menu-content",
            uiOutput(ns("checkbox"))
          )
        )
      )
    },
    # @description
    # Activate available filters. When a filter is selected or removed,
    # `set_filter_state` or `remove_filter_state` is executed for
    # the appropriate filter state id.
    srv_available_filters = function(id) {
      moduleServer(id, function(input, output, session) {
        slices_available <- self$get_available_teal_slices()
        slices_interactive <- reactive(
          Filter(function(slice) isFALSE(slice$fixed), slices_available())
        )
        slices_fixed <- reactive(
          Filter(function(slice) isTRUE(slice$fixed), slices_available())
        )
        available_slices_id <- reactive(vapply(slices_available(), `[[`, character(1), "id"))
        active_slices_id <- reactive(vapply(self$get_filter_state(), `[[`, character(1), "id"))
        duplicated_slice_references <- reactive({
          # slice refers to a particular column
          slice_reference <- vapply(slices_available(), get_default_slice_id, character(1))
          is_duplicated_reference <- duplicated(slice_reference) | duplicated(slice_reference, fromLast = TRUE)
          is_active <- available_slices_id() %in% active_slices_id()
          is_not_expr <- !vapply(slices_available(), inherits, logical(1), "teal_slice_expr")
          slice_reference[is_duplicated_reference & is_active & is_not_expr]
        })

        checkbox_group_element <- function(name, value, label, checked, disabled = FALSE) {
          tags$div(
            class = "checkbox available-filters",
            tags$label(
              tags$input(
                type = "checkbox",
                name = name,
                value = value,
                checked = checked,
                disabled = if (disabled) "disabled"
              ),
              tags$span(label, disabled = if (disabled) disabled)
            )
          )
        }

        output$checkbox <- renderUI({
          checkbox <- checkboxGroupInput(
            session$ns("available_slices_id"),
            label = NULL,
            choices = NULL,
            selected = NULL
          )
          active_slices_ids <- active_slices_id()
          duplicated_slice_refs <- duplicated_slice_references()

          checkbox_group_slice <- function(slice) {
            # we need to isolate changes in the fields of the slice (teal_slice)
            shiny::isolate({
              checkbox_group_element(
                name = session$ns("available_slices_id"),
                value = slice$id,
                label = slice$id,
                checked = if (slice$id %in% active_slices_ids) "checked",
                disabled = slice$anchored ||
                  get_default_slice_id(slice) %in% duplicated_slice_refs &&
                    !slice$id %in% active_slices_ids
              )
            })
          }

          interactive_choice_mock <- lapply(slices_interactive(), checkbox_group_slice)
          non_interactive_choice_mock <- lapply(slices_fixed(), checkbox_group_slice)

          htmltools::tagInsertChildren(
            checkbox,
            br(),
            tags$strong("Fixed filters"),
            non_interactive_choice_mock,
            tags$strong("Interactive filters"),
            interactive_choice_mock,
            .cssSelector = "div.shiny-options-group",
            after = 0
          )
        })

        observeEvent(input$available_slices_id, ignoreNULL = FALSE, ignoreInit = TRUE, {
          new_slices_id <- setdiff(input$available_slices_id, active_slices_id())
          removed_slices_id <- setdiff(active_slices_id(), input$available_slices_id)
          if (length(new_slices_id)) {
            new_teal_slices <- Filter(
              function(slice) slice$id %in% new_slices_id,
              private$available_teal_slices()
            )
            self$set_filter_state(new_teal_slices)
          }

          if (length(removed_slices_id)) {
            removed_teal_slices <- Filter(
              function(slice) slice$id %in% removed_slices_id,
              self$get_filter_state()
            )
            self$remove_filter_state(removed_teal_slices)
          }
        })

        observeEvent(private$available_teal_slices(), ignoreNULL = FALSE, {
          if (length(private$available_teal_slices())) {
            shinyjs::show("available_menu")
          } else {
            shinyjs::hide("available_menu")
          }
        })
      })
    }
  )
)
