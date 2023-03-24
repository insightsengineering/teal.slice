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
#' By design, any dataname set through `set_dataset` cannot be removed because
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
#'     mtcars = list(dataset = mtcars, metadata = list(type = "training"))
#'   )
#' )
#'
#' # get datanames
#' datasets$datanames()
#'
#' df <- datasets$get_data("iris", filtered = FALSE)
#' print(df)
#'
#' datasets$get_metadata("mtcars")
#'
#' datasets$set_filter_state(
#'   list(iris = list(Species = list(selected = "virginica")))
#' )
#' isolate(datasets$get_call("iris"))
#'
#' datasets$set_filter_state(
#'   list(mtcars = list(mpg = list(selected = c(15, 20))))
#' )
#'
#' isolate(datasets$get_filter_state())
#' isolate(datasets$get_filter_overview("iris"))
#' isolate(datasets$get_filter_overview("mtcars"))
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
    #'   Names of the list will serve as dataname.
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

      invisible(self)
    },

    #' @description
    #' Gets datanames
    #'
    #' The datanames are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of datanames
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

    #' @description
    #' Get names of datasets available for filtering.
    #' Returned datanames depending on the relationship type.
    #' If input `dataname` has parent, then parent dataname will be also
    #' returned in the output vector.
    #'
    #' @param dataname (`character`) names of the dataset. Default `"all"`
    #'   returns all datanames set in `FilteredData`
    #'
    #' @return (`character`) of dataset names
    get_filterable_datanames = function(dataname = "all") {
      if (identical(dataname, "all")) {
        dataname <- self$datanames()
      }
      checkmate::assert_subset(dataname, self$datanames())

      parents <- character(0)
      for (i in dataname) {
        while (length(i) > 0) {
          parent_i <- self$get_join_keys()$get_parent(i)
          parents <- c(parent_i, parents)
          i <- parent_i
        }
      }

      return(unique(c(parents, dataname)))
    },

    #' @description
    #' Set the variable names of a given dataset for the filtering.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param varnames (`character` or `NULL`)
    #'   variables which users can choose to filter the data;
    #'
    #' @return this `FilteredData` object invisibly
    #'
    set_filterable_varnames = function(dataname, varnames) {
      checkmate::assert_subset(dataname, self$datanames())
      if (!is.null(varnames)) {
        private$get_filtered_dataset(dataname)$set_filterable_varnames(varnames)
      }
      invisible(self)
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
        self$get_filterable_datanames(datanames),
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
    #' Gets labels of variables in the data.
    #'
    #' Variables are the column names of the data.
    #' Either, all labels must have been provided for all variables
    #' in `set_data` or `NULL`.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param variables (`character`) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #'
    #' @return (`character` or `NULL`) variable labels, `NULL` if `column_labels`
    #'   attribute does not exist for the data
    #'
    get_varlabels = function(dataname, variables = NULL) {
      stop("Please extract varlabels directly from the data.")
    },

    #' @description
    #' Gets variable names.
    #'
    #' @param dataname (`character`) the name of the dataset
    #'
    #' @return (`character` vector) of variable names
    #'
    get_varnames = function(dataname) {
      stop("Please extract varnames directly from the data")
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
      # the UI also uses datanames in ids, so no whitespaces allowed
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
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Gets all active filters in the form of a nested list.
    #' The output list is a compatible input to `self$set_filter_state`.
    #' The attribute `formatted` renders the output of `self$get_formatted_filter_state`,
    #' which is a character formatting of the filter state.
    #'
    #' @return `named list` with elements corresponding to `FilteredDataset` objects
    #' with active filters. In addition, the `formatted` attribute holds
    #' the character format of the active filter states.
    #'
    get_filter_state = function() {
      states <- lapply(private$get_filtered_dataset(), function(x) x$get_filter_state())
      filtered_states <- Filter(function(x) length(x) > 0, states)
      structure(filtered_states, formatted = self$get_formatted_filter_state())
    },

    #' @description
    #' Returns the filter state formatted for printing to an `IO` device.
    #'
    #' @return `character` the pre-formatted filter state
    #'
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #' datasets <- teal.slice:::FilteredData$new(
    #'   list(iris = list(dataset = iris),
    #'        mae = list(dataset = miniACC)
    #'   )
    #' )
    #' fs <- list(
    #'   iris = list(
    #'     Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    #'     Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    #'   ),
    #'   mae = list(
    #'     subjects = list(
    #'       years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
    #'       vital_status = list(selected = "1", keep_na = FALSE),
    #'       gender = list(selected = "female", keep_na = TRUE)
    #'     ),
    #'     RPPAArray = list(
    #'       subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
    #'     )
    #'   )
    #' )
    #' isolate(datasets$set_filter_state(state = fs))
    #' cat(shiny::isolate(datasets$get_formatted_filter_state()))
    #'
    get_formatted_filter_state = function() {
      out <-
        unlist(sapply(
          private$get_filtered_dataset(),
          function(filtered_dataset) {
            filtered_dataset$get_formatted_filter_state()
          }
        ))
      paste(out, collapse = "\n")
    },

    #' @description
    #' Sets active filter states.
    #'
    #' @param state (`named list`)\cr
    #'  nested list of filter selections applied to datasets
    #'
    #' @return `NULL`
    #'
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #'
    #' datasets <- teal.slice:::FilteredData$new(
    #'   list(iris = list(dataset = iris),
    #'        mae = list(dataset = miniACC)
    #'   )
    #' )
    #' fs <- list(
    #'   iris = list(
    #'     Sepal.Length = list(selected = c(5.1, 6.4), keep_na = TRUE, keep_inf = FALSE),
    #'     Species = list(selected = c("setosa", "versicolor"), keep_na = FALSE)
    #'   ),
    #'   mae = list(
    #'     subjects = list(
    #'       years_to_birth = list(selected = c(30, 50), keep_na = TRUE, keep_inf = FALSE),
    #'       vital_status = list(selected = "1", keep_na = FALSE),
    #'       gender = list(selected = "female", keep_na = TRUE)
    #'     ),
    #'     RPPAArray = list(
    #'       subset = list(ARRAY_TYPE = list(selected = "", keep_na = TRUE))
    #'     )
    #'   )
    #' )
    #' shiny::isolate(datasets$set_filter_state(state = fs))
    #' shiny::isolate(datasets$get_filter_state())
    #'
    set_filter_state = function(state) {
      checkmate::assert_subset(names(state), self$datanames())
      lapply(names(state), function(dataname) {
        logger::log_trace(
          "FilteredData$set_filter_state initializing, dataname: { paste(names(state), collapse = ' ') }"
        )

        fdataset <- private$get_filtered_dataset(dataname = dataname)
        dataset_state <- state[[dataname]]
        fdataset$set_filter_state(state = dataset_state)
      })
      logger::log_trace(
        "FilteredData$set_filter_state initialized, dataname: { paste(names(state), collapse = ' ') }"
      )

      invisible(NULL)
    },

    #' @description
    #' Removes one or more `FilterState` of a `FilteredDataset` in a `FilteredData` object.
    #'
    #' @param state (`named list`)\cr
    #'  nested list of filter selections applied to datasets
    #'
    #' @return `NULL` invisibly
    #'
    remove_filter_state = function(state) {
      checkmate::assert_subset(names(state), self$datanames())

      logger::log_trace(
        "FilteredData$remove_filter_state called, dataname: { paste(names(state), collapse = ' ') }"
      )

      for (dataname in names(state)) {
        fdataset <- private$get_filtered_dataset(dataname = dataname)
        fdataset$remove_filter_state(state_id = state[[dataname]])
      }

      logger::log_trace(
        "FilteredData$remove_filter_state done, dataname: { paste(names(state), collapse = ' ') }"
      )

      invisible(NULL)
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
    },

    #' @description
    #' Remove all `FilterStates` of a `FilteredDataset` or all `FilterStates`
    #' of a `FilteredData` object.
    #'
    #' @param datanames (`character`)\cr
    #'   datanames to remove their `FilterStates` or empty which removes
    #'   all `FilterStates` in the `FilteredData` object
    #'
    #' @return `NULL` invisibly
    #'
    clear_filter_states = function(datanames = self$datanames()) {
      logger::log_trace(
        "FilteredData$clear_filter_states called, datanames: { toString(datanames) }"
      )

      for (dataname in datanames) {
        fdataset <- private$get_filtered_dataset(dataname = dataname)
        fdataset$clear_filter_states()
      }

      logger::log_trace(
        paste(
          "FilteredData$clear_filter_states removed all FilterStates,",
          "datanames: { toString(datanames) }"
        )
      )

      invisible(NULL)
    },

    #' @description
    #' Disable the filter panel by adding `disable` class to `filter_add_vars`
    #' and `filter_panel_active_vars` tags in the User Interface.
    #' In addition, it will store the existing filter states in a private field called `cached_states`
    #' before removing all filter states from the object.
    #'
    filter_panel_disable = function() {
      private$filter_panel_active <- FALSE
      shinyjs::disable("add")
      shinyjs::disable("active")
      private$cached_states <- self$get_filter_state()
      self$clear_filter_states()
      invisible(NULL)
    },

    #' @description enable the filter panel
    #' Enable the filter panel by adding `enable` class to `filter_add_vars`
    #' and `filter_active_vars` tags in the User Interface.
    #' In addition, it will restore the filter states from a private field called `cached_states`.
    #'
    filter_panel_enable = function() {
      private$filter_panel_active <- TRUE
      shinyjs::enable("add")
      shinyjs::enable("active")
      if (length(private$cached_states) && (length(self$get_filter_state()) == 0)) {
        self$set_filter_state(private$cached_states)
      }
      invisible(NULL)
    },

    #' @description
    #' Gets the state of filter panel, if activated.
    #'
    get_filter_panel_active = function() {
      private$filter_panel_active
    },

    #' @description
    #' Gets the id of the filter panel UI.
    get_filter_panel_ui_id = function() {
      private$filter_panel_ui_id
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
        self$ui_add(ns("add"))
      )
    },

    #' Server function for filter panel
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param active_datanames `function / reactive` returning datanames that
    #'   should be shown on the filter panel,
    #'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
    #'   if the function returns `NULL` (as opposed to `character(0)`), the filter
    #'   panel will be hidden
    #' @return `moduleServer` function which returns `NULL`
    srv_filter_panel = function(id, active_datanames = function() "all") {
      checkmate::assert_function(active_datanames)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("FilteredData$srv_filter_panel initializing")

          active_datanames_resolved <- eventReactive(req(active_datanames()), {
            self$get_filterable_datanames(active_datanames())
          })

          self$srv_overview("overview", active_datanames_resolved)
          self$srv_active("active", active_datanames_resolved)
          self$srv_add("add", active_datanames_resolved)

          private$filter_panel_ui_id <- session$ns(NULL)

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
          shinyWidgets::prettySwitch(
            ns("filter_panel_active"),
            label = "",
            status = "success",
            fill = TRUE,
            value = TRUE,
            inline = TRUE,
            width = 30
          ),
          actionLink(
            ns("minimise_filter_active"),
            label = NULL,
            icon = icon("angle-down", lib = "font-awesome"),
            title = "Minimise panel",
            class = "remove pull-right"
          ),
          actionLink(
            ns("remove_all_filters"),
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
    srv_active = function(id, active_datanames = reactive(self$datanames())) {
      checkmate::assert_class(active_datanames, "reactive")
      shiny::moduleServer(id, function(input, output, session) {
        logger::log_trace("FilteredData$srv_active initializing")
        shiny::observeEvent(input$minimise_filter_active, {
          shinyjs::toggle("filter_active_vars_contents")
          shinyjs::toggle("filters_active_count")
          toggle_icon(session$ns("minimise_filter_active"), c("fa-angle-right", "fa-angle-down"))
          toggle_title(session$ns("minimise_filter_active"), c("Restore panel", "Minimise Panel"))
        })

        shiny::observeEvent(private$get_filter_count(), {
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

        observeEvent(
          eventExpr = input$filter_panel_active,
          handlerExpr = {
            if (isTRUE(input$filter_panel_active)) {
              self$filter_panel_enable()
              logger::log_trace("Enable the Filtered Panel with the filter_panel_enable method")
            } else {
              self$filter_panel_disable()
              logger::log_trace("Disable the Filtered Panel with the filter_panel_enable method")
            }
          }, ignoreNULL = TRUE
        )

        observeEvent(input$remove_all_filters, {
          logger::log_trace("FilteredData$srv_filter_panel@1 removing all filters")
          self$clear_filter_states()
          logger::log_trace("FilteredData$srv_filter_panel@1 removed all filters")
        })
        logger::log_trace("FilteredData$srv_active initialized")
        NULL
      })
    },

    #' @description
    #' Server module responsible for displaying dropdowns with variables to add a filter.
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
    #' Server module responsible for displaying dropdowns with variables to add a filter.
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
    #'   returning datanames that should be shown on the filter panel,
    #'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
    #'   if the function returns `NULL` (as opposed to `character(0)`), the filter
    #'   panel will be hidden.
    #' @return `moduleServer` function which returns `NULL`
    srv_overview = function(id, active_datanames = reactive(self$datanames())) {
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
            datanames <- if (identical(active_datanames(), "all")) {
              self$datanames()
            } else {
              active_datanames()
            }

            if (length(datanames) == 0) {
              return(NULL)
            }

            datasets_df <- self$get_filter_overview(datanames = datanames)


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

          shiny::outputOptions(output, "table", suspendWhenHidden = FALSE)
          logger::log_trace("FilteredData$srv_filter_overview initialized")
          NULL
        }
      )
    }
  ),

  ## __Private Methods ====
  private = list(
    # selectively hide / show to only show `active_datanames` out of all datanames

    # private attributes ----
    filtered_datasets = list(),

    # activate/deactivate filter panel
    filter_panel_active = TRUE,

    # filter panel ui id
    filter_panel_ui_id = character(0),

    # whether the datasets had a reproducibility check
    .check = FALSE,

    # preprocessing code used to generate the unfiltered datasets as a string
    code = NULL,

    # keys used for joining/filtering data a JoinKeys object (see teal.data)
    join_keys = NULL,

    # reactive i.e. filtered data
    reactive_data = list(),
    cached_states = NULL,

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
      sum(vapply(self$datanames(), function(dataname) {
        private$get_filtered_dataset(dataname)$get_filter_count()
      }, numeric(1L)))
    }
  )
)

# Wrapper functions for `FilteredData` class ----


#' Gets filter expression for multiple datanames taking into account its order.
#'
#' @description `r lifecycle::badge("stable")`
#' To be used in show R code button.
#'
#' @param datasets (`FilteredData`)
#' @param datanames (`character`) vector of dataset names
#'
#' @export
#'
#' @return (`expression`)
get_filter_expr <- function(datasets, datanames = datasets$datanames()) {
  checkmate::assert_character(datanames, min.len = 1, any.missing = FALSE)
  stopifnot(
    is(datasets, "FilteredData"),
    all(datanames %in% datasets$datanames())
  )

  paste(
    unlist(lapply(
      datanames,
      function(dataname) {
        datasets$get_call(dataname)
      }
    )),
    collapse = "\n"
  )
}
