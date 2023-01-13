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
FilteredData <- R6::R6Class( # nolint
  "FilteredData",
  ## __Public Methods ====
  public = list(
    #' @description
    #' Initialize a `FilteredData` object
    #' @param data_objects (`list`) should contain.
    #' - `dataset` data object object supported by [`FilteredDataset`].
    #' - `metatada` (optional) additional metadata attached to the `dataset`.
    #' - `keys` (optional) primary keys.
    #' - `datalabel` (optional) label describing the `dataset`.
    #' - `parent` (optional) which `NULL` is a parent of this one.
    #' @param join_keys (`JoinKeys` or NULL) see [`teal.data::join_keys()`].
    #' @param code (`CodeClass` or `NULL`) see [`teal.data::CodeClass`].
    #' @param check (`logical(1)`) whether data has been check against reproducibility.
    initialize = function(data_objects, join_keys = NULL, code = NULL, check = FALSE) {
      checkmate::assert_list(data_objects, any.missing = FALSE, min.len = 0, names = "unique")
      # Note the internals of data_objects are checked in set_dataset
      checkmate::assert_class(join_keys, "JoinKeys", null.ok = TRUE)
      checkmate::assert_class(code, "CodeClass", null.ok = TRUE)
      checkmate::assert_flag(check)

      self$set_check(check)
      if (!is.null(code)) {
        self$set_code(code)
      }

      for (dataname in names(data_objects)) {
        self$set_dataset(data_objects[[dataname]], dataname)
      }

      if (!is.null(join_keys)) {
        self$set_join_keys(join_keys)
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
      self$get_filtered_dataset(dataname)$get_dataset_label()
    },

    #' @description
    #' Gets dataset names of a given dataname for the filtering.
    #'
    #' @param dataname (`character` vector) names of the dataset
    #' @return (`character` vector) of dataset names
    get_filterable_datanames = function(dataname) {
      dataname
    },

    #' @description
    #' Gets variable names of a given dataname for the filtering.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @return (`character` vector) of variable names
    get_filterable_varnames = function(dataname) {
      self$get_filtered_dataset(dataname)$get_filterable_varnames()
    },

    #' @description
    #' Set the variable names of a given dataset for the filtering
    #' @param dataname (`character(1)`) name of the dataset
    #' @param varnames (`character` or `NULL`) The variables which
    #' users can choose to filter the data
    #' See `self$get_filterable_varnames` for more details
    #' @return invisibly this `FilteredData` object
    set_filterable_varnames = function(dataname, varnames) {
      private$check_data_varname_exists(dataname)
      self$get_filtered_dataset(dataname)$set_filterable_varnames(varnames)
      invisible(self)
    },

    # datasets methods ----
    #' @description
    #' Gets a `call` to filter the dataset according to the filter state
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
    #' @return (`call` or `list` of calls) to filter dataset
    #'  calls
    get_call = function(dataname) {
      private$check_data_varname_exists(dataname)
      self$get_filtered_dataset(dataname)$get_call()
    },

    #' @description
    #' Gets the R preprocessing code string that generates the unfiltered datasets
    #' @param dataname (`character(1)`) name(s) of dataset(s)
    #' @return (`character(1)`) deparsed code
    get_code = function(dataname = self$datanames()) {
      if (!is.null(private$code)) {
        paste0(private$code$get_code(dataname), collapse = "\n")
      } else {
        paste0("# No pre-processing code provided")
      }
    },

    #' @description
    #' Gets `FilteredDataset` object which contains all informations
    #' related to specific dataset.
    #' @param dataname (`character(1)`)\cr
    #'  name of the dataset.
    #' @return `FilteredDataset` object or list of `FilteredDataset`
    get_filtered_dataset = function(dataname = character(0)) {
      if (length(dataname) == 0) {
        private$filtered_datasets
      } else {
        private$filtered_datasets[[dataname]]
      }
    },

    #' @description
    #' Gets filtered or unfiltered dataset
    #'
    #' For `filtered = FALSE`, the original data set with
    #' `set_data` is returned including all attributes.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param filtered (`logical`) whether to return a filtered or unfiltered dataset
    get_data = function(dataname, filtered = TRUE) {
      private$check_data_varname_exists(dataname)
      checkmate::assert_flag(filtered)
      if (filtered) {
        # This try is specific for MAEFilteredDataset due to a bug in
        # S4Vectors causing errors when using the subset function on MAE objects.
        # The fix was introduced in S4Vectors 0.30.1, but is unavailable for R versions < 4.1
        # Link to the issue: https://github.com/insightsengineering/teal/issues/210
        tryCatch(
          private$reactive_data[[dataname]](),
          error = function(error) {
            shiny::validate(paste(
              "Filtering expression returned error(s). Please change filters.\nThe error message was:",
              error$message,
              sep = "\n"
            ))
          }
        )
      } else {
        self$get_filtered_dataset(dataname)$get_dataset()
      }
    },

    #' @description
    #' Returns whether the datasets in the object have had a reproducibility check
    #' @return `logical`
    get_check = function() {
      private$.check
    },

    #' @description
    #' Gets metadata for a given dataset
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @return value of metadata for given data (or `NULL` if it does not exist)
    get_metadata = function(dataname) {
      private$check_data_varname_exists(dataname)
      self$get_filtered_dataset(dataname)$get_metadata()
    },

    #' @description
    #' Get join keys between two datasets.
    #' @return (`JoinKeys`)
    get_join_keys = function() {
      return(private$keys)
    },

    #' @description
    #' Get filter overview table in form of X (filtered) / Y (non-filtered)
    #'
    #' This is intended to be presented in the application.
    #' The content for each of the data names is defined in `get_filter_overview_info` method.
    #'
    #' @param datanames (`character` vector) names of the dataset
    #'
    #' @return (`matrix`) matrix of observations and subjects of all datasets
    get_filter_overview = function(datanames) {
      if (identical(datanames, "all")) {
        datanames <- self$datanames()
      }
      check_in_subset(datanames, self$datanames(), "Some datasets are not available: ")

      rows <- lapply(
        datanames,
        function(dataname) {
          self$get_filtered_dataset(dataname)$get_filter_overview_info(
            filtered_dataset = self$get_data(dataname = dataname, filtered = TRUE)
          )
        }
      )

      do.call(rbind, rows)
    },

    #' Get keys for the dataset
    #' @param dataname (`character(1)`) name of the dataset
    #' @return (`character`) keys of dataset
    get_keys = function(dataname) {
      self$get_filtered_dataset(dataname)$get_keys()
    },
    #' @description
    #' Gets labels of variables in the data
    #'
    #' Variables are the column names of the data.
    #' Either, all labels must have been provided for all variables
    #' in `set_data` or `NULL`.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @param variables (`character`) variables to get labels for;
    #'   if `NULL`, for all variables in data
    #' @return (`character` or `NULL`) variable labels, `NULL` if `column_labels`
    #'   attribute does not exist for the data
    get_varlabels = function(dataname, variables = NULL) {
      self$get_filtered_dataset(dataname)$get_varlabels(variables = variables)
    },

    #' @description
    #' Gets variable names
    #'
    #' @param dataname (`character`) the name of the dataset
    #' @return (`character` vector) of variable names
    get_varnames = function(dataname) {
      self$get_filtered_dataset(dataname)$get_varnames()
    },

    #' When active_datanames is "all", sets them to all datanames
    #' otherwise, it makes sure that it is a subset of the available datanames
    #'
    #' @param datanames `character vector` datanames to pick
    #'
    #' @return the intersection of `self$datanames()` and `datanames`
    handle_active_datanames = function(datanames) {
      logger::log_trace("FilteredData$handle_active_datanames handling { paste(datanames, collapse = \" \") }")
      if (identical(datanames, "all")) {
        datanames <- self$datanames()
      } else {
        for (dataname in datanames) {
          tryCatch(
            check_in_subset(datanames, self$datanames(), "Some datasets are not available: "),
            error = function(e) {
              message(e$message)
            }
          )
        }
      }
      datanames <- self$get_filterable_datanames(datanames)
      intersect(self$datanames(), datanames)
    },

    #' @description
    #' Adds a dataset to this `FilteredData`
    #'
    #' @details
    #' `set_dataset` creates a `FilteredDataset` object which keeps
    #' `dataset` for the filtering purpose.
    #'
    #' @param dataset_args (`list`)\cr
    #'   containing the arguments except (`dataname`)
    #'   needed by `init_filtered_dataset`
    #' @param dataname (`string`)\cr
    #'   the name of the `dataset` to be added to this object
    #' @return (`self`) invisibly this `FilteredData`
    set_dataset = function(dataset_args, dataname) {
      logger::log_trace("FilteredData$set_dataset setting dataset, name; { deparse1(dataname) }")
      validate_dataset_args(dataset_args, dataname)

      dataset <- dataset_args[["dataset"]]
      dataset_args[["dataset"]] <- NULL

      # to include it nicely in the Show R Code; the UI also uses datanames in ids, so no whitespaces allowed
      check_simple_name(dataname)
      private$filtered_datasets[[dataname]] <- do.call(
        what = init_filtered_dataset,
        args = c(list(dataset), dataset_args, list(dataname = dataname))
      )

      private$reactive_data[[dataname]] <- reactive({
        env <- new.env(parent = parent.env(globalenv()))
        env[[dataname]] <- self$get_filtered_dataset(dataname)$get_dataset()
        filter_call <- self$get_call(dataname)
        eval_expr_with_msg(filter_call, env)
        get(x = dataname, envir = env)
      })

      invisible(self)
    },

    #' @description
    #' Set the `join_keys`
    #' @param join_keys (`JoinKeys`) join_key (converted to a nested list)
    #' @return (`self`) invisibly this `FilteredData`
    set_join_keys = function(join_keys) {
      checkmate::assert_class(join_keys, "JoinKeys")
      private$keys <- join_keys
      invisible(self)
    },

    #' @description
    #' sets whether the datasets in the object have had a reproducibility check
    #' @param check (`logical`) whether datasets have had reproducibility check
    #' @return (`self`)
    set_check = function(check) {
      checkmate::assert_flag(check)
      private$.check <- check
      invisible(self)
    },

    #' @description
    #' Sets the R preprocessing code for single dataset
    #'
    #' @param code (`CodeClass`)\cr
    #' preprocessing code that can be parsed to generate the
    #'   unfiltered datasets
    #' @return (`self`)
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
    #' The attribute `formatted` renders the output of `self$get_formatted_filter_state` which
    #' is a character formatting of the filter state.
    #' @return `list` with named elements corresponding to `FilteredDataset` objects
    #' with active filters. Additionally, an attribute `formatted` holds the character format of
    #' the active filter states.
    get_filter_state = function() {
      states <- lapply(self$get_filtered_dataset(), function(x) x$get_filter_state())
      filtered_states <- Filter(function(x) length(x) > 0, states)
      structure(filtered_states, formatted = self$get_formatted_filter_state())
    },

    #' @description
    #' Returns the filter state formatted for printing to an `IO` device.
    #'
    #' @return `character` the pre-formatted filter state
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #' datasets <- teal.slice:::FilteredData$new(
    #'   list(iris = list(dataset = iris),
    #'        mae = list(dataset = miniACC)
    #'   ),
    #'   join_keys = NULL
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
    #' datasets$set_filter_state(state = fs)
    #' cat(shiny::isolate(datasets$get_formatted_filter_state()))
    #'
    get_formatted_filter_state = function() {
      out <-
        unlist(sapply(
          self$get_filtered_dataset(),
          function(filtered_dataset) {
            filtered_dataset$get_formatted_filter_state()
          }
        ))
      paste(out, collapse = "\n")
    },

    #' @description
    #' Sets active filter states.
    #' @param state (`named list`)\cr
    #'  nested list of filter selections applied to datasets.
    #' @examples
    #' utils::data(miniACC, package = "MultiAssayExperiment")
    #'
    #' datasets <- teal.slice:::FilteredData$new(
    #'   list(iris = list(dataset = iris),
    #'        mae = list(dataset = miniACC)
    #'   ),
    #'   join_keys = NULL
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
    #' datasets$set_filter_state(state = fs)
    #' shiny::isolate(datasets$get_filter_state())
    #' @return `NULL`
    set_filter_state = function(state) {
      checkmate::assert_subset(names(state), self$datanames())
      logger::log_trace("FilteredData$set_filter_state initializing, dataname: { paste(names(state), collapse = ' ') }")
      for (dataname in names(state)) {
        fdataset <- self$get_filtered_dataset(dataname = dataname)
        dataset_state <- state[[dataname]]

        fdataset$set_filter_state(
          state = dataset_state,
          vars_include = self$get_filterable_varnames(dataname)
        )
      }
      logger::log_trace("FilteredData$set_filter_state initialized, dataname: { paste(names(state), collapse = ' ') }")

      invisible(NULL)
    },

    #' @description Remove one or more `FilterState` of a `FilteredDataset` in a `FilteredData` object
    #'
    #' @param state (`named list`)\cr
    #'  nested list of filter selections applied to datasets.
    #'
    #' @return `NULL`
    remove_filter_state = function(state) {
      checkmate::assert_subset(names(state), self$datanames())

      logger::log_trace("FilteredData$remove_filter_state called, dataname: { paste(names(state), collapse = ' ') }")

      for (dataname in names(state)) {
        fdataset <- self$get_filtered_dataset(dataname = dataname)
        fdataset$remove_filter_state(element_id = state[[dataname]])
      }

      logger::log_trace("FilteredData$remove_filter_state done, dataname: { paste(names(state), collapse = ' ') }")

      invisible(NULL)
    },

    #' @description Remove all `FilterStates` of a `FilteredDataset` or all `FilterStates` of a `FilteredData` object
    #'
    #' @param datanames (`character`)\cr
    #'  datanames to remove their `FilterStates` or empty which removes all `FilterStates` in the `FilteredData` object.
    #'
    #' @return `NULL`
    #'
    remove_all_filter_states = function(datanames = self$datanames()) {
      logger::log_trace(
        "FilteredData$remove_all_filter_states called, datanames: { paste(datanames, collapse = ', ') }"
      )

      for (dataname in datanames) {
        fdataset <- self$get_filtered_dataset(dataname = dataname)
        fdataset$queues_empty()
      }

      logger::log_trace(
        paste(
          "FilteredData$remove_all_filter_states removed all FilterStates,",
          "datanames: { paste(datanames, collapse = ', ') }"
        )
      )

      invisible(NULL)
    },

    #' @description
    #' Sets this object from a bookmarked state
    #'
    #' Only sets the filter state, does not set the data
    #' and the preprocessing code. The data should already have been set.
    #' Also checks the preprocessing code is identical if provided in the `state`.
    #'
    #' Since this function is used from the end-user part, its error messages
    #' are more verbose. We don't call the Shiny modals from here because this
    #' class may be used outside of a Shiny app.
    #'
    #' @param state (`named list`)\cr
    #'  containing fields `data_hash`, `filter_states`
    #'   and `preproc_code`.
    #' @param check_data_hash (`logical`) whether to check that `md5sums` agree
    #'   for the data; may not make sense with randomly generated data per session
    restore_state_from_bookmark = function(state, check_data_hash = TRUE) {
      stop("Pure virtual method")
    },
    #' @description
    #' Disable the filter panel by adding `disable` class to `filter_add_vars` and `filter_panel_active_vars ` tags
    #' in the User Interface.
    #' In addition, it will store the existing filter states in a private field called `cached_states`
    #' before removing all filter states from the object.
    filter_panel_disable = function() {
      private$filter_panel_active <- FALSE
      shinyjs::disable("filter_add_vars")
      shinyjs::disable("filter_active_vars")
      private$cached_states <- self$get_filter_state()
      self$remove_all_filter_states()
      invisible(NULL)
    },
    #' @description enable the filter panel
    #' Enable the filter panel by adding `enable` class to `filter_add_vars` and `filter_active_vars ` tags
    #' in the User Interface.
    #' In addition, it will restore the filter states from a private field called `cached_states`.
    filter_panel_enable = function() {
      private$filter_panel_active <- TRUE
      shinyjs::enable("filter_add_vars")
      shinyjs::enable("filter_active_vars")
      if (length(private$cached_states) && (length(self$get_filter_state()) == 0)) {
        self$set_filter_state(private$cached_states)
      }
      invisible(NULL)
    },
    #' @description get the state of filter panel, if it is activated
    get_filter_panel_active = function() {
      private$filter_panel_active
    },
    #' @description get the id of the filter panel UI
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
    ui_filter_panel = function(id) {
      ns <- NS(id)
      div(
        id = ns(NULL), # used for hiding / showing
        include_css_files(pattern = "filter-panel"),
        div(
          id = ns("switch-button"),
          class = "flex justify-content-right",
          div(
            title = "Enable/Disable filtering",
            shinyWidgets::prettySwitch(
              ns("filter_panel_active"),
              label = "",
              status = "success",
              fill = TRUE,
              value = TRUE,
              inline = FALSE,
              width = 30
            )
          )
        ),
        div(
          id = ns("filters_overview"), # not used, can be used to customize CSS behavior
          class = "well",
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-9",
              tags$label("Active Filter Summary", class = "text-primary mb-4")
            ),
            tags$div(
              class = "col-sm-3",
              tags$a(
                href = "javascript:void(0)",
                class = "remove pull-right",
                onclick = sprintf(
                  "$('#%s').toggle();",
                  ns("filters_overview_contents")
                ),
                title = "Minimise panel",
                tags$span(icon("circle-minus", lib = "font-awesome"))
              )
            )
          ),
          tags$br(),
          div(
            id = ns("filters_overview_contents"),
            self$ui_filter_overview(ns("teal_filters_info"))
          )
        ),
        div(
          id = ns("filter_active_vars"), # not used, can be used to customize CSS behavior
          class = "well",
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-6",
              tags$label("Active Filter Variables", class = "text-primary mb-4")
            ),
            tags$div(
              class = "col-sm-6",
              actionLink(
                ns("remove_all_filters"),
                "",
                icon("circle-xmark", lib = "font-awesome"),
                title = "Remove active filters",
                class = "remove_all pull-right"
              ),
              actionLink(
                ns("minimise_filter_overview"),
                label = NULL,
                icon = icon("circle-minus", lib = "font-awesome"),
                title = "Minimise panel",
                class = "remove pull-right"
              )
            )
          ),
          div(
            id = ns("filter_active_vars_contents"),
            tagList(
              lapply(
                self$datanames(),
                function(dataname) {
                  fdataset <- self$get_filtered_dataset(dataname)
                  fdataset$ui(id = ns(private$get_ui_id(dataname)))
                }
              )
            )
          ),
          div(
            id = ns("filters_active_count"),
            textOutput(ns("teal_filters_count"))
          )
        ),
        div(
          id = ns("filter_add_vars"), # not used, can be used to customize CSS behavior
          class = "well",
          tags$div(
            class = "row",
            tags$div(
              class = "col-sm-9",
              tags$label("Add Filter Variables", class = "text-primary mb-4")
            ),
            tags$div(
              class = "col-sm-3",
              tags$a(
                href = "javascript:void(0)",
                class = "remove pull-right",
                onclick = sprintf("$('#%s').toggle();", ns("filter_add_vars_contents")),
                title = "Minimise panel",
                tags$span(icon("circle-minus", lib = "font-awesome"))
              )
            )
          ),
          div(
            id = ns("filter_add_vars_contents"),
            tagList(
              lapply(
                self$datanames(),
                function(dataname) {
                  fdataset <- self$get_filtered_dataset(dataname)
                  id <- ns(private$get_ui_add_filter_id(dataname))
                  # add span with same id to show / hide
                  return(
                    span(
                      id = id,
                      fdataset$ui_add_filter_state(id)
                    )
                  )
                }
              )
            )
          )
        )
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
      stopifnot(
        is.function(active_datanames) || is.reactive(active_datanames)
      )
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("FilteredData$srv_filter_panel initializing")
          shiny::setBookmarkExclude("remove_all_filters")
          self$srv_filter_overview(
            id = "teal_filters_info",
            active_datanames = active_datanames
          )

          shiny::observeEvent(self$get_filter_state(), {
            if (length(self$get_filter_state()) == 0) {
              shinyjs::hide("remove_all_filters")
            } else {
              shinyjs::show("remove_all_filters")
            }
          })

          # use isolate because we assume that the number of datasets does not change over the course of the teal app
          # alternatively, one can proceed as in modules_filter_items to dynamically insert, remove UIs
          isol_datanames <- isolate(self$datanames()) # they are already ordered
          # should not use for-loop as variables are otherwise only bound by reference and last dataname would be used
          lapply(
            isol_datanames,
            function(dataname) {
              fdataset <- self$get_filtered_dataset(dataname)
              fdataset$server(id = private$get_ui_id(dataname))
            }
          )

          lapply(
            isol_datanames,
            function(dataname) {
              fdataset <- self$get_filtered_dataset(dataname)
              fdataset$srv_add_filter_state(
                id = private$get_ui_add_filter_id(dataname),
                vars_include = self$get_filterable_varnames(dataname)
              )
            }
          )

          output$teal_filters_count <- renderText({
            n_filters_active <- private$get_filter_count()
            # req(n_filters_active > 0L)
            sprintf(
              "%s filter%s applied across datasets",
              n_filters_active,
              ifelse(n_filters_active == 1, "", "s"))
          })

          observeEvent(input[["minimise_filter_overview"]], {
            shinyjs::toggle("filter_active_vars_contents")
            shinyjs::toggle("filters_active_count")
          })

          private$filter_panel_ui_id <- session$ns(NULL)
          observeEvent(
            eventExpr = input[["filter_panel_active"]],
            handlerExpr = {
              if (isTRUE(input[["filter_panel_active"]])) {
                self$filter_panel_enable()
                shinyjs::show("filters_active_count")
                logger::log_trace("Enable the Filtered Panel with the filter_panel_enable method")
              } else {
                self$filter_panel_disable()
                shinyjs::hide("filters_active_count")
                logger::log_trace("Disable the Filtered Panel with the filter_panel_enable method")
              }
            }, ignoreNULL = TRUE
          )

          observeEvent(
            eventExpr = active_datanames(),
            handlerExpr = {
              private$hide_inactive_datasets(active_datanames)
            },
            priority = 1
          )

          observeEvent(input$remove_all_filters, {
            logger::log_trace("FilteredData$srv_filter_panel@1 removing all filters")
            lapply(self$datanames(), function(dataname) {
              fdataset <- self$get_filtered_dataset(dataname = dataname)
              fdataset$queues_empty()
            })
            logger::log_trace("FilteredData$srv_filter_panel@1 removed all filters")
          })

          logger::log_trace("FilteredData$srv_filter_panel initialized")
          NULL
        }
      )
    },

    #' Creates the UI for the module showing counts for each dataset
    #' contrasting the filtered to the full unfiltered dataset
    #'
    #' Per dataset, it displays
    #' the number of rows/observations in each dataset,
    #' the number of unique subjects.
    #'
    #' @param id module id
    ui_filter_overview = function(id) {
      ns <- NS(id)

      div(
        class = "teal_active_summary_filter_panel",
        tableOutput(ns("table"))
      )
    },

    #' Server function to display the number of records in the filtered and unfiltered
    #' data
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param active_datanames (`function`, `reactive`)\cr
    #'   returning datanames that should be shown on the filter panel,
    #'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
    #'   if the function returns `NULL` (as opposed to `character(0)`), the filter
    #'   panel will be hidden.
    #' @return `moduleServer` function which returns `NULL`
    srv_filter_overview = function(id, active_datanames = function() "all") {
      stopifnot(
        is.function(active_datanames) || is.reactive(active_datanames)
      )
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("FilteredData$srv_filter_overview initializing")
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

            body_html <- lapply(
              seq_len(nrow(datasets_df)),
              function(x) {
                tags$tr(
                  tags$td(rownames(datasets_df)[x]),
                  tags$td(datasets_df[x, 1]),
                  tags$td(datasets_df[x, 2])
                )
              }
            )

            header_html <- tags$tr(
              tags$td(""),
              tags$td(colnames(datasets_df)[1]),
              tags$td(colnames(datasets_df)[2])
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
    hide_inactive_datasets = function(active_datanames) {
      lapply(
        self$datanames(),
        function(dataname) {
          id_add_filter <- private$get_ui_add_filter_id(dataname)
          id_filter_dataname <- private$get_ui_id(dataname)

          if (dataname %in% active_datanames()) {
            # shinyjs takes care of the namespace around the id
            shinyjs::show(id_add_filter)
            shinyjs::show(id_filter_dataname)
          } else {
            shinyjs::hide(id_add_filter)
            shinyjs::hide(id_filter_dataname)
          }
        }
      )
    },

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
    keys = NULL,

    # reactive i.e. filtered data
    reactive_data = list(),
    cached_states = NULL,

    # we implement these functions as checks rather than returning logicals so they can
    # give informative error messages immediately

    # @details
    # Composes id for the FilteredDataset shiny element (active filter vars)
    # @param dataname (`character(1)`) name of the dataset which ui is composed for.
    # @return `character(1)` - `<dataname>_filter`
    get_ui_id = function(dataname) {
      sprintf("%s_filter", dataname)
    },

    # @details
    # Composes id for the FilteredDataset shiny element (add filter state)
    # @param dataname (`character(1)`)  name of the dataset which ui is composed for.
    # @return `character(1)` - `<dataname>_filter`
    get_ui_add_filter_id = function(dataname) {
      sprintf("add_%s_filter", dataname)
    },

    # @details
    # Validates the state of this FilteredData.
    # The call to this function should be isolated to avoid a reactive dependency.
    # Getting the names of a reactivevalues also needs a reactive context.
    validate = function() {
      # Note: Here, we directly refer to the private attributes because the goal of this
      # function is to check the underlying attributes and the get / set functions might be corrupted

      has_same_names <- function(x, y) setequal(names(x), names(y))
      # check `filter_states` are all valid
      lapply(
        names(private$filter_states),
        function(dataname) {
          stopifnot(is.list(private$filter_states)) # non-NULL, possibly empty list
          lapply(
            names(private$filter_states[[dataname]]),
            function(varname) {
              var_state <- private$filter_states[[dataname]][[varname]]
              stopifnot(!is.null(var_state)) # should not be NULL, see doc of this attribute
              check_valid_filter_state(
                filter_state = var_state,
                dataname = dataname,
                varname = varname
              )
            }
          )
        }
      )

      return(invisible(NULL))
    },

    # @description
    # Checks if the dataname exists and
    # (if provided) that varname is a valid column in the dataset
    #
    # Stops when this is not the case.
    #
    # @param dataname (`character`) name of the dataset
    # @param varname (`character`) column within the dataset;
    #   if `NULL`, this check is not performed
    check_data_varname_exists = function(dataname, varname = NULL) {
      checkmate::assert_string(dataname)
      checkmate::assert_string(varname, null.ok = TRUE)

      isolate({
        # we isolate everything because we don't want to trigger again when datanames
        # change (which also triggers when any of the data changes)
        if (!dataname %in% names(self$get_filtered_dataset())) {
          # data must be set already
          stop(paste("data", dataname, "is not available"))
        }
        if (!is.null(varname) && !(varname %in% self$get_varnames(dataname = dataname))) {
          stop(paste("variable", varname, "is not in data", dataname))
        }
      })

      return(invisible(NULL))
    },

    # @description
    # Gets the number of active `FilterState` objects in all `FilterStates`
    # in all `FilteredDataset`s in this `FilteredData` object.
    # @return `integer(1)`
    get_filter_count = function() {
      sum(vapply(self$datanames(), function(dataname) {
        self$get_filtered_dataset(dataname)$get_filter_count()
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
