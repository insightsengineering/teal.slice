#' @name CDISCFilteredData
#' @docType class
#'
#' @title Class to encapsulate relational filtered datasets with its parents.
#' @description `r lifecycle::badge("stable")`
#' @details
#' The `CDISCFilteredData` class implements logic to filter a relational
#' dataset by inheriting from `FilteredData`.
#' A dataset can have up to one parent dataset. Rows are identified by the foreign
#' key and only those rows that appear in the parent dataset are kept in the filtered
#' dataset.
#'
#' The teal UI works with objects of class `FilteredData` which may mix CDISC and other
#' data (e.g. `iris`).
#'
#' @seealso `FilteredData` class
#'
#' @examples
#' library(scda)
#' library(teal.data)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#' datasets <- teal.slice:::CDISCFilteredData$new(
#'   list(
#'     ADSL = list(dataset = ADSL, keys = c("STUDYID", "USUBJID")),
#'     ADTTE = list(dataset = ADTTE, keys = c("STUDYID", "USUBJID", "PARAMCD"), parent = "ADSL")
#'   ),
#'   check = FALSE,
#'   join_keys = join_keys(join_key("ADSL", "ADTTE", c("STUDYID", "USUBJID")))
#' )
#'
#' # to avoid using isolate(), you can provide a default isolate context by calling
#' # options(shiny.suppressMissingContextError = TRUE) #nolint
#' # don't forget to deactivate this option at the end
#' # options(shiny.suppressMissingContextError = FALSE) #nolint
#'
#' isolate({
#'   datasets$datanames()
#'
#'   # number observations and subjects of filtered/non-filtered dataset
#'   datasets$get_filter_overview("ADSL")
#'
#'   print(datasets$get_call("ADSL"))
#'   print(datasets$get_call("ADTTE"))
#'
#'   df <- datasets$get_data("ADSL", filtered = FALSE)
#'   print(df)
#' })
#'
#'
#' isolate(datasets$set_filter_state(list(ADTTE = list(PARAMCD = "OS"))))
#' isolate(datasets$get_filter_state())
CDISCFilteredData <- R6::R6Class( # nolint
  "CDISCFilteredData",
  inherit = FilteredData,
  ## CDISCFilteredData ====
  ## __Public Methods ====
  public = list(
    #' @description
    #' Get datanames
    #'
    #' The datanames are returned in the order in which they must be
    #' evaluated (in case of dependencies).
    #' @return (`character` vector) of datanames
    datanames = function() {
      datanames <- super$datanames()
      child_parent <- sapply(datanames, function(i) self$get_parentname(i), USE.NAMES = TRUE, simplify = FALSE)
      ordered_datanames <- topological_sort(child_parent)
      return(as.character(intersect(as.character(ordered_datanames), datanames)))
    },

    #' @description
    #'
    #' Produces language required to filter a single dataset and merge it with its parent.
    #' The datasets in question are assumed to be available.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @return (`call` or `list` of calls ) to filter dataset
    #'
    get_call = function(dataname) {
      parent_dataname <- self$get_parentname(dataname)

      if (length(parent_dataname) == 0) {
        super$get_call(dataname)
      } else {
        dataset <- self$get_filtered_dataset(dataname)
        premerge_call <- Filter(
          f = Negate(is.null),
          x = lapply(
            dataset$get_filter_states(),
            function(x) x$get_call()
          )
        )

        join_keys <- self$get_join_keys()
        keys <-
          if (!is.null(join_keys)) {
            join_keys$get(parent_dataname, dataname)
          } else {
            character(0)
          }
        parent_keys <- names(keys)
        dataset_keys <- unname(keys)

        y_arg <-
          if (length(parent_keys) == 0L) {
            parent_dataname
          } else {
            sprintf("%s[, c(%s), drop = FALSE]", parent_dataname, toString(dQuote(parent_keys, q = FALSE)))
          }
        more_args <-
          if (length(parent_keys) == 0 || length(dataset_keys) == 0) {
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

        c(premerge_call, merge_call)
      }
    },

    #' @description
    #' Get names of datasets available for filtering
    #'
    #' @param dataname (`character` vector) names of the dataset
    #' @return (`character` vector) of dataset names
    get_filterable_datanames = function(dataname) {
      parents <- character(0)
      for (i in dataname) {
        while (length(i) > 0) {
          parent_i <- self$get_parentname(i)
          parents <- c(parents, parent_i)
          i <- parent_i
        }
      }

      return(unique(c(parents, dataname)))
    },

    #' @description
    #' Gets variable names of a given dataname for the filtering. This excludes parent dataset variable names.
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @return (`character` vector) of variable names
    get_filterable_varnames = function(dataname) {
      varnames <- self$get_filtered_dataset(dataname)$get_filterable_varnames()
      parent_dataname <- self$get_parentname(dataname)
      parent_varnames <- if (length(parent_dataname) > 0) {
        # cannot call get_filterable_varnames on the parent filtered_dataset in case
        # some of its variables are set to be non-filterable
        get_supported_filter_varnames(self$get_filtered_dataset(parent_dataname))
      }
      setdiff(varnames, parent_varnames)
    },

    #' @description
    #' Get filter overview table in form of X (filtered) / Y (non-filtered)
    #'
    #' This is intended to be presented in the application.
    #'
    #' @param datanames (`character` vector) names of the dataset (or "all")
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
          obs <- self$get_filtered_dataset(dataname)$get_filter_overview_info(
            filtered_dataset = self$get_data(dataname = dataname, filtered = TRUE)
          )[, 1]

          subs <- private$get_filter_overview_nsubjs(dataname)

          df <- cbind(
            obs, subs
          )

          rownames(df) <- if (!is.null(names(obs))) {
            names(obs)
          } else {
            dataname
          }
          colnames(df) <- c("Obs", "Subjects")
          df
        }
      )

      do.call(rbind, rows)
    },

    #' @description
    #' Get parent dataset name
    #'
    #' @param dataname (`character(1)`) name of the dataset
    #' @return (`character`) name of parent dataset
    get_parentname = function(dataname) {
      private$parents[[dataname]]
    },

    #' @description
    #' Add dataset
    #'
    #' Add dataset and preserve all attributes attached to this object.
    #' Technically `set_dataset` created `FilteredDataset` which keeps
    #' `dataset` for filtering purpose.
    #'
    #' @param dataset_args (`list`)\cr
    #'   containing the arguments except (`dataname`)
    #'   needed by `init_filtered_dataset` (can also
    #'   include `parent` which will be ignored)
    #' @param dataname (`character(1)`)\cr
    #'   the name of the `dataset` to be added to this object
    #' @return (`self`) object of this class
    set_dataset = function(dataset_args, dataname) {
      logger::log_trace("CDISCFilteredData$set_dataset setting dataset, name: { dataname }")
      validate_dataset_args(dataset_args, dataname, allowed_parent = TRUE)

      parent_dataname <- dataset_args[["parent"]]
      dataset_args[["parent"]] <- NULL
      private$parents[[dataname]] <- parent_dataname

      if (length(parent_dataname) == 0) {
        super$set_dataset(dataset_args, dataname)
      } else {
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
          env[[private$parents[[dataname]]]] <-
            private$reactive_data[[private$parents[[dataname]]]]()

          filter_call <- self$get_call(dataname)
          eval_expr_with_msg(filter_call, env)
          get(x = dataname, envir = env)
        })
      }

      invisible(self)
    }
  ),

  ## __Private Methods---------------------
  private = list(

    # named list of dataset parents parents[[child_dataset]] = its parent
    parents = NULL,

    # datanames in the order in which they must be evaluated (in case of dependencies)
    # this is a reactive and kept as a field for caching
    ordered_datanames = NULL,
    validate = function() {
      stopifnot(
        setequal(private$ordered_datanames, names(private$dataset_filters)),
      )
      super$validate()
    },
    get_filter_overview_nsubjs = function(dataname) {
      # Gets filter overview subjects number and returns a list
      # of the number of subjects of filtered/non-filtered datasets
      subject_keys <- if (length(self$get_parentname(dataname)) > 0) {
        self$get_keys(self$get_parentname(dataname))
      } else {
        self$get_filtered_dataset(dataname)$get_keys()
      }

      self$get_filtered_dataset(dataname)$get_filter_overview_nsubjs(
        self$get_data(dataname = dataname, filtered = TRUE),
        subject_keys
      )
    }
  )
)

#' Topological graph sort
#'
#' Graph is a list which for each node contains a vector of child nodes
#' in the returned list, parents appear before their children.
#'
#' Implementation of Kahn algorithm with a modification to maintain the order of input elements.
#'
#' @param graph (named `list`) list with node vector elements
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A")))
#' topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B")))
#' topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c()))
#' }
topological_sort <- function(graph) {
  # compute in-degrees
  in_degrees <- list()
  for (node in names(graph)) {
    in_degrees[[node]] <- 0
    for (to_edge in graph[[node]]) {
      in_degrees[[to_edge]] <- 0
    }
  }

  for (node in graph) {
    for (to_edge in node) {
      in_degrees[[to_edge]] <- in_degrees[[to_edge]] + 1
    }
  }

  # sort
  visited <- 0
  sorted <- list()
  zero_in <- list()
  for (node in names(in_degrees)) {
    if (in_degrees[[node]] == 0) zero_in <- append(zero_in, node)
  }
  zero_in <- rev(zero_in)

  while (length(zero_in) != 0) {
    visited <- visited + 1
    sorted <- c(zero_in[[1]], sorted)
    for (edge_to in graph[[zero_in[[1]]]]) {
      in_degrees[[edge_to]] <- in_degrees[[edge_to]] - 1
      if (in_degrees[[edge_to]] == 0) {
        zero_in <- append(zero_in, edge_to, 1)
      }
    }
    zero_in[[1]] <- NULL
  }

  if (visited != length(in_degrees)) {
    stop(
      "Graph is not a directed acyclic graph. Cycles involving nodes: ",
      paste0(setdiff(names(in_degrees), sorted), collapse = " ")
    )
  } else {
    return(sorted)
  }
}
