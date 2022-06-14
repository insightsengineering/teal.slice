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
#' library(shiny)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' # TODO make this nicer
#' datasets <- teal.slice:::CDISCFilteredData$new(
#'   ADSL = list(dataset = ADSL, keys = c("STUDYID", "USUBJID")),
#'   ADTTE = list(dataset = ADTTE, keys = c("STUDYID", "USUBJID", "PARAMCD"), parent = "ADSL"),
#'   check = FALSE, keys = list(ADSL = list(ADTTE = c("STUDYID", "USUBJID"), ADTTE = list(ADSL = c("STUDYID", "USUBJID"))))
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
#' filter_state_adtte <- teal.slice:::init_filter_state(
#'   ADTTE$PARAMCD,
#'   varname = "PARAMCD",
#'   input_dataname = as.name("ADTTE"),
#'   extract_type = "list"
#' )
#' filter_state_adtte$set_selected("OS")
#'
#' states <- datasets$get_filtered_dataset("ADTTE")$get_filter_states(1)
#' states$queue_push(filter_state_adtte, queue_index = 1L, element_id = "PARAMCD")
#'
#' isolate(datasets$get_call("ADTTE"))
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
      # get_keys checks dataname is in datanames, not by calling `self$datanames()`,
      # but `names(private$unfiltered_datasets)` to avoid an infinite recursion
      child_parent <- sapply(datanames, function(i) self$get_parentname(i), USE.NAMES = TRUE, simplify = FALSE)
      ordered_datanames <- topological_sort(child_parent)
      return(as.character(intersect(as.character(ordered_datanames), datanames)))
    },

    #' @description
    #'
    #' Returns the filter `call` to filter a single dataset including the `inner_join`
    #' with its parent dataset. It assumes that the filtered datasets it depends
    #' on are available.
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`call` or `list` of calls ) to filter dataset
    #'
    get_call = function(dataname) {
      parent_dataname <- self$get_parentname(dataname)

      if (length(parent_dataname) == 0) {
        super$get_call(dataname)
      } else {

        keys <- self$get_join_keys(parent_dataname, dataname)
        parent_keys <- names(keys)
        dataset_keys <- unname(keys)

        dataset <- self$get_filtered_dataset(dataname)

        filtered_dataname <- dataset$get_filtered_dataname()
        filtered_dataname_alone <- dataset$get_filtered_dataname(suffix = "_FILTERED_ALONE")
        filtered_parentname <- dataset$get_filtered_dataname(dataname = parent_dataname)

        premerge_call <- Filter(
          f = Negate(is.null),
          x = lapply(
            dataset$get_filter_states(),
            function(x) x$get_call()
          )
        )
        premerge_call[[1]][[2]] <- as.name(filtered_dataname_alone)
        merge_call <- call(
          "<-",
          as.name(filtered_dataname),
          call_with_colon(
            "dplyr::inner_join",
            x = as.name(filtered_dataname_alone),
            y = if (length(parent_keys) == 0) {
              as.name(filtered_parentname)
            } else {
              call_extract_array(
                dataname = filtered_parentname,
                column = parent_keys,
                aisle = call("=", as.name("drop"), FALSE)
              )
            },
            unlist_args = if (length(parent_keys) == 0 || length(dataset_keys) == 0) {
              list()
            } else if (identical(parent_keys, dataset_keys)) {
              list(by = parent_keys)
            } else {
              list(by = setNames(parent_keys, nm = dataset_keys))
            }
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
    #' @param dataname (`character`) name of the dataset
    #' @return (`character` vector) of variable names
    get_filterable_varnames = function(dataname) {
      varnames <- self$get_filtered_dataset(dataname)$get_filterable_varnames()
      parent_dataname <- self$get_parentname(dataname)
      parent_varnames <- if (length(parent_dataname) > 0) {
        super$get_filterable_varnames(parent_dataname)
      }
      setdiff(varnames, parent_varnames)
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
          df <- cbind(
            self$get_filtered_dataset(dataname)$get_filter_overview_info()[1, 1],
            private$get_filter_overview_nsubjs(dataname)
          )
          rownames(df) <- dataname
          colnames(df) <- c("Obs", "Subjects")
          df
        }
      )

      do.call(rbind, rows)
    },

    #' @description
    #' Get parent dataset name
    #'
    #' @param dataname (`character`) name of the dataset
    #' @return (`character`) name of parent dataset
    get_parentname = function(dataname) {
      #TODO validate dataname
      private$parent[[dataname]]
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
    #' @param dataname (`string`)\cr
    #'   the name of the `dataset` to be added to this object
    #' @return (`self`) object of this class
    set_dataset = function(dataset_args, dataname) {

      parent_dataname <- dataset_args[["parent"]]
      dataset_args[["parent"]] <- NULL

      #TODO what happens if parent dataset doesn't actually exist...
      super$set_dataset(dataset_args, dataname)
      private$parents[[dataname]] <- parent_dataname

      if (length(parent_dataname) > 0) {
        parent_dataset <- self$get_filtered_dataset(parent_dataname)
        fdataset <- self$get_filtered_dataset(dataname)
        fdataset$add_to_eval_env(
          parent_dataset$get_filtered_dataname(),
          list(parent_dataset$get_data_reactive())
        )
      }
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

      f_rows <- if (length(subject_keys) == 0) {
        dplyr::n_distinct(self$get_data(dataname = dataname, filtered = TRUE))
      } else {
        dplyr::n_distinct(self$get_data(dataname = dataname, filtered = TRUE)[subject_keys])
      }

      nf_rows <- if (length(subject_keys) == 0) {
        dplyr::n_distinct(self$get_data(dataname = dataname, filtered = FALSE))
      } else {
        dplyr::n_distinct(self$get_data(dataname = dataname, filtered = FALSE)[subject_keys])
      }

      list(paste0(f_rows, "/", nf_rows))
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
#' teal.slice:::topological_sort(list(A = c(), B = c("A"), C = c("B"), D = c("A")))
#' teal.slice:::topological_sort(list(D = c("A"), A = c(), B = c("A"), C = c("B")))
#' teal.slice:::topological_sort(list(D = c("A"), B = c("A"), C = c("B"), A = c()))
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
