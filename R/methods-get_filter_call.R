setOldClass(c("teal_slice", "list"))

#' Get filter call
#'
#' Get filter call
#'
#' @inheritParams filter_panel_methods
#' @inheritSection filter_panel_methods Supported data types
#'
#' @name get_filter_call
#' @rdname get_filter_call
#' @aliases get_filter_call-ANY,teal_slice-method
#' @aliases get_filter_call-data.frame,teal_slice-method
#' @aliases get_filter_call-array,teal_slice-method
#' @aliases get_filter_call-Matrix,teal_slice-method
#' @aliases get_filter_call-SummarizedExperiment,teal_slice-method
#' @aliases get_filter_call-MultiAssayExperiment,teal_slice-method
#'
#' @export
#'
# get_filter_call generic ----
setGeneric("get_filter_call", function(data, states_list) {
  NULL
})

## default method ----
setMethod("get_filter_call", c("ANY", "teal_slice"), function(data, states_list) {
  stop("get_filter_call not implemented for class ", toString(class(data)), call. = FALSE)
})

## data.frame method ----
setMethod("get_filter_call", c("data.frame", "teal_slice"), function(data, states_list) {
  dataname_lang <- str2lang(states_list[[1L]]$get_state()$dataname)
  states_predicate <- lapply(states_list, function(state) state$get_call())
  combined_predicate <- calls_combine_by(states_predicate, "&")
  rhs <- as.call(c(str2lang("dplyr::filter"), c(list(dataname_lang), combined_predicate)))

  substitute(
    expr = dataname_lang <- rhs,
    env = list(dataname_lang = dataname_lang, rhs = rhs)
  )
})

## array method ----
setMethod("get_filter_call", c("array", "teal_slice"), function(data, states_list) {
  dataname_lang <- str2lang(states_list[[1L]]$get_state()$dataname)
  states_predicate <- lapply(states_list, function(state) state$get_call())
  combined_predicate <- calls_combine_by(states_predicate, "&")
  rhs <- as.call(c(str2lang("subset"), c(list(dataname_lang), subset = combined_predicate)))

  substitute(
    expr = dataname_lang <- rhs,
    env = list(dataname_lang = dataname_lang, rhs = rhs)
  )
})

## Matrix method ----
setMethod("get_filter_call", c("Matrix", "teal_slice"), function(data, states_list) {
  dataname_lang <- str2lang(states_list[[1L]]$get_state()$dataname)
  states_predicate <- lapply(states_list, function(state) state$get_call())
  combined_predicate <- calls_combine_by(states_predicate, "&")
  rhs <- as.call(c(str2lang("subset"), c(list(dataname_lang), subset = combined_predicate)))

  substitute(
    expr = dataname_lang <- rhs,
    env = list(dataname_lang = dataname_lang, rhs = rhs)
  )
})

## SummarizedExperiment method ----
setMethod("get_filter_call", c("SummarizedExperiment", "teal_slice"), function(data, states_list) {
  state_list_grouped <- split(
    states_list,
    sapply(states_list, function(x) x$get_state()$arg)
  )

  state_list_calls <- Filter(
    length,
    lapply(
      state_list_grouped,
      function(x) {
        calls_combine_by(lapply(x, function(state) state$get_call()), "&")
      }
    )
  )

  if (length(state_list_calls) > 0L) {
    dataname <- str2lang(states_list[[1]]$get_state()$dataname)
    substitute(
      env = list(
        dataname = dataname,
        state_list_calls = as.call(c(quote(subset), c(list(dataname), state_list_calls)))
      ),
      expr = dataname <- state_list_calls
    )
  }
})

## MultiAssayExperiment method ----
setMethod("get_filter_call", c("MultiAssayExperiment", "teal_slice"), function(data, states_list) {
  states_grouped <- split(
    states_list,
    sapply(states_list, function(x) if (is.null(x$get_state()$experiment)) "_subjects_" else x$get_state()$experiment)
  )

  subject_predicates_list <- lapply(
    states_grouped[["_subjects_"]],
    function(state) state$get_call(extract_type = "list") # subsetByColData needs prefixed variables e.g. data$var
  )
  call_subjects_predicate <- calls_combine_by(subject_predicates_list, "&")

  subject_call <- if (length(call_subjects_predicate)) {
    substitute(
      dataname <- subsetByColData(x = dataname, y = call_subjects_predicate),
      list(
        dataname = str2lang(states_list[[1L]]$get_state()$dataname),
        call_subjects_predicate = call_subjects_predicate
      )
    )
  }

  experiments_state <- states_grouped[names(states_grouped) != "_subjects_"]
  experiment_calls <- lapply(
    experiments_state,
    function(x) {
      experiment_name <- x[[1]]$get_state()$experiment
      experiment <- data[[experiment_name]]
      experiment_calls <- get_filter_call(experiment, x)

      experiment_calls[[2L]] <- call("[[", experiment_calls[[2L]], experiment_name)
      experiment_calls[[3L]][[2L]] <- call("[[", experiment_calls[[3L]][[2L]], experiment_name)

      experiment_calls
    }
  )
  c(subject_call, experiment_calls)
})



# utils ----
#' @rdname default_filter_panel_internals
#' @keywords internal
get_merge_call <- function(join_keys) {
  if (length(join_keys) > 0L) {
    dataname <- names(join_keys)[1]
    parent_dataname <- teal.data::parent(join_keys, dataname)
    if (length(parent_dataname)) {
      merge_call <- call(
        "<-",
        str2lang(dataname),
        as.call(
          c(
            str2lang("dplyr::inner_join"),
            x = str2lang(dataname),
            y = str2lang(parent_dataname),
            by = make_c_call(join_keys[dataname, parent_dataname])
          )
        )
      )
    }
  }
}
