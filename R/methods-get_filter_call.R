#' Get filter call
#'
#' Get filter call
#' @inheritParams filter_panel_methods
#' @inheritSection filter_panel_methods Supported data types
#' @export
get_filter_call <- function(data, states_list) {
  if (length(states_list) > 0L) {
    UseMethod("get_filter_call")
  }
}

#' @rdname get_filter_call
#' @export
get_filter_call.default <- function(data, states_list) {
  if (inherits(data, "data.frame")) {
    get_filter_call_data.frame(data, states_list)
  } else if (inherits(data, c("array", "Matrix"))) {
    get_filter_call_array(data, states_list)
  } else if (inherits(data, "SummarizedExperiment")) {
    get_filter_call_SummarizedExperiment(data, states_list)
  } else if (inherits(data, "MultiAssayExperiment")) {
    get_filter_call_MultiAssayExperiment(data, states_list)
  } else {
    stop("get_filter_call not implemented for class ", class(data), call. = FALSE)
  }
}

#' @rdname default_filter_panel_internals
#' @keywords internal
get_filter_call_data.frame <- function(data, states_list) {
  dataname_lang <- str2lang(states_list[[1L]]$get_state()$dataname)
  states_predicate <- lapply(states_list, function(state) state$get_call())
  combined_predicate <- calls_combine_by(states_predicate, "&")
  rhs <- as.call(c(str2lang("dplyr::filter"), c(list(dataname_lang), combined_predicate)))

  substitute(
    expr = dataname_lang <- rhs,
    env = list(dataname_lang = dataname_lang, rhs = rhs)
  )
}

#' @rdname default_filter_panel_internals
#' @keywords internal
get_filter_call_array <- function(data, states_list) {
  dataname_lang <- str2lang(states_list[[1L]]$get_state()$dataname)
  states_predicate <- lapply(states_list, function(state) state$get_call(extract_type = "matrix"))
  combined_predicate <- calls_combine_by(states_predicate, "&")
  rhs <- as.call(c(str2lang("subset"), c(list(dataname_lang), subset = combined_predicate)))

  substitute(
    expr = dataname_lang <- rhs,
    env = list(dataname_lang = dataname_lang, rhs = rhs)
  )
}

#' @rdname default_filter_panel_internals
#' @keywords internal
get_filter_call_MultiAssayExperiment <- function(data, states_list) {
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
}

#' @rdname default_filter_panel_internals
#' @keywords internal
get_filter_call_SummarizedExperiment <- function(data, states_list) {
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
}


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
