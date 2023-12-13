#' Get filter call
#'
#' Get filter call
#' @export
get_filter_call <- function(data, states_list) {
  if (length(states_list) > 0L) {
    UseMethod("get_filter_call")
  }
}

#' @rdname get_filter_call
#' @export
get_filter_call.default <- function(data, states_list) {
  dataname_lang <- str2lang(states_list[[1L]]$get_state()$dataname)
  states_predicate <- lapply(states_list, function(state) state$get_call())
  combined_predicate <- calls_combine_by(states_predicate, "&")

  substitute(
    env = list(
      lhs = dataname_lang,
      rhs = as.call(c(quote(subset), c(list(dataname_lang), combined_predicate)))
    ),
    expr = lhs <- rhs
  )
}


#' @rdname get_filter_call
#' @export
get_filter_call.MultiAssayExperiment <- function(data, states_list) {
  states_grouped <- split(
    states_list,
    sapply(states_list, function(x) if (is.null(x$get_state()$experiment)) "_subjects_" else x$get_state()$experiment)
  )

  subject_predicates_list <- lapply(
    states_grouped[["_subjects_"]],
    function(state) state$get_call(extract_type = "list") # subsetByColData needs prefixed variables e.g. data$var
  )
  call_subjects <- calls_combine_by(subject_predicates_list, "&")

  subject_call <- if (length(call_subjects)) {
    substitute(
      dataname <- subsetByColData(x = dataname, y = calls),
      list(
        dataname = str2lang(states_list[[1L]]$get_state()$dataname),
        calls = call_subjects
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

#' @rdname get_filter_call
#' @export
get_filter_call.SummarizedExperiment <- function(data, states_list) {
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
        lhs = dataname,
        rhs = as.call(c(quote(subset), c(list(dataname), state_list_calls)))
      ),
      expr = lhs <- rhs
    )
  }
}
