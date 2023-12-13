get_filter_call <- function(data, dataname, calls) {
  if (length(calls) > 0L) {
    UseMethod("get_filter_call")
  }
}

get_filter_call.default <- function(data, dataname, calls) {
  dataname_lang <- str2lang(dataname)
  substitute(
    env = list(
      lhs = dataname_lang,
      rhs = as.call(c(quote(subset), c(list(dataname_lang), calls)))
    ),
    expr = lhs <- rhs
  )
}


get_filter_call.MultiAssayExperiment <- function(data, dataname, calls) {
  NULL
}
