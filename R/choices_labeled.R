#' @title Returns a `choices_labeled` object
#'
#' @param data (`data.frame`, `DFrame`, `list`)\cr
#'   where labels can be taken from in case when `varlabels` is not specified.
#'   `data` must be specified if `varlabels` is not specified.
#' @param choices (`character`)\cr
#'  the vector of chosen variables
#' @param varlabels (`character`)\cr
#'  the labels of variables in data
#' @param keys (`character`)\cr
#'  the names of the key columns in data
#' @return `character(0)` if choices are empty; a `choices_labeled` object otherwise
#' @keywords internal
data_choices_labeled <- function(data,
                                 choices,
                                 varlabels = teal.data::col_labels(data, fill = TRUE),
                                 keys = character(0)) {
  if (length(choices) == 0) {
    return(character(0))
  }
  choice_types <- stats::setNames(variable_types(data = data, columns = choices), choices)
  choice_types[keys] <- "primary_key"

  choices_labeled(
    choices = choices,
    labels = unname(varlabels[choices]),
    types = choice_types[choices]
  )
}

#' Set "`<choice>:<label>`" type of Names
#'
#' @description `r lifecycle::badge("stable")`
#' This is often useful for as it marks up the drop-down boxes for [shiny::selectInput()].
#'
#' @param choices a character / numeric / logical vector
#' @param labels character vector containing labels to be applied to `choices`. If `NA` then
#' "Label Missing" will be used.
#' @param subset a vector that is a subset of `choices`. This is useful if
#'   only a few variables need to be named. If this argument is used, the returned vector will
#'   match its order.
#' @param types vector containing the types of the columns.
#' @details If either `choices` or `labels` are factors, they are coerced to character.
#' Duplicated elements from `choices` get removed.
#'
#' @return a named character vector
#'
#' @keywords internal
#'
choices_labeled <- function(choices, labels, subset = NULL, types = NULL) {
  if (is.factor(choices)) {
    choices <- as.character(choices)
  }

  stopifnot(
    is.character(choices) ||
      is.numeric(choices) ||
      is.logical(choices) ||
      (length(choices) == 1 && is.na(choices))
  )

  if (is.factor(labels)) {
    labels <- as.character(labels)
  }

  checkmate::assert_character(labels[!is.na(labels)], any.missing = FALSE)
  if (length(choices) != length(labels)) {
    stop("length of choices must be the same as labels")
  }
  stopifnot(is.null(subset) || is.vector(subset))
  stopifnot(is.null(types) || is.vector(types))

  if (is.vector(types)) {
    stopifnot(length(choices) == length(types))
  }

  if (!is.null(subset)) {
    if (!all(subset %in% choices)) {
      stop("all of subset variables must be in choices")
    }
    labels <- labels[choices %in% subset]
    types <- types[choices %in% subset]
    choices <- choices[choices %in% subset]
  }

  is_dupl <- duplicated(choices)
  choices <- choices[!is_dupl]
  labels <- labels[!is_dupl]
  types <- types[!is_dupl]
  labels[is.na(labels)] <- "Label Missing"
  raw_labels <- labels
  combined_labels <- if (length(choices) > 0) {
    paste0(choices, ": ", labels)
  } else {
    character(0)
  }

  if (!is.null(subset)) {
    ord <- match(subset, choices)
    choices <- choices[ord]
    raw_labels <- raw_labels[ord]
    combined_labels <- combined_labels[ord]
    types <- types[ord]
  }
  choices <- structure(
    choices,
    names = combined_labels,
    raw_labels = raw_labels,
    combined_labels = combined_labels,
    class = c("choices_labeled", "character"),
    types = types
  )

  return(choices)
}
