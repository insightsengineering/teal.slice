#' Set "`<choice>:<label>`" type of names
#'
#' @description
#'
#' This is often useful for as it marks up the drop-down boxes for [shiny::selectInput()].
#'
#' @details
#' If either `choices` or `labels` are factors, they are coerced to character.
#' Duplicated elements from `choices` get removed.
#'
#' @param choices (`character` or `numeric` or `logical`) vector
#' @param labels (`character`) vector containing labels to be applied to `choices`. If `NA` then
#' "Label Missing" will be used.
#' @param types vector containing the types of the columns.
#'
#' @return A named character vector.
#'
#' @keywords internal
#'
choices_labeled <- function(choices, labels, types = NULL) {
  checkmate::assert_character(choices)
  checkmate::assert_character(labels[!is.na(labels)], len = length(choices))
  checkmate::assert_character(types, len = length(choices), null.ok = TRUE)

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

  choices <- structure(
    choices,
    names = combined_labels,
    raw_labels = raw_labels,
    combined_labels = combined_labels,
    class = c("choices_labeled", "character"),
    types = types
  )

  choices
}
