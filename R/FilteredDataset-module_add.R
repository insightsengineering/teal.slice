ui_add <- function(id, dataname) {
  ns <- NS(id)
  tagList(
    tags$label("Add", tags$code(dataname), "filter"),
    uiOutput(ns("inputs"))
  )
}

