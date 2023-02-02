
#' Active filter states module
#'
#' Active filter states module
#' @rdname module_active
#'
#' @param id (`character(1)`)\cr
#'  identifier of the element - preferably containing dataset name
#'
#' @param x (`FilteredData`, `FilteredDataset`, `FilterStates`)
#'
#' @keywords internal
#'
#' @return shiny `ui` or `serverModule`
NULL

#' @rdname module_active
ui_active_FilteredData <- function(id, datanames) {
  ns <- NS(id)
  div(
    id = id, # not used, can be used to customize CSS behavior
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
          label = "",
          icon("circle-xmark", lib = "font-awesome"),
          title = "Remove active filters",
          class = "remove_all pull-right"
        ),
        actionLink(
          ns("minimise_filter_active"),
          label = NULL,
          icon = icon("angle-down", lib = "font-awesome"),
          title = "Minimise panel",
          class = "remove pull-right"
        )
      )
    ),
    div(
      id = ns("filter_active_vars_contents"),
      tagList(
        lapply(
          datanames,
          function(dataname) {
            ui_active_FilteredDataset(id = ns(dataname), dataname = dataname)
          }
        )
      )
    ),
    shinyjs::hidden(
      div(
        id = ns("filters_active_count"),
        textOutput(ns("teal_filters_count"))
      )
    )
  )
}

#' @rdname module_active
srv_active_FilteredData <- function(id, x, active_datanames = function() "all") {
  stopifnot(is.function(active_datanames))
  checkmate::assert_class(x, "FilteredData")
  moduleServer(id = id, function(input, output, session) {
      shiny::observeEvent(input$minimise_filter_active, {
        shinyjs::toggle("filter_active_vars_contents")
        shinyjs::toggle("filters_active_count")
        toggle_icon(session$ns("minimise_filter_active"), c("fa-angle-right", "fa-angle-down"))
        toggle_title(session$ns("minimise_filter_active"), c("Restore panel", "Minimise Panel"))
      })

      observeEvent(input$remove_all_filters, {
        logger::log_trace("FilteredData$srv_filter_panel@1 removing all filters")
        lapply(x$datanames(), function(dataname) {
          fdataset <- x$get_filtered_dataset(dataname = dataname)
          fdataset$state_lists_empty()
        })
        logger::log_trace("FilteredData$srv_filter_panel@1 removed all filters")
      })

      # use isolate because we assume that the number of datasets does not change
      # over the course of the teal app
      # alternatively, one can proceed as in modules_filter_items to dynamically insert, remove UIs
      isol_datanames <- isolate(x$datanames()) # they are already ordered
      # should not use for-loop as variables are otherwise only bound by reference
      # and last dataname would be used
      lapply(
        isol_datanames,
        function(dataname) {
          fdataset <- x$get_filtered_dataset(dataname)
          srv_active_FilteredDataset(id = dataname, fdataset)
        }
      )

      # hide and show datanames cards
      observeEvent(active_datanames(), {
        lapply(isol_datanames, function(dataname) {
          if (dataname %in% active_datanames()) {
            shinyjs::show(dataname)
          } else {
            shinyjs::hide(dataname)
          }
        })
      })

      output$teal_filters_count <- shiny::renderText({
        n_filters_active <- x$get_filter_count()
        shiny::req(n_filters_active > 0L)
        sprintf(
          "%s filter%s applied across datasets",
          n_filters_active,
          ifelse(n_filters_active == 1, "", "s"))
      })

      shiny::observeEvent(x$get_filter_count(), {
        shinyjs::toggle("remove_all_filters", condition = x$get_filter_count() != 0)
        shinyjs::show("filter_active_vars_contents")
        shinyjs::hide("filters_active_count")
        toggle_icon(session$ns("minimise_filter_active"), c("fa-angle-right", "fa-angle-down"), TRUE)
        toggle_title(session$ns("minimise_filter_active"), c("Restore panel", "Minimise Panel"), TRUE)
      })
    }
  )
}

#' @rdname module_active
ui_active_FilteredDataset <- function(id, dataname) {
  checkmate::assert_string(dataname)
  ns <- NS(id)
  span(
    id = id,
    include_css_files("filter-panel"),
    div(
      id = ns("whole_ui"), # to hide it entirely
      fluidRow(
        column(
          width = 8,
          tags$span(dataname, class = "filter_panel_dataname")
        ),
        column(
          width = 4,
          tagList(
            actionLink(
              ns("remove_filters"),
              label = "",
              icon = icon("circle-xmark", lib = "font-awesome"),
              class = "remove pull-right"
            ),
            actionLink(
              ns("collapse"),
              label = "",
              icon = icon("angle-down", lib = "font-awesome"),
              class = "remove pull-right"
            )
          )
        )
      ),
      shinyjs::hidden(
        div(
          id = ns("filter_count_ui"),
          tagList(
            textOutput(ns("filter_count")),
            br()
          )
        )
      ),
      div(
        # id needed to insert and remove UI to filter single variable as needed
        # it is currently also used by the above module to entirely hide this panel
        id = ns("filters"),
        class = "parent-hideable-list-group",
        uiOutput(ns("states"))
      )
    )
  )
}

#' @rdname module_active
srv_active_FilteredDataset <- function(id, x) {
  checkmate::assert_class(x, "FilteredDataset")
  moduleServer(
    id = id,
    function(input, output, session) {
      dataname <- x$get_dataname()
      fs <- x$get_filter_states()
      logger::log_trace("FilteredDataset$server initializing, dataname: { deparse1(dataname) }")
      output$states <- renderUI(
        tagList(
          lapply(
            names(fs),
            function(fs_name) {
              tagList(ui_active_FilterStates(id = session$ns(fs_name)))
            }
          )
        )
      )

      output$filter_count <- renderText(
        sprintf(
          "%d filter%s applied",
          x$get_filter_count(),
          if (x$get_filter_count() != 1) "s" else ""
        )
      )

      lapply(names(fs), function(fs_name) {
          srv_active_FilterStates(id = fs_name, x = fs[[fs_name]])
        }
      )

      shiny::observeEvent(x$get_filter_state(), {
        shinyjs::hide("filter_count_ui")
        shinyjs::show("filters")
        shinyjs::toggle("remove_filters", condition = length(x$get_filter_state()) != 0)
        shinyjs::toggle("collapse", condition = length(x$get_filter_state()) != 0)
      })

      shiny::observeEvent(input$collapse, {
        shinyjs::toggle("filter_count_ui")
        shinyjs::toggle("filters")
        toggle_icon(session$ns("collapse"), c("fa-angle-right", "fa-angle-down"))
      })

      observeEvent(input$remove_filters, {
        logger::log_trace("FilteredDataset$server@1 removing filters, dataname: { deparse1(dataname) }")
        lapply(x$get_filter_states(), function(fs) fs$state_list_empty())
        logger::log_trace("FilteredDataset$server@1 removed filters, dataname: { deparse1(dataname) }")
      })

      logger::log_trace("FilteredDataset$initialized, dataname: { deparse1(dataname) }")
      NULL
    }
  )
}

#' @rdname module_active
ui_active_FilterStates <- function(id) {
  ns <- NS(id)
  tagList(
    include_css_files(pattern = "filter-panel"),
    uiOutput(ns("filter_cards"))
  )
}

#' @rdname module_active
srv_active_FilterStates <- function(id, x) {
  checkmate::assert_class(x, "FilterStates")
  moduleServer(
    id = id,
    function(input, output, session) {
      previous_state <- reactiveVal(character(0))
      added_state_name <- reactiveVal(character(0))
      removed_state_name <- reactiveVal(character(0))

      observeEvent(x$state_list_get(1L), {
        added_state_name(setdiff(names(x$state_list_get(1L)), names(previous_state())))
        previous_state(x$state_list_get(1L))
      })

      observeEvent(
        added_state_name(), # we want to call FilterState module only once when it's added
        ignoreNULL = TRUE,
        {
          fstates <- x$state_list_get(1L)
          for (fname in added_state_name()) {
            srv_card_module(queue_index = 1L, element_id = fname, fs = fstates[[fname]], x = x)
          }
          added_state_name(character(0))
        }
      )

      output$filter_cards <- shiny::renderUI({
        fstates <- x$state_list_get(1L) # rerenders when queue changes / not when the state changes
        datalabel <- x$get_datalabel()
        tags$div(
          class = "list-group hideable-list-group",
          `data-label` = ifelse(datalabel == "", "", datalabel), # todo: labels are not displayed for MAE - see filter-panel.css
          shiny::tagList(
            lapply(
              names(fstates),
              function(fname) {
                ui_card_module(queue_index = session$ns(1L), element_id = fname, fstates[[fname]])
              }
            )
          )
        )
      })
    }
  )
}

#' UI wrapping a single `FilterState`
#'
#' This module contains a single `FilterState` card and remove (from the `ReactiveQueue`) button.
#'
#' parameter queue_index (`character(1)`, `logical(1)`)\cr
#'   index of the `private$queue` list where `ReactiveQueue` are kept.
#' parameter element_id (`character(1)`)\cr
#'   name of `ReactiveQueue` element.
#' return `moduleServer` function which returns `NULL`
#' @keywords internal
ui_card_module <- function(queue_index, element_id, fs, datalabel = "") {
  # id needed to distinguish duplicated var names (element_id) from different slots (queue_index)
  id <- sprintf("%s-%s", queue_index, element_id)
  ns <- NS(id)

  div(
    id = ns("card"),
    class = "list-group-item",
    fs$ui(id = ns("content"))
  )
}

#' Server module for a single `FilterState`
#'
#' Calls server from `FilterState` and observes remove (from the `ReactiveQueue`) button
#' @keywords internal
srv_card_module <- function(queue_index, element_id, fs, x) {
  # id needed to distinguish duplicated var names (element_id) from different slots (queue_index)
  id <- sprintf("%s-%s", queue_index, element_id)
  moduleServer(id, function(input, output, session) {
    fs_callback <- fs$server(id = "content")
    observeEvent(
      eventExpr = fs_callback(),
      ignoreInit = TRUE,
      ignoreNULL = TRUE, # observer should be triggered only if input$remove is true
      once = TRUE, # remove button can be called once, should be destroyed afterwards
      handlerExpr = x$state_list_remove(queue_index, element_id)
    )
  })
}

#' @details
#' Composes id for the FilteredDataset shiny element (active filter vars)
#' @param dataname (`character(1)`) name of the dataset which ui is composed for.
#' @keywords internal
#' @return `character(1)` - `<dataname>_filter`
get_ui_id <- function(dataname) {
  sprintf("%s_filter", dataname)
}
