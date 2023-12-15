#' Filter panel module active
#'
#' Filter panel module active
#' @name module_active
#' @inheritParams filter_panel_methods
#' @inheritSection filter_panel_methods Supported data types
#' @return
#' - `ui_active` returns `shiny.tag`
#' - `srv_active` returns `moduleServer`
NULL

#' @rdname module_active
#' @export
ui_active <- function(id, data, label = character(0)) {
  ui <- UseMethod("ui_active", data)
  if (!inherits(ui, c("shiny.tag", "shiny.tag.list", "character"))) {
    # ui class is checked in case someone register ui_active.<custom_class>
    stop("ui_active must return a shiny.tag or shiny.tag.list or a character")
  }
  ui
}

#' @rdname module_active
#' @export
srv_active <- function(id, data, reactive_state_list, remove_state_callback) {
  UseMethod("srv_active", data)
}

#' @rdname module_active
#' @export
ui_active.default <- function(id, data, label = character(0)) {
  ns <- NS(id)
  if (inherits(data, "MultiAssayExperiment")) {
    ui_active_MultiAssayExperiment(ns("mae"), data, label)
  } else {
    ui_active_array(ns("array"), data, label)
  }
}

#' @rdname default_filter_panel_internals
#' @keywords internal
ui_active_array <- function(id, data, label = character(0)) {
  ns <- NS(id)
  tagList(
    teal.slice:::include_css_files(pattern = "filter-panel"),
    uiOutput(ns("trigger_visible_state_change"), inline = TRUE),
    uiOutput(
      ns("cards"),
      class = "accordion",
      `data-label` = ifelse(length(label), paste0("> ", label), ""),
    )
  )
}

#' @rdname default_filter_panel_internals
#' @keywords internal
ui_active_MultiAssayExperiment <- function(id, data, label) {
  ns <- NS(id)
  tagList(
    ui_active(ns("subjects"), SummarizedExperiment::colData(data), label = "subjects"),
    tagList(
      lapply(
        names(data),
        function(experiment) {
          ui_active(ns(experiment), data[[experiment]], label = experiment)
        }
      )
    )
  )
}

#' @rdname default_filter_panel_internals
#' @keywords internal
srv_active.default <- function(id, data, reactive_state_list, remove_state_callback) {
  if (inherits(data, "MultiAssayExperiment")) {
    srv_active_MultiAssayExperiment("mae", data, reactive_state_list, remove_state_callback)
  } else {
    srv_active_array("array", data, reactive_state_list, remove_state_callback)
  }
}

#' @rdname default_filter_panel_internals
#' @keywords internal
srv_active_array <- function(id, data, reactive_state_list, remove_state_callback) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_active.default initializing")
    output$filter_count <- renderText({
      sprintf(
        "%d filter%s applied",
        length(reactive_state_list()),
        if (length(reactive_state_list()) != 1) "s" else ""
      )
    })

    current_state <- reactive(reactive_state_list())
    previous_state <- reactiveVal(NULL) # FilterState list
    added_states <- reactiveVal(NULL) # FilterState list

    # gives a valid shiny ns based on a default slice id
    fs_to_shiny_ns <- function(x) {
      checkmate::assert_multi_class(x, c("FilterState", "FilterStateExpr"))
      gsub("[^[:alnum:]]+", "_", get_default_slice_id(x$get_state()))
    }

    output$trigger_visible_state_change <- renderUI({
      current_state()
      isolate({
        logger::log_trace("srv_active.default@1 determining added and removed filter states")
        # Be aware this returns a list because `current_state` is a list and not `teal_slices`.
        new_states <- setdiff_teal_slices(current_state(), previous_state())
        if (length(new_states) > 0L) {
          added_states(new_states)
        }

        previous_state(current_state())
        NULL
      })
    })

    output[["cards"]] <- shiny::renderUI({
      lapply(
        current_state(), # observes only if added/removed
        function(state) {
          shiny::isolate( # isolates when existing state changes
            state$ui(id = session$ns(fs_to_shiny_ns(state)), parent_id = session$ns("cards"))
          )
        }
      )
    })

    observeEvent(
      added_states(), # we want to call FilterState module only once when it's added
      ignoreNULL = TRUE,
      {
        added_state_names <- vapply(added_states(), function(x) x$get_state()$id, character(1L))
        logger::log_trace("FilterStates$srv_active@2 triggered by added states: { toString(added_state_names) }")
        lapply(added_states(), function(state) {
          fs_callback <- state$server(id = fs_to_shiny_ns(state))
          observeEvent(
            once = TRUE, # remove button can be called once, should be destroyed afterwards
            ignoreInit = TRUE, # ignoreInit: should not matter because we destroy the previous input set of the UI
            eventExpr = fs_callback(), # when remove button is clicked in the FilterState ui
            handlerExpr = remove_state_callback(teal_slices(state$get_state()))
          )
        })
        added_states(NULL)
      }
    )
  })
}

#' @rdname default_filter_panel_internals
#' @keywords internal
srv_active_MultiAssayExperiment <- function(id, data, reactive_state_list, remove_state_callback) {
  moduleServer(id, function(input, output, session) {
    logger::log_trace("srv_active.MultiAssayExperiment initializing")

    reactive_subject_state_list <- reactive({
      Filter(
        function(state) is.null(state$get_state()$experiment),
        reactive_state_list()
      )
    })
    srv_active("subjects", SummarizedExperiment::colData(data), reactive_subject_state_list, remove_state_callback)

    lapply(
      names(data),
      function(experiment) {
        reactive_experiment_state_list <- reactive({
          Filter(
            function(state) identical(state$get_state()$experiment, experiment),
            reactive_state_list()
          )
        })
        srv_active(experiment, data[[experiment]], reactive_experiment_state_list, remove_state_callback)
      }
    )
  })
}
