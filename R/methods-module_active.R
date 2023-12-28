#' Filter panel module active
#'
#' Filter panel module active
#' @inheritParams filter_panel_methods
#' @inheritSection filter_panel_methods Supported data types
#' @param label (`character(1)`)\cr
#'  Label of the dataset in the filter panel. Will be displayed along with dataname in the section with active filters.
#' @param reactive_state_list (`reactive`)\cr
#'  Should return a list of `FilterState` objects.
#' @param remove_state_callback (`function(state)`)\cr
#'  It should be a `FilteredData` function that removes a filter state from the `state_list`. When the remove button
#'  is clicked this method fill be called. `state` argument in the function is a `teal_slices` object containing
#' `teal_slice` to be removed.
#' @return
#' - `ui_active` returns `shiny.tag`
#' - `srv_active` returns `moduleServer`
#'
#' @name module_active
#' @rdname module_active
#'
#' @aliases ui_active
#' @aliases ui_active-ANY-method
#' @aliases ui_active-data.frame-method
#' @aliases ui_active-DataFrame-method
#' @aliases ui_active-matrix-method
#' @aliases ui_active-Matrix-method
#' @aliases ui_active-MultiAssayExperiment-method
#'
#' @aliases srv_active
#' @aliases srv_active-ANY-method
#' @aliases srv_active-data.frame-method
#' @aliases srv_active-DataFrame-method
#' @aliases srv_active-matrix-method
#' @aliases srv_active-Matrix-method
#' @aliases srv_active-MultiAssayExperiment-method
#'
#' @export
#'
# ui_active generic ----
setGeneric("ui_active", function(id, data, label = character(0)) {
  tagList(NULL)
  # include output check somehow
  # ui <- UseMethod("ui_active", data)
  # if (!inherits(ui, c("shiny.tag", "shiny.tag.list", "character"))) {
  #   # ui class is checked in case someone register ui_active.<custom_class>
  #   stop("ui_active must return a shiny.tag or shiny.tag.list or a character")
  # }
  # ui
})

## default method ----
setMethod("ui_active", c(data = "ANY"), function(id, data, label) {
  stop("ui_active not implemented for class ", toString(class(data)), call. = FALSE)
})

## data.frame method ----
setMethod("ui_active", c(data = "data.frame"), function(id, data, label = character(0)) {
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
})

## DataFrame method ----
setMethod("ui_active", c(data = "DataFrame"), function(id, data, label = character(0)) {
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
})

## matrix method ----
setMethod("ui_active", c(data = "matrix"), function(id, data, label = character(0)) {
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
})

## Matrix method ----
setMethod("ui_active", c(data = "Matrix"), function(id, data, label = character(0)) {
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
})

## SummarizedExperiment method ----
setMethod("ui_active", c(data = "SummarizedExperiment"), function(id, data, label = character(0)) {
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
})

## MultiAssayExperiment method ----
setMethod("ui_active", c(data = "MultiAssayExperiment"), function(id, data, label) {
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
})



#' @export
#'
# srv_active generic ----
setGeneric("srv_active", function(id, data, reactive_state_list, remove_state_callback) {
  NULL
})

## default method ----
setMethod(
  "srv_active",
  c(data = "ANY"),
  function(id, data, reactive_state_list, remove_state_callback) {
    stop("srv_active not implemented for class ", toString(class(data)), call. = FALSE)
  })


## data.frame method ----
setMethod(
  "srv_active",
  c(data = "data.frame"),
  function(id, data, reactive_state_list, remove_state_callback) {
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

      current_state_ids <- reactive({
        vapply(current_state(), function(x) x$get_state()$id, character(1L))
      })

      output[["cards"]] <- shiny::bindCache(
        shiny::renderUI({
          logger::log_trace("srv_active.default@2 rendering filter cards")
          lapply(
            current_state(), # observes only if added/removed
            function(state) {
              shiny::isolate( # isolates when existing state changes
                state$ui(id = session$ns(fs_to_shiny_ns(state)), parent_id = session$ns("cards"))
              )
            }
          )
        }),
        current_state_ids()
      )

      observeEvent(
        added_states(), # we want to call FilterState module only once when it's added
        ignoreNULL = TRUE,
        {
          added_state_names <- vapply(added_states(), function(x) x$get_state()$id, character(1L))
          logger::log_trace("srv_active_array@2 triggered by added states: { toString(added_state_names) }")
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
  })

## DataFrame method ----
setMethod(
  "srv_active",
  c(data = "DataFrame"),
  function(id, data, reactive_state_list, remove_state_callback) {
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

      current_state_ids <- reactive({
        vapply(current_state(), function(x) x$get_state()$id, character(1L))
      })

      output[["cards"]] <- shiny::bindCache(
        shiny::renderUI({
          logger::log_trace("srv_active.default@2 rendering filter cards")
          lapply(
            current_state(), # observes only if added/removed
            function(state) {
              shiny::isolate( # isolates when existing state changes
                state$ui(id = session$ns(fs_to_shiny_ns(state)), parent_id = session$ns("cards"))
              )
            }
          )
        }),
        current_state_ids()
      )

      observeEvent(
        added_states(), # we want to call FilterState module only once when it's added
        ignoreNULL = TRUE,
        {
          added_state_names <- vapply(added_states(), function(x) x$get_state()$id, character(1L))
          logger::log_trace("srv_active_array@2 triggered by added states: { toString(added_state_names) }")
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
  })

## matrix method ----
setMethod(
  "srv_active",
  c(data = "matrix"),
  function(id, data, reactive_state_list, remove_state_callback) {
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

      current_state_ids <- reactive({
        vapply(current_state(), function(x) x$get_state()$id, character(1L))
      })

      output[["cards"]] <- shiny::bindCache(
        shiny::renderUI({
          logger::log_trace("srv_active.default@2 rendering filter cards")
          lapply(
            current_state(), # observes only if added/removed
            function(state) {
              shiny::isolate( # isolates when existing state changes
                state$ui(id = session$ns(fs_to_shiny_ns(state)), parent_id = session$ns("cards"))
              )
            }
          )
        }),
        current_state_ids()
      )

      observeEvent(
        added_states(), # we want to call FilterState module only once when it's added
        ignoreNULL = TRUE,
        {
          added_state_names <- vapply(added_states(), function(x) x$get_state()$id, character(1L))
          logger::log_trace("srv_active_array@2 triggered by added states: { toString(added_state_names) }")
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
  })

## Matrix method ----
setMethod(
  "srv_active",
  c(data = "Matrix"),
  function(id, data, reactive_state_list, remove_state_callback) {
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

      current_state_ids <- reactive({
        vapply(current_state(), function(x) x$get_state()$id, character(1L))
      })

      output[["cards"]] <- shiny::bindCache(
        shiny::renderUI({
          logger::log_trace("srv_active.default@2 rendering filter cards")
          lapply(
            current_state(), # observes only if added/removed
            function(state) {
              shiny::isolate( # isolates when existing state changes
                state$ui(id = session$ns(fs_to_shiny_ns(state)), parent_id = session$ns("cards"))
              )
            }
          )
        }),
        current_state_ids()
      )

      observeEvent(
        added_states(), # we want to call FilterState module only once when it's added
        ignoreNULL = TRUE,
        {
          added_state_names <- vapply(added_states(), function(x) x$get_state()$id, character(1L))
          logger::log_trace("srv_active_array@2 triggered by added states: { toString(added_state_names) }")
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
  })

## SummarizedExperiment method ----
setMethod(
  "srv_active",
  c(data = "SummarizedExperiment"),
  function(id, data, reactive_state_list, remove_state_callback) {
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

      current_state_ids <- reactive({
        vapply(current_state(), function(x) x$get_state()$id, character(1L))
      })

      output[["cards"]] <- shiny::bindCache(
        shiny::renderUI({
          logger::log_trace("srv_active.default@2 rendering filter cards")
          lapply(
            current_state(), # observes only if added/removed
            function(state) {
              shiny::isolate( # isolates when existing state changes
                state$ui(id = session$ns(fs_to_shiny_ns(state)), parent_id = session$ns("cards"))
              )
            }
          )
        }),
        current_state_ids()
      )

      observeEvent(
        added_states(), # we want to call FilterState module only once when it's added
        ignoreNULL = TRUE,
        {
          added_state_names <- vapply(added_states(), function(x) x$get_state()$id, character(1L))
          logger::log_trace("srv_active_array@2 triggered by added states: { toString(added_state_names) }")
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
  })

## MultiAssayExperiment method ----
setMethod(
  "srv_active",
  c(data = "MultiAssayExperiment"),
  function(id, data, reactive_state_list, remove_state_callback) {
    moduleServer(id, function(input, output, session) {
      logger::log_trace("srv_active_MultiAssayExperiment initializing")

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
  })



# utils ----

# gives a valid shiny ns based on a default slice id
#' @keywords internal
fs_to_shiny_ns <- function(x) {
  checkmate::assert_multi_class(x, c("FilterState", "FilterStateExpr"))
  gsub("[^[:alnum:]]+", "_", get_default_slice_id(x$get_state()))
}
