#' @title `FilterStates` R6 class
#'
#' @description
#' Implements [ReactiveQueue] to the `teal` filters.
#' Class manages adding and removing `FilterState` to the reactive
#' queue and returns reproducible R expression relevant to specific
#' `FilterStates` subclass.
#' Currently `data.frame`, `MultiAssayExperiment`,
#' `SummarizedExperiment` and `matrix` are available.
#'
#' @keywords internal
#'
#' @examples
#' library(shiny)
#' filter_states <- teal.slice:::DFFilterStates$new(
#'   input_dataname = "data",
#'   output_dataname = "data_filtered",
#'   varlabels = c(x = "x variable", SEX = "Sex"),
#'   datalabel = character(0),
#'   keys = character(0)
#' )
#' filter_state <- teal.slice:::RangeFilterState$new(
#'   c(NA, Inf, seq(1:10)),
#'   varname = "x",
#'   varlabel = "x variable",
#'   input_dataname = as.name("data"),
#'   extract_type = "list"
#' )
#' isolate(filter_state$set_selected(c(3L, 8L)))
#'
#' filter_states$queue_push(
#'   x = filter_state,
#'   queue_index = 1L,
#'   element_id = "x"
#' )
#' isolate(filter_states$get_call())
FilterStates <- R6::R6Class( # nolint
  classname = "FilterStates",
  public = list(
    #' @description
    #' Initializes this `FilterStates` object.
    #'
    #' @param input_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the data used on lhs of the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #'
    #' @param output_dataname (`character(1)` or `name` or `call`)\cr
    #'   name of the output data on the lhs of the assignment expression.
    #'
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #'
    initialize = function(input_dataname, output_dataname, datalabel) {
      checkmate::assert(
        checkmate::check_class(input_dataname, "call"),
        checkmate::check_class(input_dataname, "name"),
        checkmate::check_string(input_dataname)
      )
      checkmate::assert(
        checkmate::check_class(output_dataname, "call"),
        checkmate::check_class(output_dataname, "name"),
        checkmate::check_string(output_dataname)
      )
      checkmate::assert_character(datalabel, max.len = 1, any.missing = FALSE)

      char_to_name <- function(x) {
        if (is.character(x)) {
          as.name(x)
        } else {
          x
        }
      }

      private$input_dataname <- char_to_name(input_dataname)
      private$output_dataname <- char_to_name(output_dataname)
      private$datalabel <- datalabel
      logger::log_trace("Instantiated { class(self)[1] }, dataname: { deparse1(private$input_dataname) }")
      invisible(self)
    },

    #' @description
    #' Returns the label of the dataset
    #' @return (`character(1)`) the label
    get_datalabel = function() {
      private$datalabel
    },

    #' @description
    #' Returns the formatted string representing this `FilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces before each line of the representation
    #' @return `character(1)` the formatted string
    #'
    format = function(indent) {
      stop("Pure virtual method")
    },

    #' @description
    #' Filter call
    #'
    #' Makes a subset function call based on condition calls from `FilterState`
    #' objects selection.
    #' `lhs` of the call is `private$output_dataname` and in `rhs`
    #' `self$get_fun()` with `private$input_dataname` as argument and list of
    #' condition calls from `FilterState`. If input and output data-names
    #' are the same and no filters applied, method returns `NULL` to avoid
    #' no-op call such as `x <- x`.
    #'
    #' @return `call` or `NULL`
    get_call = function() {
      # queue (list) names must be the same as argument of the function
      # for ... list should be unnamed
      queue_list <- private$queue
      filter_items <- sapply(
        X = queue_list,
        USE.NAMES = TRUE,
        simplify = FALSE,
        function(queue) {
          items <- queue$get()
          filtered_items <- Filter(f = function(x) x$is_any_filtered(), x = items)
          calls <- lapply(
            filtered_items,
            function(state) {
              state$get_call()
            }
          )
          if (length(calls) > 0) {
            calls_combine_by(
              operator = "&",
              calls = calls
            )
          }
        }
      )
      filter_items <- Filter(
        x = filter_items,
        f = Negate(is.null)
      )

      if (length(filter_items) > 0) {
        # below code translates to call by the names of filter_items
        rhs <- call_with_colon(
          self$get_fun(),
          private$input_dataname,
          unlist_args = filter_items
        )

        substitute(
          env = list(
            lhs = private$output_dataname,
            rhs = rhs
          ),
          expr = lhs <- rhs
        )
      } else if (!identical(private$output_dataname, private$input_dataname)) {
        substitute(
          env = list(
            lhs = private$output_dataname,
            rhs = private$input_dataname
          ),
          expr = lhs <- rhs
        )
      } else {
        # avoid no-op call
        NULL
      }
    },

    #' @description
    #' Prints this `FilterStates` object
    #'
    #' @param ... additional arguments to this method
    print = function(...) {
      cat(shiny::isolate(self$format()), "\n")
    },

    #' @description
    #' Gets the name of the function used to filter the data in this `FilterStates`.
    #'
    #' Get function name used to create filter call. By default it's a
    #' "subset" but can be overridden by child class method.
    #' @return `character(1)` the name of the function
    get_fun = function() {
      "subset"
    },

    #' @description
    #' Gets the number of active `FilterState` objects in this `FilterStates`.
    #' @return `numeric(1)`
    n_active_filter_states = function() {
      sum(vapply(private$queue, function(queue) {
        queue$size()
      }, FUN.VALUE = numeric(1)))
    },

    #' @description
    #' Remove all `FilterState` objects from all queues in this `FilterStates`.
    #' @return NULL
    queue_empty = function() {
      logger::log_trace("{ class(self)[1] } emptying queue, dataname: { deparse1(private$input_dataname) }")
      queue_indices <- if (is.null(names(private$queue))) {
        seq_along(private$queue)
      } else {
        names(private$queue)
      }

      lapply(queue_indices, function(queue_index) {
        queue_elements <- names(self$queue_get(queue_index = queue_index))
        lapply(queue_elements, function(element_id) {
          self$queue_remove(queue_index = queue_index, element_id = element_id)
        })
      })

      logger::log_trace("{ class(self)[1] } emptied queue, dataname: { deparse1(private$input_dataname) }")
      invisible(NULL)
    },

    #' @description
    #' Returns a list of `FilterState` objects stored in this `FilterStates.`
    #' @param queue_index (`character(1)`, `integer(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' @param element_id (`character(1)`)\cr
    #'   name of `ReactiveQueue` element.
    #' @return `list` of `FilterState` objects
    queue_get = function(queue_index, element_id = character(0)) {
      private$validate_queue_exists(queue_index)
      checkmate::assert_character(element_id, max.len = 1, null.ok = TRUE, any.missing = FALSE)

      if (length(element_id) == 0) {
        private$queue[[queue_index]]$get()
      } else {
        private$queue[[queue_index]]$get()[element_id]
      }
    },

    #' @description
    #' Sets `ReactiveQueue` objects.
    #' @param x (`list` of `ReactiveQueue`)\cr
    #'  must be a list even if single `ReactiveQueue` is set.
    queue_initialize = function(x) {
      checkmate::assert_list(x, types = "ReactiveQueue", min.len = 1)
      private$queue <- x
      invisible(NULL)
    },

    #' @description
    #' Adds a new `FilterState` object to this `FilterStates`
    #' @param x (`FilterState`)\cr
    #'   object to be added to the queue
    #' @param queue_index (`character(1)`, `integer(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' @param element_id (`character(1)`)\cr
    #'   name of the `ReactiveQueue` element.
    #' @note throws an exception if the length of `x` does not match the length of
    #'   `element_id`
    queue_push = function(x, queue_index, element_id) {
      logger::log_trace("{ class(self)[1] } pushing into queue, dataname: { deparse1(private$input_dataname) }")
      private$validate_queue_exists(queue_index)
      checkmate::assert_string(element_id)

      states <- if (is.list(x)) {
        x
      } else {
        list(x)
      }
      state <- setNames(states, element_id)
      private$queue[[queue_index]]$push(state)
      logger::log_trace("{ class(self)[1] } pushed into queue, dataname: { deparse1(private$input_dataname) }")
      invisible(NULL)
    },

    #' @description
    #' Removes a single filter state
    #'
    #' Removes a single filter state with all shiny elements associated
    #' with this state. It removes:\cr
    #' * particular `FilterState` from `private$queue`
    #' * UI card created for this filter
    #' * observers listening selection and remove button
    #' @param queue_index (`character(1)`, `logical(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' @param element_id (`character(1)`)\cr
    #'   name of `ReactiveQueue` element.
    queue_remove = function(queue_index, element_id) {
      logger::log_trace(paste(
        "{ class(self)[1] } removing a filter from queue { queue_index },",
        "dataname: { deparse1(private$input_dataname) }"
      ))
      private$validate_queue_exists(queue_index)
      checkmate::assert_string(element_id)
      checkmate::assert(
        checkmate::check_string(queue_index),
        checkmate::check_int(queue_index)
      )

      filters <- self$queue_get(queue_index = queue_index, element_id = element_id)
      private$queue[[queue_index]]$remove(filters)
      logger::log_trace(
        "{ class(self)[1] } removed from queue { queue_index }, dataname: { deparse1(private$input_dataname) }"
      )
    },

    #' @description
    #' Shiny UI module
    #'
    #' Shiny UI element being a container for `FilterState` elements.
    #' Content of this container is created using `renderUI` in
    #' `server` module
    #' @param id (`character(1)`)\cr
    #'   id of the shiny element
    #' @return shiny.tag
    ui = function(id) {
      ns <- NS(id)
      private$cards_container_id <- ns("cards")
      tagList(
        include_css_files(pattern = "filter-panel"),
        tags$div(
          id = private$cards_container_id,
          class = "list-group hideable-list-group",
          `data-label` = ifelse(private$datalabel == "", "", (paste0("> ", private$datalabel)))
        )
      )
    },

    #' @description
    #' Gets the reactive values from the active `FilterState` objects.
    #'
    #' Get active filter state from the `FilterState` objects kept in `ReactiveQueue`(s).
    #' The output list is a compatible input to `self$set_filter_state`.
    #'
    #' @return `list` containing `list` per each `FilterState` in the `ReactiveQueue`
    get_filter_state = function() {
      stop("Pure virtual method.")
    },

    #' @description
    #' Sets active `FilterState` objects.
    #'
    #' @param data (`data.frame`)\cr
    #'   data which are supposed to be filtered
    #' @param state (`named list`)\cr
    #'   should contain values which are initial selection in the `FilterState`.
    #'   Names of the `list` element should correspond to the name of the
    #'   column in `data`.
    #' @return function which throws an error
    set_filter_state = function(data, state) {
      stop("Pure virtual method.")
    },

    #' @description Remove a single `FilterState` from the `ReactiveQueue`.
    #'
    #' @param element_id (`character`)\cr
    #'  Name of variable to remove its `FilterState`.
    #'
    #' @return `NULL`
    remove_filter_state = function(element_id) {
      stop("This variable can not be removed from the filter.")
    },

    #' @description
    #' Shiny UI module to add filter variable.
    #' @param id (`character(1)`)\cr
    #'  id of shiny module
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'  object containing columns to be used as filter variables.
    #' @return shiny.tag
    ui_add_filter_state = function(id, data) {
      div("This object cannot be filtered")
    },

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'  object containing columns to be used as filter variables.
    #' @param ... ignored
    #' @return `moduleServer` function which returns `NULL`
    srv_add_filter_state = function(id, data, ...) {
      check_ellipsis(..., stop = FALSE)
      moduleServer(
        id = id,
        function(input, output, session) {
          NULL
        }
      )
    }
  ),
  private = list(
    cards_container_id = character(0),
    card_ids = character(0),
    datalabel = character(0),
    input_dataname = NULL, # because it holds object of class name
    output_dataname = NULL, # because it holds object of class name,
    ns = NULL, # shiny ns()
    observers = list(), # observers
    queue = NULL, # list of ReactiveQueue(s) initialized by self$queue_initialize

    #' Module to insert/remove `FilterState` UI
    #'
    #' This module adds the shiny UI of the newly added `FilterState` object to queue to the Active Filter
    #' Variables, calls `FilterState` modules and creates observer to remove state
    #' parameter filter_state (`FilterState`)
    #'
    #' parameter queue_index (`character(1)`, `logical(1)`)\cr
    #'   index of the `private$queue` list where `ReactiveQueue` are kept.
    #' parameter element_id (`character(1)`)\cr
    #'   name of `ReactiveQueue` element.
    #' return `moduleServer` function which returns `NULL`
    insert_filter_state_ui = function(id, filter_state, queue_index, element_id) {
      checkmate::assert_class(filter_state, "FilterState")
      checkmate::assert(
        checkmate::check_int(queue_index),
        checkmate::check_character(queue_index, len = 1),
        combine = "or"
      )
      checkmate::assert_character(element_id, len = 1)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            sprintf(
              "%s$insert_filter_state_ui, adding FilterState UI of variable %s, dataname: %s",
              class(self)[1],
              element_id,
              deparse1(private$input_dataname)
            )
          )
          shiny::setBookmarkExclude("remove")
          card_id <- session$ns("card")
          queue_id <- sprintf("%s-%s", queue_index, element_id)
          private$card_ids[queue_id] <- card_id

          insertUI(
            selector = sprintf("#%s", private$cards_container_id),
            where = "beforeEnd",
            # add span with id to be removable
            ui = {
              div(
                id = card_id,
                class = "list-group-item",
                fluidPage(
                  theme = get_teal_bs_theme(),
                  fluidRow(
                    column(
                      width = 10,
                      class = "no-left-right-padding",
                      tags$div(
                        tags$span(filter_state$get_varname(),
                          class = "filter_panel_varname"
                        ),
                        if (checkmate::test_character(filter_state$get_varlabel(), min.len = 1) &&
                          tolower(filter_state$get_varname()) != tolower(filter_state$get_varlabel())) {
                          tags$span(filter_state$get_varlabel(), class = "filter_panel_varlabel")
                        }
                      )
                    ),
                    column(
                      width = 2,
                      class = "no-left-right-padding",
                      actionLink(
                        session$ns("remove"),
                        label = "",
                        icon = icon("circle-xmark", lib = "font-awesome"),
                        class = "remove pull-right"
                      )
                    )
                  ),
                  filter_state$ui(id = session$ns("content"))
                )
              )
            }
          )
          filter_state$server(id = "content")
          private$observers[[queue_id]] <- observeEvent(
            ignoreInit = TRUE,
            ignoreNULL = TRUE,
            eventExpr = input$remove,
            handlerExpr = {
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1 removing FilterState from queue '{ queue_index }',",
                "dataname: { deparse1(private$input_dataname) }"
              ))
              self$queue_remove(queue_index, element_id)
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1 removed FilterState from queue '{ queue_index }',",
                "dataname: { deparse1(private$input_dataname) }"
              ))
            }
          )

          logger::log_trace(
            sprintf(
              "%s$insert_filter_state_ui, added FilterState UI of variable %s, dataname: %s",
              class(self)[1],
              element_id,
              deparse1(private$input_dataname)
            )
          )
          NULL
        }
      )
    },

    # Remove shiny element. Method can be called from reactive session where
    #' `observeEvent` for remove-filter-state is set and also from `FilteredDataset`
    #' level, where shiny-session-namespace is different. That is why it's important
    #' to remove shiny elements from anywhere. In `add_filter_state` `session$ns(NULL)`
    #' is equivalent to `private$ns(queue_index)`.
    #' In addition, an unused reactive is being removed from input:
    #' method searches input for the unique matches with the filter name
    #' and then removes objects constructed with current card id + filter name.
    #'
    remove_filter_state_ui = function(queue_index, element_id, .input) {
      browser()
      queue_id <- sprintf("%s-%s", queue_index, element_id)
      removeUI(selector = sprintf("#%s", private$card_ids[queue_id]))
      private$card_ids <- private$card_ids[names(private$card_ids) != queue_id]
      if (length(private$observers[[queue_id]]) > 0) {
        private$observers[[queue_id]]$destroy()
        private$observers[[queue_id]] <- NULL
      }
      # Remove unused reactive from shiny input (leftover of removeUI).
      # This default behavior may change in the future
      # making this part obsolete.
      prefix = paste0(gsub("cards$", "", private$cards_container_id))
      invisible(
        lapply(
          unique(grep(element_id, names(.input), value = TRUE)),
          function(i) {
            .subset2(.input, "impl")$.values$remove(paste0(prefix, i))
          }
        )
      )
    },
    # Checks if the queue of the given index was initialized in this `FilterStates`
    # @param queue_index (character or integer)
    validate_queue_exists = function(queue_index) {
      checkmate::assert(
        checkmate::check_string(queue_index),
        checkmate::check_int(queue_index)
      )
      if (
        !(
          is.numeric(queue_index) && all(queue_index <= length(private$queue) && queue_index > 0) ||
            is.character(queue_index) && all(queue_index %in% names(private$queue))
        )
      ) {
        stop(
          paste(
            "ReactiveQueue",
            queue_index,
            "has not been initialized in FilterStates object belonging to the dataset",
            private$datalabel
          )
        )
      }
    },

    # Maps the array of strings to sanitized unique HTML ids.
    # @param keys `character` the array of strings
    # @param prefix `character(1)` text to prefix id. Needed in case of multiple
    #  queue objects where keys (variables) might be duplicated across queues
    # @return `list` the mapping
    map_vars_to_html_ids = function(keys, prefix = "") {
      checkmate::assert_character(keys, null.ok = TRUE)
      checkmate::assert_character(prefix, len = 1)
      sanitized_values <- make.unique(gsub("[^[:alnum:]]", perl = TRUE, replacement = "", x = keys))
      sanitized_values <- paste(prefix, "var", sanitized_values, sep = "_")
      stats::setNames(object = sanitized_values, nm = keys)
    }
  )
)
