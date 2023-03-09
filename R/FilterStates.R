#' @title `FilterStates` R6 class
#'
#' @description
#' Abstract class that manages adding and removing `FilterState` objects
#' and builds a \emph{subset expression}.
#'
#' A `FilterStates` object tracks all subsetting expressions
#' (logical predicates that limit observations) associated with a given dataset
#' and composes them into a single reproducible R expression
#' that will assign a subset of the original data to a new variable.
#' This expression is hereafter referred to as \emph{subset expression}.
#'
#' The \emph{subset expression} is constructed differently for different
#' classes of the underlying data object and `FilterStates` subclasses.
#' Currently implemented for `data.frame`, `matrix`,
#' `SummarizedExperiment`, and `MultiAssayExperiment`.
#'
#' @keywords internal
#'
#' @examples
#' library(shiny)
#' filter_states <- teal.slice:::DFFilterStates$new(
#'   data = data.frame(x = 1:2, sex = c("F", "M")),
#'   dataname = "data",
#'   varlabels = c(x = "x variable", sex = "Sex"),
#'   datalabel = character(0),
#'   keys = character(0)
#' )
#' filter_states$set_filter_state(
#'   filter_settings(
#'     filter_var(dataname = "data", varname = "x", selected = 1),
#'     filter_var(dataname = "data", varname = "sex", selected = "F")
#'   )
#' )
#' isolate(filter_states$get_filter_state())
#' isolate(filter_states$get_call())
#'
FilterStates <- R6::R6Class( # nolint
  classname = "FilterStates",

  # public members ----
  public = list(
    #' @description
    #' Initializes `FilterStates` object.
    #'
    #' Initializes `FilterStates` object by setting
    #' `dataname`, and `datalabel`.
    #'
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)\cr
    #'   the R object which `subset` function is applied on.
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return an object of the same type as `data` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value
    #' @param excluded_varnames (`character`)\cr
    #'   names of variables that can \strong{not} be filtered on.
    #' @param count_type `character(0-1)`\cr
    #'   specifying how observations are tallied
    #'
    #' @return
    #' self invisibly
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = character(0),
                          excluded_varnames = character(0),
                          count_type = character(0)) {
      checkmate::assert_function(data_reactive, args = "sid")
      checkmate::assert_string(dataname)
      checkmate::assert_character(datalabel, max.len = 1, any.missing = FALSE)

      private$dataname <- dataname
      private$datalabel <- datalabel
      private$data <- data
      private$data_reactive <- data_reactive
      private$filterable_varnames <- setdiff(colnames(data), excluded_varnames)
      private$count_type <- count_type

      logger::log_trace("Instantiated { class(self)[1] }, dataname: { private$dataname }")
      invisible(self)
    },

    #' @description
    #' Returns the label of the dataset.
    #'
    #' @return `character(1)` the data label
    #'
    get_datalabel = function() {
      private$datalabel
    },

    #' @description
    #' Returns a formatted string representing this `FilterStates` object.
    #'
    #' @param indent (`numeric(1)`) the number of spaces prepended to each line of the output
    #'
    #' @return `character(1)` the formatted string
    #'
    format = function(indent) {
      sprintf(
        paste(
          "%sThis is an instance of an abstract class.",
          "Use child class constructors to instantiate objects."
        ),
        paste(rep(" ", indent), collapse = "")
      )
    },

    #' @description
    #' Filter call
    #'
    #' Builds \emph{subset expression} from condition calls stored in `FilterState`
    #' objects selection. The `lhs` of the expression is `private$dataname`.
    #' The `rhs` is a call to `self$get_fun()` with `private$dataname`
    #' as argument and a list of condition calls from `FilterState` objects
    #' stored in `private$state_list`.
    #' If no filters are applied, `NULL` is returned to avoid no-op calls such as `x <- x`.
    #'
    #' @param sid (`character`)\cr
    #'  when specified then method returns code containing filter conditions of
    #'  `FilterState` objects which `"sid"` attribute is different than this `sid` argument.
    #'
    #' @return `call` or `NULL`
    #'
    get_call = function(sid = "") {
      # state_list (list) names must be the same as argument of the function
      # for ... list should be unnamed
      states_list <- private$state_list
      filter_items <- sapply(
        X = states_list,
        USE.NAMES = TRUE,
        simplify = FALSE,
        function(state_list) {
          items <- state_list()

          # removing empty filters and filters identified by sid
          nonempty_filter_idx <- vapply(items, function(x) x$is_any_filtered(), logical(1L))
          other_filter_idx <- vapply(items, function(x) !attr(x, "sid") %in% sid, logical(1L))
          filtered_items <- items[nonempty_filter_idx & other_filter_idx]

          calls <- lapply(
            filtered_items,
            function(state) {
              state$get_call()
            }
          )
          calls_combine_by(calls, operator = "&")
        }
      )
      filter_items <- Filter(
        x = filter_items,
        f = Negate(is.null)
      )
      if (length(filter_items) > 0L) {
        filter_function <- str2lang(self$get_fun())
        data_name <- str2lang(private$dataname)
        substitute(
          env = list(
            lhs = data_name,
            rhs = as.call(c(filter_function, c(list(data_name), filter_items)))
          ),
          expr = lhs <- rhs
        )
      } else {
        # return NULL to avoid no-op call
        NULL
      }
    },

    #' @description
    #' Prints this `FilterStates` object.
    #'
    #' @param ... additional arguments to this method
    print = function(...) {
      cat(shiny::isolate(self$format()), "\n")
    },

    #' @description
    #' Gets the name of the function used to filter the data in this `FilterStates`.
    #'
    #' Get name of function used to create the \emph{subset expression}.
    #' Defaults to "subset" but can be overridden by child class method.
    #'
    #' @return `character(1)` the name of the function
    #'
    get_fun = function() {
      "subset"
    },



    #' @description
    #' Gets the number of active `FilterState` objects in this `FilterStates` object.
    #'
    #' @return `integer(1)`
    #'
    get_filter_count = function() {
      sum(vapply(private$state_list, function(state_list) {
        length(state_list())
      }, FUN.VALUE = integer(1)))
    },

    #' @description Remove a single `FilterState` from `state_list`.
    #'
    #' @param state_id (`character`)\cr
    #'   name of variable for which to remove `FilterState`
    #'
    #' @return `NULL`
    #'
    remove_filter_state = function(state_id) {
      stop("This variable can not be removed from the filter.")
    },

    #' @description
    #' Remove all `FilterState` objects from this `FilterStates` object.
    #'
    #' @return NULL
    #'
    clear_filter_states = function() {
      private$state_list_empty()
    },

    # shiny modules ----

    #' @description
    #' Shiny module UI
    #'
    #' Shiny UI element that stores `FilterState` UI elements.
    #' Populated with elements created with `renderUI` in the module server.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny element (module instance) id
    #'
    #' @return `shiny.tag`
    #'
    ui_active = function(id) {
      ns <- NS(id)
      private$cards_container_id <- ns("cards")
      tagList(
        include_css_files(pattern = "filter-panel"),
        tags$div(
          id = private$cards_container_id,
          class = "panel-group accordion",
          `data-label` = ifelse(private$datalabel == "", "", (paste0("> ", private$datalabel)))
        )
      )
    },

    #' @description
    #' Gets reactive values from active `FilterState` objects.
    #'
    #' Get active filter state from `FilterState` objects stored in `state_list`(s).
    #' The output is a list compatible with input to `self$set_filter_state`.
    #'
    #' @return `list` containing `list` per `FilterState` in the `state_list`
    #'
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
    set_filter_state = function(state) {
      stop("Pure virtual method.")
    },

    #' @description
    #' Shiny module UI that adds a filter variable.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny element (module instance) id
    #'
    #' @param data (`data.frame`, `MultiAssayExperiment`, `SummarizedExperiment`, `matrix`)
    #'   object which columns are used to choose filter variables.
    #' @return `shiny.tag`
    #'
    ui_add = function(id, data) {
      div("This object cannot be filtered")
    },

    #' @description
    #' Shiny module server that adds a filter variable.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny module instance id
    #'
    #' @return `moduleServer` function which returns `NULL`
    #'
    srv_add = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          NULL
        }
      )
    }
  ),
  private = list(
    # private fields ----
    cards_container_id = character(0),
    card_ids = character(0),
    data = NULL, # data.frame, MAE, SE or matrix
    data_reactive = NULL, # reactive
    datalabel = character(0),
    dataname = NULL, # because it holds object of class name
    filterable_varnames = character(0),
    ns = NULL, # shiny ns()
    observers = list(), # observers
    state_list = NULL, # list of `reactiveVal`s initialized by init methods of child classes,
    count_type = character(0), # specifies how observation numbers are displayed in filter cards

    # private methods ----

    # Module to insert/remove `FilterState` UI
    #
    # This module adds the shiny UI of the `FilterState` object newly added
    # to state_list to the Active Filter Variables,
    # calls `FilterState` modules and creates an observer to remove state
    # parameter filter_state (`FilterState`).
    #
    # @param id (`character(1)`)\cr
    #   shiny module instance id
    # @param filter_state (`named list`)\cr
    #   should contain values of initial selections in the `FilterState`;
    #   `list` names must correspond to column names in `data`
    # @param state_list_index (`character(1)`, `integer(1)`)\cr
    #   index on the list in `private$state_list` where filter states are kept
    # @param state_id (`character(1)`)\cr
    #   name of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return `moduleServer` function which returns `NULL`
    #
    insert_filter_state_ui = function(id, filter_state, state_list_index, state_id) {
      checkmate::assert_class(filter_state, "FilterState")
      checkmate::assert(
        checkmate::check_int(state_list_index),
        checkmate::check_character(state_list_index, len = 1),
        combine = "or"
      )
      checkmate::assert_character(state_id, len = 1)
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            sprintf(
              "%s$insert_filter_state_ui, adding FilterState UI of variable %s, dataname: %s",
              class(self)[1],
              state_id,
              private$dataname
            )
          )

          # card_id of inserted card must be saved in private$card_ids as
          # it might be removed by the several events:
          #   - remove button in FilterStates module
          #   - remove button in FilteredDataset module
          #   - remove button in FilteredData module
          #   - API call remove_filter_state
          card_id <- session$ns("card")
          state_list_id <- sprintf("%s-%s", state_list_index, state_id)
          private$card_ids[state_list_id] <- card_id

          insertUI(
            selector = sprintf("#%s", private$cards_container_id),
            where = "beforeEnd",
            ui = filter_state$ui(card_id, private$cards_container_id)
          )
          # signal sent from filter_state when it is marked for removal
          remove_fs <- filter_state$server(id = "card")

          private$observers[[state_list_id]] <- observeEvent(
            ignoreInit = TRUE,
            ignoreNULL = TRUE,
            eventExpr = remove_fs(),
            handlerExpr = {
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1",
                "removing FilterState from state_list '{ state_list_index }',",
                "dataname: { private$dataname }"
              ))
              private$state_list_remove(state_list_index, state_id)
              logger::log_trace(paste(
                "{ class(self)[1] }$insert_filter_state_ui@1",
                "removed FilterState from state_list '{ state_list_index }',",
                "dataname: { private$dataname }"
              ))
            }
          )

          logger::log_trace(
            sprintf(
              "%s$insert_filter_state_ui, added FilterState UI of variable %s, dataname: %s",
              class(self)[1],
              state_id,
              private$dataname
            )
          )
          NULL
        }
      )
    },

    # Remove shiny element. Method can be called from reactive session where
    # `observeEvent` for remove-filter-state is set and also from `FilteredDataset`
    # level, where shiny-session-namespace is different. That is why it's important
    # to remove shiny elements from anywhere. In `add_filter_state` `session$ns(NULL)`
    # is equivalent to `private$ns(state_list_index)`.
    # In addition, an unused reactive is being removed from input:
    # method searches input for the unique matches with the filter name
    # and then removes objects constructed with current card id + filter name.
    #
    remove_filter_state_ui = function(state_list_index, state_id, .input) {
      state_list_id <- sprintf("%s-%s", state_list_index, state_id)
      removeUI(selector = sprintf("#%s", private$card_ids[state_list_id]))
      private$card_ids <- private$card_ids[names(private$card_ids) != state_list_id]
      if (length(private$observers[[state_list_id]]) > 0) {
        private$observers[[state_list_id]]$destroy()
        private$observers[[state_list_id]] <- NULL
      }
      # Remove unused reactive from shiny input (leftover of removeUI).
      # This default behavior may change in the future, making this part obsolete.
      prefix <- paste0(gsub("cards$", "", private$cards_container_id))
      invisible(
        lapply(
          unique(grep(state_id, names(.input), value = TRUE)),
          function(i) {
            .subset2(.input, "impl")$.values$remove(paste0(prefix, i))
          }
        )
      )
    },

    # state_list methods ----

    # @description
    # Returns a list of `FilterState` objects stored in this `FilterStates`.
    #
    # @param state_list_index (`character(1)`, `integer(1)`)\cr
    #   index on the list in `private$state_list` where filter states are kept
    # @param state_id (`character(1)`)\cr
    #   name of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return `list` of `FilterState` objects
    #
    state_list_get = function(state_list_index, state_id = NULL) {
      private$validate_state_list_exists(state_list_index)
      checkmate::assert_string(state_id, null.ok = TRUE)

      if (is.null(state_id)) {
        private$state_list[[state_list_index]]()
      } else {
        private$state_list[[state_list_index]]()[[state_id]]
      }
    },

    # @description
    # Adds a new `FilterState` object to this `FilterStates`.\cr
    # Raises error if the length of `x` does not match the length of `state_id`.
    #
    # @param x (`FilterState`)\cr
    #   object to be added to filter state list
    # @param state_list_index (`character(1)`, `integer(1)`)\cr
    #   index on the list in `private$state_list` where filter states are kept
    # @param state_id (`character(1)`)\cr
    #   name of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return NULL
    #
    state_list_push = function(x, state_list_index, state_id) {
      logger::log_trace("{ class(self)[1] } pushing into state_list, dataname: { private$dataname }")
      private$validate_state_list_exists(state_list_index)
      checkmate::assert_string(state_id)
      checkmate::assert_class(x, "FilterState")

      state <- stats::setNames(list(x), state_id)
      new_state_list <- c(
        shiny::isolate(private$state_list[[state_list_index]]()),
        state
      )
      shiny::isolate(private$state_list[[state_list_index]](new_state_list))

      logger::log_trace("{ class(self)[1] } pushed into queue, dataname: { private$dataname }")
      invisible(NULL)
    },

    # @description
    # Removes a single filter state with all associated shiny elements:\cr
    # * specified `FilterState` from `private$state_list`
    # * UI card created for this filter
    # * observers tracking the selection and remove button
    #
    # @param state_list_index (`character(1)`, `integer(1)`)\cr
    #   index on the list in `private$state_list` where filter states are kept
    # @param state_id (`character`)\cr
    #   names of element in a filter state (which is a `reactiveVal` containing a list)
    #
    # @return NULL
    #
    state_list_remove = function(state_list_index, state_id) {
      logger::log_trace(
        "{ class(self)[1] } removing a filter from state_list: { state_list_index }; dataname: { private$dataname }"
      )
      private$validate_state_list_exists(state_list_index)
      checkmate::assert_vector(state_id, len = 1)
      checkmate::assert(
        checkmate::check_string(state_list_index),
        checkmate::check_int(state_list_index)
      )

      new_state_list <- shiny::isolate(private$state_list[[state_list_index]]())
      new_state_list[[state_id]]$destroy_observers()
      new_state_list[[state_id]] <- NULL
      shiny::isolate(private$state_list[[state_list_index]](new_state_list))

      logger::log_trace(
        "{ class(self)[1] } removed from state_list: { state_list_index }; dataname: { private$dataname }"
      )
      invisible(NULL)
    },

    # @description
    # Remove all `FilterState` objects from this `FilterStates` object.
    #
    # @return NULL
    #
    state_list_empty = function() {
      logger::log_trace("{ class(self)[1] }$state_list_empty initializing for dataname: { private$dataname }")

      for (state_list_index in seq_along(private$state_list)) {
        state_list_i <- shiny::isolate(private$state_list[[state_list_index]]())
        for (state_id in names(state_list_i)) {
          private$state_list_remove(state_list_index, state_id)
        }
      }

      logger::log_trace("{ class(self)[1] }$state_list_empty done for dataname: { private$dataname }")
      invisible(NULL)
    },

    # @description
    # Set filter state
    #
    # Utility method for `set_filter_state` to create or modify `FilterState` from a single
    #  `state_list_index`.
    # @param state (`list`)
    #   should contain values which are initial selection in the `FilterState`.
    #   Names of the `list` element should correspond to the name of the columns.
    # @param state_list_index (`vector(1)`)
    #  index of the `state_list`
    # @param data (`data.frame`, `matrix` or `DataFrame`)
    # @param data_reactive (`function`)
    #  function having `sid` as argument
    # @param extract_type (`character(0)` or `chracter(1)`)
    set_filter_state_impl = function(state,
                                     state_list_index,
                                     data,
                                     data_reactive,
                                     extract_type = character(0),
                                     na_rm = FALSE) {
      checkmate::assert(
        checkmate::check_list(state, null.ok = TRUE, names = "named"),
        checkmate::check_class(state, "teal_slices")
      )
      if (is.teal_slices(state)) {
        checkmate::assert_scalar(state_list_index)
        checkmate::assert_multi_class(data, c("data.frame", "matrix", "DataFrame"))
        checkmate::assert_function(data_reactive, args = "sid")

        lapply(state, function(x) {
          # objects has random and unique sid attribute
          # which allows a reactive from below to find a right object in the state_list
          sid <- sprintf("%s-%s-%s", state_list_index, x$varname, sample.int(size = 1L, n = .Machine$integer.max))
          fstate <- init_filter_state(
            x = data[, x$varname, drop = TRUE],
            x_reactive = reactive(data_reactive(sid)[, x$varname, drop = TRUE]),
            dataname = x$dataname,
            varname = x$varname,
            choices = x$choices,
            selected = x$selected,
            varlabel = x$varlabel,
            keep_na = x$keep_na,
            fixed = x$fixed,
            extract_type = extract_type
          )
          attr(fstate, "sid") <- sid
          private$state_list_push(x = fstate, state_list_index = state_list_index, state_id = x$varname)
        })
      } else {
        warning(paste(
          "From FilterStates:",
          "Specifying filters as lists is obsolete and will be deprecated in the next release.",
          "Please see ?set_filter_state and ?filter_settings for details."
        ),
        call. = FALSE)
        checkmate::assert_scalar(state_list_index)
        checkmate::assert_multi_class(data, c("data.frame", "matrix", "DataFrame"))
        checkmate::assert_function(data_reactive, args = "sid")
        checkmate::assert(
          checkmate::check_subset(names(state), colnames(data)),
          checkmate::check_class(state, "default_filter"),
          combine = "or"
        )
        # add new states and modify existing:
        # - modify existing states
        states_now <- shiny::isolate(private$state_list_get(state_list_index))
        state_names_existing <- intersect(names(states_now), names(state))
        mapply(
          fstate = states_now[state_names_existing],
          value = state[state_names_existing],
          function(fstate, value) fstate$set_state(resolve_state(value))
        )

        # - add new states
        max_id <- max(0L, unlist(lapply(states_now, attr, "sid")))
        state_names_new <- setdiff(names(state), names(states_now))
        mapply(
          varname = state_names_new,
          value = state[state_names_new],
          function(varname, value) {
            # objects has random and unique sid attribute
            #  which allows a reactive from below to find a right object in the state_list
            sid <- sprintf("%s-%s-%s", state_list_index, varname, sample.int(size = 1L, n = .Machine$integer.max))
            fstate <- init_filter_state(
              x = if (!is.array(data)) {
                data[[varname]]
              } else {
                data[, varname]
              },

              # data_reactive is a function which eventually calls get_call(sid).
              # This chain of calls returns column from the data filtered by everything
              # but filter identified by the sid argument. FilterState then get x_reactive
              # and this no longer needs to be a function to pass sid. reactive in the FilterState
              # is also beneficial as it can be cached and retriger filter counts only if
              # returned vector is different.
              x_reactive = if (!is.array(data)) {
                reactive(data_reactive(sid)[[varname]])
              } else {
                reactive(data_reactive(sid)[, varname])
              },
              varname = varname,
              dataname = private$dataname,
              extract_type = extract_type
            )
            attr(fstate, "sid") <- sid
            fstate$set_state(resolve_state(value))
            if (na_rm) fstate$set_na_rm(na_rm)
            private$state_list_push(x = fstate, state_list_index = state_list_index, state_id = varname)
          }
        )
      }
    },

    # Checks if the state_list of the given index was initialized in this `FilterStates`
    # @param state_list_index (character or integer)
    validate_state_list_exists = function(state_list_index) {
      checkmate::assert(
        checkmate::check_string(state_list_index),
        checkmate::check_int(state_list_index)
      )
      if (
        !(
          is.numeric(state_list_index) &&
            all(state_list_index <= length(private$state_list) && state_list_index > 0) ||
            is.character(state_list_index) && all(state_list_index %in% names(private$state_list))
        )
      ) {
        stop(
          paste(
            "Filter state list",
            state_list_index,
            "has not been initialized in FilterStates object belonging to the dataset",
            private$datalabel
          )
        )
      }
    },

    # Maps the array of strings to sanitized unique HTML ids.
    # @param keys `character` the array of strings
    # @param prefix `character(1)` text to prefix id. Needed in case of multiple
    #  state_list objects where keys (variables) might be duplicated across state_lists
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
