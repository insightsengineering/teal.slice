# DefaultFilteredDataset ------
#' @title The `DefaultFilteredDataset` R6 class
#' @keywords internal
#' @examples
#' library(shiny)
#' ds <- teal.slice:::DefaultFilteredDataset$new(iris, "iris")
#' ds$set_filter_state(
#'   filter_settings(
#'     filter_var(dataname = "iris", varname = "Species", selected = "virginica"),
#'     filter_var(dataname = "iris", varname = "Petal.Length", selected = c(2.0, 5))
#'   )
#' )
#' isolate(ds$get_filter_state())
#' isolate(ds$get_call())
DefaultFilteredDataset <- R6::R6Class( # nolint
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,
  public = list(

    #' @description
    #' Initializes this `DefaultFilteredDataset` object
    #'
    #' @param dataset (`data.frame`)\cr
    #'  single data.frame for which filters are rendered
    #' @param dataname (`character`)\cr
    #'  A given name for the dataset it may not contain spaces
    #' @param keys optional, (`character`)\cr
    #'   Vector with primary keys
    #' @param parent_name (`character(1)`)\cr
    #'   Name of the parent dataset
    #' @param parent (`reactive`)\cr
    #'   object returned by this reactive is a filtered `data.frame` from other `FilteredDataset`
    #'   named `parent_name`. Consequence of passing `parent` is a `reactive` link which causes
    #'   causing re-filtering of this `dataset` based on the changes in `parent`.
    #' @param join_keys (`character`)\cr
    #'   Name of the columns in this dataset to join with `parent`
    #'   dataset. If the column names are different if both datasets
    #'   then the names of the vector define the `parent` columns.
    #'
    #' @param label (`character`)\cr
    #'   Label to describe the dataset
    #' @param metadata (named `list` or `NULL`) \cr
    #'   Field containing metadata about the dataset. Each element of the list
    #'   should be atomic and length one.
    initialize = function(dataset,
                          dataname,
                          keys = character(0),
                          parent_name = character(0),
                          parent = NULL,
                          join_keys = character(0),
                          label = character(0),
                          metadata = NULL) {
      checkmate::assert_data_frame(dataset)
      super$initialize(dataset, dataname, keys, label, metadata)

      # overwrite filtered_data if there is relationship with parent dataset
      if (!is.null(parent)) {
        checkmate::assert_character(parent_name, len = 1)
        checkmate::assert_character(join_keys, min.len = 1)

        private$parent_name <- parent_name
        private$join_keys <- join_keys

        private$data_filtered_fun <- function(sid = "") {
          checkmate::assert_character(sid)
          if (identical(sid, integer(0))) {
            logger::log_trace("filtering data dataname: { private$dataname }")
          } else {
            logger::log_trace("filtering data dataname: { dataname }, sid: { sid }")
          }
          env <- new.env(parent = parent.env(globalenv()))
          env[[dataname]] <- private$data
          env[[parent_name]] <- parent()
          filter_call <- self$get_call(sid)
          eval_expr_with_msg(filter_call, env)
          get(x = dataname, envir = env)
        }
      }

      # todo: Should we make these defaults? It could be handled by the app developer
      if (!is.null(parent)) {
        fs <- filter_settings(
          exclude_varnames = structure(
            list(intersect(colnames(dataset), colnames(isolate(parent())))),
            names = private$dataname
          )
        )
        self$set_filter_state(fs)
      }

      invisible(self)
    },

    #' @description
    #' Gets the filter expression
    #'
    #' This functions returns filter calls equivalent to selected items
    #' within each of `filter_states`. Configuration of the calls is constant and
    #' depends on `filter_states` type and order which are set during initialization.
    #' This class contains single `FilterStates`
    #' which contains single `state_list` and all `FilterState` objects
    #' applies to one argument (`...`) in `dplyr::filter` call.
    #'
    #' @param sid (`character`)\cr
    #'  when specified then method returns code containing filter conditions of
    #'  `FilterState` objects which `"sid"` attribute is different than this `sid` argument.
    #'
    #' @return filter `call` or `list` of filter calls
    get_call = function(sid = "") {
      logger::log_trace("FilteredDatasetDefault$get_call initializing for dataname: { private$dataname }")
      dataname <- private$dataname
      parent_dataname <- private$parent_name

      states <- private$state_list_get()
      logical_calls <- Filter(
        Negate(is.null),
        lapply(states, function(state)  state$get_call())
      )

      filter_call <- if (length(logical_calls)) {
        substitute(
          dataname <- dplyr::filter(dataname, expr),
          list(
            dataname = str2lang(dataname),
            expr = calls_combine_by(logical_calls, operator = "&")
          )
        )
      }

      if (!identical(parent_dataname, character(0))) {
        join_keys <- private$join_keys
        parent_keys <- names(join_keys)
        dataset_keys <- unname(join_keys)

        y_arg <- if (length(parent_keys) == 0L) {
          parent_dataname
        } else {
          sprintf(
            "%s[, c(%s), drop = FALSE]",
            parent_dataname,
            toString(dQuote(parent_keys, q = FALSE))
          )
        }

        more_args <- if (length(parent_keys) == 0 || length(dataset_keys) == 0) {
          list()
        } else if (identical(parent_keys, dataset_keys)) {
          list(by = parent_keys)
        } else {
          list(by = stats::setNames(parent_keys, dataset_keys))
        }

        merge_call <- call(
          "<-",
          as.name(dataname),
          as.call(
            c(
              str2lang("dplyr::inner_join"),
              x = as.name(dataname),
              y = str2lang(y_arg),
              more_args
            )
          )
        )

        filter_call <- c(filter_call, merge_call)
      }
      logger::log_trace("FilteredDatasetDefault$get_call initializing for dataname: { private$dataname }")
      filter_call
    },

    #' @description
    #' Shiny server module to add filter variable.
    #'
    #' This module controls available choices to select as a filter variable.
    #' Once selected, a variable is removed from available choices.
    #' Removing a filter variable adds it back to available choices.
    #'
    #' @param id (`character(1)`)\cr
    #'   an ID string that corresponds with the ID used to call the module's UI function.
    #'
    #' @return `moduleServer` function which returns `NULL`
    srv_add = function(id) {
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace("FilterStates$srv_add initializing, dataname: { private$dataname }")

          output$inputs <- renderUI({
            div(
              teal.widgets::optionalSelectInput(
                session$ns("var_to_add"),
                choices = colnames(data),
                options = shinyWidgets::pickerOptions(
                  liveSearch = TRUE,
                  noneSelectedText = "Select variable to filter"
                )
              )
            )
          })

          # available choices to display
          avail_column_choices <- reactive({
            data <- private$data
            vars_include <- private$get_filterable_varnames()
            active_filter_vars <- slices_field(self$get_filter_state(), "varname")
            choices <- setdiff(vars_include, active_filter_vars)
            varlabels <- get_varlabels(data)

            data_choices_labeled(
              data = data,
              choices = choices,
              varlabels = varlabels,
              keys = private$keys
            )
          })

          observeEvent(
            avail_column_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(
                "FilterStates$srv_add@1 updating available column choices, dataname: { private$dataname }"
              )
              if (is.null(avail_column_choices())) {
                shinyjs::hide("var_to_add")
              } else {
                shinyjs::show("var_to_add")
              }
              teal.widgets::updateOptionalSelectInput(
                session,
                "var_to_add",
                choices = avail_column_choices()
              )
              logger::log_trace(
                "FilterStates$srv_add@1 updated available column choices, dataname: { private$dataname }"
              )
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "FilterStates$srv_add@2 adding FilterState of variable %s, dataname: %s",
                  input$var_to_add,
                  private$dataname
                )
              )

              self$set_filter_state(
                filter_settings(
                  filter_var(dataname = private$dataname, varname = input$var_to_add)
                )
              )
              logger::log_trace(
                sprintf(
                  "FilterStates$srv_add@2 added FilterState of variable %s, dataname: %s",
                  input$var_to_add,
                  private$dataname
                )
              )
            }
          )

          logger::log_trace("FilterStates$srv_add initialized, dataname: { private$dataname }")
          NULL
        }
      )
    }
  ),
  private = list(
    parent_name = character(0),
    join_keys = character(0)
  )
)
