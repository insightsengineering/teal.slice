#' @title `FilterStates` subclass for matrices
#' @description Handles filter states in a `matrix`
#' @keywords internal
#'
#'
MatrixFilterStates <- R6::R6Class( # nolint
  classname = "MatrixFilterStates",
  inherit = FilterStates,

  # public methods ----
  public = list(
    #' @description Initialize `MatrixFilterStates` object
    #'
    #' Initialize `MatrixFilterStates` object
    #'
    #' @param data (`matrix`)\cr
    #'   the R object which `subset` function is applied on.
    #' @param data_reactive (`function(sid)`)\cr
    #'   should return a `matrix` object or `NULL`.
    #'   This object is needed for the `FilterState` counts being updated
    #'   on a change in filters. If function returns `NULL` then filtered counts are not shown.
    #'   Function has to have `sid` argument being a character.
    #' @param dataname (`character(1)`)\cr
    #'   name of the data used in the expression
    #'   specified to the function argument attached to this `FilterStates`.
    #' @param datalabel (`character(0)` or `character(1)`)\cr
    #'   text label value.
    #'
    initialize = function(data,
                          data_reactive = function(sid = "") NULL,
                          dataname,
                          datalabel = character(0)) {
      checkmate::assert_matrix(data)
      super$initialize(data, data_reactive, dataname, datalabel)
    },

    # shiny modules ----

    #' @description
    #' Shiny server module to add filter variable
    #'
    #' Module controls available choices to select as a filter variable.
    #' Selected filter variable is being removed from available choices.
    #' Removed filter variable gets back to available choices.
    #'
    #' @param id (`character(1)`)\cr
    #'   shiny module instance id
    #'
    #' @return `moduleServer` function which returns `NULL`
    srv_add = function(id) {
      data <- private$data
      data_reactive <- private$data_reactive
      moduleServer(
        id = id,
        function(input, output, session) {
          logger::log_trace(
            "MatrixFilterStates$srv_add initializing, dataname: { private$dataname }"
          )

          # available choices to display
          avail_column_choices <- reactive({
            active_filter_vars <- slices_field(self$get_filter_state(), "varname")
            choices <- setdiff(
              get_supported_filter_varnames(data = data),
              active_filter_vars
            )
            data_choices_labeled(
              data = data,
              choices = choices,
              varlabels = character(0),
              keys = NULL
            )
          })
          observeEvent(
            avail_column_choices(),
            ignoreNULL = TRUE,
            handlerExpr = {
              logger::log_trace(paste(
                "MatrixFilterStates$srv_add@1 updating column choices,",
                "dataname: { private$dataname }"
              ))
              if (length(avail_column_choices()) < 0) {
                shinyjs::hide("var_to_add")
              } else {
                shinyjs::show("var_to_add")
              }
              teal.widgets::updateOptionalSelectInput(
                session,
                "var_to_add",
                choices = avail_column_choices()
              )
              logger::log_trace(paste(
                "MatrixFilterStates$srv_add@1 updated column choices,",
                "dataname: { private$dataname }"
              ))
            }
          )

          observeEvent(
            eventExpr = input$var_to_add,
            handlerExpr = {
              logger::log_trace(
                sprintf(
                  "MatrixFilterState$srv_add@2 adding FilterState of variable %s, dataname: %s",
                  deparse1(input$var_to_add),
                  private$dataname
                )
              )
              varname <- input$var_to_add
              self$set_filter_state(filter_settings(filter_var(private$dataname, varname)))

              logger::log_trace(
                sprintf(
                  "MatrixFilterState$srv_add@2 added FilterState of variable %s, dataname: %s",
                  deparse1(varname),
                  private$dataname
                )
              )
            }
          )

          logger::log_trace(
            "MatrixFilterStates$srv_add initialized, dataname: { private$dataname }"
          )
          NULL
        }
      )
    }
  ),
  private = list(
    extract_type = "matrix"
  )
)
