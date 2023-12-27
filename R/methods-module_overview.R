#' Server function to display the number of records in the filtered and unfiltered data
#'
#' @param id (`character(1)`)\cr
#'   an ID string that corresponds with the ID used to call the module's UI function.
#' @param active_datanames (`reactive`)\cr
#'   returning `datanames` that should be shown on the filter panel,
#'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
#'   if the function returns `NULL` (as opposed to `character(0)`), the filter
#'   panel will be hidden.
#' @return `moduleServer` function which returns `NULL`
#'
#' @name module_overview_data
#' @rdname module_overview_data
#'
#' @aliases get_filter_overview
#' @aliases get_filter_overview-ANY-method
#' @aliases get_filter_overview-data.frame-method
#' @aliases get_filter_overview-DataFrame-method
#' @aliases get_filter_overview-array-method
#' @aliases get_filter_overview-Matrix-method
#' @aliases get_filter_overview-SummarizedExperiment-method
#' @aliases get_filter_overview-MultiAssayExperiment-method
#'
#' @keywords internal
ui_overview_data <- function(id, filtered_data) {
  ns <- NS(id)
  div(
    id = id, # not used, can be used to customize CSS behavior
    class = "well",
    tags$div(
      class = "row",
      tags$div(
        class = "col-sm-9",
        tags$label("Active Filter Summary", class = "text-primary mb-4")
      ),
      tags$div(
        class = "col-sm-3",
        actionLink(
          ns("minimise_filter_overview"),
          label = NULL,
          icon = icon("angle-down", lib = "font-awesome"),
          title = "Minimise panel",
          class = "remove pull-right"
        )
      )
    ),
    div(
      id = ns("filters_overview_contents"),
      div(
        class = "teal_active_summary_filter_panel",
        tableOutput(ns("table"))
      )
    )
  )
}

#' @rdname module_overview_data
#' @keywords internal
srv_overview_data <- function(id, filtered_data, active_datanames = filtered_data$datanames) {
  checkmate::assert_class(active_datanames, "reactive")
  moduleServer(
    id = id,
    function(input, output, session) {
      logger::log_trace("FilteredData$srv_filter_overview initializing")

      shiny::observeEvent(input$minimise_filter_overview, {
        shinyjs::toggle("filters_overview_contents")
        toggle_icon(session$ns("minimise_filter_overview"), c("fa-angle-right", "fa-angle-down"))
        toggle_title(session$ns("minimise_filter_overview"), c("Restore panel", "Minimise Panel"))
      })

      output$table <- renderUI({
        logger::log_trace("FilteredData$srv_filter_overview@1 updating counts")
        if (length(active_datanames()) == 0) {
          return(NULL)
        }

        rows <- lapply(
          active_datanames(),
          function(dataname) {
            data_unfiltered <- filtered_data$get_data(dataname, filtered = FALSE)
            data_filtered <- filtered_data$get_data(dataname, filtered = TRUE)
            get_filter_overview(data_unfiltered, data_filtered, dataname)
          }
        )

        unssuported_idx <- vapply(rows, function(x) all(is.na(x[-1])), logical(1))
        datasets_df <- dplyr::bind_rows(c(rows[!unssuported_idx], rows[unssuported_idx]))

        if (!is.null(datasets_df$obs)) {
          # some datasets (MAE colData) doesn't return obs column
          datasets_df$Obs <- ifelse(
            !is.na(datasets_df$obs),
            sprintf("%s/%s", datasets_df$obs_filtered, datasets_df$obs),
            ""
          )
        }

        if (!is.null(datasets_df$subjects)) {
          # some datasets (without keys) doesn't return subjects
          datasets_df$Subjects <- ifelse(
            !is.na(datasets_df$subjects),
            sprintf("%s/%s", datasets_df$subjects_filtered, datasets_df$subjects),
            ""
          )
        }
        datasets_df <- datasets_df[, colnames(datasets_df) %in% c("dataname", "Obs", "Subjects")]

        body_html <- apply(
          datasets_df,
          1,
          function(x) {
            tags$tr(
              tagList(
                tags$td(
                  if (all(x[-1] == "")) {
                    icon(
                      name = "exclamation-triangle",
                      title = "Unsupported dataset",
                      `data-container` = "body",
                      `data-toggle` = "popover",
                      `data-content` = "object not supported by the filter panel"
                    )
                  },
                  x[1]
                ),
                lapply(x[-1], tags$td)
              )
            )
          }
        )


        header_html <- tags$tr(
          tagList(
            lapply(colnames(datasets_df), tags$td)
          )
        )

        table_html <- tags$table(
          class = "table custom-table",
          tags$thead(header_html),
          tags$tbody(body_html)
        )
        table_html
      })
      NULL
    }
  )
}


#' Get filter overview
#'
#' Summary of the dataset in the filter panel
#' @inheritParams filter_panel_methods
#' @details
#' # Registering custom methods
#' If you want to change a displayed information in the filter panel overview for your custom data type,
#' you can register S3 method `get_filter_overview.<custom_class>`. Output of this function is checked
#' in `teal.slice` and must be a `data.frame` with columns `dataname`, `obs`, `obs_filtered`.
#' @inheritSection filter_panel_methods Supported data types
#'
#' @return `data.frame` with columns `dataname` and usually `obs`, `obs_filtered`.
#'
#' @export
#'
# get_filter_overview generic ----
setGeneric("get_filter_overview", function(data_unfiltered, data_filtered, dataname) {
  data.frame(
    dataname = dataname,
    obs = NA,
    obs_filtered = NA
  )
  # check out put somehow
  # if (!inherits(df, "data.frame") || all(c("dataname", "obs", "obs_filtered") %in% colnames(df))) {
  #   stop("get_filter_overview must return a data.frame with columns `dataname`, `obs`, `obs_filtered`", call. = FALSE)
  # }
})

## data.frame method ----
setMethod(
  "get_filter_overview",
  c(data_unfiltered = "data.frame"),
  function(data_unfiltered, data_filtered, dataname) {
    data.frame(
      dataname = dataname,
      obs = nrow(data_unfiltered),
      obs_filtered = nrow(data_filtered)
    )
  })

## DataFrame method ----
setMethod(
  "get_filter_overview",
  c(data_unfiltered = "DataFrame"),
  function(data_unfiltered, data_filtered, dataname) {
    data.frame(
      dataname = dataname,
      obs = nrow(data_unfiltered),
      obs_filtered = nrow(data_filtered)
    )
  })

## array method ----
setMethod(
  "get_filter_overview",
  c(data_unfiltered = "array"),
  function(data_unfiltered, data_filtered, dataname) {
    data.frame(
      dataname = dataname,
      obs = nrow(data_unfiltered),
      obs_filtered = nrow(data_filtered)
    )
  })

## Matrix method ----
setMethod(
  "get_filter_overview",
  c(data_unfiltered = "Matrix"),
  function(data_unfiltered, data_filtered, dataname) {
    data.frame(
      dataname = dataname,
      obs = nrow(data_unfiltered),
      obs_filtered = nrow(data_filtered)
    )
  })

## SummarizedExperiment method ----
setMethod(
  "get_filter_overview",
  c(data_unfiltered = "SummarizedExperiment"),
  function(data_unfiltered, data_filtered, dataname) {
    data.frame(
      dataname = dataname,
      obs = nrow(data_unfiltered),
      obs_filtered = nrow(data_filtered)
    )
  })

## MultiAssayExperiment method ----
setMethod(
  "get_filter_overview",
  c(data_unfiltered = "MultiAssayExperiment"),
  function(data_unfiltered, data_filtered, dataname) {
    experiment_names <- names(data_unfiltered)
    mae_info <- data.frame(
      dataname = dataname,
      subjects = nrow(SummarizedExperiment::colData(data_unfiltered)),
      subjects_filtered = nrow(SummarizedExperiment::colData(data_filtered))
    )

    experiment_obs_info <- do.call("rbind", lapply(
      experiment_names,
      function(experiment_name) {
        transform(
          get_filter_overview(
            data_unfiltered[[experiment_name]],
            data_filtered[[experiment_name]],
            experiment_name
          ),
          dataname = paste0(" - ", dataname)
        )
      }
    ))

    experiment_subjects_info <- do.call("rbind", lapply(
      experiment_names,
      function(experiment_name) {
        data.frame(
          subjects = get_experiment_keys(data_filtered, data_unfiltered[[experiment_name]]),
          subjects_filtered = get_experiment_keys(data_filtered, data_filtered[[experiment_name]])
        )
      }
    ))

    experiment_info <- cbind(experiment_obs_info, experiment_subjects_info)
    dplyr::bind_rows(mae_info, experiment_info)
  })



# utils ----

#' @keywords internal
get_experiment_keys <- function(mae, experiment) {
  sample_subset <- subset(MultiAssayExperiment::sampleMap(mae), subset = colname %in% colnames(experiment))
  length(unique(sample_subset$primary))
}
