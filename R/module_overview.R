#' Server function to display the number of records in the filtered and unfiltered
#' data
#'
#' @name module_overview_data
#' @param id (`character(1)`)\cr
#'   an ID string that corresponds with the ID used to call the module's UI function.
#' @param active_datanames (`reactive`)\cr
#'   returning `datanames` that should be shown on the filter panel,
#'   must be a subset of the `datanames` argument provided to `ui_filter_panel`;
#'   if the function returns `NULL` (as opposed to `character(0)`), the filter
#'   panel will be hidden.
#' @return `moduleServer` function which returns `NULL`
NULL

#' @rdname module_overview_data
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
          datasets_df <- transform(
            datasets_df,
            Obs = ifelse(
              !is.na(obs),
              sprintf("%s/%s", obs_filtered, obs),
              ""
            )
          )
        }

        if (!is.null(datasets_df$subjects)) {
          # some datasets (without keys) doesn't return subjects
          datasets_df <- transform(
            datasets_df,
            Subjects = ifelse(
              !is.na(subjects),
              sprintf("%s/%s", subjects_filtered, subjects),
              ""
            )
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
        logger::log_trace("FilteredData$srv_filter_overview@1 updated counts")
        table_html
      })
      logger::log_trace("FilteredData$srv_filter_overview initialized")
      NULL
    }
  )
}


#' Get filter overview
#'
#' Summary of the dataset in the filter panel
#' @details Supported data types
#' `teal.slice` provide `get_filter_overview.default` method for:
#' - `data.frame`
#' - `DFrame`
#' - `matrix`
#' - `Matrix`
#' - `SummarizedExperiment`
#' - `MultiAssayExperiment`
#' Datasets which don't inherit from these classes will return
#' `data.frame(dataname = dataname, obs = NA, obs_filtered = NA))`
#' and they will be denoted as "Unsupported dataset" in UI of the filter panel overview.
#'
#' To extend the `get_filter_overview` function to support custom data types,
#' you need to define a new method for your custom class. This method should be named
#' `get_filter_overview.your_class_name`.
#'
#' Here's a general example:
#'
#' ```
#' get_filter_overview.your_class_name <- function(data_unfiltered, data_filtered, dataname) {
#'   # Your implementation here.
#'   # The function must return a data.frame with columns `dataname`, `obs`, and `obs_filtered`.
#' }
#' ```
#' Replace your_class_name with the name of your custom class. Inside the function,
#' implement the logic to generate the required data frame for your specific data type.
#' Remember, the returned `data.frame` must have the columns `"dataname"`, `"obs"`, and `"obs_filtered"`.
#' The `"dataname"` should be the name of the dataset, `"obs"` should be the number of observations in
#' the `unfiltered_data`, and `"obs_filtered"` should be the number of observations in the `filtered_data`.
#'
#' @param data_unfiltered,data_filtered (`object`)\cr
#' unfiltered and filtered data.
#' @param dataname (`character`)\cr
#' name of the dataset
#' @return `data_frame` with columns `dataname` and usually `obs`, `obs_filtered`.
#' @keywords internal
get_filter_overview <- function(data_unfiltered, data_filtered, dataname) {
  df <- UseMethod("get_filter_overview", data_unfiltered)
  if (!inherits(df, "data.frame") || all(c("dataname", "obs", "obs_filtered") %in% colnames(df))) {
    stop("get_filter_overview must return a data.frame with columns `dataname`, `obs`, `obs_filtered`", call. = FALSE)
  }
  df
}

#' @rdname get_filter_overview
#' @export
get_filter_overview.default <- function(data_unfiltered, data_filtered, dataname) {
  if (inherits(data_unfiltered, c("data.frame", "DFrame", "matrix", "Matrix"))) {
    get_filter_overview_data.frame(data_unfiltered, data_filtered, dataname)
  } else if (inherits(data_unfiltered, "SummarizedExperiment")) {
    get_filter_overview_SummarizedExperiment(data_unfiltered, data_filtered, dataname)
  } else if (inherits(data_unfiltered, "MultiAssayExperiment")) {
    get_filter_overview_MultiAssayExperiment(data_unfiltered, data_filtered, dataname)
  } else {
    data.frame(
      dataname = dataname,
      obs = NA,
      obs_filtered = NA
    )
  }
}


#' Default methods for get_filter_overview
#'
#' @description
#' Default methods for classes. They are not S3 methods. Reason for this is that anyone can define their own
#' method for any class. If we register S3 methods for these classes, user wouldn't be able to override S3 methods
#' for those classes as methods defined in `teal.slice` will have precedence over user defined methods.
#' @name get_filter_overview_default
#' @inheritParams get_filter_overview
#' @return `data_frame` with columns `dataname` and usually `obs`, `obs_filtered`.
#' @keywords internal
NULL

#' @rdname get_filter_overview_default
#' @keywords internal
get_filter_overview_data.frame <- function(data_unfiltered, data_filtered, dataname) {
  data.frame(
    dataname = dataname,
    obs = nrow(data_unfiltered),
    obs_filtered = nrow(data_filtered)
  )
}

#' @rdname get_filter_overview_default
#' @keywords internal
get_filter_overview_MultiAssayExperiment <- function(data_unfiltered, data_filtered, dataname) {
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

  get_experiment_keys <- function(mae, experiment) {
    sample_subset <- subset(MultiAssayExperiment::sampleMap(mae), colname %in% colnames(experiment))
    length(unique(sample_subset$primary))
  }

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
}

#' @rdname get_filter_overview_default
#' @keywords internal
get_filter_overview_SummarizedExperiment <- function(data_unfiltered, data_filtered, dataname) {
  get_filter_overview_data.frame(data_unfiltered, data_filtered, dataname)
}
