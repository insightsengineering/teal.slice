#' Initialize `FilteredData`
#'
#' Initialize `FilteredData`
#' @param x (named `list` or `TealData`) In case of `TealData` see [teal.data::teal_data()].
#'  If the list is provided, it should contain `list`(s) containing following fields:
#' - `dataset` data object object supported by [`FilteredDataset`].
#' - `metatada` (optional) additional metadata attached to the `dataset`.
#' - `keys` (optional) primary keys.
#' - `datalabel` (optional) label describing the `dataset`.
#' - `parent` (optional) which `dataset` is a parent of this one.
#' @param join_keys (`JoinKeys`) see [teal.data::join_keys()].
#' @param code (`CodeClass`) see [`teal.data::CodeClass`].
#' @param cdisc (`logical(1)`) whether data is of `cdisc` type (relational).
#' @param check (`logical(1)`) whether data has been check against reproducibility.
#' @examples
#' library(shiny)
#' datasets <- teal.slice::init_filtered_data(
#'   x = list(
#'     iris = list(dataset = iris),
#'     mtcars = list(dataset = mtcars, metadata = list(type = "training"))
#'   )
#' )
#' @export
init_filtered_data <- function(x, join_keys, code, cdisc, check) {
  UseMethod("init_filtered_data")
}

#' @keywords internal
#' @export
init_filtered_data.TealData <- function(x, # nolint
                                        join_keys = x$get_join_keys(),
                                        code = x$get_code_class(),
                                        cdisc = FALSE,
                                        check = x$get_check()) {
  cdisc <- length(join_keys$get_parents()) > 0
  data_objects <- lapply(x$get_datanames(), function(dataname) {
    dataset <- x$get_dataset(dataname)

    parent <- if (cdisc) join_keys$get_parent(dataname) else NULL

    return_list <- list(
      dataset = dataset$get_raw_data(),
      keys = dataset$get_keys(),
      metadata = dataset$get_metadata(),
      label = dataset$get_dataset_label()
    )

    if (cdisc) return_list[["parent"]] <- parent
    return_list
  })

  names(data_objects) <- x$get_datanames()

  init_filtered_data(
    x = data_objects,
    join_keys = join_keys,
    code = code,
    check = check,
    cdisc = cdisc
  )
}

#' @keywords internal
#' @export
init_filtered_data.default <- function(x, join_keys = NULL, code = NULL, cdisc = FALSE, check = FALSE) { # nolint
  checkmate::assert_list(x, any.missing = FALSE, names = "unique")
  mapply(validate_dataset_args, x, names(x), MoreArgs = list(allowed_parent = cdisc))
  checkmate::assert_class(code, "CodeClass", null.ok = TRUE)
  checkmate::assert_class(join_keys, "JoinKeys", null.ok = TRUE)
  checkmate::assert_flag(check)

  datasets <- if (cdisc) {
    CDISCFilteredData$new(x, join_keys = join_keys, code = code, check = check)
  } else {
    FilteredData$new(x, join_keys = join_keys, code = code, check = check)
  }
}

#' Validate dataset arguments
#'
#' Validate dataset arguments
#' @param dataset_args (`list`)\cr
#'   containing the arguments except (`dataname`)
#'   needed by `init_filtered_dataset`
#' @param dataname (`character(1)`)\cr
#'   the name of the `dataset` to be added to this object
#' @param allowed_parent (`logical(1)`)\cr
#'   whether `FilteredDataset` can have a parent - i.e. if it's a part of `CDISCFilteredData`
#' @keywords internal
#' @return (`NULL` or throws an error)
validate_dataset_args <- function(dataset_args, dataname, allowed_parent = FALSE) {
  check_simple_name(dataname)
  checkmate::assert_flag(allowed_parent)
  checkmate::assert_list(dataset_args, names = "unique")

  allowed_names <- c("dataset", "keys", "label", "metadata")
  if (allowed_parent) {
    allowed_names <- c(allowed_names, "parent")
  }

  checkmate::assert_subset(names(dataset_args), choices = allowed_names)

  checkmate::assert_multi_class(dataset_args[["dataset"]], classes = c("data.frame", "MultiAssayExperiment"))
  checkmate::assert_character(dataset_args[["keys"]], null.ok = TRUE)
  teal.data::validate_metadata(dataset_args[["metadata"]])
  checkmate::assert_character(dataset_args[["label"]], null.ok = TRUE, min.len = 0, max.len = 1)
  checkmate::assert_character(dataset_args[["parent"]], null.ok = TRUE, min.len = 0, max.len = 1)
}

#' Evaluate expression with meaningful message
#'
#' Method created for the `FilteredData` to execute filter call with
#' meaningful message. After evaluation used environment should contain
#' all necessary bindings.
#' @param expr (`language`)
#' @param env (`environment`) where expression is evaluated.
#' @return invisible `NULL`.
#' @keywords internal
eval_expr_with_msg <- function(expr, env) {
  lapply(
    expr,
    function(x) {
      tryCatch(
        eval(x, envir = env),
        error = function(e) {
          stop(
            sprintf(
              "Call execution failed:\n - call:\n   %s\n - message:\n   %s ",
              deparse1(x, collapse = "\n"), e
            )
          )
        }
      )
      return(invisible(NULL))
    }
  )
}


#' Toggle button properties.
#'
#' Switch between different icons or titles on a button.
#'
#' Wrapper functions that use `shinyjs::runjs` to change button properties in response to events,
#' typically clicking those very buttons.
#' `shiny`'s `actionButton` and `actionLink` create `<a>` tags,
#' which may contain a child `<i>` tag that specifies an icon to be displayed.
#' `toggle_icon` calls the `toggleClass` (when `one_way = FALSE`) or
#' `removeClass` and `addClass` methods (when `one_way = TRUE`) to change icons.
#' `toggle_title` calls the `attr` method to modify the `Title` attribute of the button.
#'
#' @param input_id `character(1)` (namespaced) id of the button
#' @param icons,titles `character(2)` vector specifying values between which to toggle
#' @param one_way `logical(1)` flag specifying whether to keep toggling;
#'                if TRUE, the target will be changed
#'                from the first element of `icons`/`titles` to the second
#'
#' @return Invisible NULL.
#'
#' @name toggle_button
#'
#' @examples
#' \dontrun{
#'
#' library(shiny)
#'
#' ui <- fluidPage(
#'   shinyjs::useShinyjs(),
#'   actionButton("hide_content", label = "hide", icon = icon("xmark")),
#'   actionButton("show_content", label = "show", icon = icon("check")),
#'   actionButton("toggle_content", label = "toggle", icon = icon("angle-down")),
#'   br(),
#'   div(
#'     id = "content",
#'     verbatimTextOutput("printout")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   observeEvent(input$hide_content, {
#'     shinyjs::hide("content")
#'     toggle_icon("toggle_content", c("fa-angle-down", "fa-angle-right"), one_way = TRUE)
#'   }, ignoreInit = TRUE)
#'
#'   observeEvent(input$show_content, {
#'     shinyjs::show("content")
#'     toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"), one_way = TRUE)
#'   }, ignoreInit = TRUE)
#'
#'   observeEvent(input$toggle_content, {
#'     shinyjs::toggle("content")
#'     toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"))
#'   }, ignoreInit = TRUE)
#'
#'   output$printout <- renderPrint({
#'     head(faithful, 10)
#'   })
#'
#' }
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#' }
#'
#' @rdname toggle_button
#' @keywords internal
toggle_icon <- function(input_id, icons, one_way = FALSE) {
  checkmate::assert_string(input_id)
  checkmate::assert_character(icons, len = 2L)
  checkmate::assert_flag(one_way)

  expr <-
    if (one_way) {
      sprintf(
        "$('#%s i').removeClass('%s').addClass('%s');",
        input_id, icons[1], icons[2]
      )
    } else {
      sprintf("$('#%s i').toggleClass('%s');", input_id, paste(icons, collapse = " "))
    }

  shinyjs::runjs(expr)

  invisible(NULL)
}

#' @rdname toggle_button
#' @keywords internal
toggle_title <- function(input_id, titles, one_way = FALSE) {
  checkmate::assert_string(input_id)
  checkmate::assert_character(titles, len = 2L)
  checkmate::assert_flag(one_way)

  expr <-
    if (one_way) {
      sprintf(
        "$('a#%s').attr('title', '%s');",
        input_id, titles[2]
      )
    } else {
      sprintf(
        paste0(
          "var button_id = 'a#%1$s';",
          "var curr = $(button_id).attr('title');",
          "if (curr == '%2$s') { $(button_id).attr('title', '%3$s');",
          "} else { $(button_id).attr('title', '%2$s');",
          "}"
        ),
        input_id, titles[1], titles[2]
      )
    }

  shinyjs::runjs(expr)

  invisible(NULL)
}
