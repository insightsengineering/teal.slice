#' Ensure the ellipsis, ..., in method arguments are empty
#'
#' Ellipsis, ..., are needed as part of method arguments to allow for its arguments to be different from its generic's
#' arguments and for this to pass check(). Hence, ..., should always be empty. This function will check for this
#' condition.
#'
#' @param ... it should literally just be ...
#' @param stop TRUE to raise an error; FALSE will output warning message
#' @param allowed_args character vector naming arguments that are allowed in the \code{...}.
#'   to allow for unnamed arguments, let "" be one of the elements in this character vector.
#'
#' @return \code{NULL} if ... is empty
#'
#' @keywords internal
#'
#' @examples
#' method.class <- function(a, b, c, ...) {
#'   check_ellipsis(...)
#' }
#' method.class <- function(a, b, c, ...) {
#'   check_ellipsis(..., allowed_args = c("y", "z"))
#' }
check_ellipsis <- function(..., stop = FALSE, allowed_args = character(0)) {
  if (!missing(...)) {
    checkmate::assert_flag(stop)
    checkmate::assert_character(allowed_args, min.len = 0, null.ok = TRUE, any.missing = FALSE)
    args <- list(...)
    arg_names <- names(args)
    if (is.null(arg_names)) {
      arg_names <- rep("", length(args))
    }
    extra_args <- arg_names[!is.element(arg_names, allowed_args)]
    if (length(extra_args) == 0) {
      return(invisible(NULL))
    }
    message <- paste(length(extra_args), "total unused argument(s).")

    named_extra_args <- extra_args[!vapply(extra_args, identical, logical(1), "")]
    if (length(named_extra_args) > 0) {
      message <- paste0(
        message,
        " ",
        length(named_extra_args),
        " with name(s): ",
        paste(named_extra_args, collapse = ", "),
        "."
      )
    }
    if (stop) {
      stop(message)
    } else {
      warning(message)
    }
  }
}

#' Whether the variable name is good to use within Show R Code
#'
#' Spaces are problematic because the variables must be escaped with backticks.
#' Also, they should not start with a number as R may silently make it valid by changing it.
#' Therefore, we only allow alphanumeric characters with underscores.
#' The first character of the `name` must be an alphabetic character and can be followed by alphanumeric characters.
#'
#' @md
#'
#' @param name `character, single or vector` name to check
#' @keywords internal
#'
#' @examples
#' teal.slice:::check_simple_name("aas2df")
#' teal.slice:::check_simple_name("ADSL")
#' teal.slice:::check_simple_name("ADSLmodified")
#' teal.slice:::check_simple_name("ADSL_modified")
#' teal.slice:::check_simple_name("ADSL_2")
#' teal.slice:::check_simple_name("a1")
#' # the following fail
#' \dontrun{
#' teal.slice:::check_simple_name("1a")
#' teal.slice:::check_simple_name("ADSL.modified")
#' teal.slice:::check_simple_name("a1...")
#' }
check_simple_name <- function(name) {
  checkmate::assert_character(name, min.len = 1, any.missing = FALSE)
  if (!grepl("^[[:alpha:]][a-zA-Z0-9_]*$", name, perl = TRUE)) {
    stop(
      "name '",
      name,
      "' must only contain alphanumeric characters (with underscores)",
      " and the first character must be an alphabetic character"
    )
  }
}

#' Resolve the expected bootstrap theme
#' @keywords internal
get_teal_bs_theme <- function() {
  bs_theme <- getOption("teal.bs_theme")
  if (is.null(bs_theme)) {
    NULL
  } else if (!inherits(bs_theme, "bs_theme")) {
    warning("teal.bs_theme has to be of a bslib::bs_theme class, the default shiny bootstrap is used.")
    NULL
  } else {
    bs_theme
  }
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
#' # continuously switch between right- and down-pointing chevrons
#' toggle_icon("toggle_element", c("fa-angle-right", "fa-angle-down"))
#'
#' # switch right- to down-pointing chevron
#' toggle_icon("toggle_element", c("fa-angle-right", "fa-angle-down"), one_way = TRUE)
#' }
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
#'   observeEvent(input[["hide_content"]], {
#'     shinyjs::hide("content")
#'     toggle_icon("toggle_content", c("fa-angle-down", "fa-angle-right"), one_way = TRUE)
#'   }, ignoreInit = TRUE)
#'
#'   observeEvent(input[["show_content"]], {
#'     shinyjs::show("content")
#'     toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"), one_way = TRUE)
#'   }, ignoreInit = TRUE)
#'
#'   observeEvent(input[["toggle_content"]], {
#'     shinyjs::toggle("content")
#'     toggle_icon("toggle_content", c("fa-angle-right", "fa-angle-down"))
#'   }, ignoreInit = TRUE)
#'
#'   output[["printout"]] <- renderPrint({
#'     head(faithful, 10)
#'   })
#'
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' @rdname toggle_button
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
