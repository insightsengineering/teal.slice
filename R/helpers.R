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

toggle_title <- function(input_id, titles, one_way = FALSE) {
  checkmate::assert_string(input_id)
  checkmate::assert_character(titles, len = 2L)
  checkmate::assert_flag(one_way)

  expr <-
    if (one_way) {
      sprintf(
        "$(a#%s).attr('title', '%s');",
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
if (one_way) print(expr)

  shinyjs::runjs(expr)

  invisible(NULL)
}
