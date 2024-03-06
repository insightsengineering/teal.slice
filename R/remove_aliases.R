
#' Remove aliases from package index.
#'
#' Removes entries from package index page that list aliases of topics given in `names`.
#'
#' @param names (`character`) names of objects for which to remove aliases
#'
#' @return Invisible `TRUE` if operation succeded, invisible `FALSE` otherwise.
#'
#' @keywords internal
#'
remove_aliases <- function(names) {
  checkmate::assert_character(names)

  index_file <- system.file("html", "00Index.html", package = "teal.slice")

  if (identical(index_file, "") || !isTRUE(utils::file_test("-w", index_file))) return(invisible(FALSE))

  index_contents <- readLines(index_file)

  indices <- as.list(
    lapply(names, function(name) {
      intersect(
        grep(sprintf("<a href=\"%s\\.html\">", name), index_contents),
        grep(sprintf("<a href=\".*\\.html\">%s</a></td>", name), index_contents, invert = TRUE)
      )
    })
  )

  ind <- Reduce(union, indices)
  if (identical(ind, integer(0L))) return(invisible(TRUE))
  ind <- c(ind, ind + 1L)
  index_contents <- index_contents[-ind]

  writeLines(index_contents, index_file)
  invisible(TRUE)
}
