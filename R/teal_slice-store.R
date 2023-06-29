
#' Store teal_slices object to a file
#'
#' This function takes a teal_slices object and saves it to a file in JSON format. The teal_slices object contains
#' information about filter states and can be used to create, modify, and delete filter states. The saved file can
#' be later loaded using the `slices_restore` function.
#'
#' @param tss (`teal_slices`) object to be stored.
#' @param file (`character(1)`) The file path where `teal_slices` object will be saved.
#'  The file extension should be ".json".
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' # Create a teal_slices object
#' tss <- teal_slices(
#'   teal_slice(dataname = "data", varname = "var"),
#'   teal_slice(dataname = "data", expr = "x > 0", id = "positive_x", title = "Positive x"),
#' )
#'
#' # Store the teal_slices object to a file
#' slices_store(tss, "path/to/file.json")
#' }
#'
#' @export
slices_store <- function(tss, file) {
  checkmate::assert_class(tss, "teal_slices")
  checkmate::assert_path_for_output(file, overwrite = TRUE, extension = "json")

  cat(format(tss, trim_lines = FALSE), "\n", file = file)
}

#' Restore teal_slices object from a file
#'
#' This function takes a file path to a JSON file containing a teal_slices object and restores it to its original form. The restored teal_slices object can be used to access filter states and their corresponding attributes.
#'
#' @param file The file path where the teal_slices object is stored. The file should be in JSON format and have a ".json" extension.
#'
#' @return A teal_slices object restored from the file.
#'
#' @examples
#' \dontrun{
#' # Restore a teal_slices object from a file
#' tss_restored <- slices_restore("path/to/file.json")
#' }
#' @export
slices_restore <- function(file) {
  checkmate::assert_file_exists(file, access = "r", extension = "json")

  tss_json <- jsonlite::fromJSON(file, simplifyDataFrame = FALSE)

  tss_elements <- lapply(tss_json$slices, as.teal_slice)

  do.call(teal_slices, c(tss_elements, tss_json$attributes))
}


#' Convert a list to a justified JSON string
#'
#' This function takes a list and converts it to a JSON string. The resulting JSON string is then
#' justified to improve readability. Additionally, the function has an option to trim the lines of
#' the JSON string.
#'
#' @param x `list` containing JSON strings.
#' @param trim_lines (`function`) or not to trim lines of the JSON string (default is FALSE).
#' @return A justified JSON string representation of the input list.
#' @keywords internal
#'
jsonify <- function(x, trim_lines) {
  checkmate::assert_list(x)

  x_json <- to_json(x)
  x_json_justified <- justify_json(x_json)
  if (trim_lines) x_json_justified <- trim_lines(x_json_justified)
  paste(x_json_justified, collapse = "\n")
}

#' Converts a list to a JSON string
#'
#' Converts a list representation of `teal_slices` into a JSON string. This function is used by the
#' `format` method for `teal_slices` objects.
#' @param x (`list`) representation of `teal_slices` object.
#' @keywords internal
#'
to_json <- function(x) {
  no_unbox <- function(x) {
    vars <- c("selected", "choices")
    if (is.list(x)) {
      for (var in vars) {
        if (!is.null(x[[var]])) x[[var]] <- I(x[[var]])
      }
      lapply(x, no_unbox)
    } else {
      x
    }
  }

  jsonlite::toJSON(no_unbox(x), pretty = TRUE, auto_unbox = TRUE, digits = 16, null = "null")
}

#' Justify Colons in JSON String
#'
#' This function takes a JSON string as input and returns a modified version of the input where colons are justified in each line.
#'
#' @param json (`character(1)`) representing the input JSON.
#'
#' @return A character string with justified colons in each line of the JSON.
#' @keywords internal
#'
justify_json <- function(json) {
  format_name <- function(name, name_width) {
    if (nchar(name) == 1 || nchar(gsub("\\s", "", name)) <= 2) {
      return(name)
    } else if (grepl("slices|attributes", name)) {
      paste0(name, ":")
    } else {
      paste(format(name, width = name_width), ":")
    }
  }

  json_lines <- strsplit(json, "\n")[[1]]
  json_lines_split <- strsplit(json_lines, split = ":", fixed = TRUE)
  name_width <- max(unlist(gregexpr(":", json_lines))) - 1
  vapply(json_lines_split, function(x) paste0(format_name(x[1], name_width), stats::na.omit(x[2])), character(1))
}

#' Justify Colons in JSON String
#'
#' This function takes a JSON string as input and returns a modified version of the input where colons are justified in each line.
#'
#' @param json A character string representing the input JSON.
#'
#' @return A character string with justified colons in each line of the JSON.
#' @keywords internal
#'
trim_lines <- function(x) {
  name_width <- max(unlist(gregexpr(":", x))) - 1
  trim_position <- name_width + 17L
  x_trim <- substr(x, 1, trim_position)
  substr(x_trim, trim_position - 2, trim_position) <- "..."
  x_trim
}
