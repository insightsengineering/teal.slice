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
