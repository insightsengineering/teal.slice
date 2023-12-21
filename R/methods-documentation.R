#' Methods for custom data class
#'
#' @description
#' Methods for custom data class
#'
#' @name filter_panel_methods
#'
#' @param id (`character`) Module id
#' @param data (`object`) Object of any class
#' @param data_filtered (`object`) Object of any class but must be the same class as `data`
#' @param dataname (`character`) Name of the dataset
#' @param filtered_data (`FilteredData`) Object of class `FilteredData`.
#' @param states_list (`list`) List of `FilterState` objects
#'
#' @details
#'
#' # Overview
#'
#' `teal.slice` provides set of methods to manage filters for different data types.
#' # todo: write more about [srv_add()] and consequences of creating `teal_slice` for certain dataset
#' #     - how slice defines location of the variable
#' #     - how slice is handled by different methods [srv_active()], [get_filter_call()], etc.
#'
#'
#' # Supported data types
#'
#' `teal.slice` provide methods for:
#' - `data.frame`
#' - `DataFrame`
#' - `matrix`
#' - `Matrix`
#' - `SummarizedExperiment`
#' - `MultiAssayExperiment`
#'
#' Datasets which don't inherit from these classes will trigger default methods which in most of the
#' cases do nothing. Methods for unsupported data types are made in a way that they don't break the app.
#' If you want to extend filter panel for your custom data type, you can register S3 methods for
#' any of these methods. For example, if you want to extend filter panel for `custom_class`, you can
#' register a new S3 methods `<method>.custom_class` where `<method>` is one of the exported `teal.slice` methods.
#'
#' Same applies to supported data types. If you want to override default methods for any of the supported
#' data types, say data.frame, you can register a new S3 methods `<method>.data.frame`.
#'
#' @seealso [module_active], [get_filter_overview], [module_add], [get_filter_call], [get_slice_variable],
#' [init_filter_state]
#'
#' @keywords internal
NULL


#' Default methods for supported datasets
#'
#' @description
#' Default methods for supported datasets. These are internal functions that handle supported classes for
#' in `<method>.default` [filter_panel_methods].
#'
#' @name default_filter_panel_internals
#'
#' @description
#' Default methods for classes. They are not S3 methods. Reason for this is that anyone can define their own
#' method for any class. If we register S3 methods for these classes, user wouldn't be able to override S3 methods
#' for those classes as methods defined in `teal.slice` will have precedence over user defined methods.
#'
#' @seealso [module_active], [module_overview], [get_filter_overview],
#' [module_add], [get_filter_call], [get_slice_variable]
#'
#' @keywords internal
NULL
