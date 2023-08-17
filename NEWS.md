# teal.slice 0.4.0.9001

# teal.slice 0.4.0

### New features

* Filter panel API is now based on `teal_slice` and `teal_slices` objects.
* It is now possible to specify a filter based on an arbitrary logical expression. See `expr` argument in `teal_slice`.
* It is now possible to limit choices in a single filter card. See `choices` argument in `teal_slice`.
* It is now possible to initialize the filter panel without the "Add filter variables" panel through `allow_add` in `teal_slices`.
* It is now possible to set a filter that cannot be removed by the app user. See `anchored` argument in `teal_slice`.
* It is now possible to set a filter whose selection cannot be changed. See `fixed` argument in `teal_slice`.
* It is now possible to limit choices within a variable to a single value only. See `multuple` argument in `teal_slice`  .
* Changed appearance of filter cards to a collapsible accordion.
* Replaced `sliderInput` with interactive `plotly` chart to allow the user to zoom in on the variable distribution.
* Implemented reactive counts in single filter cards to compare filtered and unfiltered variable distributions. See `count_type` argument in `teal_slices`.
* Added state history: individual filter states track changes so the user can rewind them or reset the initial state.

### Breaking changes

* Setting filters using a list is now deprecated. Use `teal_slices` and `teal_slice` instead.
* Removed `CDISCFilteredData` and `CDISCFilteredDataset` and implementing `JoinKeys` handling in their parent classes (`FilteredData` and `DefaultFilteredDataset`).
* Specifying set of filterable columns is done through `include_varnames` and `exclude_varnames` in `teal_slices`. Specifying `attr(, "filterable")` is hard deprecated.

# teal.slice 0.3.0

* Examples now use `scda.2022` instead of `scda.2021`.
* Transferred data hashing step in `FilteredDataset` to `teal`.
* Removed constructor of `Queue` class.

### New features

* Added a global turn on/off button for the Filter Panel.
* Added ability to collapse Active Filter Display panel.
* Added ability to collapse all filters of an individual dataset.
* Added fixed filter states.

### Enhancements

* Improved filter state presentation in `FilterState$format`.

### Bug fixes

* Fixed an error where the `RangeFilterState` produced an error when using `bootstrap 4`.
* Fixed a bug that caused the range slider to omit values selected programmatically through the filter API.
* Fixed a bug where setting incorrect values for Date and Date time ranges caused the app to crash.

### Miscellaneous

* Calculation of step in slider for `RangeFilterState` now uses `checkmate::test_integerish` instead of `is.integer`.
* Updated `init_filtered_data` to take into account the removal of `CDISCTealData` from `teal.data` package.
* Added `shinyvalidate` validation for Date and Date time ranges.
* Added examples apps for `FilterState` child classes and `DFFilterStates`.

# teal.slice 0.2.0

### New features

* Added `set_filterable_varnames` method to `FilteredData` and `FilteredDataset` objects so that there is control over which variables can be filtered.
* Added support for custom `bslib` bootstrap themes via the `teal.bs_theme` option. See the `teal` vignette `teal-bs-themses` for more information.
* Removed `CDISCFilteredDataset` class and functionality moved to `CDISCFilteredData`.
* Changed constructor of `FilteredData` to not require `TealData` object. See `help(init_filtered_data)` for more details.
* The filtered data is now stored in `FilteredData` not `FilteredDataset`.
* The join keys stored inside `FilteredData` are now `JoinKeys` objects.
* Updated `get_filter_state` to return a list of active filter states and an attribute with the character form of the filter states.
* Updated the `get_varlabels` method for `FilterStates` classes. It now accepts a vector input.
* Exported `S3` generic function `init_filter_states` so that it can be used in other packages.
* Added a `FilterPanelAPI` class to encapsulate the API of a filter panel.

### Enhancements

* Redesigned the count bars for filter panel check box inputs.
* Redesigned the filter panel input for dates to use `CSS flexbox`.
* Update icons to be compatible with Font Awesome 6.
* Updates the `FilteredData` method `get_formatted_filter_state` so it no longer appends empty filters.
* Added clearer installation instructions to `README`.

### Breaking changes

* Renamed internal S3 method `get_filterable_varnames` to `get_supported_filter_varnames`.

### Bug fixes

* Fixed a bug when the filter panel overview would not refresh if the panel was hidden during a transition between active modules.
* Fixed a bug in `FilterState` where `sliderInput` step values were too precise.

# teal.slice 0.1.1

### New features
* Added a formatting function for filter panel classes.

### Miscellaneous
* Added a template to the `pkgdown` site.
* Updated package authors.
* Added package vignettes.

### Fix
* Fixed a bug in `FilteredDataset`, where launching a `shiny` application without `FilteredData` would not attach the appropriate CSS files.

# teal.slice 0.1.0

* Initial release of `teal.slice` - a package providing a filter module for `teal` applications.

## Changes (from behavior when functionality was part of `teal`)

### Breaking changes
* `default_filter` has been deprecated - use `list()` instead for a default filter.

### Bug fixes
* Add counts to filtering categorical variables bar charts in the filtering panel in cases where they were missing.
* Fixed a bug causing an error when both sliders of `RangeFilterState` where put to either end of the allowed range
in the `UI`.

### Miscellaneous
* Added `is_any_filtered` method to all `FilterState` classes to detect if selected values actually filters out any data. This is used to decide if an explicit filter statement is added to the call.
* The filter panel now displays a helpful message when data has no rows or columns in place of an empty drop down widget.
* `FilteredData` now stores whether its datasets had a reproducibility check or not via two new methods to its class: `set_check` and `get_check`.
