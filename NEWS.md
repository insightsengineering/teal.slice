# teal.slice 0.2.0.9004

* Examples now use `scda.2022` instead of `scda.2021`.

### Enhancements

* Improved filter state presentation in `FilterState$format`.

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
* Redesigned the filter panel input for dates to use CSS flexbox.
* Update icons to be compatible with Font Awesome 6.
* Updates the `FilteredData` method `get_formatted_filter_state` so it no longer appends empty filters.
* Added clearer installation instructions to README.

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
