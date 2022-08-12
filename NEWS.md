# teal.slice 0.1.1.9014.1

* Removed `CDISCFilteredDataset` class and functionality moved to `CDISCFilteredData`.
* Changed constructor of `FilteredData` to not require `TealData` object. See `help(init_filtered_data)` for more details.
* The filtered data is now stored in `FilteredData` not `FilteredDataset`.
* The join keys stored inside `FilteredData` are now `JoinKeys` objects.
* Updated `get_filter_state` to return a list of active filter states and an attribute with the character form of the filter states.

# Bug fixes

* Fixed a bug when the filter panel overview would not refresh if the panel was hidden during a transition between active modules.
* Fixed a bug where `varlabels` were assumed to be a character in `FilterStates` (now it is a character vector) and a || was used.

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
