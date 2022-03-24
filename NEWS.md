# teal.slice 0.0.0.9009

* Initial release of `teal.slice` a package providing a filter module for `teal` applications.

## Changes (from behavior when functionality was part of `teal`)
* The filter panel now displays a helpful message when data has no rows or columns.
* `FilteredData` now stores whether its datasets had a reproducibility check or not.

### Breaking changes
* `default_filter` has been deprecated - use `list()` instead for a default filter.

### Bug fixes
* Add counts to filtering categorical variables bar charts in the filtering panel in cases where they were missing.

### Miscellaneous
* Added `is_any_filtered` method to all `FilterState` classes to detect if selected values actually filters out any data. This is used to decide if an explicit filter statement is added to the call.
