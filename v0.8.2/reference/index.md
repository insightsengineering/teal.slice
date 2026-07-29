# Package index

## `teal` filter-panel API

Functions used initialize filter-panel and to modify its states.

- [`init_filtered_data()`](https://insightsengineering.github.io/teal.slice/reference/init_filtered_data.md)
  :

  Initialize `FilteredData`

- [`set_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)
  [`get_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)
  [`remove_filter_state()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)
  [`clear_filter_states()`](https://insightsengineering.github.io/teal.slice/reference/filter_state_api.md)
  :

  Managing `FilteredData` states

- [`get_filter_expr()`](https://insightsengineering.github.io/teal.slice/reference/get_filter_expr.md)
  :

  Gets filter expression for multiple `datanames` taking into account
  its order.

- [`teal_slice()`](https://insightsengineering.github.io/teal.slice/reference/teal_slice.md)
  : Specify single filter

- [`teal_slices()`](https://insightsengineering.github.io/teal.slice/reference/teal_slices.md)
  : Complete filter specification

## For developers

Abstract and concrete classes used to build teal functionality.

### R6 Classes

Abstract and concrete classes used to build teal functionality.

- [`FilteredData`](https://insightsengineering.github.io/teal.slice/reference/FilteredData.md)
  : Class to encapsulate filtered datasets

- [`FilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/FilteredDataset.md)
  :

  `FilteredDataset` `R6` class

- [`FilterPanelAPI`](https://insightsengineering.github.io/teal.slice/reference/FilterPanelAPI.md)
  : Class to encapsulate the API of the filter panel of a teal app

- [`DataframeFilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/DataframeFilteredDataset.md)
  :

  The `DataframeFilteredDataset` `R6` class

- [`MAEFilteredDataset`](https://insightsengineering.github.io/teal.slice/reference/MAEFilteredDataset.md)
  :

  `MAEFilteredDataset` `R6` class
