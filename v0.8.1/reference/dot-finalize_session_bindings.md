# Destroys inputs and observers stored in `private$session_bindings`

Call a `destroy` method to remove `observer` and `input` from obsolete
session which happens when `filter_panel_srv` is called again in new
`FilteredData` object. Inputs are not stored directly in a field as they
don't have `destroy` method. Instead, we store callback `destroy`
function for inputs which removes bindings from a `session`.

## Usage

``` r
.finalize_session_bindings(self, private)
```

## Arguments

- self, private:

  slots of a `R6` class

## Value

`NULL` invisibly
