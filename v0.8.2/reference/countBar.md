# Progress bar with label

`shiny` element displaying a progress bar and observation count.

## Usage

``` r
countBar(inputId, label, countmax, countnow = NULL, counttotal = countmax)

updateCountBar(
  session = getDefaultReactiveDomain(),
  inputId,
  label,
  countmax,
  countnow = NULL,
  counttotal
)

updateCountText(
  session = getDefaultReactiveDomain(),
  inputId,
  label,
  countmax,
  countnow
)
```

## Arguments

- inputId:

  (`character(1)`) `shiny` id of the parent element (e.g. a check-box
  group input).

- label:

  (`character(1)`) Text to display followed by counts.

- countmax:

  (`numeric(1)`) Maximum count for a single element.

- countnow:

  (`numeric(1)`) Current count for a single element.

- counttotal:

  (`numeric(1)`) Sum total of maximum counts of all elements, see
  `Details`.

- session:

  (`session`) `shiny` `session` object passed to function given to
  `shinyServer`.

## Value

`shiny.tag` object with a progress bar and a label.

## Details

A progress bar is created to visualize the number of counts in a
variable, with filling and a text label.

- progress bar width is derived as a fraction of the container width
  equal to `<countmax> / <counttotal>%"`,

- progress bar is filled up to the fraction `<countnow> / <countmax>`,

- text label is obtained by `<label> (<countnow> / <countmax>)`.
