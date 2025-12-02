# Format `POSIXt` for storage

Convert `POSIXt` date time object to character representation in UTC
time zone.

## Usage

``` r
format_time(x)
```

## Arguments

- x:

  (`POSIXt`) vector of date time values or anything else

## Value

If `x` is of class `POSIXt`, a character vector, otherwise `x` itself.

## Details

Date times are stored as string representations expressed in the UTC
time zone. The storage format is `YYYY-MM-DD HH:MM:SS`.
