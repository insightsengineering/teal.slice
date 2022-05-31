# teal.slice

`teal.slice` is an R library used in the development of `teal` `shiny` modules. It provides:

* a class accepting an array of `data.frames` as an input, then supporting basic `dplyr` operations on the data,
* a method returning the code needed to slice the data in the desired way,
* a set of `shiny` modules helping to acquire the desired slice of data using a `shiny` `GUI`.

## Installation

It is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("devtools")) install.packages("devtools")
devtools::install_github("insightsengineering/teal.slice@*release", dependencies = FALSE)
```

You might need to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to `install_github` it may produce an error.

See package vignettes `browseVignettes(package = "teal.slice")` for usage of this package.
