# teal.slice

`teal.slice` is an R library used in the development of `teal` `shiny` modules. It provides:

* a class accepting an array of `data.frames` as an input, then supporting basic `dplyr` operations on the data,
* a method returning the code needed to slice the data in the desired way,
* a set of `shiny` modules helping to acquire the desired slice of data using a `shiny` `GUI`.

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.slice@*remotes")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.slice")` for usage of this package.

[![Stargazers repo roster for @insightsengineering/teal.slice](https://reporoster.com/stars/insightsengineering/teal.slice)](https://github.com/insightsengineering/teal.slice/stargazers)
[![Forkers repo roster for @insightsengineering/teal.slice](https://reporoster.com/forks/insightsengineering/teal.slice)](https://github.com/insightsengineering/teal.slice/network/members)

## Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.slice.svg)](https://starchart.cc/insightsengineering/teal.slice)
