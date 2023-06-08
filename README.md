# teal.slice

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/teal.slice/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.slice/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/teal.slice/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.slice/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.slice/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.slice/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.slice?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.slice?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.slice)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.slice)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.slice)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.slice)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.slice)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.slice)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.slice/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.slice/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.slice?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.slice/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
<!-- end badges -->

`teal.slice` is an R library used in the development of `teal` `shiny` modules. It provides:

* a class accepting an array of `data.frames` as an input, then supporting basic `dplyr` operations on the data,
* a method returning the code needed to slice the data in the desired way,
* a set of `shiny` modules helping to acquire the desired slice of data using a `shiny` `GUI`.

## Installation

For releases from August 2022 it is recommended that you [create and use a GitHub PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.slice@*remotes")
```

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.slice")` for usage of this package.

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.slice.svg)](https://starchart.cc/insightsengineering/teal.slice)

### Stargazers

[![Stargazers repo roster for @insightsengineering/teal.slice](https://reporoster.com/stars/insightsengineering/teal.slice)](https://github.com/insightsengineering/teal.slice/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/teal.slice](https://reporoster.com/forks/insightsengineering/teal.slice)](https://github.com/insightsengineering/teal.slice/network/members)
