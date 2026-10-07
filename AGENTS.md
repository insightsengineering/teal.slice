
# teal.slice R Package Development Guide

## Package Overview

`teal.slice` is part of the `teal` framework and provides the filter
panel used in `teal` applications. It allows users to interactively
filter data (`data.frame`, `MultiAssayExperiment`,
`SummarizedExperiment` and `matrix`), displays filtered/unfiltered
observation counts and generates reproducible filtering code.

## Development Context

It provides 3 main features to the framework:

- `teal_slice` / `teal_slices`: plain S3 objects (built on
  `shiny::reactiveValues`) that store the specification of a filter and
  of the filter panel
  - `teal_slice()` defines one filter on a variable (`dataname`,
    `varname`, `selected`, `keep_na`, `fixed`, `anchored`, …) or a
    custom expression (`id`, `expr`)
  - `teal_slices()` is a collection of `teal_slice` with panel-wide
    settings (`include_varnames`, `exclude_varnames`, `count_type`,
    `allow_add`)
- `FilteredData` R6 class that holds the data, manages the filter states
  and serves the shiny modules of the filter panel
  (`ui/srv_filter_panel`, `ui/srv_active`, `ui/srv_overview`,
  `ui/srv_available_filters`)
- Reproducible filter calls so that filtering is part of the code shown
  by “Show R code” and the reporter

See `vignettes/teal-slice-classes.Rmd` for the detailed class design and
`vignettes/filter-panel-for-developers.Rmd` for the public API usage.

### Relationships with other packages

Direct dependencies:

- `teal.data`: provides `join_keys` used to determine parent/child
  relationships between datasets.
  - A child dataset (e.g. `ADAE`) is filtered also by the filters of its
    parent (e.g. `ADSL`) through a join on the keys.
  - Any issue with `join_keys` should be addressed in `teal.data`.
- `teal.widgets`: reusable UI components used in the filter cards.
- `MultiAssayExperiment` and `SummarizedExperiment` are in `Suggests`;
  code paths that use them must check `requireNamespace()` and tests
  must use `testthat::skip_if_not_installed()`.

Usage in other framework packages:

- `teal`: the main consumer of this package.
  - `teal::init(filter = teal_slices(...))` takes the filter
    specification; `teal` has its own `teal::teal_slices()` that extends
    `teal.slice::teal_slices()` with `module_specific` and `mapping`
    arguments.
  - `teal` creates the `FilteredData` object via
    `teal.slice::init_filtered_data()` from the `teal_data` object,
    renders the filter panel and appends the code from
    `get_filter_expr()` to the object passed to the modules.
  - Snapshot manager and module-specific filters in `teal` rely on the
    shared state of `teal_slice` objects.

### Workflows

- Before fixing an issue consider that this package is part of teal
  framework, so a bug that manifests in this package might have a
  different origin.
- Changes to the public API (`FilteredData` public methods,
  `teal_slice(s)` arguments, exported functions) can break `teal` and
  must be verified against it.

<!-- markdownlint-disable-file MD002 MD041 -->

This package is part of the teal framework. The following configuration
applies to all packages within the teal framework.

## Package Structure and Organization

### Key Directories

Follow the standard R package structure with teal-specific conventions:

``` text
package_name/
├── .github           # CI/CD workflows
├── R/                # R source code
├── tests/testthat/   # Unit tests using testthat
├── vignettes/        # Long-form documentation
├── inst/             # Package assets
├── AGENTS.md         # Development guide for AI agents (this file)
├── DESCRIPTION       # Package metadata
├── NAMESPACE         # Exports and imports
├── NEWS.md           # Change log
├── README.md         # Package overview
├── _pkgdown.yml      # Documentation website config
├── .lintr            # Linting configuration
└── .Rbuildignore     # Build exclusions
```

### Naming Conventions

- **Function names**: Use `snake_case` consistently
- **Class names**: Use `PascalCase` (e.g., `TealAppDriver`)
- **Module functions**: Prefix UI functions with `ui_` and server
  functions with `srv_`
- **Internal functions**: Use descriptive names without export

### File Organization

- **One main function per file** when the function is substantial
- **Group related utilities** in shared files (e.g., `utils.R`,
  `validations.R`)
- **Module files**: Use pattern `tm_<name>.R` for teal modules
- **Helper functions**: Prefix with the main function they support

## Code Style and Standards

### Code Quality

- **Run `pre-commit` hooks**: Always run `pre-commit run --all-files`
  before committing, Fix any issues it reports - the error messages are
  informative and will guide you. It automatically checks code style,
  documentation and other quality issues. If pre-commit is not
  available, run the checks manually. Lint the R code manually as well
  if not called by pre-commit.
- **Follow `tidyverse` style**: General R code style follows the
  `tidyverse` style guide.
- **Documentation**: All exported functions must have `roxygen2`
  documentation with `@returns` and `@examples` fields.
- **Formatting** rules are configured in the `.lintr` file.

## Dependencies and Imports

### Dependency Management

- **Minimize dependencies**: Only add dependencies that provide
  significant value
- **Version constraints**: Specify minimum versions for critical
  dependencies
- **Ecosystem coherence**: Prefer packages already used within teal
  ecosystem

### Import Best Practices

Avoid importing package functions via roxygen2 (`#' @import pkg`)tags in
favor of explicit namespacing for clarity when appropriate. When needed
prefer specific imports over full package imports.

## Testing Framework

### Testing Philosophy

- **Test public functions only**: Internal utilities should be tested
  through public interfaces
- **Precise, focused tests**: Each test should verify one specific
  behavior
- **High coverage**: Maintain at least 80% test coverage as measured by
  `covr`
- **Integration over units**: Test realistic usage patterns
- **Test Dependencies**.: Add
  `testthat::skip_if_not_installed(package_name)` only for dependencies
  in `Suggests` or related to tests cases

### Shiny Module Testing

- **Server functions**: Test with `shiny::testServer()`
- **UI functions**: Test basic usage with regular testing (class checks,
  error generation, snapshots, regexp search). Test UI scenarios and
  interactions with `teal:::TealAppDriver` (based on
  `shinytest2::AppDriver`) for integration testing
- **Reactive behavior**: Test reactive chains and side effects

### Test Organization and Naming

- **One test file per R file**: `test-module_example.R` for
  `module_example.R`
- **Descriptive test names**: Clearly describe what is being tested
- **End to end test names**: `test-shinytest2-module_example.R` for
  `module_example.R`
- **Logical grouping**: Group related tests using `describe()` and
  individual tests with `it()` when beneficial
- **Test data**: Create minimal test datasets, avoid external
  dependencies

## Documentation and Communication

### Package Documentation

- **`README.md`**: Clear overview, installation, basic usage examples
- **Vignettes**: Comprehensive guides for complex functionality
- **Function documentation**: All exported functions must have
  `roxygen2` documentation
- **`NEWS.md`**: Detailed changelog of features, bugs and miscellanea
  changes affecting the users

### Package Version Management

Do not change versions on your own. There is a CI/CD workflow that
manages the versions automatically on the `main` branch.

## CI/CD and Development Workflow

Prefer to reuse templates from r.pkg.template. Main checks in place are:

- `check.yaml`: R CMD check, unit tests, coverage
- `docs.yaml`: Documentation building and deployment
- `audit.yaml`: Security and dependency auditing
- `pkgdown.yaml`: Website generation

## Quality Assurance

### Code Quality Metrics

- **Test Coverage**: ≥80% line coverage
- **Linting**: No lint violations using configured `.lintr`
- **Documentation**: 100% of exports documented

### Code Review Process

- **Pull Request Reviews**: All changes require human review and
  approval
- **Automated Checks**: CI must pass before merging
- **Breaking Changes**: Require special consideration and communication
- **Documentation Updates**: Must accompany functional changes

### Performance Considerations

- **Shiny Reactivity**: Minimize unnecessary reactive computations
- **Data Processing**: Use efficient data manipulation patterns
- **Memory Usage**: Consider memory implications for large datasets
- **Loading Time**: Optimize package loading and module initialization

## Maintenance Guidelines

- **Long-term Support**: Maintain backward compatibility when possible
- **Dependencies**: Minimal and justified dependencies only
- **Deprecation**: Use `lifecycle` package for function deprecation
