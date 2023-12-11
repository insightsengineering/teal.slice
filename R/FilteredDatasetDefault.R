# DefaultFilteredDataset ----

DefaultFilteredDataset <- R6::R6Class(
  classname = "DefaultFilteredDataset",
  inherit = FilteredDataset,

  public = list(

    initialize = function(dataset,
                          dataname,
                          keys = character(0),
                          parent_name = character(0),
                          parent = NULL,
                          join_keys = character(0),
                          label = character(0)) {
      super$initialize(dataset, dataname)
    },

    format = function(show_all, trim_lines) {
      sprintf("%s:\n\"%s\": %s", class(self)[1], private$dataname, toString(class(private$dataset)))
    },

    get_call = function(sid) {
    },
    get_filter_state = function() {
      warning("DefaultFilterState does not have state to return")
      NULL
    },
    set_filter_state = function(state) {
      warning("DefaultFilterState cannnot set state")
    },
    clear_filter_states = function(force) {
      warning("DefaultFilterState does not have filter states to clear")
    },

    get_filter_overview = function() {
    }

  ),

  private = list(
    add_filter_states = function(filter_states, id) {
      warning("DefaultFilterState cannnot add filter states")
    }
  )
)
