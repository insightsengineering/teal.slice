### Examples for init_filter_state ###
###    for developer use only
###
###                   CoreDev Team ###

require(teal.slice)

readline("create filter state >")

filter_state <- teal.slice:::init_filter_state(
  x = c(1:10, NA, Inf),
  x_reactive = reactive(c(1:10, NA, Inf)),
  slice = teal_slice(
    varname = "varname",
    dataname = "dataname"
  )
)

shiny::isolate(filter_state$get_call())

readline("next: set state >")

filter_state$set_state(
  teal_slice("dataname", "varname", selected = c(3.1, 7.3), keep_na = TRUE)
)

shiny::isolate(filter_state$get_call())

readline("next: continue to application >")

shinyApp(
  ui = fluidPage(
    filter_state$ui(id = "app"),
    verbatimTextOutput("call")
  ),
  server = function(input, output, session) {
    filter_state$server("app")

    output$call <- renderText(
      deparse1(filter_state$get_call(), collapse = "\n")
    )
  }
)
