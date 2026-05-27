capture_runjs_output <- function(fn, ...) {
  captured_js <- NULL
  testthat::with_mocked_bindings(
    runjs = function(js_expr) {
      captured_js <<- js_expr
      invisible(NULL)
    },
    fn(...),
    .package = "shinyjs"
  )
  captured_js
}

# eval_expr_with_msg ----
testthat::describe("eval_expr_with_msg", {
  testthat::it("returns NULL invisibly on successful evaluation", {
    test_env <- new.env()
    assign_expr <- list(quote(result_value <- 1))
    testthat::expect_invisible(teal.slice:::eval_expr_with_msg(assign_expr, test_env))
    testthat::expect_null(teal.slice:::eval_expr_with_msg(assign_expr, test_env))
  })

  testthat::it("evaluates expression into the provided environment", {
    test_env <- new.env()
    teal.slice:::eval_expr_with_msg(list(quote(assigned_value <- 42L)), test_env)
    testthat::expect_identical(test_env$assigned_value, 42L)
  })

  testthat::it("error message includes the call header, deparsed call, and original error", {
    test_env <- new.env()
    failing_call <- quote(stop("something went wrong"))
    error_msg <- tryCatch(
      teal.slice:::eval_expr_with_msg(list(failing_call), test_env),
      error = function(e) conditionMessage(e)
    )
    testthat::expect_match(error_msg, "Filter call execution failed")
    testthat::expect_match(error_msg, "stop\\(\"something went wrong\"\\)")
    testthat::expect_match(error_msg, "something went wrong")
  })
})

# toggle_icon (two-way branch) ----
testthat::describe("toggle_icon two-way JS expression", {
  testthat::it("produces JS targeting the <i> child of the button", {
    generated_js <- capture_runjs_output(
      teal.slice:::toggle_icon, "panel_toggle", c("fa-angle-down", "fa-angle-right")
    )
    testthat::expect_match(generated_js, "#panel_toggle i", fixed = TRUE)
  })

  testthat::it("calls toggleClass with both icon classes space-separated", {
    generated_js <- capture_runjs_output(
      teal.slice:::toggle_icon, "panel_toggle", c("fa-angle-down", "fa-angle-right")
    )
    testthat::expect_match(
      generated_js,
      "$('#panel_toggle i').toggleClass('fa-angle-down fa-angle-right');",
      fixed = TRUE
    )
  })

  testthat::it("interpolates button_id and both icon classes correctly", {
    generated_js <- capture_runjs_output(
      teal.slice:::toggle_icon, "filter_btn", c("fa-eye", "fa-eye-slash")
    )
    testthat::expect_match(generated_js, "#filter_btn", fixed = TRUE)
    testthat::expect_match(generated_js, "fa-eye fa-eye-slash", fixed = TRUE)
  })
})

# toggle_title (two-way branch) ----
testthat::describe("toggle_title two-way JS expression", {
  testthat::it("produces JS that stores the button id as a variable", {
    generated_js <- capture_runjs_output(
      teal.slice:::toggle_title, "panel_toggle", c("Show panel", "Hide panel")
    )
    testthat::expect_match(generated_js, "var button_id = 'a#panel_toggle'", fixed = TRUE)
  })

  testthat::it("reads the current title into a variable", {
    generated_js <- capture_runjs_output(
      teal.slice:::toggle_title, "panel_toggle", c("Show panel", "Hide panel")
    )
    testthat::expect_match(generated_js, "var curr = $(button_id).attr('title')", fixed = TRUE)
  })

  testthat::it("sets title to titles[2] when current title matches titles[1]", {
    generated_js <- capture_runjs_output(
      teal.slice:::toggle_title, "panel_toggle", c("Show panel", "Hide panel")
    )
    testthat::expect_match(
      generated_js,
      "if (curr == 'Show panel') { $(button_id).attr('title', 'Hide panel');",
      fixed = TRUE
    )
  })

  testthat::it("interpolates button_id, titles[1], and titles[2] correctly", {
    generated_js <- capture_runjs_output(
      teal.slice:::toggle_title, "filter_btn", c("Open filters", "Close filters")
    )
    testthat::expect_match(generated_js, "a#filter_btn", fixed = TRUE)
    testthat::expect_match(generated_js, "'Open filters'", fixed = TRUE)
    testthat::expect_match(generated_js, "'Close filters'", fixed = TRUE)
  })
})
