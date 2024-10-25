# make_c_call ----
testthat::test_that("make_c_call", {
  testthat::expect_identical(make_c_call(1:3), quote(c(1L, 2L, 3L)))
  testthat::expect_identical(make_c_call(1), 1)
})

# sanitize_id ----
testthat::describe("sanitize_id", {
  testthat::it("should replace dots with `_` when id is otherwise valid", {
    id <- "a.b"
    ns <- teal.slice:::NS("app")
    testthat::expect_identical(
      ns(id),
      paste0("app-h", substr(rlang::hash(id), 1, 4), "_a_b")
    )
  })

  testthat::it("should take vector input", {
    id <- c("a.b", "a", "b", " c")
    ns <- teal.slice:::NS("app")
    testthat::expect_identical(
      ns(id),
      c(
        paste0("app-h", substr(rlang::hash(id[1]), 1, 4), "_a_b"),
        "app-a",
        "app-b",
        paste0("app-h", substr(rlang::hash(id[4]), 1, 4), "__c")
      )
    )
  })

  testthat::it("should allow for integer input", {
    id <- c(1L, 2L, 3L)
    ns <- teal.slice:::NS("app")
    testthat::expect_identical(
      ns(id),
      c("app-1", "app-2", "app-3")
    )
  })

  testthat::it("should replace non-ASCII characters in middle of id with `_`", {
    id <- "a$b"
    ns <- teal.slice:::NS("app")
    testthat::expect_identical(
      ns(id),
      paste0("app-h", substr(rlang::hash(id), 1, 4), "_a_b")
    )
  })

  # Test using moduleServer to access the sanitized id
  testthat::it("should replace non-ASCII characters in the start/end of id with `_`", {
    id <- "%a bad symbol$"
    id2 <- "a&b#"
    id_from_module <- shiny::withReactiveDomain(
      MockShinySession$new(),
      teal.slice:::moduleServer(id, function(input, output, session) session$ns("a_good_name"))
    )

    testthat::expect_identical(
      id_from_module,
      paste0("h", substr(rlang::hash(id), 1, 4), "__a_bad_symbol_-a_good_name")
    )
  })

  testthat::it("should replace all quotes characters with `_`", {
    id <- " a.b.c\"d`e'j"
    testthat::expect_identical(
      teal.slice:::NS("app", id),
      paste0("app-h", substr(rlang::hash(id), 1, 4), "__a_b_c_d_e_j")
    )
  })

  testthat::it("should replace all escape characters from JQuery selectors", {
    forbidden <- " !\"#$%&'()*+,./:;<=>?@[\\]^`{|}~]"
    testthat::expect_identical(
      teal.slice:::NS("app", forbidden),
      paste0(
        "app-h",
        substr(rlang::hash(forbidden), 1, 4),
        paste(rep("_", nchar(forbidden) + 1), collapse = "")
      )
    )
  })

  testthat::it("should replace UTF characters outside the allowed range", {
    id <- "\U41\U05E\U30\U5F\U7A\U1F4AA" # "A:circumflex_accent:0_z:flexed_biceps:
    testthat::expect_identical(
      teal.slice:::NS("app", id),
      paste0(
        "app-h",
        substr(rlang::hash(id), 1, 4),
        "_A_0_z_"
      )
    )
  })
})
