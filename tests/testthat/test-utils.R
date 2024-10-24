# make_c_call ----
testthat::test_that("make_c_call", {
  testthat::expect_identical(make_c_call(1:3), quote(c(1L, 2L, 3L)))
  testthat::expect_identical(make_c_call(1), 1)
})

testthat::describe("sanitize_id", {
  testthat::it("should replace non-ASCII characters in middle of id with `_`", {
    id <- "a$b"
    testthat::expect_identical(
      NS("app", id),
      paste0("app-", substr(rlang::hash(id), 1, 4), "_a_b")
    )
  })

  testthat::it("should replace non-ASCII characters in the start/end of id with `_`", {
    id <- "%a.b%c$"
    testthat::expect_identical(
      NS("app", id),
      paste0("app-", substr(rlang::hash(id), 1, 4), "__a_b_c_")
    )
  })

  testthat::it("should replace all quotes characters with `_`", {
    id <- " a.b.c\"d`e'j"
    testthat::expect_identical(
      NS("app", id),
      paste0("app-", substr(rlang::hash(id), 1, 4), "__a_b_c_d_e_j")
    )
  })

  testthat::it("should replace UTF-8 special characters with `_`", {
    id <- "a\U1F643"
    testthat::expect_identical(
      NS("app", id),
      paste0("app-", substr(rlang::hash(id), 1, 4), "_a_")
    )
  })

  testthat::it("should replace all escape characters from JQuery selectors", {
    forbidden <- " !\"#$%&'()*+,./:;<=>?@[\\]^`{|}~]"
    testthat::expect_identical(
      NS("app", forbidden),
      paste0(
        "app-",
        substr(rlang::hash(forbidden), 1, 4),
        paste(rep("_", nchar(forbidden) + 1), collapse = "")
      )
    )
  })
})
