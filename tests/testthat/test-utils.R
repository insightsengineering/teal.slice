# make_c_call ----
testthat::test_that("make_c_call", {
  testthat::expect_identical(make_c_call(1:3), quote(c(1L, 2L, 3L)))
  testthat::expect_identical(make_c_call(1), 1)
})

testthat::describe("sanitize_id", {
  testthat::it("should replace non-ASCII characters in middle of id with `_`", {
    testthat::expect_identical(NS("app", "a$b"), paste0("app-", substr(rlang::hash("a$b"), 1, 4), "_a_b"))
  })

  testthat::it("should replace non-ASCII characters in the start/end of id with `_`", {
    testthat::expect_identical(NS("app", "%a.b%c$"), paste0("app-", substr(rlang::hash("%a.b%c$"), 1, 4), "__a_b_c_"))
  })

  testthat::it("should replace all quotes characters with `_`", {
    testthat::expect_identical(NS("app", " a.b.c\"d`e'j"), paste0("app-", substr(rlang::hash(" a.b.c\"d`e'j"), 1, 4), "__a_b_c_d_e_j"))
  })

  testthat::it("should replace UTF-8 special characters with `_`", {
    testthat::expect_identical(
      NS("app", "a\U1F643"),
      paste0("app-", substr(rlang::hash("a\U1F643"), 1, 4), "_a_")
    )
  })

  testthat::it("should replace all escape characters from JQuery selectors", {
    forbidden <- " !\"#$%&'()*+,./:;<=>?@[\\]^`{|}~]"
    testthat::expect_identical(
      NS("app", forbidden),
      paste0("app-", substr(rlang::hash(forbidden), 1, 4), paste(rep("_", nchar(forbidden) + 1), collapse = ""))
    )
  })
})
