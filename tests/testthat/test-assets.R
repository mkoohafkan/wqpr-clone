for (test.db in all.databases) {
  for (test.program in all.programs) {

    test_that(glue("asset listing works ({test.db}:{test.program})"), {
      skip_if_no_db()

      expect_warning(expect_s3_class(
        wqp_verification_instrument_types(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_standard_solution_types(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_action_types(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_sondes(test.program, test.db),
        "data.frame"
      ), regexp = NA)

    })

  }
}

test_that("asset insertion/update works", {

  skip("insertion and update tests are not run")
})
