for (test.db in all.databases) {
  for (test.program in all.programs) {

    test_that(glue("event listing works ({test.db}:{test.program})"), {
      skip_if_no_db()

      expect_warning(expect_s3_class(
        wqp_event_details(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_event_types(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_event_reasons(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_event_summaries(test.program, test.db),
        "data.frame"
      ), regexp = NA)

    })

  }
}



test_that("event insertion/update works", {

  skip("insertion and update tests are not run")
})



for (test.db in all.databases) {
  for (test.program in all.programs) {

    test_that(glue("event asset listing works ({test.db}:{test.program})"), {
      skip_if_no_db()

      expect_warning(expect_s3_class(
        wqp_verification_instrument_details(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_standard_solution_details(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_action_details(test.program, test.db),
        "data.frame"
      ), regexp = NA)

    })
  }
}



test_that("event asset insertion/update works", {

  skip("insertion and update tests are not run")
})
