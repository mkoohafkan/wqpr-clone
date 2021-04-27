all.programs = c("marsh")
all.databases = c("test", "production")

for (test.db in all.databases) {
  for (test.program in all.programs) {

    test_that(glue("result listing works ({test.db}:{test.program})"), {
      skip_if_no_db()

      expect_warning(expect_s3_class(
        wqp_result_details(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_result_constituents(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_result_reading_types(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      test.result = pull(
        wqp_result_details(test.program, test.db),
        .data$result_id)[1]

      expect_warning(expect_s3_class(
        wqp_result_dates(test.result, test.program, test.db),
        "data.frame"
      ), regexp = NA)
    })

  }
}



test_that("result insertion/update works", {

  skip("insertion and update tests are not run")
})
