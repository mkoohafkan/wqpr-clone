for (test.db in all.databases) {
  for (test.program in all.programs) {

    test_that(glue("meta listing works ({test.db}:{test.program})"), {
      skip_if_no_db()

      expect_warning(expect_s3_class(
        wqp_stations(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_contacts(test.program, test.db),
        "data.frame"
      ), regexp = NA)

      expect_warning(expect_s3_class(
        wqp_locations(test.program, test.db),
        "data.frame"
      ), regexp = NA)

    })

  }
}

test_that("meta insertion/update works", {

  skip("insertion and update tests are not run")
})
