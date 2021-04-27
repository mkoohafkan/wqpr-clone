#' @include services.r
#' @include setup.r
#' @include util.r
NULL

#' WQP Reports
#'
#' Functions for generating reports with WQP.
#' @name wqp-reports
NULL

#' @describeIn wqp-reports Progressive Daily Mean report.
#'
#' @inheritParams wqp-listings
#' @inheritParams wqp_result_data
#' @param station.id Integer ID of station to query.
#' @param year The report year.
#' @param month The report month.
#' @param qaqc.flag Return data with the specified flag only.
#'   Default is `"*"` for all flags.
#'
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_if
#' @importFrom readr read_delim
#' @export
wqp_report_pdm = function(station.id, year, month, qaqc.flag = "*",
  program, database, bind = FALSE) {
  database = validate_database(database)
  program = validate_program(program)
  d = data.frame(station = as.integer(station.id),
    year = as.integer(year), month = as.integer(month))
  results = map(glue(
    "{base.url[[database]]}/{report.pdm.service}", "?",
    "program={program.id[[program]]}", "&",
    "station={d$station}", "&",
    "year={d$year}", "&",
    "month={d$month}", "&",
    "qaqc={qaqc.flag}"), basic_get)
  # process results
  result.data = map_if(results,
    ~ .x != "No records found.",
    ~ read_delim(.x, delim = ",", trim_ws = TRUE, na = "null",
      col_types = report.pdm.format,
      locale = wqpr.locale),
    .else = function(...) empty.pdm.results)
  missing.ids = station.id[map_lgl(result.data, ~ nrow(.x) < 1L)]
  if (length(missing.ids) > 0L) {
    warning("No data returned for Station ID(s) ",
      paste(missing.ids, collapse = ", "), ".",
      call. = FALSE)
  }
  if (bind) {
    bind_rows(result.data)
  } else {
    result.data
  }
}
