#' @include services.r
#' @include setup.r
#' @include util.r
#' @include list.r
#' @include getters.r
NULL


#' Result Data Query
#'
#' Get data for the specified result id.
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @inheritParams wqp-versions
#' @param result.id The WQP result id.
#' @param start.date The query start date. If missing, the earliest
#'   available date is used.
#' @param end.date The query end date. If missing, the latest
#'   available date is used.
#' @param bind If `TRUE`, bind the results of multiple queries into a
#'   single data frame.
#' @return The query results.
#'
#' @importFrom glue glue
#' @importFrom dplyr tibble bind_rows
#' @importFrom purrr map map_lgl
#' @export
wqp_result_data = function(result.id, start.date, end.date, version,
  program, database, bind = FALSE) {
  database = validate_database(database)
  program = validate_program(program)
  version = validate_version(version, program)
  result.id = as.integer(result.id)
  if (any(is.na(result.id)) || length(result.id) < 1L) {
    stop("Invalid format of argument \"result.id\".")
  }
  # start/end date specification
  start.date = process_datetime(start.date, "1900-01-01")
  end.date = process_datetime(end.date, Sys.Date() + 1)
  if (any(start.date > end.date)) {
    stop("Specified start date is later than end date.", call. = FALSE)
  }
  # format for query
  formstart = reformat_wqp_datetime(start.date, ":")
  formend = reformat_wqp_datetime(end.date, ":")
  # vector recycling
  d = data.frame(result_id = result.id, start_date = formstart,
    end_date = formend, version = version, stringsAsFactors = FALSE)
  # query WQP
  results = multi_get(
    glue("{base.url[[database]]}/{result.data.service}", "?",
      "program={program.id[[program]]}", "&",
      "resultid={d$result_id}", "&",
      "start={d$start_date}", "&",
      "end={d$end_date}", "&",
      "version={d$version}"),
    stop.on.fail = TRUE)
  # process results
  result.data = map(results,
    ~ read_results(.x, result.data.format, empty.results))
  # check for empty returns
  missing.ids = result.id[map_lgl(result.data, ~ nrow(.x) < 1L)]
  if (length(missing.ids) > 0L) {
    warning("No data returned for Result ID(s) ",
      paste(missing.ids, collapse = ", "), ".",
      call. = FALSE)
  }
  if (bind) {
    bind_rows(result.data)
  } else {
    result.data
  }
}


#' Insert Result Data Records
#'
#' Insert Result records into WQP.
#'
#' @param records A dataset with 5 required columns, in order:
#'   **result_id**, **time**, **value**, **qaqc_flag_id**, and
#'   **version**. `NA` flag entries will automatically be assigned
#'   code `"U"`.
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @inheritParams wqp-tokens
#' @param overwrite If `TRUE`, perform an update if the record already
#'  exists.
#' @return A data frame with the server response to each insert attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate if_else setdiff
#' @importFrom stringr str_c
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
wqp_insert_result_data = function(records, overwrite = FALSE,
  program, database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  required.vars = c("result_id", "time", "value", "qaqc_flag_id",
    "version")
  check_fields(required.vars, names(records))
  check_na(records, setdiff(required.vars, "value"),
    "result data records", "error")
  # check existing data
  checked.records = suppressWarnings(check_result_data(records,
    program, database))
  # prepare insert data
  insert.data = mutate(checked.records,
    time = reformat_wqp_datetime(.data$time, ":"),
    insert.ok = is.na(.data$qaqc_flag_id.existing) | overwrite,
    insert.url = if_else(!.data$insert.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/",
        "{result.data.insert.service}"))),
    insert.body = post_body(
      resultid = .data$result_id,
      time = .data$time,
      value = .data$value,
      qaqc = .data$qaqc_flag_id,
      program = program.id[[program]],
      version = .data$version,
      update = as.integer(overwrite),
      token = token
    ),
    result = NA_character_
  )
  # asynchronous POST
  insert.data$result[!insert.data$insert.ok] = paste("Existing data",
    "preserved.")
  if (any(insert.data$insert.ok)) {
    insert.data$result[insert.data$insert.ok] = multi_post(
      insert.data$insert.url[insert.data$insert.ok],
      insert.data$insert.body[insert.data$insert.ok],
      stop.on.fail = FALSE
    )
  }
  check_insert(insert.data[c(required.vars, "result")], "result",
    "result data records", "warning")
  select(insert.data, .data$result_id, .data$time, .data$result)
}

#' Check Result Records
#'
#' @param records A dataset of result ids, timestamps, values, and flag
#'   codes. These records are checked against existing data.
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @return The records dataset, with additional columns
#'   `"values.existing"` and `"qaqc_flag_id.existing"`.
#'
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom dplyr summarize group_by select left_join
#' @importFrom rlang .data
#' @keywords internal
check_result_data = function(records, program, database) {
  ranges = summarize(group_by(records, .data$result_id),
    start.date = floor_date(min(.data$time), "days"),
    end.date = ceiling_date(max(.data$time), "days"))
  existing.data = wqp_result_data(ranges$result_id, ranges$start.date,
    ranges$end.date, ranges$version, program, database, bind = TRUE)
  left_join(records, existing.data,
    by = c("result_id", "time", "version"), suffix = c("", ".existing"))
}

#' Update Flags
#'
#' Update QAQC flags in WQP. This function does not modify the actual
#'   data values.
#'
#' @param records A dataset of result ids, timestamps, values, flag
#'   codes, and versions. The data values are used to check for changes
#'   to the data. No changes to the data are pushed to the database.
#' @param overwrite Which flag codes are allowed to be overwritten.
#'  By default, only `"U"` (unchecked) flags can be overwritten.
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @inheritParams wqp-tokens
#' @return A data frame with the server response to each update attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate pull if_else near
#' @importFrom stringr str_c
#' @importFrom rlang .data
#' @export
wqp_update_result_flags = function(records, overwrite = c("U"),
  program, database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  required.vars = c("result_id", "time", "value", "qaqc_flag_id",
    "version")
  check_fields(required.vars, names(records))
  records["version"] = validate_version(records$version, program)
  # check existing data
  checked.records = suppressWarnings(
    check_result_data(records, program, database)
  )
  # verify that the record values match existing data
  na.records = is.na(checked.records$value.existing) &
    is.na(checked.records$value.existing)
  missing.records = is.na(checked.records$value.existing)
  nomatch.records = !missing.records &
    !near(checked.records$value, checked.records$value.existing)
  verified.records = na.records | !missing.records | !nomatch.records
  if (!all(verified.records)) {
    stop(sum(!verified.records), " of ",
      length(verified.records), " records do not match",
      " existing data. Check that timestamps are correct.",
      call. = FALSE)
  }
  # prepare update data
  update.data = mutate(checked.records,
    formtime = reformat_wqp_datetime(.data$time, ":"),
    update.ok = .data$qaqc_flag_id.existing %in% overwrite,
    update.url = if_else(!.data$update.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/",
        "{result.data.insert.service}"))),
    update.body = post_body(
      resultid = .data$result_id,
      time = .data$formtime,
      qaqc = .data$qaqc_flag_id,
      program = program.id[[program]],
      version = .data$version,
      update = as.integer(TRUE),
      token = token
    ),
    result = NA_character_
  )
  # asynchronous post
  update.data$result[!update.data$update.ok] = paste("Existing data",
    "preserved.")
  if (any(update.data$update.ok)) {
    update.data$result[update.data$update.ok] = multi_post(
      update.data$update.url[update.data$update.ok],
      update.data$update.body[update.data$update.ok],
      stop.on.fail = FALSE
    )
  }
  check_insert(update.data[c(required.vars, "result")], "result",
    "result data flags", "warning")
  select(update.data, .data$result_id, .data$time, .data$result)
}



#' New Results to WQP Records
#'
#' Format result data for insertion to WQP, including the
#' identification of target result IDs.
#'
#' @param new.data A dataframe of processed data. Must contain
#'   data columns `"time"` and `"value"`, and additional identification
#'   columns such as `"reading_type_name"`, `"analyte_name"`,
#'   `"cdec_code"`, etc.
#' @param na.action How missing IDs are handled, i.e., data that cannot
#'   be associated with an existing result ID will return either a
#'   `warning` or an `error`.
#' @inheritParams wqp_insert_result_data
#' @return A dataframe of records formatted for insertion into WQP using
#'  `wqp_insert_result_data`
#'
#' @seealso wqpr::list_all() wqpr::wqp_insert_result_data()
#'
#' @importFrom rlang .data
#' @importFrom dplyr intersect setdiff
#' @importFrom purrr pmap_chr
#' @export
wqp_format_result_data = function(new.data,
  na.action = c("warning", "error"), program, database) {
  na.action = match.arg(na.action, c("warning", "error"))
  database = validate_database(database)
  program = validate_program(program)
  # variable checking
  id.vars = c("cdec_code", "analyte_name", "unit_name",
    "interval_name", "reading_type_name")
  optional.vars = c("rank_name", "aggregate_name", "probe_depth",
    "equipment_name")
  time.var = "time"
  value.var = "value"
  qa.var = "qaqc_flag_id"
  required.vars = c(id.vars, time.var, value.var)
  included.id.vars = intersect(c(id.vars, optional.vars),
    names(new.data))
  input.data = new.data
  # field check
  check_fields(required.vars, names(input.data))
  check_na(input.data, included.id.vars, "identifiers", "error")
  # timestamp check
  input.data[time.var] = check_timestamps(input.data, time.var,
    na.action)
  # value check
  check_na(input.data, value.var, "values", "warn")
  # flag check
  if (!(qa.var %in% names(input.data))) {
    input.data[qa.var] = NA_character_
  }
  # version check
  if (!("version" %in% names(input.data))) {
    input.data["version"] = validate_version(input.data$version,
      program = program)
  }
  # result ID
  select.vars = c("result_id", "time", "value", "qaqc_flag_id",
    "version")
  input.data["result_id"] = get_result_id(input.data, program, database)
  # check NA
  check_ids("result", pmap_chr(input.data[names(new.data)],
    paste, sep = "  "), input.data$result_id, na.action)
  # assign flags as "unchecked" or "missing
  input.data[is.na(input.data[[qa.var]]) &
    is.na(input.data[[value.var]]), qa.var] = "M"
  input.data[is.na(input.data[[qa.var]]), qa.var] = "U"
  # arrange records and return
  return.vars = c(select.vars, setdiff(names(new.data), select.vars))
  input.data[return.vars]
}
