#' @include services.r
#' @include setup.r
#' @include util.r
#' @include list.r
#' @include getters.r
NULL

#' Events Query
#'
#' Get data for the specified result id.
#'
#' @param event.id The WQP event id.
#' @inheritParams wqp_result_data
#' @return The query results.
#'
#' @importFrom glue glue
#' @importFrom dplyr tibble bind_rows
#' @importFrom purrr map map_lgl
#' @export
wqp_events = function(event.id, program, database, bind = FALSE) {
  database = validate_database(database)
  program = validate_program(program)
  event.id = as.integer(event.id)
  if (any(is.na(event.id)) || length(event.id) < 1L)
    stop("Invalid format of argument \"event.id\".")
  results = multi_get(glue(
    "{base.url[[database]]}/{event.detail.service}", "?",
    "program={program.id[[program]]}", "&",
    "event_id={event.id}"
    ), stop.on.fail = TRUE)
  # process results
  result.data = map(results,
    ~ read_results(.x, event.data.format, empty.events))
  # check for empty returns
  missing.ids = event.id[map_lgl(result.data, ~ nrow(.x) < 1L)]
  if (length(missing.ids) > 0L) {
    warning("No data returned for Event ID(s) ",
      paste(missing.ids, collapse = ", "), ".",
      call. = FALSE)
  }
  if (bind) {
    bind_rows(result.data)
  } else {
    result.data
  }
}

#' New Event to WQP Records
#'
#' Format event for insertion to WQP, including the identification of
#' target event IDs.
#'
#' @param new.data A dataframe of processed data. Must contain
#'   data columns `"time"` and `"value"`, and additional identification
#'   columns such as `"reading_type_name"`, `"analyte_name"`,
#'   `"cdec_code"`, etc...
#' @inheritParams wqp_format_result_data
#' @inheritParams wqp-versions
#' @return A dataframe of records formatted for insertion into WQP using
#'  `wqp_insert_result_data`
#'
#' @seealso wqpr::list_all() wqpr::wqp_insert_result_data()
#'
#' @importFrom rlang .data
#' @importFrom dplyr select mutate intersect setdiff
#' @importFrom stringr str_c str_subset str_to_upper
#' @importFrom lubridate is.instant as_datetime
#' @importFrom purrr map modify_if
#' @export
wqp_format_events = function(new.data,
  na.action = c("warning", "error"), program, database) {
  na.action = match.arg(na.action, c("warning", "error"))
  database = validate_database(database)
  program = validate_program(program)
  required.vars = c("arrival_time", "cdec_code", "contact_name",
    "event_type_name")
  optional.vars = c("summary_name", "reason_name", "departure_time",
    "watch_time", "restart_time", "comments")
  time.vars = intersect(c("arrival_time", "departure_time",
    "watch_time", "restart_time"), names(new.data))
  included.vars = intersect(names(new.data),
    c(required.vars, optional.vars))
  check_fields(required.vars, names(new.data))
  check_optional_fields(optional.vars, names(new.data))
  check_na(new.data, setdiff(
    included.vars, time.vars), "records", "warning")
  # lookup translations
  input.data = new.data
  select.vars = c("arrival_time", "station_id", "contact_id",
    "event_type_id")
  # timestamp check
  for (time.var in time.vars) {
    input.data[time.var] = check_timestamps(input.data, time.var,
      na.action)
  }
  # station ID
  if (!("station_id" %in% included.vars)) {
    input.data["station_id"] = get_station_id(input.data,
      program, database)
    check_ids("station", input.data$cdec_code, input.data$station_id,
      na.action)
  }
  # contact ID
  if (!("contact_id" %in% included.vars)) {
    input.data["contact_id"] = get_contact_id(input.data, program,
      database)
    check_ids("contact", input.data$contact_name, input.data$contact_id,
      na.action)
  }
  # event type ID
  if (!("event_type_id" %in% included.vars)) {
    input.data["event_type_id"] = get_event_type_id(input.data, program,
      database)
    check_ids("event type", input.data$event_type_name,
      input.data$event_type_id, na.action)
  }
  # Summary ID
  if ("summary_id" %in% included.vars) {
    select.vars = append(select.vars, "summary_id")
  } else if ("summary_name" %in% included.vars) {
    input.data["summary_id"] = get_summary_id(input.data, program,
      database)
    check_ids("summary", input.data$summary_name,
      input.data$summary_id, na.action)
    select.vars = append(select.vars, "summary_id")
  }
  # Reason ID
  if ("reason_id" %in% included.vars) {
    select.vars = append(select.vars, "reason_id")
  } else if ("reason_name" %in% included.vars) {
    input.data["reason_id"] = get_reason_id(input.data, program,
      database)
    check_ids("reason", input.data$reason_name, input.data$reason_id,
      na.action)
    select.vars = append(select.vars, "reason_id")
  }
  # arrange records and return
  return.vars = c(select.vars, setdiff(names(new.data), select.vars))
  input.data[return.vars]
  }


#' Insert Event Records
#'
#' Insert event records into WQP.
#'
#' @param records A dataset with 4 required columns and
#'   6 optional columns, in order: **arrival_time**,
#'   **station_id**, **contact_id**, **event_type_id**, *summary_id*,
#'   *reason_id*, *departure_time*, *watch_time*, *restart_time*,
#'   *comments*.
#' @inheritParams wqp_insert_result_data
#' @return A data frame with the server response to each insert attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate mutate_at if_else matches vars
#' @importFrom stringr str_c str_subset
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
wqp_insert_events = function(records, overwrite = FALSE, program,
  database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  expected.vars = c("arrival_time", "station_id", "contact_id",
    "event_type_id", "summary_id", "reason_id",
    "departure_time", "watch_time", "restart_time", "comments")
  required.vars = c("arrival_time", "station_id", "contact_id",
    "event_type_id")
  check_fields(required.vars, names(records))
  check_optional_fields(expected.vars, names(records))
  invalid.records = check_na(records, required.vars,
    "event records", "warning")
  # check existing data
  checked.records = suppressWarnings(check_events(records, program,
    database))
  # prepare insert data
  insert.data = mutate_at(checked.records, vars(matches("time")),
      ~reformat_wqp_datetime(.x, ":"))
  insert.data = mutate(insert.data,
    insert.ok = is.na(.data$event_id) | overwrite,
    insert.url = if_else(!.data$insert.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/{event.insert.service}"))),
    insert.body = post_body(
      arrivaltime = .data$arrival_time,
      station_id = .data$station_id,
      contact_id = .data$contact_id,
      event_type_id = .data$event_type_id,
      summary_id = .data$summary_id,
      reason_id = .data$reason_id,
      departuretime = .data$departure_time,
      watchtime = .data$watch_time,
      restarttime = .data$restart_time,
      comments = .data$comments,
      program = program.id[[program]],
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
    "event records", "warning")
  select(insert.data, .data$arrival_time, .data$station_id,
    .data$contact_id, .data$event_type_id, .data$result)
}

#' Check Event Records
#'
#' @param records A dataset of arrival times, station IDs, and
#'   contact IDs. These records are checked against existing data.
#' @return The records dataset, with additional columns
#'   with suffix `".existing"`.
#'
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom dplyr left_join
#' @importFrom rlang .data
#' @keywords internal
check_events = function(records, program, database) {
  existing.data = wqp_event_details(program, database)
  left_join(records, existing.data,
    by = c("arrival_time", "station_id", "contact_id",
      "event_type_id"),
    suffix = c("", ".existing"))
}
