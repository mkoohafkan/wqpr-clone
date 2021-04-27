#' @include services.r
#' @include setup.r
#' @include util.r
#' @include list.r
#' @include events.r
#' @include getters.r
NULL

#' Verification Instruments Query
#'
#' Get data on verification instruments associated with the specified
#' event id.
#'
#' @inheritParams wqp_events
#' @inheritParams wqp_result_data
#' @return The query results.
#'
#' @importFrom dplyr filter group_split
#' @export
wqp_verification_instruments = function(event.id, program, database,
  bind = FALSE) {
  event.id = as.integer(event.id)
  if (any(is.na(event.id)) || length(event.id) < 1L) {
    stop("Invalid format of argument \"event.id\".")
  }
  instrument.data = filter(
    wqp_verification_instrument_details(program, database),
    .data$event_id %in% event.id
  )
  if (!bind) {
    out = group_split(instrument.data, .data$event_id, keep = FALSE)
    attr(out, "ptype") = NULL
    out
  } else {
    instrument.data
  }
}

#' New Verification Instrument to WQP Records
#'
#' Format verification instruments for insertion to WQP, including the
#' identification of target event IDs.
#'
#' @param new.data A dataframe of processed data. Must contain
#'   data column **instrument_name** to identify the
#'   standard solution type. If column *event_id* is not provided
#'   and `match.event` is `TRUE`,
#'   additional columns **arrival_time**, **cdec_code**,
#'   **contact_name**, and **event_type_name** must be provided to
#'   identify the event ID.
#' @param match.event If `TRUE`, search for an existing event to
#'   associate with `new.data`.
#' @inheritParams wqp_format_result_data
#' @inheritParams wqp-versions
#' @return A dataframe of records formatted for insertion into WQP using
#'  `wqp_insert_verification_instruments`
#'
#' @seealso wqpr::wqp_verification_instrument_types()
#'   wqpr::wqp_verification_instrument_details()
#'
#' @importFrom rlang .data
#' @importFrom purrr map_chr
#' @export
wqp_format_verification_instruments = function(new.data,
  na.action = c("warning", "error"), match.event = FALSE,
  program, database) {
  na.action = match.arg(na.action, c("warning", "error"))
  database = validate_database(database)
  program = validate_program(program)
  event.id.var = "event_id"
  event.lookup.vars = c("arrival_time", "cdec_code", "contact_name",
    "event_type_name")
  instrument.lookup.var = "instrument_name"
  optional.vars = c("serial_number", "calibration_due_date")
  check_fields(instrument.lookup.var, names(new.data))
  check_optional_fields(optional.vars, names(new.data))
  # begin data reformatting
  input.data = new.data
  select.vars = c("event_id", "instrument_id")
  # serial number checking
  if ("serial_number" %in% names(input.data)) {
    check_na(input.data, "serial_number", "serial numbers", "warning")
    select.vars = c(select.vars, "serial_number")
  }
  # calibration due date timestamps
  if ("calibration_due_date" %in% names(input.data)) {
    input.data["calibration_due_date"] = check_timestamps(input.data,
      "calibration_due_date", "warning")
    select.vars = c(select.vars, "calibration_due_date")
  }
  # event ID lookup
  if (!(event.id.var %in% names(input.data)) && match.event) {
    message(sprintf("Column \"%s\" not found. ", event.id.var),
      "Looking up event ID...")
    input.data[event.id.var] = get_event_id(input.data,
      program, database)
    check_ids("event", pmap_chr(input.data, paste, sep = "  "),
      input.data$event_id, na.action)
  } else if (!(event.id.var %in% names(input.data)) && !match.event) {
    input.data[event.id.var] = NA_integer_
  }
  # instrument ID lookup
  input.data["instrument_id"] = get_instrument_id(input.data,
    program, database)
  check_ids(input.data$instrument_name, input.data$instrument_id,
    na.action)
  # arrange and records
  return.vars = c(select.vars, setdiff(names(new.data), select.vars))
  input.data[return.vars]
}

#' Insert Verification Instrument Records
#'
#' Insert verification instrument records into WQP.
#'
#' @param records A dataset with 2 required columns and 2 optional
#'   columns, in order:
#'   **event_id**, **instrument_id**, *serial_number*,
#'   *calibration_due_date*.
#' @inheritParams wqp_insert_events
#' @return A data frame with the server response to each insert attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate if_else
#' @importFrom stringr str_c str_subset
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
wqp_insert_verification_instruments = function(records,
  overwrite = FALSE, program, database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  required.vars = c("event_id", "instrument_id")
  optional.vars = c("serial_number", "calibration_due_date")
  check_fields(required.vars, names(records))
  check_optional_fields(optional.vars, names(records))
  # check existing data
  checked.records = suppressWarnings(check_verification_instruments(
    records, program, database))
  # prepare insert data
  insert.data = mutate(checked.records,
    calibration_due_date = reformat_wqp_datetime(
      .data$calibration_due_date, ":"),
    insert.ok = is.na(.data$edit_timestamp) | overwrite,
    insert.url = if_else(!.data$insert.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/",
        "{event.verificationinstrument.insert.service}"))),
    insert.body = post_body(
      event_id = .data$event_id,
      instrument_id = .data$instrument_id,
      serial_number = .data$serial_number,
      calibration_due_date = .data$calibration_due_date,
      program = program.id[[program]],
      token = token,
      update = as.integer(overwrite)
    ),
    result = NA_character_
  )
  # asynchronous POST
  insert.data$result[!insert.data$insert.ok] =
    "Existing data preserved."
  if (any(insert.data$insert.ok)) {
    insert.data$result[insert.data$insert.ok] = multi_post(
      insert.data$insert.url[insert.data$insert.ok],
      insert.data$insert.body[insert.data$insert.ok],
      stop.on.fail = FALSE
    )
  }
  check_insert(insert.data[c(required.vars, "result")], "result",
    "verification instrument records", "warning")
  select(insert.data, .data$event_id, .data$instrument_id,
    .data$serial_number, .data$calibration_due_date, .data$result)
}


#' Check Verification Instrument Records
#'
#' @param records A dataset of event IDs, instrument IDs, serial
#'   numbers, and calibration due dates. These records are checked
#'   against existing data.
#' @return The records dataset, with additional columns
#'   with suffix `".existing"`.
#'
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom dplyr summarize group_by select left_join
#' @importFrom rlang .data
#' @keywords internal
check_verification_instruments = function(records, program, database) {
  existing.data = wqp_verification_instrument_details(program, database)
  left_join(records, existing.data,
    by = c("event_id", "instrument_id"),
    suffix = c("", ".existing"))
}


#' Insert Verification Instrument Types
#'
#' Insert Verification Instrument types into WQP.
#'
#' @param records A dataset with 1 required column "instrument" and
#'   1 optional column "notes".
#' @inheritParams wqp_insert_result_data
#' @return A data frame with the server response to each insert attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate if_else
#' @importFrom stringr str_c str_subset
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
wqp_insert_verification_instrument_types = function(records,
  overwrite = FALSE, program, database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  expected.vars = c("instrument_name", "notes")
  required.vars = c("instrument_name")
  check_fields(required.vars, names(records))
  check_optional_fields(expected.vars, names(records))
  # check existing data
  checked.records = suppressWarnings(
    check_verification_instrument_types(records, program, database))
  # prepare insert data
  insert.data = mutate(checked.records,
    insert.ok = is.na(.data$instrument_id) | overwrite,
    insert.url = if_else(!.data$insert.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/",
        "{asset.verificationinstrument.types.insert.service}"))),
    insert.body = post_body(
      instrument_id = if_else(is.na(.data$instrument_id), -1L,
        .data$instrument_id),
      instrument_name = .data$instrument_name,
      notes = .data$notes,
      program = program.id[[program]],
      update = as.integer(overwrite),
      token = token
    ),
    result = NA_character_
  )
  # asynchronous POST
  insert.data$result[!insert.data$insert.ok] =
    "Existing data preserved."
  if (any(insert.data$insert.ok)) {
    insert.data$result[insert.data$insert.ok] = multi_post(
      insert.data$insert.url[insert.data$insert.ok],
      insert.data$insert.body[insert.data$insert.ok],
      stop.on.fail = FALSE
    )
  }
  inserts.requested = sum(insert.data$insert.ok)
  inserts.succeeded = length(str_subset(insert.data$result,
    "(Insert|Update) was successful."))
  if (inserts.succeeded < inserts.requested) {
    warning(inserts.requested - inserts.succeeded, " of ",
      inserts.requested, " inserts failed")
  }
  select(insert.data, .data$instrument_name, .data$instrument_id,
    .data$result)
}

#' Check Verification Instrument Types
#'
#' @param records A dataset of verification instruments.
#'   These records are checked against existing data.
#'
#' @return The records dataset, with additional columns
#'   with suffix `".existing"`.
#'
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom dplyr summarize group_by select left_join
#' @importFrom rlang .data
#' @keywords internal
check_verification_instrument_types = function(records,
  program, database) {
  existing.data = wqp_verification_instrument_types(program, database)
  left_join(records, existing.data,
    by = c("instrument_name"),
    suffix = c("", ".existing"))

    #TODO: think about how to allow updating instrument name

   #bind_rows(
     #left_join(filter(records, is.na(instrument_id)), existing.data,
       #by = c("instrument"),
       #suffix = c("", ".existing")),
     #mutate(
       #left_join(filter(records, !is.na(instrument_id)), existing.data,
         #by = c("instrument_id"),
         #suffix = c("", ".existing")),
       #instrument_id.existing = .data$instrument_id
     #)
   #)

}
