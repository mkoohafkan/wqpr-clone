#' @include services.r
#' @include setup.r
#' @include util.r
#' @include list.r
#' @include events.r
#' @include getters.r
NULL

#' Standard Solutions Query
#'
#' Get data on standard solutions associated with the specified
#' event id.
#'
#' @inheritParams wqp_events
#' @inheritParams wqp_result_data
#' @return The query results.
#'
#' @importFrom dplyr filter group_split
#' @export
wqp_standard_solutions = function(event.id, program, database,
  bind = FALSE) {
  event.id = as.integer(event.id)
  if (any(is.na(event.id)) || length(event.id) < 1L) {
    stop("Invalid format of argument \"event.id\".")
  }
  solution.data = filter(
    wqp_standard_solution_details(program, database),
    .data$event_id %in% event.id
  )
  if (!bind) {
    out = group_split(solution.data, .data$event_id, keep = FALSE)
    attr(out, "ptype") = NULL
    out
  } else {
    solution.data
  }
}

#' New Standard Solution to WQP Records
#'
#' Format standard solutionsfor insertion to WQP, including the
#' identification of target event IDs.
#'
#' @param new.data A dataframe of processed data. Must contain
#'   data column **solution_name** to identify the
#'   standard solution type. If column *event_id* is not provided and
#'   argument `match.event` is `TRUE`,
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
#' @importFrom dplyr select case_when mutate left_join
#' @importFrom stringr str_c str_subset str_to_upper str_to_sentence
#' @importFrom purrr map modify_if
#' @export
wqp_format_standard_solutions = function(new.data,
  na.action = c("warning", "error"), match.event = FALSE,
  program, database) {
  na.action = match.arg(na.action, c("warning", "error"))
  database = validate_database(database)
  program = validate_program(program)
  event.id.var = "event_id"
  event.lookup.vars = c("arrival_time", "cdec_code", "contact_name",
    "event_type_name")
  solution.lookup.var = "solution_name"
  optional.vars = c("lot_number", "expiration_date")
  check_fields(solution.lookup.var, names(new.data))
  check_optional_fields(optional.vars, names(new.data))
  # begin data reformatting
  input.data = new.data
  select.vars = c("event_id", "solution_id")
  # lot number checking
  if ("lot_number" %in% names(input.data)) {
    check_na(input.data, "lot_number", "lot numbers", "warning")
    select.vars = c(select.vars, "lot_number")
  }
  # expiration date timestamps
  if ("expiration_date" %in% names(input.data)) {
    input.data["expiration_date"] = check_timestamps(input.data,
      "expiration_date", "warning")
    select.vars = c(select.vars, "expiration_date")
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
  input.data["solution_id"] = get_solution_id(input.data, program,
    database)
  check_ids(input.data$solution_name, input.data$solution_id, na.action)
  # arrange and return records
  return.vars = c(select.vars, setdiff(names(new.data), select.vars))
  input.data[return.vars]
}




#' Insert Standard Solution Records
#'
#' Insert standard solution records into WQP.
#'
#' @param records A dataset with 2 required columns and 2 optional
#'   columns, in order:
#'   **event_id**, **solution_id**, *lot_number*, *expiration_date*.
#' @inheritParams wqp_insert_events
#' @return A data frame with the server response to each insert attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate if_else
#' @importFrom stringr str_c str_subset
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
wqp_insert_standard_solutions = function(records, overwrite = FALSE,
  program, database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  required.vars = c("event_id", "solution_id")
  optional.vars = c("lot_number", "expiration_date")
  check_fields(required.vars, names(records))
  check_optional_fields(optional.vars, names(records))
  # check existing data
  checked.records = suppressWarnings(check_standard_solutions(records,
    program, database))
  # prepare insert data
  insert.data = mutate(checked.records,
    expiration_date = reformat_wqp_datetime(.data$expiration_date, ":"),
    insert.ok = is.na(.data$edit_timestamp) | overwrite,
    insert.url = if_else(!.data$insert.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/",
        "{event.standardsolution.insert.service}"))),
    insert.body = post_body(
      event_id = .data$event_id,
      solution_id = .data$solution_id,
      lot_number = .data$lot_number,
      expiration_date = .data$expiration_date,
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
    "standard solution records", "warning")
  select(insert.data, .data$event_id, .data$solution_id,
    .data$lot_number, .data$expiration_date, .data$result)
}


#' Check Standard Solution Records
#'
#' @param records A dataset of event IDs, solution IDs, lot
#'   numbers, and expiration dates. These records are checked
#'   against existing data.
#' @return The records dataset, with additional columns
#'   with suffix `".existing"`.
#'
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom dplyr summarize group_by select left_join
#' @importFrom rlang .data
#' @keywords internal
check_standard_solutions = function(records, program, database) {
  existing.data = wqp_standard_solution_details(program, database)
  left_join(records, existing.data,
    by = c("event_id", "solution_id"),
    suffix = c("", ".existing"))
}



#' Insert Standard Solution Types
#'
#' Insert Standard Solution types into WQP.
#'
#' @param records A dataset with 1 required column and 1 optional
#'   column, in order: **solution**, *notes*.
#' @inheritParams wqp_insert_result_data
#' @return A data frame with the server response to each insert attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate if_else
#' @importFrom stringr str_c str_subset
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
wqp_insert_standard_solution_types = function(records,
  overwrite = FALSE, program, database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  expected.vars = c("solution_name", "notes")
  required.vars = c("solution_name")
  check_fields(required.vars, names(records))
  check_optional_fields(expected.vars, names(records))
  # check existing data
  checked.records = suppressWarnings(check_standard_solution_types(
    records, program, database))
  # prepare insert data
  insert.data = mutate(checked.records,
    insert.ok = is.na(.data$solution_id) | overwrite,
    insert.url = if_else(!.data$insert.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/",
        "{asset.verificationinstrument.types.insert.service}"))),
    insert.body = post_body(
      solution_id = if_else(is.na(.data$solution_id), -1L,
        .data$solution_id),
      solution_name = .data$solution_name,
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
  check_insert(insert.data, "result", "standard solution types",
    "warning")
  select(insert.data, .data$solution_name, .data$solution_id,
    .data$result)
}

#' Check Standard Solution Types
#'
#' @param records A dataset of standard solutions.
#'   These records are checked against existing data.
#'
#' @return The records dataset, with additional columns
#'   with suffix `".existing"`.
#'
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom dplyr summarize group_by select left_join
#' @importFrom rlang .data
#' @keywords internal
check_standard_solution_types = function(records, program, database) {
  existing.data = wqp_standard_solution_types(program, database)
  left_join(records, existing.data,
    by = c("solution_name"),
    suffix = c("", ".existing"))
}
