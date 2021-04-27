#' @include services.r
#' @include setup.r
#' @include util.r
#' @include list.r
#' @include events.r
#' @include getters.r
NULL

#' Actions Query
#'
#' Get data on actions associated with the specified event id.
#'
#' @inheritParams wqp_events
#' @inheritParams wqp_result_data
#' @return The query results.
#'
#' @importFrom dplyr filter group_split
#' @export
wqp_actions = function(event.id, program, database, bind = FALSE) {
  event.id = as.integer(event.id)
  if (any(is.na(event.id)) || length(event.id) < 1L) {
    stop("Invalid format of argument \"event.id\".")
  }
  action.data = filter(wqp_action_details(program, database),
    .data$event_id %in% event.id)
  if (!bind) {
    out = group_split(action.data, .data$event_id, keep = FALSE)
    attr(out, "ptype") = NULL
    out
  } else {
    action.data
  }
}

#' New Action to WQP Records
#'
#' Format actions for insertion to WQP, including the identification of
#' target event IDs.
#'
#' @param new.data A dataframe of processed data. Must contain
#'   columns **sonde_number**, **location_name**, and **action_name**
#'   to identify the Sonde, Location, and Action IDs.
#'   If column *event_id* is not provided and `match.event` is `TRUE`,
#'   additional columns **arrival_time**, **cdec_code**,
#'   **contact_name**, and **event_type_name** must be provided to
#'   identify the event ID.
#' @param match.event If `TRUE`, search for an existing event to
#'   associate with `new.data`.
#' @inheritParams wqp_format_result_data
#' @inheritParams wqp-versions
#' @return A dataframe of records formatted for insertion into WQP using
#'  `wqp_insert_result_data`
#'
#' @seealso wqpr::list_all() wqpr::wqp_insert_result_data()
#'
#' @importFrom rlang .data
#' @importFrom dplyr select case_when mutate left_join
#' @importFrom stringr str_c str_subset str_to_upper str_to_sentence
#' @importFrom purrr map modify_if
#' @export
wqp_format_actions = function(new.data,
  na.action = c("warning", "error"), match.event = FALSE,
  program, database) {
  na.action = match.arg(na.action, c("warning", "error"))
  database = validate_database(database)
  program = validate_program(program)
  event.id.var = "event_id"
  event.lookup.vars = c("arrival_time", "cdec_code", "contact_name",
    "event_type_name")
  action.lookup.vars = c("sonde_name", "location_name", "action_name")
  included.vars = intersect(names(new.data),
    c(event.id.var, event.lookup.vars, action.lookup.vars))
  check_fields(action.lookup.vars, included.vars)
  check_na(new.data, intersect(included.vars, action.lookup.vars),
    "records", "warning")
  # begin data reformatting
  input.data = new.data
  select.vars = c("event_id", "station_id", "sonde_id", "location_id",
    "action_id")
  # event ID lookup
  if (!(event.id.var %in% included.vars) && match.event) {
    message(sprintf("Column \"%s\" not found. ", event.id.var),
      "Looking up event ID...")
    input.data[event.id.var] = get_event_id(input.data, program,
      database)
    check_ids("event", pmap_chr(input.data, paste, sep = "  "),
      input.data$event_id, na.action)
  } else if (!(event.id.var %in% included.vars) && !match.event) {
    input.data[event.id.var] = NA_integer_
  }
  # station ID lookup
  if (!("station_id" %in% included.vars)) {
    input.data["station_id"] = get_station_id(input.data,
      program, database)
    check_ids("station", input.data$cdec_code, input.data$station_id,
      na.action)
  }
  # sonde ID lookup
  if (!("sonde_id" %in% included.vars)) {
    input.data["sonde_id"] = get_sonde_id(input.data, program,
      database)
    check_ids("sonde", input.data$sonde_name, input.data$sonde_id,
      na.action)
  }
  # location ID lookup
  if (!("location_id" %in% included.vars)) {
    input.data["location_id"] = get_location_id(input.data, program,
      database)
    check_ids("location", input.data$location_name,
      input.data$location_id, na.action)
  }
  # action type ID lookup
  if (!("action_id" %in% included.vars)) {
    input.data["action_id"] = get_action_id(input.data, program,
      database)
    check_ids("action", input.data$action_name, input.data$action_id,
      na.action)
  }
  # arrange and return records
  return.vars = c(select.vars, setdiff(names(new.data), select.vars))
  input.data[return.vars]
}

#' Insert Action Records
#'
#' Insert action records into WQP.
#'
#' @param records A dataset with 4 required columns, in order:
#'   **event_id**, **sonde_id**, **location_id**, **action_id**.
#' @param overwrite included for consistency with other functions.
#'   Due to database structure limitations, actions cannot be updated.
#'   Setting `overwrite = TRUE` will result in an error.
#' @inheritParams wqp_insert_events
#' @return A data frame with the server response to each insert attempt.
#'
#' @importFrom glue glue
#' @importFrom dplyr select mutate if_else
#' @importFrom stringr str_c str_subset
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#' @export
wqp_insert_actions = function(records, overwrite = FALSE, program,
  database, token) {
  if (overwrite) {
    warning("Actions cannot be updated.", call. = FALSE)
  }
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  required.vars = c("event_id", "sonde_id", "location_id", "action_id")
  check_fields(required.vars, names(records))
  # check existing data
  checked.records = suppressWarnings(check_actions(records, program,
    database))
  # prepare insert data
  insert.data = mutate(checked.records,
    insert.ok = is.na(.data$edit_timestamp),
    insert.url = if_else(!.data$insert.ok, NA_character_,
      str_c(glue("{base.url[[database]]}/",
        "{event.action.insert.service}"))),
    insert.body = post_body(
      event_id = .data$event_id,
      sonde_id = .data$sonde_id,
      location_id = .data$location_id,
      action_id = .data$action_id,
      program = program.id[[program]],
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
    "action records", "warning")
  select(insert.data, .data$event_id, .data$sonde_id,
    .data$location_id, .data$action_id, .data$result)
}

#' Check Action Records
#'
#' @param records A dataset of event IDs, sonde IDs, location IDs,
#'   and actions. These records are checked against existing data.
#' @return The records dataset, with additional columns
#'   with suffix `".existing"`.
#'
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom dplyr summarize group_by select left_join
#' @importFrom rlang .data
#' @keywords internal
check_actions = function(records, program, database) {
  existing.data = wqp_action_details(program, database)
  left_join(records, existing.data,
    by = c("event_id", "location_id", "action_id"),
    suffix = c("", ".existing"))
}


#' Check Prior Actions
#'
#' Check if the specified actions are consistent with prior actions.
#'
#' @param records Action records, i.e., output of
#'   [`wqp_format_actions()`].
#' @param actions The action types to check for consistency. Default is
#'   to check only "Installed" and "Removed" records.
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @return A logical vector identifying each action record as
#'   consistent (`TRUE`) or not (`FALSE`).
#'
#' @importFrom dplyr select filter left_join slice mutate arrange
#'   group_by group_nest ungroup desc setdiff intersect row_number
#'   all_of
#' @importFrom purrr map2_lgl
#' @keywords internal
check_prior_action = function(records,
  actions = c("Installed", "Removed"), program, database) {

  event.id.var = "event_id"
  time.var = "arrival_time"
  sonde.var = "sonde_name"
  sonde.id.var = "sonde_id"
  action.type.var = "action_name"
  station.var = "station_name"

  # extract subset of actions from records
  selected.records = select(
    filter(records, .data[[action.type.var]] %in% actions),
    -.data[[event.id.var]]
  )
  record.time = unique(selected.records[[time.var]])
  # check that records only include a single event (arrival_time)
  if (length(record.time) > 1L) {
    stop("more than one unique timestamp included in \"records\".")
  }
  # get list of all actions
  all.events = select(wqp_event_details(), .data[[event.id.var]],
    .data[[time.var]])
  all.actions = left_join(wqp_action_details(), all.events,
    by = event.id.var)
  # filter actions by date, type, and sonde
  selected.actions = filter(all.actions,
    .data[[action.type.var]] %in% actions,
    .data[[time.var]] < record.time,
    .data[[sonde.id.var]] %in% selected.records[[sonde.id.var]])
  # extract only the most recent preceding action
  previous.actions = ungroup(slice(
    group_by(arrange(selected.actions, desc(.data[[time.var]])),
      .data[[sonde.id.var]]),
    1
  ))
  common.cols = setdiff(intersect(names(previous.actions),
    names(selected.records)), time.var)
  # add index
  previous.actions = mutate(previous.actions, idx = row_number())
  # join index to current actions and group
  current.group = group_nest(select(
    left_join(selected.records, previous.actions,
      by = c(sonde.var, sonde.id.var), suffix = c("", ".prior")),
    all_of(c("idx", common.cols))
  ), .data$idx, .key = "current")
  # group prior actions
  previous.group = group_nest(select(previous.actions,
    all_of(c("idx", common.cols))
  ), .data$idx, .key = "prior")
  # check consistency
  combined.group = left_join(current.group, previous.group, by = "idx")
  combined.group = mutate(combined.group,
    consistent = map2_lgl(.data$current, .data$prior,
    check_action_consistency))
  combined.group$consistent
}


#' Action Consistency
#'
#' Helper function to check if visit action sequence makes sense.
#'
#' @param current.record One-row dataframe of "current" action.
#' @param prior.record One-row dataframe of "current" action.
#' @return `TRUE` if the action is consistent with the prior record,
#'   `FALSE` otherwise.
#'
#' @keywords internal
check_action_consistency = function(current.record, prior.record) {
  possible.actions = c("Installed", "Removed", "Used to verify",
    "Calibrated", "Checked")
  action.var = "action_name"
  action.id.var = "action_id"
  if (is.null(prior.record)) {
    return(NA)
  }
  if (nrow(current.record) != 1L || nrow(prior.record) != 1L) {
    stop("arguments \"current.action\" and \"prior.action\" ",
      "must consist of one row each.")
  }
  if (!(current.record[[action.var]] %in% possible.actions)) {
    stop("No logic available for action: ",
      current.record[[action.var]])
  }
  if (current.record[[action.var]] == "Removed") {
    verify.cols = setdiff(names(current.record),
      c(action.var, action.id.var))
    identical(current.record[verify.cols], prior.record[verify.cols])
  } else if (current.record[[action.var]] == "Installed") {
    prior.record[[action.var]] == "Removed"
  } else {
    prior.record[[action.var]] != "Installed"
  }
}
