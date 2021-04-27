#' @include services.r
#' @include setup.r
#' @include util.r
#' @include list.r
#' @include checkers.r
NULL

#' Getters
#'
#' internal functions for getting IDs based on other available fields.
#'
#' @param new.data A dataframe.
#' @return A vector of IDs.
#'
#' @name getters
#' @keywords internal
NULL


#' @describeIn getters Get Contact ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr mutate select left_join pull
#' @importFrom stringr str_to_upper str_subset
#' @importFrom purrr modify_if map
#' @importFrom rlang .data
#' @keywords internal
get_contact_id = function(new.data, program, database) {
  required.var = "contact_name"
  if (!(required.var %in% names(new.data))) {
    stop(sprintf("Columns of \"new.data\" must include field '%s'.",
      required.var))
  }
  check_na(new.data, required.var, "contact names", "error")
  contact.data = mutate(wqp_contacts(program, database),
    contact_name = str_to_upper(.data$contact_name))
  output.data = mutate(new.data,
    check.name = unlist(modify_if(map(str_to_upper(.data$contact_name),
      ~ str_subset(contact.data$contact_name, .x)),
      ~ length(.x) == 0L, ~ NA_character_)))
  output.data = left_join(
    output.data,
    select(contact.data, "contact_name", "contact_id"),
      by = c("check.name" = "contact_name")
  )
  pull(output.data, .data$contact_id)
}

#' @describeIn getters Get Station ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr intersect left_join select_at pull
#' @importFrom stringr str_c
#' @importFrom rlang .data
#' @keywords internal
get_station_id = function(new.data, program, database) {
  possible.vars = c("station_name", "cdec_code", "station_code",
    "abbreviation")
  included.vars = intersect(names(new.data), possible.vars)
  check_fields(possible.vars, included.vars, 1L)
  check_na(new.data, included.vars, "identifiers", "error")
  output.data = left_join(
    new.data,
    select_at(wqp_stations(program, database),
      c(included.vars, "station_id")),
      by = included.vars
  )
  pull(output.data, .data$station_id)
}

#' @describeIn getters Get Event ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr select_at left_join pull
#' @importFrom rlang .data
#' @importFrom glue glue
#' @keywords internal
get_event_id = function(new.data, program, database) {
  required.vars = c("arrival_time", "cdec_code", "contact_name",
    "event_type_name")
  check_fields(required.vars, names(new.data))
  # lookup translations
  input.data = new.data
  input.data["arrival_time"] = check_timestamps(input.data,
    "arrival_time", "warning")
  # station ID
  if (!("station_id" %in% names(input.data))) {
    input.data["station_id"] = get_station_id(input.data,
      program, database)
    check_ids("station", input.data$cdec_code, input.data$station_id,
      "warning")
  }
  # contact ID
  if (!("contact_id" %in% names(input.data))) {
    input.data["contact_id"] = get_contact_id(input.data,
      program, database)
    check_ids("contact", input.data$contact_name, input.data$contact_id,
      "warning")
  }
  # event type ID
  if (!("event_type_id" %in% names(input.data))) {
    input.data["event_type_id"] = get_event_type_id(input.data,
      program, database)
    check_ids("event type", input.data$event_type_name,
      input.data$event_type_id, "warning")
  }
  join.vars = c("arrival_time", "station_id", "contact_id",
    "event_type_id")
  # fail for NA identifiers
  check_na(input.data, join.vars, "identifiers", "error")
  # event ID
  input.data = left_join(
    input.data,
    select_at(wqp_event_details(program, database),
      c(join.vars, "event_id")),
    by = join.vars
  )
  pull(input.data, .data$event_id)
}

#' @describeIn getters Get Event Type ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull
#' @importFrom rlang .data
#' @keywords internal
get_event_type_id = function(new.data, program, database) {
  required.var = "event_type_name"
  check_fields(required.var, names(new.data))
  check_na(new.data, required.var, "event type names", "error")
  output.data = left_join(new.data, wqp_event_types(program, database),
      by = required.var)
  pull(output.data, .data$event_type_id)
}

#' @describeIn getters Get Reason ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull
#' @importFrom rlang .data
#' @keywords internal
get_reason_id = function(new.data, program, database) {
  required.var = "reason_name"
  check_fields(required.var, names(new.data))
  check_na(new.data, required.var, "reason names", "error")
  output.data = left_join(
    new.data,
    wqp_event_reasons(program, database),
    by = required.var
  )
  pull(output.data, .data$reason_id)
}

#' @describeIn getters Get Summary ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull
#' @importFrom rlang .data
#' @keywords internal
get_summary_id = function(new.data, program, database) {
  required.var = "summary_name"
  check_fields(required.var, names(new.data))
  check_na(new.data, required.var, "summary names", "error")
  output.data = left_join(
    new.data,
    wqp_event_summaries(program, database),
    by = required.var
  )
  pull(output.data, .data$summary_id)
}

#' @describeIn getters Get Result ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr filter left_join pull group_by_at
#'   summarize intersect
#' @importFrom stringr str_c
#' @importFrom purrr pmap_chr
#' @importFrom rlang .data
#' @keywords internal
get_result_id = function(new.data, program, database) {
  required.vars = c("cdec_code", "analyte_name", "unit_name",
    "interval_name", "reading_type_name")
  optional.vars = c("rank_name", "aggregate_name", "probe_depth",
    "equipment_name")
  included.vars = intersect(names(new.data),
    c(required.vars, optional.vars))
  check_fields(required.vars, included.vars)
  check_na(new.data, included.vars, "identifiers", "error")
  output.data = left_join(new.data, wqp_result_details(program,
    database), by = included.vars)
  if (nrow(output.data) < nrow(new.data)) {
    stop("lost rows of \"new.data\", this should never happen.")
  }
  if (nrow(output.data) > nrow(new.data)) {
    e = summarize(
      filter(group_by_at(output.data, included.vars),
        length(unique(.data$result_id)) > 1L),
      result_id = str_c(unique(.data$result_id),
        collapse = ", ")
    )
    e.string = pmap_chr(e, paste, sep = "  ")
    stop("Multiple matching result ids found:\n",
      str_c("  ", e.string, collapse = "\n"))
  }
  pull(output.data, .data$result_id)
}

#' @describeIn getters Get Reading Type ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull intersect
#' @importFrom rlang .data
#' @keywords internal
get_reading_type_id = function(new.data, program, database) {
  required.var = "reading_type_name"
  optional.vars = c("aggregate_name", "interval_name")
  included.vars = intersect(names(new.data),
    append(optional.vars, required.var))
  check_fields(required.var, included.vars)
  output.data = left_join(new.data, wqp_result_reading_types(program,
    database), by = included.vars)
  pull(output.data, .data$reading_type_id)
}


#' @describeIn getters Get Sonde ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull select
#' @importFrom rlang .data
#' @keywords internal
get_sonde_id = function(new.data, program, database) {
  required.var = "sonde_name"
  check_fields(required.var, names(new.data))
  output.data = left_join(
    new.data,
    select(wqp_sondes(program, database), "sonde_id", "sonde_name"),
    by = required.var
  )
  pull(output.data, .data$sonde_id)
}

#' @describeIn getters Get Location ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull
#' @importFrom rlang .data
#' @keywords internal
get_location_id = function(new.data, program, database) {
  required.var = "location_name"
  check_fields(required.var, names(new.data))
  output.data = left_join(new.data, wqp_locations(program, database),
    by = required.var)
  pull(output.data, .data$location_id)
}

#' @describeIn getters Get Action Type ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull mutate
#' @importFrom rlang .data
#' @keywords internal
get_action_id = function(new.data, program, database) {
  required.var = "action_name"
  check_fields(required.var, names(new.data))
  if (!(required.var %in% names(new.data))) {
    stop(sprintf("Columns of \"new.data\" must include field '%s'.",
      required.var))
  }
  output.data = left_join(new.data, wqp_action_types(program, database),
    by = required.var)
  pull(output.data, .data$action_id)
}

#' @describeIn getters Get Verification Instrument Type ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull
#' @importFrom rlang .data
#' @keywords internal
get_instrument_id = function(new.data, program, database) {
  required.var = "instrument_name"
  check_fields(required.var, names(new.data))
  output.data = left_join(
    new.data,
    wqp_verification_instrument_types(program, database),
    by = required.var
  )
  check_ids("verification instrument", output.data$instrument_name,
    output.data$instrument_id)
  pull(output.data, .data$instrument_id)
}

#' @describeIn getters Get Standard Solution Type ID
#'
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#'
#' @importFrom dplyr left_join pull
#' @importFrom rlang .data
#' @keywords internal
get_solution_id = function(new.data, program, database) {
  required.var = "solution_name"
  check_fields(required.var, names(new.data))
  output.data = left_join(
    new.data,
    wqp_standard_solution_types(program, database),
    by = required.var
  )
  check_ids("standard solution", output.data$solution_name,
    output.data$solution_id)
  pull(output.data, .data$solution_id)
}
