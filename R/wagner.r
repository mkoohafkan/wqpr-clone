#' @include services.r
#' @include setup.r
#' @include util.r
#' @include list.r
NULL

#' Wagner Field Names
#'
#' Vector of field names containing Wagner method results.
#'
#' @keywords internal
wagner.fields = c(
  "a. Arrvl Sonde Reading",
  "b. Arrvl. Verification",
  "c. Dirty Sonde Reading",
  "d. Dirty Verification",
  "e. Clean Sonde Reading",
  "f. Clean Verification",
  "g. Dep. Sonde Reading",
  "h. Dep. Verification"
)

#' Wagner Fields
#'
#' Get the Wagner field names and result ids.
#'
#' @return A dataframe.
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter select distinct
#' @keywords internal
get_wagner_ids = function(program, database) {
  distinct(
    select(
      filter(
        wqp_result_details(program, database),
        .data$reading_type_name %in% wagner.fields
      ),
      .data$reading_type_id, .data$reading_type_name
    )
  )
}


#' Wagner Fields
#'
#' Get the Wagner fields associated with a given time series result id.
#'
#' @inheritParams wqp_result_dates
#' @return A dataframe of Wagner fields and corresponding result ids
#'   associated with the specified time series.
#'
#' @importFrom rlang .data
#' @importFrom dplyr pull filter select
#' @keywords internal
get_wagner_fields = function(result.id, program, database) {
  all.listings = wqp_result_details(program, database)
  result.listing = filter(all.listings, .data$result_id == result.id)
  station.id = pull(result.listing, .data$station_id)
  constituent.id = pull(result.listing, .data$constituent_id)
  wagner.ids = pull(get_wagner_ids(program, database),
    .data$reading_type_id)

  select(
    filter(all.listings,
      .data$station_id == station.id,
      .data$constituent_id == constituent.id,
      .data$reading_type_id %in% wagner.ids
    ),
    .data$result_id,
    .data$reading_type_name,
    .data$reading_type_id
  )
}

#' Wagner Event Sequences
#'
#' Group events into the Wagner sequence.
#'
#' @param action.types Ordered vector of action types, defines the
#'   event sequence to search for.
#' @param reason.name A "reason" indentifier to filter the number of
#'   events to search through.
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @return a `tibble` with columns corresponding to each action in
#'   `action.types`. Each row is a sequence of events where the actions
#'   occurred.
#'
#' @importFrom utils head tail
#' @importFrom dplyr select filter mutate transmute arrange left_join
#'   group_by group_nest ungroup
#' @importFrom tidyr unnest pivot_wider
#' @importFrom purrr map
#' @export
wag_event_sequence = function(reason.name = "Wagner QAQC",
  action.types = c("Calibrated", "Installed", "Removed", "Checked"),
  program, database) {
  database = validate_database(database)
  program = validate_program(program)
  # field names
  event.field = "event_id"
  sonde.field = "sonde_name"
  station.fields = c("station_id", "station_name")
  reason.field = "reason_name"
  action.field = "action_name"
  time.field = "arrival_time"
  join.fields = c(event.field, station.fields)
  # fix initial ordering when checks, calibrations have same timestamp
  action.sequence = c(tail(action.types, 1), head(action.types, -1))
  # join event and actions tables
  event.actions = left_join(wqp_action_details(program, database),
    wqp_event_details(program, database), by = join.fields)
  # filter by reason and actions
  event.actions = filter(event.actions,
    .data[[reason.field]] == reason.name,
    .data[[action.field]] %in% action.types
  )
  # enforce action order
  event.actions[action.field] = factor(event.actions[[action.field]],
    action.sequence, ordered = TRUE)
  # order actions within sonde groups
  sonde.groups = group_nest(event.actions, .data[[sonde.field]])
  sonde.groups = mutate(sonde.groups, data = map(.data$data,
    ~ arrange(.x, .data[[time.field]], .data[[action.field]])))
  # detect action sequence
  sonde.groups = mutate(sonde.groups, data = map(.data$data,
    ~ mutate(.x, index = detect_sequence(.data[[action.field]],
      action.types))))
  # drop NA sequences
  sonde.groups = mutate(sonde.groups, data = map(.data$data,
    ~ filter(.x, !is.na(index))))
  # regroup by sequence
  sonde.sequences = unnest(sonde.groups, .data$data)
  sonde.sequences = group_nest(sonde.sequences,
    .data[[sonde.field]], .data$index, .key = "events")
  # spread into table
  unnest(transmute(sonde.sequences,
    .data[[sonde.field]],
    data = map(.data$events,
      ~ pivot_wider(
          select(.x, .data[[action.field]], .data[[event.field]]),
          names_from = .data[[action.field]],
          values_from = .data[[event.field]])
    )), .data$data)
}

#' Calculate Fouling Error
#'
#' Calculate fouling error from pre/post cleaning visit data.
#'
#' @param event.id A vector of event IDs.
#' @param interval.name The interval name, used to filter the
#'   event data.
#' @param dirty.tag regex label to identify the dirty sonde
#'   data components.
#' @param clean.tag regex label to identify the clean sonde
#'   data components.
#' @param deployed.tag regex label to identify the deployed sonde
#'   data components.
#' @param verification.tag regex label to identify the verification
#'   sonde data components.
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @inheritParams wqp-versions
#' @return A tibble of fouling calculations.
#'
#' @details This function is highly sensitive to the labeling
#'   convention used to differentiate clean/dirty deployed/verification
#'   sonde data. The `reading_type_name` of the result data must be in
#'   the following format (`---` signifies ignored text):
#'
#'   `<--->. <dirty/clean tag> <deployed/verification tag> <--->`
#'
#' @importFrom tidyr separate pivot_wider
#' @importFrom dplyr filter select mutate left_join
#' @importFrom stringr str_detect str_c
#' @importFrom rlang .data
#' @export
wag_fouling_error = function(event.id, interval.name = "Visit",
  dirty.tag = "Dirty", clean.tag = "Clean",
  deployed.tag = "Sonde", verification.tag = "Verification",
  program, database, version) {
  database = validate_database(database)
  program = validate_program(program)
  version = validate_version(version, program)
  event.field = "event_id"
  station.field = "station_id"
  event.time.field = "arrival_time"
  result.id.field = "result_id"
  result.time.field = "time"
  result.reading.field = "reading_type_name"
  result.analyte.field = "analyte_name"
  result.unit.field = "unit_name"
  result.equipment.field = "equipment_name"
  result.value.field = "value"
  result.interval.field = "interval_name"
  result.rank.field = "rank_name"
  result.version.field = "version"
  sonde.name.field = "sonde_name"
  event.data = select(
    wqp_events(event.id, program, database, bind = TRUE),
    .data[[event.field]],
    .data[[station.field]],
    .data[[event.time.field]]
  )
  result.data = filter(
    left_join(event.data, wqp_result_details(), by = station.field),
    .data[[station.field]] %in% event.data[[station.field]],
    .data[[result.interval.field]] == interval.name,
    str_detect(.data[[result.reading.field]],
      glue("({deployed.tag})|({verification.tag})")),
    str_detect(.data[[result.reading.field]],
      glue("({dirty.tag})|({clean.tag})"))
  )
  # get result values
  result.values = unnest(mutate(
    summarize(
      group_by(result.data, .data[[result.id.field]]),
      start.date = min(.data[[event.time.field]]),
      end.date = max(.data[[event.time.field]]),
      .groups = "drop"
    ),
    result = wqp_result_data(.data[[result.id.field]],
      .data$start.date, .data$end.date,
      version = version, program = program, database = database),
    .keep = "none"
  ), .data$result)
  # attach result data to event data
  join.fields = c(result.id.field, result.version.field,
    result.time.field)
  names(join.fields) = c(result.id.field, result.version.field,
    event.time.field)
  result.data = left_join(result.data, result.values,
    by = join.fields)
  # pivot to fouling data
  fouling.data = pivot_wider(
    separate(
      select(result.data,
        .data[[event.field]], .data[[result.reading.field]],
        .data[[result.analyte.field]], .data[[result.unit.field]],
        .data[[result.equipment.field]], .data[[result.rank.field]],
        .data$value
      ),
      .data[[result.reading.field]], c(NA, "type", "sonde"),
        extra = "drop"
    ),
    names_from = .data$type, values_from = .data$value
  )
  dirty.readings = select(
    filter(fouling.data, str_detect(.data$sonde, deployed.tag)),
    .data[[event.field]],
    .data[[result.analyte.field]], .data[[result.unit.field]],
    .data[[result.equipment.field]], .data[[result.rank.field]],
    .data[[dirty.tag]])
  fouling.data = left_join(pivot_wider(
    mutate(fouling.data, diff = .data[[clean.tag]] -
        .data[[dirty.tag]], .keep = "unused"),
    names_from = .data$sonde, values_from = .data$diff
  ), dirty.readings, by = c(event.field, result.analyte.field,
    result.unit.field, result.equipment.field, result.rank.field))
  # compute fouling
  mutate(fouling.data,
     fouling_error = .data[[deployed.tag]] - .data[[verification.tag]],
     fouling_percent = 100 * .data$fouling_error / .data[[dirty.tag]],
     .keep = "unused"
  )
}

#' Calculate Sensor Drift
#'
#' Calculate sensor drift from post-deployment check data.
#'
#' @param event.id A vector of event IDs.
#' @param interval.name The interval name, used to filter the result
#'   data based on the event type.
#' @param reference.tag A regex string to identify the calibration
#'   standard reference value.
#' @param check.tag A regex string to identify the post-deployment
#'   check reading.
#' @param rank.tag A regex label to identify the ranks to use for
#'   calculating drift (i.e., only calculate drift for ranks that
#'   contain the word "Standard").
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @inheritParams wqp-versions
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr filter select mutate left_join group_by
#'   summarize across
#' @importFrom stringr str_detect str_c
#' @importFrom rlang .data
#' @export
wag_drift_error = function(event.id,
  interval.name = "Post-deployment check",
  reference.tag = "Calibration Standard",
  check.tag = "Post-deployment", rank.tag = "Standard",
  program, database, version) {
  database = validate_database(database)
  program = validate_program(program)
  version = validate_version(version, program)
  event.field = "event_id"
  station.field = "station_id"
  event.time.field = "arrival_time"
  result.id.field = "result_id"
  result.time.field = "time"
  result.reading.field = "reading_type_name"
  result.analyte.field = "analyte_name"
  result.unit.field = "unit_name"
  result.value.field = "value"
  result.interval.field = "interval_name"
  result.rank.field = "rank_name"
  result.version.field = "version"
  # get event data
  event.data = select(
    wqp_events(event.id, program, database, bind = TRUE),
    .data[[event.field]],
    .data[[station.field]],
    .data[[event.time.field]]
  )
  # get result details
  result.data = mutate(
    filter(
      left_join(event.data, wqp_result_details(), by = station.field),
      .data[[station.field]] %in% event.data[[station.field]],
      .data[[result.interval.field]] == interval.name,
      str_detect(.data[[result.reading.field]],
        glue("({reference.tag})|({check.tag})")),
      str_detect(.data[[result.rank.field]], rank.tag)
    )
  )
  # get result data
  result.values = unnest(mutate(
    summarize(
      group_by(result.data, .data[[result.id.field]]),
      start.date = min(.data[[event.time.field]]),
      end.date = max(.data[[event.time.field]]),
      .groups = "drop"
    ),
    result = wqp_result_data(.data[[result.id.field]],
      .data$start.date, .data$end.date,
      version = version, program = program, database = database),
    .keep = "none"
  ), .data$result)
  # attach result data to event data
  join.fields = c(result.id.field, result.version.field,
    result.time.field)
  names(join.fields) = c(result.id.field, result.version.field,
    event.time.field)
  result.data = left_join(result.data, result.values,
    by = join.fields)
  # pivot to drift data
  drift.data = pivot_wider(
    select(result.data,
      .data[[event.field]], .data[[result.reading.field]],
      .data[[result.analyte.field]], .data[[result.unit.field]],
      .data[[result.rank.field]], .data$value
    ),
    names_from = .data[[result.reading.field]],
    values_from = .data$value
  )
  ref.names = str_detect(names(drift.data), reference.tag)
  check.names = str_detect(names(drift.data), check.tag)
  names(drift.data)[ref.names] = reference.tag
  names(drift.data)[check.names] = check.tag
  # drop NA entries
#  drift.data = filter(drift.data, !is.na(.data[[reference.tag]]),
#    !is.na(.data[[check.tag]]))
  # calculate drift
  drift.data = mutate(drift.data,
     drift_error = .data[[check.tag]] - .data[[reference.tag]],
     drift_percent = 100 * .data$drift_error / .data[[reference.tag]],
     .keep = "unused"
  )
  summarize(
    group_by(select(drift.data, -.data[[result.rank.field]]),
      .data[[event.field]], .data[[result.analyte.field]],
      .data[[result.unit.field]]),
    across(.fns = mean),
    .groups = "drop"
  )
}


#' Calculate Total Error
#'
#' Calculate total error from combined fouling and
#' calibration drift data.
#'
#' @param sequence.data Event sequence data,
#'   e.g. output from [wag_event_sequence()].
#' @param fouling.data Fouling error data,
#'   e.g. output from [wag_fouling_error()].
#' @param drift.data Calibration drift error data,
#'   e.g. output from [wag_drift_error()].
#' @return a tibble of total drift calculations.
#'
#' @importFrom dplyr left_join mutate
#' @importFrom rlang .data
#' @export
wag_total_error = function(sequence.data, fouling.data, drift.data) {
  event.field = "event_id"
  analyte.field = "analyte_name"
  unit.field = "unit_name"
  removed.field = "Removed"
  checked.field = "Checked"
  fouling.error.field = "fouling_error"
  fouling.percent.field = "fouling_percent"
  drift.error.field = "drift_error"
  drift.percent.field = "drift_percent"
  # join fouling data to sequence data
  fouling.join.fields = c(event.field)
  names(fouling.join.fields) = removed.field
  join.data = left_join(sequence.data, fouling.data,
    by = fouling.join.fields)
  # join drift data to sequence data
  drift.join.fields = c(event.field, analyte.field, unit.field)
  names(drift.join.fields) = c(checked.field, analyte.field, unit.field)
  join.data = left_join(join.data, drift.data,
    by = drift.join.fields)
  # calculate total error
  mutate(join.data,
    total_error = abs(.data[[fouling.error.field]]) +
      abs(.data[[drift.error.field]]),
    total_percent = abs(.data[[fouling.percent.field]]) +
      abs(.data[[drift.percent.field]]),
    .keep = "unused"
  )
}

#' Wagner Rating
#'
#' Assign Wagner ratings to based on total error.
#'
#' @param error.data Total error data, i.e. output of
#'   [wag_total_error()].
#' @param rating.spec YAML specification defining Wagner ratings.
#' @param default.rating Default rating to use when data is missing or
#'   specifications are not defined.
#' @return A tibble of Wagner ratings.
#'
#' @importFrom dplyr row_number
#' @importFrom purrr map_at map_dfr
#' @importFrom yaml read_yaml
#' @export
wag_ratings = function(error.data, rating.spec,
  default.rating = "Unreliable") {
  analyte.field = "analyte_name"
  unit.field = "unit_name"
  total.error.field = "total_error"
  total.percent.field = "total_percent"
  if (missing(rating.spec)) {
    rating.spec = system.file("wagner-ratings.yml",
      package = "wqpr", mustWork = TRUE)
  }
  # read rating specifications
  spec = read_yaml(rating.spec)
  spec = map_dfr(spec, ~ as_tibble(map_at(.x, "rating", list)))
  spec = unnest(mutate(spec, rating = map(.data$rating, rating_table)),
    .data$rating)
  spec = mutate(spec,
    unit_name = str_replace(.data$unit_name, "\U00B5", "\U03BC"))
  # create index for Wagner sequences
  error.data = mutate(error.data, index = row_number())
  # attach ratings to data
  spec.table = left_join(spec, error.data,
    c(analyte.field, unit.field))
  # calculate distance to rating thresholds
  spec.table = mutate(spec.table,
    absolute.diff = .data$absolute - .data[[total.error.field]],
    percent.diff = .data$percent - .data[[total.percent.field]]
  )
  # drop negative distances (crossed rating threshold)
  spec.table = filter(spec.table,
    (.data$absolute.diff > 0) | (.data$percent.diff > 0))
  # compute distance order
  spec.table = ungroup(mutate(group_by(spec.table, .data$index),
    absolute.order = if_else(is.finite(.data$absolute.diff),
      order(.data$absolute.diff), NA_integer_),
    percent.order = if_else(is.finite(.data$percent.diff),
      order(.data$percent.diff), NA_integer_)
  ))
  spec.table = mutate(spec.table,
    combined.order = pmin(.data$absolute.order, .data$percent.order,
      na.rm = TRUE))
  # retrieve lowest-order rating
  spec.table = ungroup(filter(
    group_by(spec.table, .data$index),
    .data$combined.order == min(.data$combined.order)
  ))
  # join  ratings back to error data
  select(replace_na(
    left_join(
      error.data,
      select(spec.table, "index", "rating"),
      by = "index"
    ),
    list(rating = default.rating)
  ), -.data$index)
}


#' Wagner Rating Table
#'
#' Helper function to reformat Wagner specifications.
#' @param ratings list of Wagner ratings.
#' @return A tibble.
#'
#' @importFrom tibble as_tibble
#' @importFrom purrr map_dfr
#' @keywords internal
rating_table = function(ratings) {
  rating.table = map_dfr(ratings, as_tibble, .id = "rating")
  if (!("percent" %in% names(rating.table))) {
    rating.table["percent"] = NA
  }
  if (!("absolute" %in% names(rating.table))) {
    rating.table["absolute"] = NA
  }
  rating.table
}

#' Wagner Rating Meta Data
#'
#' Define the time interval and station that ratings apply to.
#'
#' @param rating.data A tibble of rating data,
#'   i.e. output of [wag_ratings()].
#' @param start The column name containing event IDs
#'   corresponding to the equipment deployment. Default
#'   is `"Installed"`.
#' @param end The column name containing event IDs
#'   corresponding to the equipment removal. Default
#'   is `"Removed"`.
#' @return The tibble `rating.data`, with additional columns
#'   `"start_time"`, `"end_time"`, and `"cdec_code"`.
#'
#' @importFrom rlang .data .env
#' @importFrom purrr map2
#' @importFrom dplyr select filter left_join rename
#' @importFrom stats setNames
#' @export
wag_rating_meta = function(rating.data, start = "Installed",
  end = "Removed") {
  # field names
  event.id.field = "event_id"
  exchange.field = "restart_time"
  station.id.field = "station_id"
  station.name.field = "cdec_code"
  station.suffix = c("", ".check")
  # get event data
  all.event.ids = unique(c(rating.data[[start]], rating.data[[end]]))
  all.events = select(
    filter(wqp_event_details(),
      .data[[event.id.field]] %in% .env$all.event.ids),
    .env$event.id.field, .env$exchange.field, .env$station.id.field
  )
  station.names = select(wqp_stations(), .env$station.id.field,
    .env$station.name.field)
  # join exchange times to rating data
  rating.data = left_join(
    rating.data,
    rename(all.events, start_time = exchange.field),
    by = setNames(event.id.field, start)
  )
  rating.data = left_join(
    rating.data,
    rename(all.events, end_time = exchange.field),
    by = setNames(event.id.field, end),
    suffix = station.suffix
  )
  # join station names to rating.data
  in.station = str_c(station.id.field, station.suffix[1])
  out.station = str_c(station.id.field, station.suffix[2])
  station.err = rating.data[[in.station]] != rating.data[[out.station]]
  if (any(station.err)) {
    warning("Inconsistent sonde installation/removal records:\n",
      print_table(rating.data, station.err)
    )
  }
  select(
    left_join(rating.data, station.names, by = station.id.field),
    - .env$in.station, - .env$out.station
  )
}

wag_series = function(rating.data, interval = "15 mins") {
  station.field = "cdec_code"
  start.field = "start_time"
  end.field = "end_time"
  rating.field = "rating"

  series.data = mutate(rating.data,
    series = map2(.data[[start.field]], .data[[end.field]],
      ~ seq(.x, .y, by = "15 min")
    ))
#  map2(rating.data,
#  )


}