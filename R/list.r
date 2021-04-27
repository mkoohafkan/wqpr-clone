#' @include services.r
#' @include setup.r
#' @include util.r
NULL

#' WQP Listings
#'
#' Functions for listing WQP contents.
#' @name wqp-listings
#'
#' @section Meta Listings:
#' see [`wqp-meta-listings`]
#'
#' @section Result Listings:
#' see [`wqp-result-listings`]
#'
#' @section Event Listings:
#' see [`wqp-event-listings`]
#'
#' @section Asset Listings:
#' see [`wqp-asset-listings`]
#'
NULL


#' Result Listings
#'
#' Listings for Result Data.
#' @name wqp-result-listings
#'
NULL

#' @describeIn wqp-result-listings List result sets.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_result_details = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{result.detail.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], result.details.format, empty.results)
}

#' @describeIn wqp-result-listings List constituents.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom dplyr select distinct
#' @importFrom rlang .data
#' @export
wqp_result_constituents = function(program, database) {
  distinct(
    select(
      wqp_result_details(program, database),
      .data$constituent_id, .data$analyte_name,
      .data$unit_name, .data$equipment_name
    )
  )
}

#' @describeIn wqp-result-listings List reading, interval, and
#'   aggregate types.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom dplyr select distinct
#' @importFrom rlang .data
#' @export
wqp_result_reading_types = function(program, database) {
  distinct(
    select(wqp_result_details(program, database),
      .data$reading_type_name, .data$reading_type_id,
      .data$interval_name, .data$interval_id,
      .data$aggregate_name, .data$aggregate_id
    )
  )
}

#' @describeIn wqp-result-listings List result start/end dates.
#'
#' @param result.id The WQP result id.
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom purrr map_dfr
#' @export
wqp_result_dates = function(result.id, program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = multi_get(glue(
    "{base.url[[database]]}/{result.dates.service}", "?",
    "program={program.id[[program]]}", "&",
    "resultid={result.id}"))
  map_dfr(results,
  ~ read_results(.x, result.dates.format, empty.result.dates))
}


#' Meta Listings
#'
#' Listings for Meta Data.
#' @name wqp-meta-listings
#'
NULL

#' @describeIn wqp-meta-listings List stations.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_stations = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{meta.stations.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], meta.stations.format)
}

#' @describeIn wqp-meta-listings List contacts.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_contacts = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{meta.contacts.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], meta.contacts.format)
}

#' @describeIn wqp-meta-listings List station equipment locations.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_locations = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{meta.locations.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], meta.locations.format)
}


#' Event Listings
#'
#' Listings for Event Data.
#' @name wqp-event-listings
#'
NULL

#' @describeIn wqp-event-listings List events.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_event_details = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{event.detail.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], event.data.format)
}

#' @describeIn wqp-event-listings List event types.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_event_types = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{event.types.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], event.types.format, empty.events)
}

#' @describeIn wqp-event-listings List event reasons.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_event_reasons = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{event.reasons.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], event.reasons.format)
}

#' @describeIn wqp-event-listings List event summaries.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_event_summaries = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{event.summaries.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], event.summaries.format)
}


#' Asset Listings
#'
#' Listings for Asset Data.
#' @name wqp-asset-listings
#'
NULL

#' @describeIn wqp-asset-listings List sondes.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_sondes = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{asset.sondes.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], asset.sondes.format)
}

#' @describeIn wqp-asset-listings List actions.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom glue glue
#' @export
wqp_action_details = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{event.action.detail.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], event.action.details.format, empty.actions)
}

#' @describeIn wqp-asset-listings List action types.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom tibble tibble
#' @export
wqp_action_types = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{meta.action.types.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], asset.action.types.format)
}

#' @describeIn wqp-asset-listings List verification instrument types.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom tibble tibble
#' @export
wqp_verification_instrument_types = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}", "/",
    "{asset.verificationinstrument.types.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], asset.verificationinstrument.types.format)
}

#' @describeIn wqp-asset-listings List verification instruments uses.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom tibble tibble
#' @export
wqp_verification_instrument_details = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}", "/",
    "{event.verificationinstrument.detail.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]],
    event.verificationinstrument.details.format,
    empty.verificationinstruments)
}

#' @describeIn wqp-asset-listings List standard solution types.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom tibble tibble
#' @export
wqp_standard_solution_types = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}", "/",
    "{asset.standardsolution.types.service}", "?",
    "program={program.id[[program]]}"))
  read_results(results[[1]], asset.standardsolution.types.format)
}

#' @describeIn wqp-asset-listings List verification instruments uses.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-programs
#' @importFrom tibble tibble
#' @export
wqp_standard_solution_details = function(program, database) {
  database = validate_database(database)
  program = validate_program(program)
  results = basic_get(glue(
    "{base.url[[database]]}/{event.standardsolution.detail.service}",
    "?", "program={program.id[[program]]}"))
  read_results(results[[1]], event.standardsolution.details.format,
    empty.standardsolutions)
}
