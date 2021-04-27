#' wqpr timezone
#'
#' The timezone used by WQP. Timestamps are in Pacific Standard
#' Time (PST) and daylight savings is ignored.
#'
#' @keywords internal
wqpr.tz = "Etc/GMT+8"

#' @rdname wqpr.tz
#' @importFrom readr locale
#' @keywords internal
wqpr.locale = locale(tz = wqpr.tz, encoding = "UTF-8")

#' Meta Stations Format
#'
#' Formatting specifications for meta.stations.service.
#'
#' @importFrom readr cols col_integer col_character col_double
#' @keywords internal
meta.stations.format = cols(
  station_id = col_integer(),
  station_name = col_character(),
  abbreviation = col_character(),
  station_code = col_character(),
  cdec_code = col_character(),
  station_description = col_character(),
  coordinate_system = col_character(),
  coordinate_units = col_character(),
  latitude = col_double(),
  longitude = col_double(),
  elevation = col_double(),
  tech_contact_id = col_integer(),
  contact_name = col_character(),
  public_station = col_character(),
  active = col_character(),
  owner_agency_id = col_integer(),
  agency_name = col_character(),
  notes = col_character()
)

#' Meta Contacts Format
#'
#' Formatting specifications for meta.contacts.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
meta.contacts.format = cols(
  contact_id = col_integer(),
  contact_name = col_character(),
  company = col_character(),
  department = col_character(),
  job_title = col_character(),
  email_address = col_character(),
  active = col_character()
)

#' Meta Sondes Format
#'
#' Formatting specifications for meta.contacts.service.
#'
#' @importFrom readr cols col_integer col_character col_datetime
#' @keywords internal
asset.sondes.format = cols(
  sonde_id = col_integer(),
  sonde_name = col_character(),
  station_id = col_integer(),
  event_date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  notes = col_character(),
  active = col_character()
)

#' Meta Locations Format
#'
#' Formatting specifications for meta.locations.service.
#'
#' @importFrom readr cols col_integer col_character col_datetime
#' @keywords internal
meta.locations.format = cols(
  location_id = col_integer(),
  location_name = col_character(),
  active = col_character()
)

#' Result Format
#'
#' Formatting specifications for web service Results/Result.
#'
#' @importFrom readr cols col_integer col_datetime col_double
#'   col_character
#' @format A `reader::cols()` specification or empty `tibble` object.
#' @keywords internal
result.data.format = cols(
  result_id = col_integer(),
  time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  value = col_double(), qaqc_flag_id = col_character(),
  version = col_integer()
)
#' @rdname result.data.format
#' @importFrom dplyr tibble
#' @importFrom lubridate as_datetime
#' @keywords internal
empty.results = tibble(
  result_id = integer(0),
  time = as_datetime(character(0), tz = wqpr.tz),
  value = numeric(0),
  qaqc_flag_id = character(0),
  version = integer(0)
)

#' Result Details Format
#'
#' Formatting specifications for result.details.service.
#'
#' @importFrom readr cols col_integer col_datetime col_double
#'   col_character
#' @keywords internal
result.details.format = cols(result_id = col_integer(),
  station_id = col_integer(), station_name = col_character(),
  latitude = col_double(), longitude = col_double(),
  elevation = col_double(), elevation = col_double(),
  coordinate_system = col_character(),
  coordinate_units = col_character(),
  station_description = col_character(),
  station_active = col_character(),
  constituent_id = col_integer(), analyte_name = col_character(),
  unit_name = col_character(), equipment_name = col_character(),
  aggregate_id = col_integer(), aggregate_name = col_character(),
  interval_id = col_integer(), interval_name = col_character(),
  reading_type_id = col_integer(), reading_type_name = col_character(),
  rank_id = col_integer(), rank_name = col_character(),
  probe_depth = col_character(),
  start_date = col_datetime(format = "%Y-%m-%d"),
  end_date = col_datetime(format = "%Y-%m-%d"),
  cdec_code = col_character(),
  automated = col_character(), automated_name = col_character(),
  public_station = col_character()
)

#' Result Dates Format
#'
#' Formatting specifications for result.data.service.
#'
#' @importFrom readr cols col_integer col_datetime
#' @keywords internal
result.dates.format = cols(
  result_id = col_integer(),
  first_date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  last_date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  version = col_integer(),
  records = col_integer()
)
#' @rdname result.dates.format
#' @importFrom dplyr tibble
#' @importFrom lubridate as_datetime
#' @keywords internal
empty.result.dates = tibble(
  result_id = integer(0),
  first_date = as_datetime(character(0), tz = wqpr.tz),
  last_date = as_datetime(character(0), tz = wqpr.tz),
  version = integer(0),
  records = integer(0)
)

#' Event Data Format
#'
#' Formatting specifications for event.data.service.
#'
#' @importFrom readr cols col_integer col_character col_datetime
#' @keywords internal
event.data.format = cols(
  event_id = col_integer(),
  arrival_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  station_id = col_integer(),
  station_name = col_character(),
  contact_id = col_integer(),
  contact_name = col_character(),
  event_type_id = col_integer(),
  event_type_name = col_character(),
  summary_id = col_integer(),
  summary_name = col_character(),
  reason_id = col_integer(),
  reason_name = col_character(),
  departure_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  watch_time = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  restart_time = col_datetime(format = "%Y-%m-%d %H:%M:%S")
)
#' @rdname event.data.format
#' @importFrom dplyr tibble
#' @importFrom lubridate as_datetime
#' @keywords internal
empty.events = tibble(
  event_id = integer(0),
  arrival_time = as_datetime(character(0), tz = wqpr.tz),
  station_id = integer(0),
  station_name = character(0),
  contact_id = integer(0),
  contact_name = character(0),
  event_type_id = integer(0),
  event_type_name = character(0),
  summary_id = integer(0),
  summary_name = character(0),
  reason_id = integer(0),
  reason_name = character(0),
  departure_time = as_datetime(character(0), tz = wqpr.tz),
  watch_time = as_datetime(character(0), tz = wqpr.tz),
  restart_time = as_datetime(character(0), tz = wqpr.tz)
)

#' Event Type Format
#'
#' Formatting specifications for event.types.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
event.types.format = cols(
  event_type_id = col_integer(),
  event_type_name = col_character(),
  active = col_character()
)

#' Event Reason Format
#'
#' Formatting specifications for event.reasons.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
event.reasons.format = cols(
  reason_id = col_integer(),
  reason_name = col_character(),
  active = col_character()
)

#' Event Summary Format
#'
#' Formatting specifications for event.summaries.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
event.summaries.format = cols(
  summary_id = col_integer(),
  summary_name = col_character(),
  active = col_character()
)

#' Event Action Details Format
#'
#' Formatting specifications for event.action.details.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
event.action.details.format = cols(
  event_id = col_integer(),
  sonde_id = col_integer(),
  sonde_name = col_character(),
  location_id = col_integer(),
  location_name = col_character(),
  station_id = col_integer(),
  station_name = col_character(),
  action_id = col_integer(),
  action_name = col_character(),
  edit_timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")
)
#' @rdname event.action.details.format
#' @importFrom dplyr tibble
#' @importFrom lubridate as_datetime
#' @keywords internal
empty.actions = tibble(
  event_id = integer(0),
  sonde_id = integer(0),
  sonde_name = character(0),
  location_id = integer(0),
  location_name = character(0),
  station_id = integer(0),
  station_name = character(0),
  action_id = integer(0),
  action_name = character(0),
  edit_timestamp = as_datetime(character(0), tz = wqpr.tz)
)


#' Event Verification Instrument Details Format
#'
#' Formatting specifications for
#' event.verificationinstrument.details.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
event.verificationinstrument.details.format = cols(
  event_id = col_integer(),
  instrument_id = col_integer(),
  serial_number = col_character(),
  calibration_due_date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  edit_timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")
)
#' @rdname event.verificationinstrument.details.format
#' @importFrom dplyr tibble
#' @importFrom lubridate as_datetime
#' @keywords internal
empty.verificationinstruments = tibble(
  event_id = integer(0),
  instrument_id = integer(0),
  serial_number = character(0),
  calibration_due_date = as_datetime(character(0), tz = wqpr.tz),
  edit_timestamp = as_datetime(character(0), tz = wqpr.tz)
)


#' Event Standard Solution Details Format
#'
#' Formatting specifications for
#' event.verificationinstrument.details.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
event.standardsolution.details.format = cols(
  event_id = col_integer(),
  solution_id = col_integer(),
  lot_number = col_character(),
  expiration_date = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  edit_timestamp = col_datetime(format = "%Y-%m-%d %H:%M:%S")
)
#' @rdname event.standardsolution.details.format
#' @importFrom dplyr tibble
#' @importFrom lubridate as_datetime
#' @keywords internal
empty.standardsolutions = tibble(
  event_id = integer(0),
  solution_id = integer(0),
  lot_number = character(0),
  expiration_date = as_datetime(character(0), tz = wqpr.tz),
  edit_timestamp = as_datetime(character(0), tz = wqpr.tz)
)

#' Action Types Format
#'
#' Formatting specifications for meta.action.types.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
asset.action.types.format = cols(
  action_id = col_integer(),
  action_name = col_character(),
  notes = col_character()
)

#' Verification Instrument Types Format
#'
#' Formatting specifications for
#' asset.verificationinstrument.types.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
asset.verificationinstrument.types.format = cols(
  instrument_id = col_integer(),
  instrument_name = col_character(),
  notes = col_character()
)

#' Standard Solution Types Format
#'
#' Formatting specifications for asset.standardsolution.types.service.
#'
#' @importFrom readr cols col_integer col_character
#' @keywords internal
asset.standardsolution.types.format = cols(
  solution_id = col_integer(),
  solution_name = col_character(),
  notes = col_character()
)


#' PDM Report Format
#'
#' Formatting specifications for report.pdm.service.
#'
#' @importFrom readr cols col_integer col_datetime col_double
#'   col_character
#' @keywords internal
report.pdm.format = cols(StationID = col_integer(),
  StationName = col_character(),
  StartDateTime = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
  EndDateTime = col_datetime(format = "%Y/%m/%d %H:%M:%S"),
  NumPtsForDay = col_integer(),
  StagePeakTimeOne = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  ECValueOne = col_double(),
  StagePeakTimeTwo = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
  ECValueTwo = col_double(),
  NumberOfPksFoundForDay = col_integer(), DailyMean = col_double(),
  `Progressive Daily Mean` = col_double()
)
#' @rdname report.pdm.format
#' @importFrom dplyr tibble
#' @importFrom lubridate as_datetime
#' @keywords internal
empty.pdm.results = tibble(StationID = integer(0),
  StationName = character(0),
  StartDateTime = as_datetime(character(0), tz = wqpr.tz),
  EndDateTime = as_datetime(character(0), tz = wqpr.tz),
  NumPtsForDay = integer(0),
  StagePeakTimeOne = as_datetime(character(0), tz = wqpr.tz),
  ECValueOne = numeric(0),
  StagePeakTimeTwo = as_datetime(character(0), tz = wqpr.tz),
  ECValueTwo = numeric(0),
  NumberOfPksFoundForDay = integer(0), DailyMean = numeric(0),
  `Progressive Daily Mean` = numeric(0))
