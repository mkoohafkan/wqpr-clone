#' @include serviceformats.r
NULL

#'Program ID
#'
#' Integer codes for WQP programs.
#'
#' @keywords internal
program.id = list(
  MARSH = NA_integer_,
  EMP = NA_integer_,
  DAYFLOW = NA_integer_
)

#' Database URL
#'
#' URLs for WQP test and production databases.
#'
#' @keywords internal
base.url = list(
  test = NA_character_,
  production = NA_character_
)

#' Web Services
#'
#' WQP web services.
#' @name wqp-services
#'
#' @section Help Services:
#' see [`wqp-help-services`]
#'
#' @section Meta Services:
#' see [`wqp-meta-services`]
#'
#' @section Asset Services:
#' see [`wqp-asset-services`]
#'
#' @section Result Services:
#' see [`wqp-result-services`]
#'
#' @section Event Services:
#' see [`wqp-event-services`]
#'
#' @section Report Services:
#' see [`wqp-report-services`]
#'
#' @keywords internal
NULL


#' Help Services
#'
#' WQP help services.
#' @name wqp-help-services
#' @format a URL part.
#' @keywords internal
NULL

#' @rdname wqp-help-services
#' @keywords internal
help.service = "TelemetryDirectWeb/OnlineHelp"
#' @rdname wqp-help-services
#' @keywords internal
token.service = "TelemetryDirectWeb/Token"
#' @rdname wqp-help-services
#' @keywords internal
valid.token.service = "TelemetryDirect/api/Valid"


#' Meta Services
#'
#' WQP meta services.
#' @name wqp-meta-services
#' @format a URL part.
#' @keywords internal
NULL

#' @rdname wqp-meta-services
#' @keywords internal
meta.contacts.service = "TelemetryDirect/api/Meta/Contacts"
#' @rdname wqp-meta-services
#' @keywords internal
meta.stations.service = "TelemetryDirect/api/Meta/Stations"
#' @rdname wqp-meta-services
#' @keywords internal
meta.locations.service = "TelemetryDirect/api/Meta/Locations"

#' Asset Services
#'
#' WQP asset services.
#' @name wqp-asset-services
#' @format a URL part.
#' @keywords internal
NULL

#' @rdname wqp-asset-services
#' @keywords internal
asset.sondes.service = "TelemetryDirect/api/Assets/Sondes"
#' @rdname wqp-meta-services
#' @keywords internal
meta.action.types.service = "TelemetryDirect/api/Assets/ActionTypes"
#' @rdname wqp-asset-services
#' @keywords internal
asset.verificationinstrument.types.service =
  "TelemetryDirect/api/Assets/VerificationInstrumentTypes"
#' @rdname wqp-asset-services
#' @keywords internal
asset.standardsolution.types.service =
  "TelemetryDirect/api/Assets/StandardSolutionTypes"
#' @rdname wqp-asset-services
#' @keywords internal
asset.action.types.insert.service =
  "TelemetryDirect/AssetData/ActionTypeInsert"
#' @rdname wqp-asset-services
#' @keywords internal
asset.verificationinstrument.types.insert.service =
  "TelemetryDirect/AssetsData/VerificationInstrumentTypeInsert"
#' @rdname wqp-asset-services
#' @keywords internal
asset.standardsolution.types.insert.service =
  "TelemetryDirect/AssetsData/StandardSolutionTypeInsert"



#' Result Services
#'
#' WQP result services.
#' @name wqp-result-services
#' @format a URL part.
#' @keywords internal
NULL

#' @name wqp-result-services
#' @keywords internal
result.data.service = "TelemetryDirect/api/Results/ResultData"
#' @name wqp-result-services
#' @keywords internal
result.detail.service = "TelemetryDirect/api/Results/ResultDetails"
#' @name wqp-result-services
#' @keywords internal
result.dates.service = "TelemetryDirect/api/Results/ReadingDates"
#' @name wqp-result-services
#' @keywords internal
result.data.insert.service = "TelemetryDirect/ResultData/Insert"


#' Event Services
#'
#' WQP event services.
#' @name wqp-event-services
#' @format a URL part.
#' @keywords internal
NULL

#' @rdname wqp-event-services
#' @keywords internal
event.types.service = "TelemetryDirect/api/Events/Types"
#' @rdname wqp-event-services
#' @keywords internal
event.reasons.service = "TelemetryDirect/api/Events/Reasons"
#' @rdname wqp-event-services
#' @keywords internal
event.summaries.service = "TelemetryDirect/api/Events/Summaries"
#' @rdname wqp-event-services
#' @keywords internal
event.detail.service = "TelemetryDirect/api/Events/EventDetails"
#' @rdname wqp-event-services
#' @keywords internal
event.action.detail.service = "TelemetryDirect/api/Events/ActionDetails"
#' @rdname wqp-event-services
#' @keywords internal
event.verificationinstrument.detail.service =
  "TelemetryDirect/api/Events/VerificationInstrumentDetails"
#' @rdname wqp-event-services
#' @keywords internal
event.standardsolution.detail.service =
  "TelemetryDirect/api/Events/StandardSolutionDetails"
#' @rdname wqp-event-services
#' @keywords internal
event.insert.service = "TelemetryDirect/EventData/Insert"
#' @rdname wqp-event-services
#' @keywords internal
event.action.insert.service = "TelemetryDirect/EventData/ActionInsert"
#' @rdname wqp-event-services
#' @keywords internal
event.verificationinstrument.insert.service =
  "TelemetryDirect/EventData/VerificationInstrumentInsert"
#' @rdname wqp-event-services
#' @keywords internal
event.standardsolution.insert.service =
  "TelemetryDirect/EventData/StandardSolutionInsert"


#' Report Services
#'
#' WQP report services.
#' @name wqp-report-services
#' @format a URL part.
#' @keywords internal
NULL

#' @rdname wqp-report-services
#' @keywords internal
report.pdm.service = "Reports/api/pdm"
