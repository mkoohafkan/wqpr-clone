#' @include setup.r
NULL

#' KOR Data Export Format
#'
#' Formatting specifications for CSV files exported from KOR.
#'
#' @importFrom readr cols col_date col_time col_skip col_guess
#' @keywords internal
kor.col.spec = cols(
  `Date (MM/DD/YYYY)` = col_date("%m/%d/%Y"),
  `Time (HH:mm:ss)` = col_time(format = "%H:%M:%S"),
  `Time (Fract. Sec)` = col_skip(),
  `Site Name` = col_skip()
)

#' KOR Data Export Locale
#'
#' Locale specifications for CSV files exported from KOR.
#'
#' @importFrom readr cols col_date col_time col_skip col_guess
#' @keywords internal
#' @importFrom readr locale
#' @keywords internal
kor.locale = locale(
  tz = wqpr.tz,
  encoding = "latin1"
)


#' KOR Constituent Dictionary
#'
#' Field name equivalencies for KOR file and Suisun Marsh Analytes.
#'
#' @keywords internal
kor.analyte.dictionary = c(
  "ODO" = "Dissolved Oxygen",
  "SpCond" = "Specific Conductance",
  "Temp" = "Temperature"
)


#' Read KOR File
#'
#' Read time series data from file exported from KOR.
#'
#' @param file The CSV file exported from KOR.
#' @param cdec.regex Regular expression to extract the station CDEC
#'   code from the filename.
#' @param col.spec The column format specifications for the file. Use
#'   this to override the default expected format. See 'details' for
#'   more information.
#' @param locale.spec The locale specifications for the file. Use
#'   this to override the default expected locale. Default locale
#'   assumes Latin character encoding and the standard WQP timezone,
#'   i.e. `"Etc/GMT+8"` (PST).
#' @return A tibble of time series data.
#'
#' @details The file is expected to the following columns:
#'   - Date (MM/DD/YYYY)
#'   - Time (HH:mm:ss)
#'   - Time (Fract. Sec)
#'   - Site Name
#'   - Columns for each analyte in format `<analyte> <units>`.
#'
#' @importFrom rlang .data .env
#' @importFrom readr read_csv
#' @importFrom stringr str_extract str_replace
#' @importFrom dplyr mutate if_else
#' @importFrom tidyr matches pivot_longer unite separate
#' @importFrom lubridate as_datetime
#' @export
kor_read = function(file, cdec.regex = "^[A-Z]{3}", col.spec,
  locale.spec) {
  if (missing(col.spec)) {
    col.spec = kor.col.spec
  }
  if (missing(locale.spec)) {
    locale.spec = kor.locale
  }
  cdec.code = str_extract(basename(file), cdec.regex)
  d = read_csv(file, col_types = col.spec, locale = locale.spec)
  d = pivot_longer(d, - matches("(Date)|(Time)"),
    names_to = "Constituent", values_to = "value")
  d = separate(d, "Constituent", into = c("analyte_name", "unit_name"),
    sep = " ", extra = "drop")
  d = unite(d, "time", matches("(Date)|(Time)"), sep = " ")
  mutate(d,
    time = as_datetime(.data$time, tz = locale.spec$tz),
    analyte_name = if_else(
      .data$analyte_name %in% names(kor.analyte.dictionary),
      kor.analyte.dictionary[.data$analyte_name],
      .data$analyte_name
    ),
    cdec_code = .env$cdec.code,
    unit_name = str_replace(.data$unit_name, "\U00B5", "\U03BC")
  )
}

#' KOR Data to WQP Records
#'
#' Format result data for insertion to WQP, including the
#' identification of target result IDs.
#'
#' @param new.data Timeseries data, i.e. output of [`kor_read()`].
#' @param reading.type The default reading type assumed for the KOR
#'   data. Default is `"Time Series"`.
#' @param interval The default data interval assumed for the KOR
#'   data. Default is `"15 min"`.
#' @inheritParams wqp_format_result_data
#' @return A tibble of records formatted for WQP.
#'
#' @details This is mainly a wrapper for [`wqp_format_result_data`]
#'   which sets some sensible defaults for result components.
#'
#' @export
kor_format = function(new.data, reading.type = "Time Series",
  interval = "15 min", na.action = c("warning", "error"), program,
  database) {
  formatted.data = new.data
  formatted.data["reading_type_name"] = reading.type
  formatted.data["interval_name"] = interval
  wqp_format_result_data(formatted.data, na.action, program, database)
}

#' Trim KOR Data
#'
#' Remove KOR data records that already exist in WQP. Useful for
#' extracting a subset of KOR records for backfilling data.
#'
#' @param new.data The formatted KOR records, i.e. output of
#'   [`kor_format()`].
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @return A subset of records from `new.data`.
#'
#' @details Records are selected based on presence/absence of
#'   records in WQP.
#'
#' @export
kor_trim = function(new.data, program, database) {
  d = check_result_data(new.data, program, database)
  d[is.na(d$value.existing), names(new.data)]
}
