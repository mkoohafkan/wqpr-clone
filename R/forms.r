#' @include getters.r
#' @include events.r
#' @include actions.r
#' @include verificationinstruments.r
#' @include standardsolutions.r
#' @include results.r
NULL


#' Read Event Sheet
#'
#' Read event data from an electronic form.
#'
#' @param workbook The Excel workbook (.xlsx) containing
#'   the event data. See 'details' for the required format.
#' @param timezone The timezone assumed for the visit sheet.
#'   Default is the standard WQP timezone, i.e. `"Etc/GMT+8"` (PST).
#' @return A tibble of visit data.
#'
#' @details The workbook is expected to contain one or more of the
#'   following sheets:
#'   - **EVENT**: this sheet is formatted to contain
#'     columns that match the WQP event components:
#'     - `cdec_code`
#'     - `event_type_name`
#'     - `contact_name`
#'     - `arrival_time`
#'     - `restart_time`
#'     - `departure_time`
#'     - `reason_name`
#'     - `summary_name`
#'     - `comments`.
#'   - **RESULT**: this sheet is formatted to contain
#'     columns that match the WQP result components:
#'     - `cdec_code`
#'     - `interval_name`
#'     - `analyte_name`
#'     - `unit_name`
#'     - `rank_name`
#'     - `reading_type_name`
#'     - `time`
#'     - `value`
#'   - **ACTION**: this sheet is formatted to contain columns
#'     that match the WQP action components:
#'     - `arrival_time`
#'     - `cdec_code`
#'     - `contact_name`
#'     - `event_type_name`
#'     - `sonde_name`
#'     - `location_name`
#'     - `action_name`
#'   - **INSTRUMENT**: this sheet is formatted to contain columns
#'     that match the WQP verification instrument components:
#'     - `cdec_code`
#'     - `event_type_name`
#'     - `contact_name`
#'     - `arrival_time`
#'     - `instrument_name`
#'     - `serial_number`
#'     - `calibration_due_date`
#'   - **SOLUTION**: this sheet is formatted to contain columns
#'     that match the WQP standard solution components:
#'     - `cdec_code`
#'     - `event_type_name`
#'     - `contact_name`
#'     - `arrival_time`
#'     - `solution_name`
#'     - `lot_number`
#'     - `expiration_date`
#'
#' @importFrom readxl read_excel excel_sheets
#' @importFrom lubridate as_datetime
#' @importFrom dplyr mutate mutate_at vars matches case_when rename
#' @importFrom tibble tibble
#' @importFrom stringr str_replace
#' @export
frm_read = function(workbook, timezone = wqp_tz()) {
  # logging
  loglines = NULL
  log_fun = function(w) loglines <<- append(loglines, w$message)
  sheets = excel_sheets(workbook)
  # read RESULT sheet as table
  if ("RESULT" %in% sheets) {
    result = suppressMessages(read_excel(workbook, "RESULT",
      col_names = TRUE, na = c("", " "), skip = 0,
      trim_ws = TRUE, col_types = "text"))
    # parse result timestamps
    result = mutate(result,
      time = as_datetime(.data$time, tz = timezone))
    # warn about missing entries
    missing.entries = apply(apply(result, 1, is.na), 2, any)
    if (any(missing.entries)) {
      warning(sum(missing.entries), " of ", length(missing.entries),
        " result records are missing or improperly formatted in ",
        "workbook ", workbook, ".", call. = FALSE)
    }
  } else {
    result = NULL
  }
  # read EVENT sheet as table
  if ("EVENT" %in% sheets) {
    required.event.vars = c("arrival_time", "cdec_code", "contact_name",
      "reason_name", "summary_name")
    event = suppressMessages(read_excel(workbook, "EVENT",
      col_names = TRUE, na = c("", " "), skip = 0,
      trim_ws = TRUE, col_types = "text"))
    # parse event timestamps
    event = mutate_at(event, vars(matches("time")),
      ~ as_datetime(.x, tz = timezone)
    )
    # warn about missing entries
    missing.entries = apply(apply(event, 1, is.na), 2, any)
    if (any(missing.entries)) {
      warning(sum(missing.entries), " of ", length(missing.entries),
        " event records are missing or improperly formatted in ",
        "workbook ", workbook, ".", call. = FALSE)
    }
    incomplete.error = withCallingHandlers(
      check_na(event, required.event.vars,
        "required event records", "warning"),
      warning = log_fun
    )
  } else {
    event = NULL
    incomplete.error = TRUE
    withCallingHandlers(warning("No event records found."),
      warning = log_fun)
  }
  # read ACTION sheet as table
  if ("ACTION" %in% sheets) {
    action = suppressMessages(read_excel(workbook, "ACTION",
      col_names = TRUE, na = c("", " "), skip = 0,
      trim_ws = TRUE, col_types = "text"))
    # parse event timestamps
    action = mutate(action,
      arrival_time = as_datetime(.data$arrival_time, tz = timezone))
    # warn about missing entries
    missing.entries = apply(apply(action, 1, is.na), 2, any)
    if (any(missing.entries)) {
      warning(sum(missing.entries), " of ", length(missing.entries),
      " action records are missing or improperly formatted in ",
      "workbook ", workbook, ".", call. = FALSE)
    }
  } else {
    action = NULL
  }
  # read INSTRUMENT sheet as table
  if ("INSTRUMENT" %in% sheets) {
    instrument = suppressMessages(read_excel(workbook, "INSTRUMENT",
      col_names = TRUE, na = c("", " "), skip = 0,
      trim_ws = TRUE, col_types = "text"))
    # parse event timestamps
    instrument = mutate(
      filter(instrument, !is.na(.data$instrument_name)),
      calibration_due_date = as_datetime(.data$calibration_due_date,
        tz = timezone)
    )
    # warn about missing entries
    missing.entries = apply(apply(instrument, 1, is.na), 2, any)
    if (any(missing.entries)) {
      warning(sum(missing.entries), " of ", length(missing.entries),
      " verification instrument records are missing or improperly ",
      "formatted in workbook ", workbook, ".", call. = FALSE)
    }
  } else {
    instrument = NULL
  }
  # read SOLUTION sheet as table
  if ("SOLUTION" %in% sheets) {
    solution = suppressMessages(read_excel(workbook, "SOLUTION",
      col_names = TRUE, na = c("", " "), skip = 0,
      trim_ws = TRUE, col_types = "text"))
    # parse event timestamps
    solution = mutate(solution,
      expiration_date = as_datetime(.data$expiration_date,
        tz = timezone))
    # warn about missing entries
    missing.entries = apply(apply(solution, 1, is.na), 2, any)
    if (any(missing.entries)) {
      warning(sum(missing.entries), " of ", length(missing.entries),
      " standard solution records are missing or improperly formatted ",
      "in workbook ", workbook, ".", call. = FALSE)
    }
  } else {
    solution = NULL
  }
  tibble(
    event = list(event),
    result = list(result),
    action = list(action),
    instrument = list(instrument),
    solution = list(solution),
    completed = !incomplete.error,
    log = list(loglines)
  )
}

#' Format Form Data for WQP
#'
#' Format all components of a form to the Water Quality Portal.
#'
#' @param form Form data, i.e. output of `frm_read()`.
#'   Must contain list column `event` and optional list columns
#'   `result`, `action`, `instrument`, and `solution`.
#' @param na.omit If `TRUE`, omit `NA` records under specific
#'   circumstances before validating. See "Details" For more
#'   information.
#' @inheritParams wqp_format_result_data
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @return A tibble of formatted data, with additional columns
#'  "validated" (logical) signaling if the form data was validated
#'  and "log" containing information on validation failures.
#'
#' @details If `na.omit = TRUE`, records with `NA` values will be
#'   removed under the following circumstances:
#'   * For `result` data, records where both `result_id` and `value`
#'     are `NA` will be omitted.
#'   * For `action` data, records where `sonde_name` is `NA` will be
#'     omitted.
#'   * For `instrument` data, records where `instrument_name` is `NA`
#'     will be removed.
#'   * For `solution` data, records where `solution_name` is `NA`
#'     will be removed.
#'   Note that no `event` data records are removed.
#'
#' @export
frm_format = function(form, na.omit = FALSE,
  na.action = c("warning", "error"), program, database) {
  database = validate_database(database)
  program = validate_program(program)
  na.action = match.arg(na.action, c("warning", "error"))
  na_fun = switch(na.action,
    "warning" = warning,
    "error" = stop
  )
  # logging
  loglines = NULL
  log_fun = function(w) loglines <<- append(loglines, w$message)
  # extract form data
  event = suppressWarnings(form$event[[1]])
  result = suppressWarnings(form$result[[1]])
  action = suppressWarnings(form$action[[1]])
  instrument = suppressWarnings(form$instrument[[1]])
  solution = suppressWarnings(form$solution[[1]])
  # required fields
  required.event.vars = c("arrival_time", "station_id", "contact_id")
  required.action.vars = c("sonde_id", "location_id", "action_id")
  required.instrument.vars = c("instrument_id")
  required.solution.vars = c("solution_id")
  required.result.vars = c("result_id", "time", "version")
  validation.error = FALSE
  # event metadata
  if (missing(event) || is.null(event) || nrow(event) < 1L) {
    stop("No event data found.")
  } else {
    if (nrow(event) > 1L) {
      stop("Only one event can be processed per function call.")
    }
    event.formatted = wqp_format_events(event, na.action = "warning",
      program = program, database = database)
    event.validated = withCallingHandlers(
      check_na(event.formatted, required.event.vars,
        "event records", "warning"),
      warning = log_fun
    )
    validation.error = validation.error || event.validated
  }
  # action data
  if (!is.null(action)) {
    action.formatted = wqp_format_actions(action,
      na.action = "warning", match.event = FALSE,
      program = program, database = database)
    if (na.omit) {
      action.formatted = omit_na(action.formatted, "sonde_name")
    }
    action.validated = withCallingHandlers(
      check_na(action.formatted, required.action.vars,
        "action records", "warning"),
      warning = log_fun
    )
    validation.error = validation.error || action.validated
    # convert zero-row table to NULL
    if (nrow(action.formatted) < 1L) {
      action.formatted = NULL
    }
  } else {
    action.formatted = NULL
  }
  # instrument data
  if (!is.null(instrument)) {
    instrument.formatted = wqp_format_verification_instruments(
      instrument, na.action = "warning", match.event = FALSE,
      program = program, database = database)
    if (na.omit) {
      instrument.formatted = omit_na(instrument.formatted,
        "instrument_name")
    }
    instrument.validated = withCallingHandlers(
      check_na(instrument.formatted, required.instrument.vars,
        "verification instrument records", "warning"),
      warning = log_fun
    )
    validation.error = validation.error || instrument.validated
    # convert zero-row table to NULL
    if (nrow(instrument.formatted) < 1L) {
      instrument.formatted = NULL
    }
  } else {
    instrument.formatted = NULL
  }
  # solution data
  if (!is.null(solution)) {
    solution.formatted = wqp_format_standard_solutions(solution,
      na.action = "warning", match.event = FALSE,
      program = program, database = database)
    if (na.omit) {
      solution.formatted = omit_na(solution.formatted, "solution_name")
    }
    solution.validated = withCallingHandlers(
      check_na(solution.formatted, required.solution.vars,
        "standard solution records", "warning"),
      warning = log_fun
    )
    validation.error = validation.error || solution.validated
    # convert zero-row table to NULL
    if (nrow(solution.formatted) < 1L) {
      solution.formatted = NULL
    }
  } else {
    solution.formatted = NULL
  }
  # result data
  if (!is.null(result)) {
    result.formatted = wqp_format_result_data(result,
      na.action = "warning", program = program, database = database)
    if (na.omit) {
      result.formatted = omit_na(result.formatted,
        c("result_id", "value"), "all")
    }
    result.validated = withCallingHandlers(
      check_na(result.formatted, required.result.vars,
        "result data records", "warning"),
      warning = log_fun
    )
    validation.error = validation.error || result.validated
    # convert zero-row table to NULL
    if (nrow(result.formatted) < 1L) {
      result.formatted = NULL
    }
  } else {
    result.formatted = NULL
  }
  if (validation.error) {
    na_fun("Some records could not be validated.", call. = FALSE)
  }
  tibble(
    event = list(event.formatted),
    result = list(result.formatted),
    action = list(action.formatted),
    instrument = list(instrument.formatted),
    solution = list(solution.formatted),
    validated = !validation.error,
    log = list(loglines)
  )
}

#' Insert Form Data to WQP
#'
#' Insert all components of a form to the Water Quality Portal.
#'
#' @param form Validated form data, i.e. output of `frm_format()`.
#'   Must contain list column `event` and optional list columns
#'   `result`, `action`, `instrument`, and `solution`.
#' @inheritParams wqp_insert_result_data
#' @inheritParams wqp-programs
#' @inheritParams wqp-databases
#' @inheritParams wqp-tokens
#' @return A tibble of insert query responses.
#'
#' @export
frm_insert = function(form, overwrite = FALSE,
  program, database, token) {
  database = validate_database(database)
  program = validate_program(program)
  token = validate_token(token, database)
  loglines = NULL
  log_fun = function(w) loglines <<- append(loglines, w$message)
  insert.error = FALSE
  # extract form data
  event = suppressWarnings(form$event[[1]])
  result = suppressWarnings(form$result[[1]])
  action = suppressWarnings(form$action[[1]])
  instrument = suppressWarnings(form$instrument[[1]])
  solution = suppressWarnings(form$solution[[1]])
  if (missing(event) || is.null(event) || nrow(event) < 1L) {
    stop("No event data found.")
  }
 # insert event
  event.inserted = wqp_insert_events(event, overwrite = overwrite,
    program = program, database = database, token = token)
  event.error = withCallingHandlers(
    check_insert(event.inserted, "result",
      "event records", "warning"),
    warning = log_fun
  )
  insert.error = insert.error || event.error
  # get event id
  event.id = get_event_id(event, program, database)
  if (is.na(event.id)) {
    insert.error = TRUE
  }
  # insert action
  if (!is.null(action)) {
    action$event_id = rep(event.id, nrow(action))
    action.inserted = wqp_insert_actions(action, overwrite = overwrite,
      program = program, database = database, token = token)
    action.error = withCallingHandlers(
      check_insert(action.inserted, "result",
        "action records", "warning"),
      warning = log_fun
    )
    insert.error = insert.error || action.error
  } else {
    action.inserted = NULL
  }
  # insert instrument
  if (!is.null(instrument)) {
    instrument$event_id = rep(event.id, nrow(instrument))
    instrument.inserted = wqp_insert_verification_instruments(
      instrument, overwrite = overwrite, program = program,
      database = database, token = token
    )
    instrument.error = withCallingHandlers(
      check_insert(instrument.inserted, "result",
        "verification instrument records", "warning"),
      warning = log_fun
    )
    insert.error = insert.error || instrument.error
  } else {
    instrument.inserted = NULL
  }
  # insert solution
  if (!is.null(solution)) {
    solution$event_id = rep(event.id, nrow(solution))
    solution.inserted = wqp_insert_standard_solutions(solution,
      overwrite = overwrite, program = program, database = database,
      token = token)
    solution.error = withCallingHandlers(
      check_insert(solution.inserted, "result",
        "standard solution records", "warning"),
      warning = log_fun
    )
    insert.error = insert.error || solution.error
  } else {
    solution.inserted = NULL
  }
  # insert result
  if (!is.null(result)) {
    result.inserted = wqp_insert_result_data(result,
      overwrite = overwrite, program = program,
      database = database, token = token)
    result.error = withCallingHandlers(
      check_insert(result.inserted, "result",
        "result data records", "warning"),
      warning = log_fun
    )
    insert.error = insert.error || result.error
  } else {
    result.inserted = NULL
  }
  if (insert.error) {
    warning("Some records could not be inserted.",
      call. = FALSE)
  }
  tibble(
    event = list(event.inserted),
    result = list(result.inserted),
    action = list(action.inserted),
    instrument = list(instrument.inserted),
    solution = list(solution.inserted),
    inserted = !insert.error,
    log = list(loglines)
  )
}

#' Check for existing form data in WQP
#'
#' Check if data matching the supplied form already exists in WQP.
#'
#' @inheritParams frm_insert
#' @return A tibble of insert query responses.
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @export
frm_check = function(form, program, database) {
  database = validate_database(database)
  program = validate_program(program)
  loglines = NULL
  log_fun = function(w) loglines <<- append(loglines, w$message)
  # extract form data
  event = suppressWarnings(form$event[[1]])
  result = suppressWarnings(form$result[[1]])
  action = suppressWarnings(form$action[[1]])
  instrument = suppressWarnings(form$instrument[[1]])
  solution = suppressWarnings(form$solution[[1]])
  exists = FALSE
  if (missing(event) || is.null(event) || nrow(event) < 1L) {
    stop("No event data found.")
  }
  # check event
  event.checked = mutate(
    suppressWarnings(check_events(event, program, database)),
    exists = !is.na(.data$event_id)
  )
  check.error = withCallingHandlers(
    check_existing(event.checked, "exists",
      "event records", "warning"),
    warning = log_fun
  )
  exists = exists || check.error
  # check action
  if (!is.null(action)) {
    action.checked = mutate(
      suppressWarnings(check_actions(action, program, database)),
      exists = !is.na(.data$edit_timestamp)
    )
    check.error = withCallingHandlers(
      check_existing(action.checked,
        "exists", "action records", "warning"),
      warning = log_fun
    )
    exists = exists || check.error
  } else {
    action.checked = NULL
  }
  # check instrument
  if (!is.null(instrument)) {
    instrument.checked = mutate(
      suppressWarnings(check_verification_instruments(instrument,
        program, database)),
      exists = !is.na(.data$edit_timestamp)
    )
    check.error = withCallingHandlers(
      check_existing(instrument.checked, "exists",
        "verification instrument records", "warning"),
      warning = log_fun
    )
    exists = exists || check.error
  } else {
    instrument.checked = NULL
  }
  # check solution
  if (!is.null(solution)) {
    solution.checked = mutate(
      suppressWarnings(check_standard_solutions(solution,
        program, database)),
      exists = !is.na(.data$edit_timestamp)
    )
    check.error = withCallingHandlers(
      check_existing(solution.checked, "exists",
        "standard solution records", "warning"),
      warning = log_fun
    )
    exists = exists || check.error
  } else {
    solution.checked = NULL
  }
  # check result
  if (!is.null(result)) {
    result.checked = mutate(
      suppressWarnings(check_result_data(result, program, database)),
      exists = !is.na(.data$qaqc_flag_id.existing)
    )
    check.error = withCallingHandlers(
      check_existing(result.checked, "exists",
        "result data records", "warning"),
      warning = log_fun
    )
    exists = exists || check.error
  } else {
    result.checked = NULL
  }
  # return check results
  tibble(
    event = list(event.checked[c(names(event), "exists")]),
    result = list(result.checked[c(names(result), "exists")]),
    action = list(action.checked[c(names(action), "exists")]),
    instrument = list(instrument.checked[c(names(instrument),
      "exists")]),
    solution = list(solution.checked[c(names(solution), "exists")]),
    unique = !exists,
    log = list(loglines)
  )
}
