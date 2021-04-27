#' Omit NA Values
#'
#' Helper function for ommitting NA values. Provides greater
#' flexibility compared to [stats::na.omit()].
#'
#' @param data The data to process.
#' @param id.cols Vector of columns to check for NA values. If missing,
#'   all columns will be used.
#' @param choose Method for identifying rows to omit. If
#'   `choose = "any"`, `NA` values in any of the columns specified by
#'   `id.cols` will trigger an ommission. If `choose = "all"`, only rows
#'   with `NA` values in all columns specified by `id.cols` will trigger
#'   an omission.
#' @return The data, with rows omitted.
#'
#' @importFrom purrr pmap_lgl
#' @importFrom stats setNames
#' @keywords internal
omit_na = function(data, id.cols, choose = c("any", "all")) {
  if (missing(id.cols)) {
    id.cols = names(data)
  }
  choose = match.arg(choose, c("any", "all"))
  op = switch(choose,
    "any" = any,
    "all" = all
  )
  omit = pmap_lgl(lapply(data[id.cols], is.na), op)
  xx = data[!omit,, drop = FALSE]
  if (any(omit > 0L)) {
    temp <- setNames(seq(omit)[omit], attr(data, "row.names")[omit])
    attr(temp, "class") <- "omit"
    attr(xx, "na.action") <- temp
  }
  xx
}


#' Print Table
#'
#' Helper function for printing error rows.
#' @param data The data to concatenate for printing.
#' @param selection Logical vector of rows to subset from `data`.
#' @return A string.
#'
#' @importFrom utils head tail
#' @importFrom glue glue
#' @importFrom stringr str_c
#' @importFrom purrr pmap_chr map imap
#' @keywords internal
print_table = function(data, selection) {
  dnames = names(data)
  data["row"] = 1:nrow(data)
  fdata = imap(data[selection, ], ~ c(.y, paste(.x)))
  fdata = map(fdata, ~ format(.x, width = max(nchar(.x))))
  row.string = pmap_chr(fdata[c("row", dnames)], paste, sep = " | ")
  linebreak = str_c(rep("-", nchar(row.string[1])), collapse = "")
  str_c(c(linebreak, head(row.string, 1), linebreak,
    tail(row.string, -1), linebreak), collapse = "\n")
}

#' Check Insertion
#'
#' Check that record insertion was successful.
#'
#' @inheritParams check_na
#' @param response.col The field containing the server response.
#' @return `TRUE` if `response.cols` contain server errors,
#'   `FALSE` otherwise.
#' @keywords internal
check_insert = function(input.data, response.col = "result",
  what = "records", na.action = c("warning", "error")) {
  na.action = match.arg(na.action, c("warning", "error"))
  na_fun = switch(na.action,
    "warning" = warning,
    "error" = stop
  )
  success = grepl("Insert|Update|Existing", input.data[[response.col]])
  if (!all(success)) {
    na_fun(sum(!success), " failed ", what,
      " insertions:\n", print_table(input.data, !success),
      call. = FALSE)
    TRUE
  } else {
    FALSE
  }
}


#' Field Checker
#'
#' Check for missing fields.
#'
#' @param required Vector of required fields.
#' @param provided Vector of provided fields.
#' @param num.required Number of fields required to be present.
#'
#' @importFrom stringr str_c
#' @keywords internal
check_fields = function(required, provided,
  num.required = length(required)) {
  missing.vars = setdiff(required, provided)
  if (length(provided) < num.required) {
    if (num.required == length(required)) {
      num.required = "all"
    }
    stop("Missing required fields: ", num.required, " of ",
      str_c(wrap_quotes(missing.vars), collapse = ", "),
      call. = FALSE)
  }
}

#' @rdname check_fields
#'
#' @param optional Vector of optional fields.
#'
#' @importFrom stringr str_c
#' @keywords internal
check_optional_fields = function(optional, provided) {
  missing.vars = setdiff(optional, provided)
  if (length(missing.vars) > 0) {
    warning("Missing optional fields: ",
      str_c(wrap_quotes(missing.vars), collapse = ", "),
      call. = FALSE)
  }
}

#' Identifier Checker
#'
#' Helper function for verifying that all identifier records
#' are not `NA`.
#'
#' @param input.data The input dataset.
#' @param id.cols A vector of identifier column names.
#' @param what A label for messaging.
#' @param na.action The output response.
#' @return `TRUE` if `id.cols` contain `NA` values, `FALSE` otherwise.
#'
#' @importFrom purrr pmap_lgl
#' @keywords internal
check_na = function(input.data, id.cols, what = "records",
  na.action = c("warning", "error")) {
  na.action = match.arg(na.action, c("warning", "error"))
  na_fun = switch(na.action,
    "warning" = warning,
    "error" = stop
  )
  na.rows = pmap_lgl(input.data[id.cols], ~ any(is.na(list(...))))
  if (any(na.rows)) {
    na_fun(sum(na.rows), " incomplete or improperly-formatted ", what,
      " detected:\n", print_table(input.data, na.rows), call. = FALSE)
    TRUE
  } else {
    FALSE
  }
}

#' Timestamp Checker
#'
#' Check and parse timestamps.
#'
#' @inheritParams check_na
#' @param time.col The field containing timestamps.
#' @return A vector of timestamps.
#'
#' @importFrom lubridate as_datetime is.instant
#' @keywords internal
check_timestamps = function(input.data, time.col,
  na.action = c("warning", "error")) {
  na.action = match.arg(na.action, c("warning", "error"))
  na_fun = switch(na.action,
    "warning" = warning,
    "error" = stop
  )
  if (!is.instant(input.data[[time.col]])) {
    orig.times = input.data[[time.col]]
    output.times = as_datetime(input.data[[time.col]],
      tz = wqpr.tz)
  } else {
    output.times = input.data[[time.col]]
  }
  na.datetimes = is.na(output.times)
  if (any(na.datetimes)) {
    na_fun(sprintf("%d %s", sum(na.datetimes), time.col),
      " timestamps could not be parsed:\n",
      print_table(input.data, na.datetimes), call. = FALSE)
  }
  output.times
}

#' Lookup Checker
#'
#' Helper function for verifying that all entries have an associated ID.
#'
#' @param label The type of ID to check. Used for messaging.
#' @param name.col A vector of names.
#' @param id.col A vector of IDs.
#'
#' @keywords internal
check_ids = function(label, name.col, id.col,
  response = c("warning", "error")) {
  response = match.arg(response, c("warning", "error"))
  if (response == "warning") {
    response_fun = warning
  } else {
    response_fun = stop
  }
  maxchar = nchar(format(length(name.col))) + 2L
  if (any(is.na(id.col))) {
    response_fun("No corresponding ", label, " ID found for records:\n",
      print_table(data.frame(name = name.col, stringsAsFactors = FALSE),
        is.na(id.col)), call. = FALSE)
  }
  invisible(NULL)
}

#' Existence Checker
#'
#' Helper function for identifying that all entries are new.
#'
#' @inheritParams check_na
#' @param exist.col The field identifying entries as new or existing.
#' @return A vector of timestamps.
#'
#' @keywords internal
check_existing = function(input.data, exist.col = "exists",
  what = "records", response = c("warning", "error")) {
  response = match.arg(response, c("warning", "error"))
  response_fun = switch(response,
    "warning" = warning,
    "error" = stop
  )
  exist.rows = input.data[[exist.col]]
  if (any(exist.rows)) {
    response_fun(sum(exist.rows), " existing ", what,
      " detected:\n", print_table(input.data, exist.rows),
      call. = FALSE)
    TRUE
  } else {
    FALSE
  }
}
