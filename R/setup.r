#' @include services.r
#' @include util.r
NULL

#' WQP timezone
#'
#' The timezone used by WQP. Timestamps are in Pacific Standard
#' Time (PST) and daylight savings is ignored.
#' @export
wqp_tz = function() {
  wqpr.tz
}

#' WQP Online Help
#'
#' Open the WQP online help page in a browser window.
#'
#' @inheritParams wqp-databases
#' @inheritParams wqp-tokens
#'
#' @importFrom glue glue
#' @export
wqp_help = function(database, ...) {
  database = validate_database(database)
  browseURL(glue("{base.url[[database]]}/{help.service}"), ...)
  invisible(NULL)
}

#' WQP Database Selection
#'
#' Switch between test and production database access.
#' @name wqp-databases
#' @param database The database name.
#' @param validate If `TRUE`, validate the database selection.
NULL

#' @rdname wqp-databases
#' @export
wqp_use_database = function(database, validate = TRUE) {
  if (validate)
    database = validate_database(database)
  assign("active.db", database, envir = authenv)
  invisible(wqp_active_database())
}

#' @rdname wqp-databases
#' @export
wqp_active_database = function() {
  get("active.db", envir = authenv)
}

#' @keywords internal
validate_database = function(database) {
  if (missing(database))
    database = wqp_active_database()
  match.arg(tolower(database), c("test", "production"))
}


#' WQP Program Selection
#'
#' Switch between WQP programs.
#' @name wqp-programs
#' @param program The program name.
#' @param validate If `TRUE`, validate the program selection.
NULL

#' @rdname wqp-programs
#' @export
wqp_use_program = function(program, validate = TRUE) {
  if (validate)
    program = validate_program(program)
  assign("active.program", program, envir = authenv)
  invisible(wqp_active_program())
}

#' @rdname wqp-programs
#' @export
wqp_active_program = function() {
  get("active.program", envir = authenv)
}

#' @keywords internal
validate_program = function(program) {
  if (missing(program))
    program = wqp_active_program()
  match.arg(toupper(program), c("MARSH", "EMP", "DAYFLOW"))
}


#' WQP Version Validation
#'
#' Validate WQP versions based on program.
#' @name wqp-versions
#' @param version The version number.
#' @inheritParams wqp-programs
#' @keywords internal
validate_version = function(version, program) {
  program = validate_program(program)
  if (program != "DAYFLOW") {
    return(1L)
  } else if (missing(version) || any(as.integer(version) < 1L) ||
      any(is.na(version))) {
    stop("Version must be a positive integer.")
  } else {
    as.integer(version)
  }
}

#' WQP Token Definition
#'
#' Get or set WQP token for authentication. Use `wqp_request_token`
#' to open the token request web page in a browser
#' (authentication required).
#'
#' @name wqp-tokens
#' @param token The token string.
#' @param validate If `TRUE`, validate the token.
#' @inheritParams wqp-databases
NULL

#' @rdname wqp-tokens
#' @export
wqp_use_token = function(token, database, validate = TRUE) {
  if (validate) {
    database = validate_database(database)
    token = validate_token(token, database)
  }
  # use authenv$ here because we want NULL, not error when not exist
  prior.token = authenv$active.token
  updated.token = replace(prior.token, database, token)
  assign("active.token", updated.token, envir = authenv)
  invisible(NULL)
}

#' @rdname wqp-tokens
#' @param ... Additional arguments to `browseURL`.
#' @importFrom utils browseURL
#' @importFrom glue glue
#' @export
wqp_request_token = function(database, ...) {
  database = validate_database(database)
  browseURL(glue("{base.url[[database]]}/{token.service}"), ...)
  invisible(NULL)
}


#' @keywords internal
get_active_token = function(database) {
  database = validate_database(database)
  get(database, pos = get("active.token", envir = authenv))
}

#' @rdname wqp-tokens
#' @importFrom glue glue
#' @export
wqp_check_token = function(token, database) {
  database = validate_database(database)
  if (missing(token))
    token = get_active_token(database)
  if (is.null(token) || !nzchar(token) || is.na(token) ||
    identical(token, character(0))) {
    FALSE
  } else {
    # validate using "valid" webservice
    result = as.logical(basic_get(
      glue("{base.url[[database]]}/{valid.token.service}", "?",
        "token={token}")))
    !(is.na(result) || isFALSE(result))
  }
}

#' @keywords internal
validate_token = function(token, database) {
  if (missing(token)) {
    token = get_active_token(database)
  }
  if (!wqp_check_token(token, database)) {
    stop("Token is not valid.", call. = FALSE)
  } else {
    token
  }
}

#' Check Connection to WQP
#'
#' Check if WQP database can be accessed.
#'
#' @inheritParams wqp-databases
#'
#' @return `TRUE` if database can be reached, `FALSE` otherwise.
#'
#' @importFrom curl curl_fetch_memory
#' @importFrom glue glue
#' @export
wqp_check_connection = function(database) {
  database = validate_database(database)
  res = try(curl_fetch_memory(glue("{base.url[[database]]}"),
        handle = wqpr_handle()), silent = TRUE)
  !inherits(res, "try-error")
}
