#' Detect Sequence
#'
#' Detect a specific pattern in a vector.
#'
#' @param x A vector of values.
#' @param pattern The pattern to search for.
#' @return A vector of same length as `x`, with unique integer
#'   values for each detected sequence and `NA` elsewhere.
#'
#' @importFrom stats embed
#' @keywords internal
detect_sequence = function(x, pattern) {
  if (length(x) < length(pattern))
    return(rep(NA, length(x)))
  # identify pattern occurrence
  x = as.integer(factor(x, levels = pattern))
  pattern = seq_along(pattern)
  pr = rev(pattern)
  w = embed(x, length(pr))
  w.pos = which(apply(w, 1, function(r) all(r == pr)))
  # generate grouping vector
  groups = rep(NA, length(x))
  idx = unlist(lapply(w.pos, function(x) x:(x + length(pattern) - 1)))
  id = unlist(lapply(w.pos, rep, length(pattern)))
  groups[idx] = id
  groups
}

#' Wrap In Quotes
#'
#' Helper function for wrapping a value in double quotes.
#'
#' @param x Value to wrap in double quotes.
#' @return The value `x`, converted to character and wrapped in
#'   double quotes.
#'
#' @keywords internal
wrap_quotes = function(x) {
  sprintf("\"%s\"", x)
}

#' wqpr curl handle
#'
#' Configuration for curl URL handling in wqpr.
#'
#' @importFrom curl new_handle handle_setopt handle_setheaders
#' @importFrom utils packageName
#' @keywords internal
wqpr_handle = function() {
  # only configure settings that apply to ALL requests
  new_handle(
#    cainfo = system.file("CurlSSL", "Water-RootCA.crt",
#      package = packageName(), mustWork = TRUE)
  )
}

#' wqpr curl pool
#'
#' Configuration for asynchronous curl request handling in wqpr.
#' @importFrom curl new_pool
#' @keywords internal
wqpr_pool = function() {
  new_pool(host_con = 10)
}

#' Basic GET
#'
#' Basic WQP GET interface.
#'
#' @param service.url The complete URL of the web service.
#' @return The query results.
#'
#' @importFrom curl curl_fetch_memory parse_headers
#' @keywords internal
basic_get = function(service.url) {
  result = curl_fetch_memory(service.url, handle = wqpr_handle())
  extract_response(result, TRUE)
}

#' Multi GET
#'
#' Asynchronous WQP GET interface.
#'
#' @param service.url The complete URL of the web service.
#' @return The query results.
#'
#' @importFrom curl curl_fetch_multi multi_run
#' @importFrom stringr str_c str_pad
#' @importFrom purrr map
#' @keywords internal
multi_get = function(service.url, stop.on.fail = TRUE) {
  results = list()
  cb = function(i) {
    function(res) {
      results[[i]] <<- res
    }
  }
  pool = wqpr_pool()
  map(seq_along(service.url),
  ~ curl_fetch_multi(service.url[.x], pool = pool,
    done = cb(.x), fail = cb(.x),
      handle = wqpr_handle()))

  out = multi_run(pool = pool)
  if (stop.on.fail & out$error > 0) {
    stop(out$error, " of ", out$success + out$error,
      " requests failed.")
  } else if (out$error > 0) {
    warning(out$error, " of ", out$success + out$error,
      " requests failed.")
  }
  map(results, extract_response, stop.on.fail)
}


#' Basic POST
#'
#' Basic WQP POST interface.
#'
#' @inheritParams basic_get
#' @param body The POST field(s) to include in the request body.
#' @return The server response.
#'
#' @importFrom curl curl_fetch_memory parse_headers handle_setform
#' @keywords internal
basic_post = function(service.url, body) {
  h = wqpr_handle()
  handle_setform(h, .list = body)
  result = curl_fetch_memory(service.url, handle = h)
  extract_response(result, TRUE)
}

#' Multi POST
#'
#' Asynchronous WQP POST interface.
#'
#' @inheritParams basic_get
#' @param body The POST field(s) to include in the request body.
#' @return The server response.
#'
#' @importFrom curl curl_fetch_multi handle_setform multi_run
#' @importFrom purrr map_chr
#' @importFrom stringr str_c str_pad
#' @keywords internal
multi_post = function(service.url, body, stop.on.fail = FALSE) {
  results = list()
  cb = function(i) {
    function(res) {
      results[[i]] <<- res
    }
  }
  pool = wqpr_pool()
  map(seq_along(service.url),
  ~ curl_fetch_multi(service.url[.x], pool = pool,
    done = cb(.x), fail = cb(.x),
      handle = handle_setform(wqpr_handle(), .list = body[[.x]])))

  out = multi_run(pool = pool)
  if (stop.on.fail & out$error > 0) {
    stop(out$error, " of ", out$success + out$error,
      " requests failed.")
  } else if (out$error > 0) {
    warning(out$error, " of ", out$success + out$error,
      " requests failed.")
  }
  map_chr(results, extract_response, stop.on.fail)
}

#' Extract Response
#'
#' @param result The result object, i.e. output of `curl_fetch_memory`.
#'
#' @importFrom curl parse_headers
#' @keywords internal
extract_response = function(result, stop.on.fail = FALSE) {
  if (stop.on.fail)
    status_fun = stop
  else
    status_fun = warning
  if (result$status_code != 200L)
    status_fun("WQP query failed with status ",
      parse_headers(result$headers)[1], "\n  ",
      "URL request: ", result$url,
      call. = FALSE)
  value = rawToChar(result$content)
  Encoding(value) = "UTF-8"
  value
}

#' POST Body Formatting
#'
#' Generate the body contents of a POST request. Vectorized
#'  for use with tidyverse
#'
#' @importFrom purrr map transpose modify_if
#' @importFrom dplyr as_tibble
#' @keywords internal
post_body = function(...) {
  body = map(list(...), as.character)
  tbody = transpose(as.list(as_tibble(body)))
  map(tbody, ~ modify_if(.x, is.na, ~ "null"))
}

#' Read Results
#'
#' Read results into a dataframe.
#'
#' @param content The result content, i.e. output of `basic_get()`
#' @param result.cols Result column type specification, i.e.,
#'   `readr::cols()`, `readr::cols_only()`, etc.
#' @return a tibble.
#'
#' @importFrom readr read_delim
#' @keywords internal
read_results = function(content, result.cols, empty.data) {
  if (content == "No records found.") {
    empty.data
  } else {
    read_delim(content, delim = "|", trim_ws = TRUE, na = "null",
    col_types = result.cols, locale = wqpr.locale)
  }
}

#' Reformat WQP Datetime
#'
#' Reformat datetime for WQP URLs.
#'
#' @param x The datetime to reformat.
#' @return The reformatted datetime.
#'
#' @importFrom glue glue
#' @importFrom dplyr if_else
#' @importFrom lubridate tz
#' @keywords internal
reformat_wqp_datetime = function(x, space) {
  if (tz(x) != wqp_tz()) {
    stop(glue("Timezone of input data is '{tz(x)}' ",
      "but WQP expects '{wqp_tz()}'."))
  }
  if_else(is.na(x), NA_character_,
    format(x, glue("%Y-%m-%d{space}%H:%M:%S")))
}

#' Process datetimes for WQP queries
#'
#' This function handles some timezone quirks.
#'
#' @param x A vector that can be coerced to a datetime.
#' @param default The default value to use if `x` is missing.
#' @return A vector of datetimes.
#'
#' @importFrom lubridate as_datetime force_tz is.Date
#' @importFrom dplyr if_else
#' @keywords internal
process_datetime = function(x, default) {
  if (missing(x) || is.null(x) || all(is.na(x))) {
    x = default
  }
  if (is.Date(x)) {
    x = force_tz(as_datetime(x), tzone = wqpr.tz)
  } else {
    x = suppressWarnings(as_datetime(x, tz = wqpr.tz))
  }
  if (any(is.na(x))) {
    stop("Timestamp format failed to parse.", call. = FALSE)
  }
  x
}
