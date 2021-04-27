#' @include services.r
#' @include setup.r
NULL

#' Interface to WQP
#'
#' This packages provides an interface to the California Department of
#' Water Resources (DWR) Water Quality Portal (WQP) database via the
#' Vista Web Services REST API. For DWR internal use only.
#'
#' @section Package options:
#'
#' wqpr uses the following [options()] to configure behavior:
#'
#' \itemize{
#'   \item `wqpr.database`: The default database to use on start
#'     ("test" or "production").
#'   \item `wqpr.program`: The default program to use on start
#'     ("MARSH", "EMP", or "DAYFLOW").
#'   \item `wqpr.token`: A named list of tokens to use for
#'     authentication to each database ("test" and/or "production").
#' }
#'
#' @docType package
#' @name wqpr
"_PACKAGE"
