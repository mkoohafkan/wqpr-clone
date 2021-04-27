#' @include services.r
#' @include setup.r
NULL

.onLoad = function(libname, pkgname) {
  # set up environment for authentication storage
  authenv = new.env(parent = getNamespace(pkgname))
  assign("authenv", authenv, envir = getNamespace(pkgname))
  assign("authenticated", FALSE, envir = authenv)
}

.onAttach = function(libname, pkgname) {
  # set the database
  if (is.null(getOption("wqpr.database"))) {
    packageStartupMessage("Defaulting to \"",
      wqp_use_database("test"), "\" database.")
  } else {
    packageStartupMessage("Using \"",
      wqp_use_database(getOption("wqpr.database"), FALSE),
      "\" database.")
    tryCatch(validate_database(), error = function(e) warning(e))
  }
  # set the program
  if (is.null(getOption("wqpr.program"))) {
    packageStartupMessage("Defaulting to \"",
      wqp_use_program("MARSH"), "\" program.")
  } else {
    packageStartupMessage("Using \"",
      wqp_use_program(getOption("wqpr.program"), FALSE),
      "\" program.")
    tryCatch(validate_program(), error = function(e) warning(e))
  }
  # check/set the token
  if (!is.null(getOption("wqpr.token")[[wqp_active_database()]])) {
    packageStartupMessage("Using existing token for authentication.")
    for (i in seq_along(getOption("wqpr.token"))) {
      wqp_use_token(getOption("wqpr.token")[i],
        names(getOption("wqpr.token"))[i], FALSE)
    }
  }
}

.onDetach = function(libname) {
  #delete authentication information
  assign("active.token", NULL, envir = authenv)
  assign("authenticated", FALSE, envir = authenv)
}
