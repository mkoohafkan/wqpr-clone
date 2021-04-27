skip_if_no_db <- function() {
  if (!wqp_check_connection()) {
    skip("API not available")
  }
}

all.programs = c("marsh")
all.databases = c("test", "production")
