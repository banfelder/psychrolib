#######################################################################################################
# Helper functions
#######################################################################################################

#' The system of units in use
PSYCHROLIB_UNITS <- NA_character_

#' The options for PSYCHROLIB_UNITS
PSYCHROLIB_UNITS_OPTIONS <- c("IP", "SI")

#' Tolerance of temperature calculations
PSYCHROLIB_TOLERANCE <- NA_real_

#' Approximate a freshly loaded package
#'
#' This exists only to support testing, and is not exported
init_psychrolib <- function() {
  PSYCHROLIB_UNITS <<- NA_character_
  PSYCHROLIB_UNITS_OPTIONS <<- c("IP", "SI")
  PSYCHROLIB_TOLERANCE <<- NA_real_
}

#' Set the system of units to use (SI or IP).
#'
#' @param units string indicating the system of units chosen ("SI" or "IP")
#'
#' Notes:
#' This function *HAS TO BE CALLED* before the library can be used
#' @export
set_unit_system <- function(units) {

  # Define tolerance on temperature calculations
  # The tolerance is the same in IP and SI
  PSYCHROLIB_TOLERANCES <- c(IP = 0.001 * 9. / 5., SI = 0.001)

  if (units %in% PSYCHROLIB_UNITS_OPTIONS) {
    PSYCHROLIB_UNITS <<- units
    PSYCHROLIB_TOLERANCE <<- PSYCHROLIB_TOLERANCES[units]
  } else {
    stop("The system of units has to be either SI or IP.")
  }
}

#' Return system of units in use.
#'
#' @return string indicating system of units in use ("SI" or "IP")
#' @export
get_unit_system <- function() {
  PSYCHROLIB_UNITS
}

#' Check whether the system in use is IP or SI.
#'
#' @return boolean TRUE if unit system is IP
#' @export
is_ip <- function() {
  if (is.na(PSYCHROLIB_UNITS)) {
    stop("The system of units has not been defined.")
  } else if (PSYCHROLIB_UNITS == "IP") {
    return(TRUE)
  } else if (PSYCHROLIB_UNITS == "SI") {
    return(FALSE)
  } else {
    stop("The system of units is not correctly defined.")
  }
}
