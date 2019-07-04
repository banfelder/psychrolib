#######################################################################################################
# Conversions between dew point, or relative humidity and vapor pressure
#######################################################################################################

#' Return partial pressure of water vapor as a function of relative humidity and temperature.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param rel_hum numeric Relative humidity in range [0, 1]
#'
#' @return numeric Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 12, 22
#' @export
get_vap_pres_from_rel_hum <- function(t_dry_bulb, rel_hum) {

  if (rel_hum < 0.0 || rel_hum > 1.0) {
    stop("Relative humidity is outside range [0, 1]")
  }

  rel_hum * get_sat_vap_pres(t_dry_bulb)
}

#' Return relative humidity given dry-bulb temperature and vapor pressure.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param vap_pres numeric Partial pressure of water vapor in moist air in Psi [IP] or Pa [SI]
#'
#' @return numeric Relative humidity in range [0, 1]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 12, 22
#' @export
get_rel_hum_from_vap_pres <- function(t_dry_bulb, vap_pres) {
  if (vap_pres < 0.0) {
    stop("Partial pressure of water vapor in moist air cannot be negative")
  }
  vap_pres / get_sat_vap_pres(t_dry_bulb)
}
