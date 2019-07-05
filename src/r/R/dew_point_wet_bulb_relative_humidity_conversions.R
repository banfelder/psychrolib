#######################################################################################################
# Conversions between dew point, wet bulb, and relative humidity
#######################################################################################################

#' Return relative humidity given dry-bulb temperature and dew-point temperature.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param t_dew_point numeric Dew-point temperature in °F [IP] or °C [SI]
#'
#' @return numeric Relative humidity in range [0, 1]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 22
#' @export
get_rel_hum_from_t_dew_point <- function(t_dry_bulb, t_dew_point) {

  if(t_dew_point > t_dry_bulb) {
    stop("Dew point temperature is above dry bulb temperature")
  }

  vap_pres <- get_sat_vap_pres(t_dew_point)
  sat_vap_pres <- get_sat_vap_pres(t_dry_bulb)
  vap_pres / sat_vap_pres

}
