#######################################################################################################
# Conversions between dew point, wet bulb, and relative humidity
#######################################################################################################

#' Return wet-bulb temperature given dry-bulb temperature, dew-point temperature, and pressure.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param t_dew_point numeric Dew-point temperature in °F [IP] or °C [SI]
#' @param pressure numeric Atmospheric pressure in Psi [IP] or Pa [SI]
#'
#' @return numeric Wet-bulb temperature in °F [IP] or °C [SI]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1
#' @export
get_t_wet_bulb_from_t_dew_point <- function(t_dry_bulb, t_dew_point, pressure) {

  if(t_dew_point > t_dry_bulb) {
    stop("Dew point temperature is above dry bulb temperature")
  }

  hum_ratio <- get_hum_ratio_from_t_dew_point(t_dew_point, pressure)
  get_t_wet_bulb_from_hum_ratio(t_dry_bulb, hum_ratio, pressure)
}

#' Return wet-bulb temperature given dry-bulb temperature, relative humidity, and pressure.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param rel_hum numeric Relative humidity in range [0, 1]
#' @param pressure numeric Atmospheric pressure in Psi [IP] or Pa [SI]
#'
#' @return numeric Wet-bulb temperature in °F [IP] or °C [SI]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1
#' @export
get_t_wet_bulb_from_rel_hum <- function(t_dry_bulb, rel_hum, pressure) {

  if (rel_hum < 0.0 || rel_hum > 1.0) {
    stop("Relative humidity is outside range [0, 1]")
  }

  hum_ratio <- get_hum_ratio_from_rel_hum(t_dry_bulb, rel_hum, pressure)
  get_t_wet_bulb_from_hum_ratio(t_dry_bulb, hum_ratio, pressure)

}

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

#' Return dew-point temperature given dry-bulb temperature and relative humidity.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param rel_hum numeric Relative humidity in range [0, 1]
#'
#' @return numeric Dew-point temperature in °F [IP] or °C [SI]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1
#' @export
get_t_dew_point_from_rel_hum <- function(t_dry_bulb, rel_hum) {

  if(rel_hum < 0.0 || rel_hum > 1.0) {
    stop("Relative humidity is outside range [0, 1]")
  }

  vap_pres <- get_vap_pres_from_rel_hum(t_dry_bulb, rel_hum)
  get_t_dew_point_from_vap_pres(t_dry_bulb, vap_pres)
}
