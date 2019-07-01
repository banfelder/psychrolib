#######################################################################################################
# Moist Air Calculations
#######################################################################################################

#' Return moist air enthalpy given dry-bulb temperature and humidity ratio.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param hum_ratio numeric Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
#'
#' @return numeric Moist air enthalpy in Btu lb⁻¹ [IP] or J kg⁻¹
#'
#' Reference:
#'  ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 30
#' @export
get_moist_air_enthalpy <- function(t_dry_bulb, hum_ratio) {

  if(hum_ratio < 0.0) {
    stop("Humidity ratio is negative")
  }
  bounded_hum_ratio <- max(hum_ratio, MIN_HUM_RATIO)

  if(is_ip()) {
    moist_air_enthalpy <- 0.240 * t_dry_bulb + bounded_hum_ratio * (1061 + 0.444 * t_dry_bulb)
  } else {
    moist_air_enthalpy <- (1.006 * t_dry_bulb + bounded_hum_ratio * (2501. + 1.86 * t_dry_bulb)) * 1000
  }

  return(moist_air_enthalpy)
}
