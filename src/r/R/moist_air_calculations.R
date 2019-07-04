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

#' Return moist air specific volume given dry-bulb temperature, humidity ratio, and pressure.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param hum_ratio numeric Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
#' @param pressure Atmospheric pressure in Psi [IP] or Pa [SI]
#'
#' @return numeric Specific volume of moist air in ft³ lb⁻¹ of dry air [IP] or in m³ kg⁻¹ of dry air [SI]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 26
#'
#' Notes:
#'   In IP units, R_DA_IP / 144 equals 0.370486 which is the coefficient appearing in eqn 26
#'   The factor 144 is for the conversion of Psi = lb in⁻² to lb ft⁻².
#' @export
get_moist_air_volume <- function(t_dry_bulb, hum_ratio, pressure) {

    if (hum_ratio < 0.0) {
    stop("Humidity ratio is negative")
  }
  bounded_hum_ratio <- max(hum_ratio, MIN_HUM_RATIO)

  if(is_ip()) {
    moist_air_volume <- R_DA_IP * get_t_rankine_from_t_fahrenheit(t_dry_bulb) * (1 + 1.607858 * bounded_hum_ratio) / (144 * pressure)
  } else {
    moist_air_volume <- R_DA_SI * get_t_kelvin_from_t_celsius(t_dry_bulb) * (1 + 1.607858 * bounded_hum_ratio) / pressure
  }
  return(moist_air_volume)
}

#' Return moist air density given humidity ratio, dry bulb temperature, and pressure.
#'
#' @param t_dry_bulb numeric Dry-bulb temperature in °F [IP] or °C [SI]
#' @param hum_ratio numeric Humidity ratio in lb_H₂O lb_Air⁻¹ [IP] or kg_H₂O kg_Air⁻¹ [SI]
#' @param pressure numeric Atmospheric pressure in Psi [IP] or Pa [SI]
#'
#' @return numeric MoistAirDensity: Moist air density in lb ft⁻³ [IP] or kg m⁻³ [SI]
#'
#' Reference:
#'   ASHRAE Handbook - Fundamentals (2017) ch. 1 eqn 11
#' @export
get_moist_air_density <- function(t_dry_bulb, hum_ratio, pressure) {

    if(hum_ratio < 0.0) {
    stop("Humidity ratio is negative")
  }
  bounded_hum_ratio <- max(hum_ratio, MIN_HUM_RATIO)

  moist_air_volume <- get_moist_air_volume(t_dry_bulb, bounded_hum_ratio, pressure)
  (1 + bounded_hum_ratio) / moist_air_volume
}
