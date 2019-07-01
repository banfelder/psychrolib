# Behave like pytest.approx when used with rel option
# TODO: confirm this works as we think it does.
expect_equal_rel <- function(object, expected, tolerance) {
  expect_equal(object, expected, tolerance, scale = expected)
}

# Behave like pytest.approx when used with abs option
# TODO: confirm this works as we think it does.
expect_equal_abs <- function(object, expected, tolerance) {
  expect_equal(object, expected, tolerance, scale = 1.0)
}

# Test saturation vapour pressure calculation
# The values are tested against the values published in Table 3 of ch. 1 of the 2017 ASHRAE Handbook - Fundamentals
# over the range [-148, +392] F
# ASHRAE's assertion is that the formula is within 300 ppm of the true values, which is true except for the value at -76 F
test_that("saturated vapor pressure calculations match ASHRAE's tabulated results for IP units", {
  set_unit_system("IP")
  expect_equal_abs(get_sat_vap_pres(-76.0), 0.000157, tolerance = 0.00001)
  expect_equal_rel(get_sat_vap_pres( -4.0), 0.014974, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres( 23.0), 0.058268, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres( 41.0), 0.12656, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres( 77.0), 0.45973, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(122.0), 1.79140, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(212.0), 14.7094, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(300.0), 67.0206, tolerance = 0.0003)
})

# Test saturation vapour pressure calculation
# The values are tested against the values published in Table 3 of ch. 1 of the 2017 ASHRAE Handbook - Fundamentals
# over the range [-100, +200] C
# ASHRAE's assertion is that the formula is within 300 ppm of the true values, which is true except for the value at -60 C
test_that("saturated vapor pressure calculations match ASHRAE's tabulated results for SI units", {
  set_unit_system("SI")
  expect_equal_abs(get_sat_vap_pres(-60.0), 1.08, tolerance = 0.01)
  expect_equal_rel(get_sat_vap_pres(-20.0), 103.24, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(-5.0), 401.74, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(5.0), 872.6, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(25.0), 3169.7, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(50.0), 12351.3, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(100.0), 101418.0, tolerance = 0.0003)
  expect_equal_rel(get_sat_vap_pres(150.0), 476101.4, tolerance = 0.0003)
})

# Test saturation humidity ratio
# The values are tested against those published in Table 2 of ch. 1 of the 2017 ASHRAE Handbook - Fundamentals
# Agreement is not terrific - up to 2% difference with the values published in the table
test_that("saturated humidity ratio calculations match ASHRAE's tabulated results for IP units", {
  set_unit_system("IP")
  expect_equal_rel(get_sat_hum_ratio(-58.0, 14.696), 0.0000243, tolerance = 0.01)
  expect_equal_rel(get_sat_hum_ratio(-4.0, 14.696), 0.0006373, tolerance = 0.01)
  expect_equal_rel(get_sat_hum_ratio(23.0, 14.696), 0.0024863, tolerance = 0.005)
  expect_equal_rel(get_sat_hum_ratio(41.0, 14.696), 0.005425, tolerance = 0.005)
  expect_equal_rel(get_sat_hum_ratio(77.0, 14.696), 0.020173, tolerance = 0.005)
  expect_equal_rel(get_sat_hum_ratio(122.0, 14.696), 0.086863, tolerance = 0.01)
  expect_equal_rel(get_sat_hum_ratio(185.0, 14.696), 0.838105, tolerance = 0.02)
})

# Test saturation humidity ratio
# The values are tested against those published in Table 2 of ch. 1 of the 2017 ASHRAE Handbook - Fundamentals
# Agreement is not terrific - up to 2% difference with the values published in the table
test_that("saturated humidity ratio calculations match ASHRAE's tabulated results for SI units", {
  set_unit_system("SI")
  expect_equal_rel(get_sat_hum_ratio(-50.0, 101325), 0.0000243, tolerance = 0.01)
  expect_equal_rel(get_sat_hum_ratio(-20.0, 101325), 0.0006373, tolerance = 0.01)
  expect_equal_rel(get_sat_hum_ratio(-5.0, 101325), 0.0024863, tolerance = 0.005)
  expect_equal_rel(get_sat_hum_ratio(5.0, 101325), 0.005425, tolerance = 0.005)
  expect_equal_rel(get_sat_hum_ratio(25, 101325), 0.020173, tolerance = 0.005)
  expect_equal_rel(get_sat_hum_ratio(50.0, 101325), 0.086863, tolerance = 0.01)
  expect_equal_rel(get_sat_hum_ratio(85.0, 101325), 0.838105, tolerance = 0.02)
})
