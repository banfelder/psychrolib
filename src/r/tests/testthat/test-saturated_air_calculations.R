# Test saturation vapour pressure calculation
# The values are tested against the values published in Table 3 of ch. 1 of the 2017 ASHRAE Handbook - Fundamentals
# over the range [-148, +392] F
# ASHRAE's assertion is that the formula is within 300 ppm of the true values, which is true except for the value at -76 F
test_that("saturated vapor pressure calculations match ASHRAE's tabulated results for IP units", {
  set_unit_system("IP")
  expect_equal(get_sat_vap_pres(-76.0), 0.000157, tolerance = 0.00001, scale = 1)
  expect_equal(get_sat_vap_pres( -4.0), 0.014974, tolerance = 0.0003, scale = 0.014974)
  expect_equal(get_sat_vap_pres( 23.0), 0.058268, tolerance = 0.0003, scale = 0.058268)
  expect_equal(get_sat_vap_pres( 41.0), 0.12656, tolerance = 0.0003, scale = 0.12656)
  expect_equal(get_sat_vap_pres( 77.0), 0.45973, tolerance = 0.0003, scale = 0.45973)
  expect_equal(get_sat_vap_pres(122.0), 1.79140, tolerance = 0.0003, scale = 1.79140)
  expect_equal(get_sat_vap_pres(212.0), 14.7094, tolerance = 0.0003, scale = 14.7094)
  expect_equal(get_sat_vap_pres(300.0), 67.0206, tolerance = 0.0003, scale = 67.0206)
})

# Test saturation vapour pressure calculation
# The values are tested against the values published in Table 3 of ch. 1 of the 2017 ASHRAE Handbook - Fundamentals
# over the range [-100, +200] C
# ASHRAE's assertion is that the formula is within 300 ppm of the true values, which is true except for the value at -60 C
test_that("saturated vapor pressure calculations match ASHRAE's tabulated results for SI units", {
  set_unit_system("SI")
  expect_equal(get_sat_vap_pres(-60.0), 1.08, tolerance = 0.01, scale = 1)
  expect_equal(get_sat_vap_pres(-20.0), 103.24, tolerance = 0.0003, scale = 103.24)
  expect_equal(get_sat_vap_pres(-5.0), 401.74, tolerance = 0.0003, scale = 401.74)
  expect_equal(get_sat_vap_pres(5.0), 872.6, tolerance = 0.0003, scale = 872.6)
  expect_equal(get_sat_vap_pres(25.0), 3169.7, tolerance = 0.0003, scale = 3169.7)
  expect_equal(get_sat_vap_pres(50.0), 12351.3, tolerance = 0.0003, scale = 12351.3)
  expect_equal(get_sat_vap_pres(100.0), 101418.0, tolerance = 0.0003, scale = 101418.0)
  expect_equal(get_sat_vap_pres(150.0), 476101.4, tolerance = 0.0003, scale = 476101.4)
})
