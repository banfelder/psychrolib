# Test of relationships between vapour pressure and relative humidity in IP units
test_that("expected relationships between vapor pressure and relative humidity hold in IP units", {
  set_unit_system("IP")
  vap_pres <- get_vap_pres_from_rel_hum(77.0, 0.8)
  expect_equal_rel(vap_pres, 0.45973 * 0.8, tolerance = 0.0003)
  rel_hum <- get_rel_hum_from_vap_pres(77.0, vap_pres)
  expect_equal_rel(rel_hum, 0.8, tolerance = 0.0003)
})

# Test of relationships between vapour pressure and relative humidity in SI units
test_that("expected relationships between vapor pressure and relative humidity hold in SI units", {
  set_unit_system("SI")
  vap_pres <- get_vap_pres_from_rel_hum(25.0, 0.8)
  expect_equal_rel(vap_pres, 3169.7 * 0.8, tolerance = 0.0003)
  rel_hum <- get_rel_hum_from_vap_pres(25.0, vap_pres)
  expect_equal_rel(rel_hum, 0.8, tolerance = 0.0003)
})

# Test of relationships between vapour pressure and dew point temperature in IP units
# No need to test vapour pressure calculation as it is just the saturation vapour pressure tested elsewhere
test_that("reciprocal relationships between vapor pressure and sew point hold in IP units", {
  set_unit_system("IP")
  vap_pres <- get_vap_pres_from_t_dew_point(-4.0)
  expect_equal_abs(get_t_dew_point_from_vap_pres(59.0, vap_pres), -4.0, tolerance = 0.001)
  vap_pres <- get_vap_pres_from_t_dew_point(41.0)
  expect_equal_abs(get_t_dew_point_from_vap_pres(59.0, vap_pres), 41.0, tolerance = 0.001)
  vap_pres <- get_vap_pres_from_t_dew_point(122.0)
  expect_equal_abs(get_t_dew_point_from_vap_pres(140.0, vap_pres), 122.0, tolerance = 0.001)
})

# Test of relationships between vapour pressure and dew point temperature in SI units
# No need to test vapour pressure calculation as it is just the saturation vapour pressure tested above
test_that("reciprocal relationships between vapor pressure and sew point hold in IP units", {
  set_unit_system("SI")
  vap_pres <- get_vap_pres_from_t_dew_point(-20.0)
  expect_equal_abs(get_t_dew_point_from_vap_pres(15.0, vap_pres), -20.0, tolerance = 0.001)
  vap_pres <- get_vap_pres_from_t_dew_point(5.0)
  expect_equal_abs(get_t_dew_point_from_vap_pres(15.0, vap_pres), 5.0, tolerance = 0.001)
  vap_pres <- get_vap_pres_from_t_dew_point(50.0)
  expect_equal_abs(get_t_dew_point_from_vap_pres(60.0, vap_pres), 50.0, tolerance = 0.001)
})

# Test that the NR in GetTDewPointFromVapPres converges in IP units.
# This test was known problem in versions of PsychroLib <= 2.0.0
# test_that("the NR in GetTDewPointFromVapPres converges in IP units", {
#
#   set_unit_system("IP")
#
#   t_dry_bulb <- seq(-148.0, 392.0, by = 1.0)
#   rel_hum <- seq(0, 1, by = 0.1)
#   pressure <- seq(8.6, 17.4, by = 1.0)
#
#   for (t in t_dry_bulb) {
#     for (rh in rel_hum) {
#       for (p in pressure) {
#         get_t_wet_bulb_from_rel_hum(t, rh, p)   # TODO: why call this, and how to assert that this converges?
#       }
#     }
#   }
# })

# Test that the NR in GetTDewPointFromVapPres converges in SI units.
# This test was known problem in versions of PsychroLib <= 2.0.0
# test_that("the NR in GetTDewPointFromVapPres converges in SI units", {
#
#   set_unit_system("SI")
#
#   t_dry_bulb <- seq(-100.0, 200.0, by = 1.0)
#   rel_hum <- seq(0, 1, by = 0.1)
#   pressure <- seq(60000.0, 120000.0, by = 1.0)
#
#   for (t in t_dry_bulb) {
#     for (rh in rel_hum) {
#       for (p in pressure) {
#         get_t_wet_bulb_from_rel_hum(t, rh, p)   # TODO: why call this, and how to assert that this converges?
#       }
#     }
#   }
# })
