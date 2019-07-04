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
