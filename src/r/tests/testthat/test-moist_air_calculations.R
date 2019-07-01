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

# Values are compared against values calculated with Excel
test_that("moist air calculations match values calculated with Excel using IP units", {
  set_unit_system("IP")
  expect_equal_rel(get_moist_air_enthalpy(86, 0.02), 42.6168, tolerance = 0.0003)
})

# Values are compared against values calculated with Excel
test_that("moist air calculations match values calculated with Excel using SI units", {
  set_unit_system("SI")
  expect_equal_rel(get_moist_air_enthalpy(30, 0.02), 81316, tolerance = 0.0003)
})
