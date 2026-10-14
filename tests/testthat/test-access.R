test_that("have_access returns logical", {
  res <- have_access()
  expect_type(res, "logical")
  expect_length(res, 1L)
})

test_that("check_lookups_access passes when have_access is TRUE", {
  testthat::local_mocked_bindings(have_access = function() TRUE, .package = "phslookups")
  expect_silent(check_lookups_access())
})

test_that("check_lookups_access aborts when have_access is FALSE and fail_on_no_access is TRUE", {
  testthat::local_mocked_bindings(have_access = function() FALSE, .package = "phslookups")
  expect_error(
    check_lookups_access(fail_on_no_access = TRUE),
    "don't have the appropriate file permissions"
  )
})

test_that("check_lookups_access warns when have_access is FALSE and fail_on_no_access is FALSE", {
  testthat::local_mocked_bindings(have_access = function() FALSE, .package = "phslookups")
  expect_warning(
    check_lookups_access(fail_on_no_access = FALSE),
    "don't have the appropriate file permissions"
  )
})
