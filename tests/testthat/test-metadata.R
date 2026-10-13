test_that("metadata errors when data is not a tibble", {
  expect_error(
    metadata(c(1, 2, 3)),
    "must be a tibble"
  )
  expect_error(
    metadata(data.frame(x = 1)),
    "must be a tibble"
  )
})

test_that("metadata errors when metadata_ref is missing", {
  df <- tibble::tibble(a = 1)
  expect_error(
    metadata(df),
    "Metadata is not available for this data"
  )
})

test_that("metadata returns cached metadata if present", {
  df <- tibble::tibble(a = 1)
  meta_df <- tibble::tibble(variable = "a", description = "col a")
  df <- set_metadata(df, meta_df)

  expect_identical(metadata(df), meta_df)
})

test_that("metadata errors when ref$exists is FALSE", {
  df <- tibble::tibble(a = 1)
  df <- set_metadata_ref(
    df,
    path = "/nonexistent/path.csv",
    type = "SPD",
    version = "latest",
    exists = FALSE
  )

  expect_error(
    metadata(df),
    "SPD metadata not available"
  )
})

test_that("metadata reads file and attaches metadata when ref$exists is TRUE", {
  tmp_csv <- tempfile(fileext = ".csv")
  readr::write_csv(
    tibble::tibble(
      v = c("var1", "pc7"),
      d = c("desc1", "Postcode")
    ),
    tmp_csv
  )

  df <- tibble::tibble(a = 1)
  df <- set_metadata_ref(
    df,
    path = tmp_csv,
    type = "SPD",
    version = "2023_1",
    exists = TRUE
  )

  expect_warning(
    meta_res <- metadata(df),
    "Metadata corresponds to the latest version"
  )

  expect_s3_class(meta_res, "tbl_df")
  expect_named(meta_res, c("variable", "description"))
})

test_that("inform_metadata_access emits information message", {
  expect_message(
    inform_metadata_access(tibble::tibble()),
    "Metadata is available"
  )
})
