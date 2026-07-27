test_that("location_code_to_granularity_geo maps known location code prefixes", {
  expect_identical(location_code_to_granularity_geo("norge"), "nation")
  expect_identical(location_code_to_granularity_geo("county_nor03"), "county")
  expect_identical(
    location_code_to_granularity_geo("municip_nor0301"),
    "municip"
  )
  expect_identical(
    location_code_to_granularity_geo("wardoslo_nor030101"),
    "wardoslo"
  )
})

test_that("location_code_to_granularity_geo returns the whole string when the code is all lowercase letters", {
  expect_identical(
    location_code_to_granularity_geo("notmatching"),
    "notmatching"
  )
})

test_that("location_code_to_granularity_geo returns NA when no lowercase prefix is present", {
  expect_identical(location_code_to_granularity_geo(""), NA_character_)
  expect_identical(location_code_to_granularity_geo(NA), NA_character_)
  expect_identical(location_code_to_granularity_geo(42), NA_character_)
})

test_that("location_code_to_granularity_geo is elementwise and length preserving", {
  x <- c(
    "norge",
    "county_nor03",
    "municip_nor0301",
    "wardoslo_nor030101",
    "notmatching",
    "",
    NA
  )
  retval <- location_code_to_granularity_geo(x)
  expect_length(retval, length(x))
  expect_identical(
    retval,
    c("nation", "county", "municip", "wardoslo", "notmatching", NA, NA)
  )
})

test_that("location_code_to_granularity_geo reads the location_code column of a data.table", {
  d <- data.table::data.table(
    location_code = c(
      "norge",
      "county_nor03",
      "municip_nor0301",
      "wardoslo_nor030101"
    )
  )
  expect_identical(
    location_code_to_granularity_geo(d),
    c("nation", "county", "municip", "wardoslo")
  )
})

test_that("location_code_to_iso3 returns nor for every location code", {
  expect_identical(location_code_to_iso3("norge"), "nor")
  expect_identical(location_code_to_iso3("county_nor03"), "nor")
  expect_identical(
    location_code_to_iso3(c("norge", "county_nor03", "municip_nor0301", "xyz")),
    c("nor", "nor", "nor", "nor")
  )
})

test_that("location_code_to_iso3 returns one nor per row of a data.table", {
  d <- data.table::data.table(location_code = c("norge", "county_nor03", "xyz"))
  retval <- location_code_to_iso3(d)
  expect_length(retval, nrow(d))
  expect_identical(retval, c("nor", "nor", "nor"))
})
