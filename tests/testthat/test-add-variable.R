test_that("add_granularity_geo_to_data_set creates a granularity_geo column", {
  d <- data.table::data.table(
    location_code = c("norge", "county_nor03", "municip_nor0301")
  )
  add_granularity_geo_to_data_set(d)
  expect_true("granularity_geo" %in% names(d))
  expect_identical(d[["granularity_geo"]], c("nation", "county", "municip"))
})

test_that("add_iso3_to_data_set preserves an existing granularity_geo", {
  d <- data.table::data.table(
    location_code = c("norge", "county_nor03", "municip_nor0301")
  )
  add_granularity_geo_to_data_set(d)
  before <- c(d[["granularity_geo"]])
  expect_identical(before, c("nation", "county", "municip"))

  add_iso3_to_data_set(d)
  expect_identical(d[["granularity_geo"]], before)
})

test_that("add_iso3_to_data_set creates an iso3 column", {
  d <- data.table::data.table(
    location_code = c("norge", "county_nor03", "municip_nor0301")
  )
  add_iso3_to_data_set(d)
  expect_true("iso3" %in% names(d))
  expect_identical(d[["iso3"]], c("nor", "nor", "nor"))
})
