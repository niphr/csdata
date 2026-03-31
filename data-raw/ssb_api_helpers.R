# Shared helper for fetching SSB tables via PxWebApiData with caching and query logging.
# Sourced by 01_nor_pop_original.R and 04_nor_pop.R.

fetch_ssb_table <- function(table_id, ..., cache_name, force_refresh = FALSE) {
  query_dir <- "data-raw/files/population/api_queries"
  response_dir <- "data-raw/files/population/api_responses"
  dir.create(query_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(response_dir, showWarnings = FALSE, recursive = TRUE)

  cache_file <- file.path(response_dir, paste0(cache_name, ".rds"))
  raw_cache_file <- file.path(response_dir, paste0(cache_name, "_full.rds"))

  if (!force_refresh && file.exists(cache_file)) {
    cat("  Using cached data:", cache_name, "\n")
    return(data.table::as.data.table(readRDS(cache_file)))
  }

  # Save the query for reproducibility
  query <- PxWebApiData::ApiData(
    paste0("https://data.ssb.no/api/v0/en/table/", table_id),
    ...,
    returnApiQuery = TRUE
  )
  saveRDS(query, file.path(query_dir, paste0(cache_name, "_query.rds")))

  # Fetch data
  cat("  Fetching from SSB table", table_id, "(", cache_name, ") ...\n")
  result <- PxWebApiData::ApiData(
    paste0("https://data.ssb.no/api/v0/en/table/", table_id),
    ...
  )

  # Save full response (both text and code versions) and the code-based version separately
  saveRDS(result, raw_cache_file)
  saveRDS(result[[2]], cache_file)

  data.table::as.data.table(result[[2]])
}
