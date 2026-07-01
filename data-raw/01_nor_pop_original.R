library(data.table)
source("data-raw/ssb_api_helpers.R")
#' Population in Norway (2020 borders).
#'
#' We conveniently package population data taken from Statistics Norway.
#' This data is licensed under the Norwegian License for
#' Open Government Data (NLOD) 2.0.
#'
#' This dataset contains national/county/municipality/ward (city district) level population data
#' for every age (0 to 105 years old). The national level data is from year 1986, while all the
#' other levels have data from 2005.
#'
#' The counties and municipalities are updated for the 2020 borders.
#'
#' @format
#' \describe{
#' \item{year}{Year.}
#' \item{location_code}{The location code.}
#' \item{granularity_geo}{National/County/Municipality/BAregion.}
#' \item{age}{1 year ages from 0 to 105.}
#' \item{pop_jan1}{Number of people as of 1st of January.}
#' \item{imputed}{FALSE if real data. TRUE if it is the last real data point carried forward.}
#' }
#' @source \url{https://www.ssb.no/en/statbank/table/07459/tableViewLayout1/}
"norway_population_by_age_b2020"



nor_population_by_age_original <- function(force_refresh = FALSE) {

  # variables used in data.table functions
  . <- NULL
  value <- NULL
  age <- NULL
  sex <- NULL
  Kjonn <- NULL
  municip_code <- NULL
  ward_code <- NULL
  ward_prefix <- NULL
  imputed <- NULL
  population <- NULL

  # ---- SSB table metadata ----
  cat("Fetching SSB table metadata...\n")
  meta_07459 <- PxWebApiData::ApiData(
    "https://data.ssb.no/api/v0/en/table/07459",
    returnMetaFrames = TRUE
  )
  municip_codes <- meta_07459$Region$values[nchar(meta_07459$Region$values) == 4]
  all_years_07459 <- meta_07459$Tid$values

  meta_10826 <- PxWebApiData::ApiData(
    "https://data.ssb.no/api/v0/en/table/10826",
    returnMetaFrames = TRUE
  )
  all_ward_codes <- meta_10826$Region$values
  all_years_10826 <- meta_10826$Tid$values

  # ---- Municipality data from table 07459 ----
  cat("Fetching municipality population from SSB table 07459...\n")

  # Fetch one year at a time to stay well under SSB's cell limit
  municip_years <- all_years_07459[all_years_07459 >= "2005"]

  pop_municip_list <- vector("list", length(municip_years))
  for (i in seq_along(municip_years)) {
    pop_municip_list[[i]] <- fetch_ssb_table(
      "07459",
      Region = municip_codes,
      Kjonn = TRUE,
      Alder = TRUE,
      ContentsCode = "Personer1",
      Tid = municip_years[i],
      cache_name = paste0("07459_municip_bysex_", municip_years[i]),
      force_refresh = force_refresh
    )
  }
  pop_municip_raw <- rbindlist(pop_municip_list)

  # Transform to match expected format (Kjonn: 1 = Males, 2 = Females)
  pop_municip_raw[, municip_code := paste0("municip_nor", Region)]
  pop_municip_raw[, sex := fcase(as.character(Kjonn) == "1", "male", as.character(Kjonn) == "2", "female")]
  pop_municip_raw[, age := as.numeric(gsub("\\+", "", Alder))]
  pop_municip_raw[, calyear := as.numeric(Tid)]
  setnames(pop_municip_raw, "value", "population")

  pop_municip <- pop_municip_raw[, .(
    population = sum(population)
  ), keyby = .(
    municip_code, age, sex, calyear
  )]

  # Remove municipality-year combinations where total population is 0
  # (historical municipalities that don't exist in that year)
  pop_municip_totals <- pop_municip[, .(total = sum(population)), by = .(municip_code, calyear)]
  pop_municip <- pop_municip[pop_municip_totals[total > 0], on = .(municip_code, calyear)]
  pop_municip[, total := NULL]

  # ---- Ward data from table 10826 ----
  cat("Fetching ward population from SSB table 10826...\n")

  pop_ward_raw <- fetch_ssb_table(
    "10826",
    Region = all_ward_codes,
    Kjonn = TRUE,
    Alder = TRUE,
    ContentsCode = "Personer",
    Tid = all_years_10826,
    cache_name = paste0("10826_ward_bysex_", min(all_years_10826), "_", max(all_years_10826)),
    force_refresh = force_refresh
  )
  pop_ward_raw[, sex := fcase(as.character(Kjonn) == "1", "male", as.character(Kjonn) == "2", "female")]

  # Strip letter suffixes to get numeric ward codes
  pop_ward_raw[, ward_numeric := gsub("[a-zA-Z]", "", Region)]
  pop_ward_raw[, municip_code := paste0("municip_nor", substr(ward_numeric, 1, 4))]

  # Assign ward prefixes based on municipality
  pop_ward_raw[, ward_prefix := ""]
  pop_ward_raw[municip_code %in% c("municip_nor0301"), ward_prefix := "wardoslo_nor"]
  pop_ward_raw[municip_code %in% c("municip_nor1201", "municip_nor4601"), ward_prefix := "wardbergen_nor"]
  pop_ward_raw[municip_code %in% c("municip_nor1103"), ward_prefix := "wardstavanger_nor"]
  pop_ward_raw[municip_code %in% c("municip_nor1601", "municip_nor5001"), ward_prefix := "wardtrondheim_nor"]

  pop_ward_raw[, ward_code := paste0(ward_prefix, ward_numeric)]

  # Extra wards in Oslo (Sentrum, Marka)
  pop_ward_raw[ward_code == "wardoslo_nor030116", ward_code := "extrawardoslo_nor030116"]
  pop_ward_raw[ward_code == "wardoslo_nor030117", ward_code := "extrawardoslo_nor030117"]

  # Drop rows with no ward prefix (shouldn't happen, but be safe)
  pop_ward_raw <- pop_ward_raw[ward_prefix != ""]

  pop_ward_raw[, age := as.numeric(gsub("\\+", "", Alder))]
  pop_ward_raw[, calyear := as.numeric(Tid)]
  setnames(pop_ward_raw, "value", "population")

  # Use ward_code as municip_code for consistency with downstream
  pop_ward <- pop_ward_raw[, .(
    population = sum(population)
  ), keyby = .(
    municip_code = ward_code, age, sex, calyear
  )]

  # Combine municipality and ward data
  pop_municip <- rbind(pop_municip, pop_ward)

  # NOTE: The old CSV-based pipeline had "Fixing broken parts" code that split
  # merged municipality populations (0710, 1756, 5046, 1505) back into their
  # constituent parts using crude division (e.g., /3, /2). This was necessary
  # because the CSVs only had the merged code for all years.
  #
  # The SSB API already provides correct data for each historical municipality
  # code (e.g., 0706 = old Sandefjord pre-2016, 0710 = merged Sandefjord
  # 2017-2019, 3804 = renumbered Sandefjord 2020+). No split fixes needed.


  pop_municip[, imputed := FALSE]

  setnames(pop_municip, "municip_code", "location_code")
  setnames(pop_municip, "population", "pop_jan1_n")

  pop_municip[, granularity_geo := stringr::str_extract(location_code, "^[a-z]+")]

  # ---- National data from table 07459 (Region = "0") ----
  cat("Fetching national population from SSB table 07459...\n")

  pop_national_raw <- fetch_ssb_table(
    "07459",
    Region = "0",
    Kjonn = TRUE,
    Alder = TRUE,
    ContentsCode = "Personer1",
    Tid = all_years_07459,
    cache_name = paste0("07459_national_bysex_", min(all_years_07459), "_", max(all_years_07459)),
    force_refresh = force_refresh
  )

  pop_national <- pop_national_raw[, .(
    location_code = "nation_nor",
    age = as.numeric(gsub("\\+", "", Alder)),
    sex = fcase(as.character(Kjonn) == "1", "male", as.character(Kjonn) == "2", "female"),
    calyear = as.numeric(Tid),
    pop_jan1_n = value,
    imputed = FALSE,
    granularity_geo = "nation"
  )]

  # Combine all (each row is male/female); build sex == "total" as their sum
  pop_all <- rbind(pop_municip, pop_national)

  pop_total <- pop_all[, .(
    pop_jan1_n = sum(pop_jan1_n),
    sex = "total"
  ), keyby = .(
    location_code, age, calyear, imputed, granularity_geo
  )]

  pop_all <- rbindlist(list(pop_all, pop_total), use.names = TRUE)

  return(pop_all)
}

nor_population_by_age_b0000 <- nor_population_by_age_original()
saveRDS(nor_population_by_age_b0000, "data-raw/data-temp/nor_population_by_age_b0000.rds")
