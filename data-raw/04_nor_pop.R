library(data.table)
source("data-raw/ssb_api_helpers.R")
devtools::load_all(".")
#' Population in Norway (2020 borders).
#'
#' We conveniently package population data taken from Statistics Norway.
#' This data is licensed under the Norwegian License for
#' Open Government Data (NLOD) 2.0.
#'
#' This dataset contains national/county/municipality/ward (city district) level population data
#' for every age (0 to 105 years old). The national level data is from year 1846, while all the
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


nor_population_by_age <- function(
  x_year_end = 2024
  ) {

  nor_population_by_age_b0000 <- readRDS("data-raw/data-temp/nor_population_by_age_b0000.rds")

  # municip/ward (exclude national data — it's handled separately below)
  pop <- merge(
    nor_population_by_age_b0000[granularity_geo != "nation", c("location_code", "age", "calyear", "pop_jan1_n", "imputed")],
    nor_locations_redistricting(border = x_year_end),
    by.x = c("location_code", "calyear"),
    by.y = c("location_code_original", "calyear")
  )
  pop_municip <- pop[
    ,
    .(
      pop_jan1_n = round(sum(pop_jan1_n*weighting))
      ),
                             keyby = .(
                               calyear,
                               location_code = location_code_current,
                               age,
                               imputed
                             )
  ]

  # Forward-fill 10 years
  missing_years <- (max(pop_municip$calyear) + 1):(max(pop_municip$calyear) + 10)
  fill_list <- vector("list", length(missing_years))
  for (i in seq_along(missing_years)) {
    fill_list[[i]] <- pop_municip[calyear == max(calyear)]
    fill_list[[i]][, calyear := missing_years[i]]
  }
  fill_list <- rbindlist(fill_list)
  fill_list[, imputed := TRUE]
  pop_municip <- rbind(pop_municip, fill_list)


  # county ----
  pop_county <- merge(
    pop_municip,
    nor_loc_hierarchy_from_to(
      from = "municip",
      to = "county"
    ),
    by.x = "location_code",
    by.y = "from_code"
  )

  # aggregate by county
  pop_county <- pop_county[, .(
    pop_jan1_n = sum(pop_jan1_n )
  ), keyby = .(
    calyear,
    location_code = to_code,
    age,
    imputed
  )]

  # ba ----

  cat("creating population for baregion ... \n")

  pop_baregion <- merge(
    pop_municip,
    nor_loc_hierarchy_from_to(
      from = "municip",
      to = "baregion"
    ),
    by.x = "location_code",
    by.y = "from_code"
  )

  # aggregate by county
  pop_baregion <- pop_baregion[, .(
    pop_jan1_n = sum(pop_jan1_n )
  ), keyby = .(
    calyear,
    location_code = to_code,
    age,
    imputed
  )]

  # georegion ----
  cat("creating population for georegion ... \n")

  pop_georegion <- merge(
    pop_municip,
    nor_loc_hierarchy_from_to(
      from = "municip",
      to = "georegion"
    ),
    by.x = "location_code",
    by.y = "from_code"
  )

  # aggregate by georegion
  pop_georegion <- pop_georegion[, .(
    pop_jan1_n = sum(pop_jan1_n )
  ), keyby = .(
    calyear,
    location_code = to_code,
    age,
    imputed
  )]


  # norway ----
  cat("creating population for nation ... \n")

  # National data is now included in the intermediate RDS from 01_nor_pop_original.R
  # (fetched from SSB table 07459, Region = "0")
  pop_norway <- nor_population_by_age_b0000[granularity_geo == "nation", .(calyear, pop_jan1_n, age, imputed, location_code)]

  # Forward-fill to match municipality year range
  missing_years_national <- (max(pop_norway$calyear) + 1):max(pop_municip$calyear)
  if (length(missing_years_national) > 0) {
    fill_nat <- vector("list", length(missing_years_national))
    for (i in seq_along(missing_years_national)) {
      fill_nat[[i]] <- pop_norway[calyear == max(calyear)]
      fill_nat[[i]][, calyear := missing_years_national[i]]
    }
    fill_nat <- rbindlist(fill_nat)
    fill_nat[, imputed := TRUE]
    pop_norway <- rbind(pop_norway, fill_nat)
  }


  pop_all <- rbind(pop_norway, pop_county, pop_municip, pop_baregion, pop_georegion)
  pop_all[, granularity_geo := location_code_to_granularity_geo(location_code)]

  pop_all[, sex := "total"]

  cat("done \n")


  # notmainland, missing ----
  #  svalbard + jan mayen ----/
  # age: -99
  cat("creating population for notmainland and missing ... \n")

  # Fetch Svalbard settlement data from SSB table 07430 (Jan 1st = H1)
  sv_years_h1 <- paste0(1990:lubridate::year(lubridate::today()), "H1")
  pop_svalbard_raw <- fetch_ssb_table(
    "07430",
    Bosetting = c("Svalb00", "Svalb01", "Svalb02", "Svalb03"),
    ContentsCode = "Personer",
    Tid = sv_years_h1,
    cache_name = "07430_svalbard"
  )
  pop_svalbard_raw[, calyear := as.numeric(gsub("H1", "", Tid))]

  # Pivot: one row per year with settlement columns
  pop_sv <- dcast(pop_svalbard_raw, calyear ~ Bosetting, value.var = "value")
  # Svalb01 = Longyearbyen/Ny-Ålesund (mainland residents)
  # Svalb00 = Longyearbyen/Ny-Ålesund (from abroad)
  # Svalb02 = Barentsburg and Pyramiden
  # Svalb03 = Hornsund

  # county21 (Svalbard total): all 4 settlements
  pop_sv[, notmainlandcounty_nor21 := Svalb01 + Svalb00 + Svalb02 + Svalb03]
  # municip2100 (Svalbard excl. Barentsburg): Longyearbyen + Ny-Ålesund + Hornsund
  pop_sv[, notmainlandmunicip_nor2100 := Svalb01 + Svalb00 + Svalb03]
  pop_sv[, imputed := FALSE]
  pop_sv[, age := -99]

  # Forward-fill to match municipality year range
  missing_years <- (max(pop_sv$calyear) + 1):max(pop_municip$calyear)
  if (length(missing_years) > 0) {
    copied_years <- vector("list", length = length(missing_years))
    for (i in seq_along(copied_years)) {
      copied_years[[i]] <- pop_sv[calyear == max(calyear)]
      copied_years[[i]][, calyear := missing_years[i]]
    }
    copied_years <- rbindlist(copied_years)
    copied_years[, imputed := TRUE]
    pop_sv <- rbind(pop_sv, copied_years)
  }

  # jan mayen (county22) — hardcoded, no API source
  pop_jm <- data.table(
    calyear = unique(c(pop_sv$calyear, missing_years)),
    notmainlandmunicip_nor2200 = 26,
    notmainlandcounty_nor22 = 26,
    imputed = FALSE,
    age = -99
  )
  pop_jm[calyear > lubridate::year(lubridate::today()), imputed := TRUE]


  # separate county, municip
  pop_notmainlandcounty_nor21 <- pop_sv[, .(calyear, pop_jan1_n = notmainlandcounty_nor21, imputed, age)]
  pop_notmainlandcounty_nor21[, location_code := 'notmainlandcounty_nor21']
  pop_notmainlandcounty_nor21[, granularity_geo := 'notmainlandcounty']

  pop_notmainlandmunicip_nor2100 <- pop_sv[, .(calyear, pop_jan1_n = notmainlandmunicip_nor2100, imputed, age)]
  pop_notmainlandmunicip_nor2100[, location_code := 'notmainlandmunicip_nor2100']
  pop_notmainlandmunicip_nor2100[, granularity_geo := 'notmainlandmunicip']

  pop_notmainlandcounty_nor22 <- pop_jm[, .(calyear, pop_jan1_n = notmainlandcounty_nor22, imputed, age)]
  pop_notmainlandcounty_nor22[, location_code := 'notmainlandcounty_nor22']
  pop_notmainlandcounty_nor22[, granularity_geo := 'notmainlandcounty']

  pop_notmainlandmunicip_nor2200 <- pop_jm[, .(calyear, pop_jan1_n = notmainlandmunicip_nor2200, imputed, age)]
  pop_notmainlandmunicip_nor2200[, location_code := 'notmainlandmunicip_nor2200']
  pop_notmainlandmunicip_nor2200[, granularity_geo := 'notmainlandmunicip']

  # match year: from 2005 to 2022
  pop_notmainlandcounty_nor21 <- pop_notmainlandcounty_nor21[calyear>=2005]
  pop_notmainlandmunicip_nor2100 <- pop_notmainlandmunicip_nor2100[calyear>=2005]
  pop_notmainlandcounty_nor22 <- pop_notmainlandcounty_nor22[calyear>=2005]
  pop_notmainlandmunicip_nor2200 <- pop_notmainlandmunicip_nor2200[calyear>=2005]





  # ukjent ----/
  # age: -99
  # pop: NA
  pop_county_unknown <- data.table(
    calyear = unique(pop_municip$calyear),
    pop_jan1_n = NA_real_,
    location_code = 'missingcounty_nor99',
    granularity_geo = 'missingcounty',
    imputed = F,
    age = -99
  )

  pop_municip_unknown <- data.table(
    calyear = unique(pop_municip$calyear),
    pop_jan1_n = NA_real_,
    location_code = 'missingmunicip_nor9999',
    granularity_geo = 'missingmunicip',
    imputed = F,
    age = -99
    )


  # combine svalbard and unknown, set age, force imputed T for greater than this year
  pop_notmain_missing <- rbind(
    pop_notmainlandcounty_nor21,
    pop_notmainlandmunicip_nor2100,
    pop_notmainlandcounty_nor22,
    pop_notmainlandmunicip_nor2200,
    pop_county_unknown,
    pop_municip_unknown
  )
  pop_notmain_missing[, age := -99]
  pop_notmain_missing[, sex := "total"]

  cat("done \n")

  # final ----
  final_order <- c("granularity_geo", "location_code", "age", "sex", "calyear", "pop_jan1_n", "imputed")
  setcolorder(pop_all, final_order)
  setcolorder(pop_notmain_missing, final_order)

  # finally bind these two
  pop <- rbind(pop_all, pop_notmain_missing)
  setorderv(pop, final_order)
  setkeyv(pop, final_order)


  # pop[, calyear := year]
  # pop[, pop_jan1 := pop]


  return(pop)
}


env = new.env()
load("R/sysdata.rda", envir = env)

env$nor_population_by_age_b2024 <- nor_population_by_age(2024)

for(i in names(env)){
  .GlobalEnv[[i]] <- env[[i]]
}
txt <- paste0("usethis::use_data(",paste0(names(env),collapse=","),", overwrite = TRUE, internal = TRUE, compress = 'xz', version = 3)")
eval(parse(text = txt))





