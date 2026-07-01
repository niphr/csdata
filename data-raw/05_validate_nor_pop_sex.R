# Validation of the sex dimension in nor_population_by_age_b2024.
# Run after 01 + 04. Bare assertions; a regression aborts with an informative message.

library(data.table)
suppressMessages(devtools::load_all("."))

# Optional backward-compat check: point at a snapshot of the pre-change R/sysdata.rda
# (total-only). Set via env var CSDATA_BASELINE_SYSDATA; the equivalence check is skipped
# if the snapshot is unavailable.
baseline <- Sys.getenv("CSDATA_BASELINE_SYSDATA", unset = NA)
b <- NULL
if (!is.na(baseline) && file.exists(baseline)) {
  b <- new.env()
  load(baseline, envir = b)
}

e <- new.env()
load("R/sysdata.rda", envir = e)

k <- c("granularity_geo", "location_code", "age", "calyear")

for (bd in c("b2024")) {
  new <- copy(get(paste0("nor_population_by_age_", bd), envir = e))
  setDT(new)

  # -- schema --
  stopifnot(identical(sort(unique(new$sex)), c("female", "male", "total")))
  stopifnot(identical(
    sort(names(new)),
    sort(c(
      "granularity_geo",
      "location_code",
      "age",
      "sex",
      "calyear",
      "pop_jan1_n",
      "imputed"
    ))
  ))

  # -- 1. backward-compat equivalence: total slice identical to baseline --
  if (!is.null(b)) {
    old <- copy(get(paste0("nor_population_by_age_", bd), envir = b))
    setDT(old)
    nt <- new[sex == "total", c(k, "pop_jan1_n", "imputed"), with = FALSE]
    ot <- old[, c(k, "pop_jan1_n", "imputed"), with = FALSE] # baseline is total-only
    setkeyv(nt, k)
    setkeyv(ot, k)
    m <- merge(nt, ot, by = k, suffixes = c("_new", "_old"))
    stopifnot(nrow(m) == nrow(ot), nrow(nt) == nrow(ot))
    diff_pop <- m[, max(abs(pop_jan1_n_new - pop_jan1_n_old), na.rm = TRUE)]
    stopifnot(isTRUE(all.equal(m$pop_jan1_n_new, m$pop_jan1_n_old)))
    stopifnot(identical(m$imputed_new, m$imputed_old))
    cat(sprintf(
      "[%s] total-slice equivalence: %d rows, max|diff| = %g\n",
      bd,
      nrow(m),
      diff_pop
    ))
  } else {
    cat(sprintf(
      "[%s] total-slice equivalence: SKIPPED (no baseline snapshot)\n",
      bd
    ))
  }

  # -- 3. rectangularity: every cell has exactly male/female/total --
  cnt <- new[, .N, keyby = k]$N
  stopifnot(all(cnt == 3L))
  cat(sprintf(
    "[%s] rectangular: all %d cells have male/female/total\n",
    bd,
    length(cnt)
  ))
}

# -- 2. additivity (b2024 only, where real splits exist): male + female == total --
# Exact at national level (not redistricted). At redistricted sub-national levels each
# sex is rounded independently after applying fractional border weights, so male+female
# may differ from total by a few people. Assert exact for nation, small & bounded elsewhere.
new24 <- copy(get("nor_population_by_age_b2024", envir = e))
setDT(new24)
w <- dcast(
  new24,
  granularity_geo + location_code + age + calyear + imputed ~ sex,
  value.var = "pop_jan1_n"
)
real <- w[!is.na(male)]
real[, diff := total - (male + female)]
stopifnot(real[granularity_geo == "nation", max(abs(diff))] == 0)
stopifnot(real[, max(abs(diff))] <= 3)
stopifnot(real[diff != 0, .N] / nrow(real) < 0.01) # < 1% of cells affected
cat(sprintf(
  "b2024 additivity: nation exact; sub-national max|diff|=%d on %.3f%% of cells (rounding)\n",
  real[, max(abs(diff))],
  100 * real[diff != 0, .N] / nrow(real)
))
# not-mainland/missing carry NA male/female by design
cat(sprintf(
  "b2024 NA-split cells (Svalbard/JanMayen/unknown): %d\n",
  nrow(w[is.na(male)])
))
# -- 4. source cross-check: national 2026 total = sum over ages, matches SSB fetch --
nat26 <- new24[
  granularity_geo == "nation" & sex == "total" & calyear == 2026,
  sum(pop_jan1_n)
]
nat_cache <- list.files(
  "data-raw/files/population/api_responses",
  pattern = "^07459_national_bysex_.*[0-9]\\.rds$",
  full.names = TRUE
)
nat_cache <- nat_cache[!grepl("_full", nat_cache)]
ssb26 <- as.data.table(readRDS(nat_cache))
ssb26 <- ssb26[Tid == "2026", sum(value)]
stopifnot(nat26 == ssb26)
cat(sprintf("national 2026 total = %d (matches SSB fetch)\n", nat26))

# -- 5. function tests --
cats <- list("0-9" = 0:9, "10-19" = 10:19)

# 5a. nor_population_by_age_cats returns total only, unchanged schema
r_old <- nor_population_by_age_cats(cats = cats, border = 2024)
stopifnot(identical(unique(r_old$sex), "total"))
cat("nor_population_by_age_cats: total-only, OK\n")

# 5b. by_sex filtered to total reproduces nor_population_by_age_cats
r_sex <- nor_population_by_sex_age_cats(
  cats = cats,
  include_total_sex = TRUE,
  border = 2024
)
stopifnot(identical(sort(unique(r_sex$sex)), c("female", "male", "total")))
r_sex_tot <- r_sex[sex == "total"]
setkeyv(r_old, c("granularity_geo", "location_code", "age", "calyear"))
setkeyv(r_sex_tot, c("granularity_geo", "location_code", "age", "calyear"))
mm <- merge(
  r_old[, .(granularity_geo, location_code, age, calyear, a = pop_jan1_n)],
  r_sex_tot[, .(granularity_geo, location_code, age, calyear, b = pop_jan1_n)],
  by = c("granularity_geo", "location_code", "age", "calyear")
)
stopifnot(nrow(mm) == nrow(r_old), isTRUE(all.equal(mm$a, mm$b)))
cat("by_sex[total] == by_age_cats: OK\n")

# 5c. include_total_sex = FALSE drops total; include_total_age = FALSE drops age total
r_nf <- nor_population_by_sex_age_cats(
  cats = cats,
  include_total_sex = FALSE,
  border = 2024
)
stopifnot(identical(sort(unique(r_nf$sex)), c("female", "male")))
r_na <- nor_population_by_sex_age_cats(
  cats = cats,
  include_total_age = FALSE,
  border = 2024
)
stopifnot(!"total" %in% unique(r_na$age))
cat("include_total_sex / include_total_age toggles: OK\n")

# 5d. border = 2020 is no longer supported
stopifnot(inherits(try(nor_population_by_sex_age_cats(cats = cats, border = 2020), silent = TRUE), "try-error"))
stopifnot(inherits(try(nor_population_by_age_cats(cats = cats, border = 2020), silent = TRUE), "try-error"))
cat("border = 2020 rejected: OK\n")

cat("\nALL VALIDATION CHECKS PASSED\n")
