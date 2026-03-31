# Version 2026.3.30

- Population data pipeline now fetches directly from SSB PxWeb API (tables 07459 and 10826) instead of manually downloaded CSV files.
- Including 2025 and 2026 population data.
- Municipality population data now uses actual historical data per municipality code instead of crude split approximations for merged municipalities.
- National population data sourced from table 07459 (1986+), replacing the defunct dataset 59322.
- Svalbard population data now fetched from SSB table 07430 (up to 2026), replacing the static Excel file.
- Removed unused Nordic country population and location Excel files.

# Version 2024.9.26

- location_name_file_nb_ascii in csdata::nor_location_names is now in lowercase.

# Version 2024.4.26

- Fixing an error that wouldn't allow for include_total = TRUE in nor_population_by_age_cats
- CRAN submission.

# Version 2024.4.21

- Including 2024 population data.

# Version 2024.1.17

- Fixing an error in merging for 2023->2024.
- Including population for georegion.

# Version 2023.12.26

- Improving names for georegion for border=2024.

# Version 2023.11.29

- Fixing error in redistricting of counties for border=2024.

# Version 2023.8.21

- Fixing error in the name Trøndelag-Trööndelage.

# Version 2023.7.31

- Including Sami names for counties.

# Version 2023.6.19

- Inclusion of location_name_short in nor_locations_names for border=2020.
- CRAN submission.

# Version 2023.5.26

- Inclusion of location_name_short in nor_locations_names for border=2024.

# Version 2023.5.22

- Including redistricting data for [2024](https://www.regjeringen.no/no/tema/kommuner-og-regioner/kommunestruktur/nye-kommune-og-fylkesnummer-fra-1.-januar-2024/id2924701/?expand=factbox2924711)
- Setting default for border_nor to 2024.
- CRAN submission.

# Version 2023.4.21

- Include population data for 2023.
