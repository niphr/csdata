# Location codes and names for Norwegian geographic units

Returns a data.table of all Norwegian geographic units with their
location codes, display names, and presentation metadata. Coverage
includes nation, counties, municipalities, city districts (Oslo, Bergen,
Stavanger, Trondheim), BA-regions, and lab regions.

## Usage

``` r
nor_locations_names(border = csdata::config$border_nor)
```

## Source

<https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer>

## Arguments

- border:

  Integer. The geographic border year determining which administrative
  boundaries are used. Valid values: `2020`, `2024`. Defaults to
  `csdata::config$border_nor`.

## Value

A data.table with one row per geographic unit and the columns:

- location_code:

  Location code (e.g. `"nation_nor"`, `"county_nor03"`).

- location_name:

  Full location name.

- location_name_short:

  Abbreviated name: 1-letter for nation and county, shorter display name
  for Oslo and Bergen city districts.

- location_name_description_nb:

  Location name with a parenthetical description of geographic level
  (Norwegian Bokmal).

- location_name_file_nb_utf:

  Name suitable for use in file names, retaining Norwegian characters.

- location_name_file_nb_ascii:

  Name suitable for use in file names, with Norwegian characters
  replaced by ASCII equivalents.

- location_order:

  Integer giving the preferred presentation order.

- granularity_geo:

  Geographic granularity: one of `"nation"`, `"georegion"`,
  `"mtregion"`, `"county"`, `"municip"`, `"baregion"`, `"wardoslo"`,
  `"wardbergen"`, `"wardstavanger"`, `"wardtrondheim"`,
  `"extrawardoslo"`, `"lab"`.

## Examples

``` r
d <- nor_locations_names()
head(d)
#>     location_code            location_name location_name_short
#>            <char>                   <char>              <char>
#> 1:     nation_nor       Norge-Noreg-Norway                   L
#> 2: georegion_nor1 Nord-Norge-Davvi-Norggas                   N
#> 3: georegion_nor2    Trøndelag-Trööndelage                   T
#> 4: georegion_nor3               Vestlandet                   V
#> 5: georegion_nor4                    Agder                   S
#> 6: georegion_nor5     Østlandet-Austlandet                   Ø
#>           location_name_description_nb         location_name_file_nb_utf
#>                                 <char>                            <char>
#> 1:                  Norge-Noreg-Norway                Norge_Noreg_Norway
#> 2: Nord-Norge-Davvi-Norggas (landsdel) Nord_Norge_Davvi_Norggas_landsdel
#> 3:    Trøndelag-Trööndelage (landsdel)    Trøndelag_Trööndelage_landsdel
#> 4:               Vestlandet (landsdel)               Vestlandet_landsdel
#> 5:                    Agder (landsdel)                    Agder_landsdel
#> 6:     Østlandet-Austlandet (landsdel)     Østlandet_Austlandet_landsdel
#>          location_name_file_nb_ascii location_order granularity_geo
#>                               <char>          <int>          <char>
#> 1:                norge_noreg_norway              1          nation
#> 2: nord_norge_davvi_norggas_landsdel              2       georegion
#> 3:    trondelag_troondelage_landsdel              3       georegion
#> 4:               vestlandet_landsdel              4       georegion
#> 5:                    agder_landsdel              5       georegion
#> 6:     ostlandet_austlandet_landsdel              6       georegion
d[granularity_geo == "county"]
#>     location_code         location_name location_name_short
#>            <char>                <char>              <char>
#>  1:  county_nor42                 Agder                 AGD
#>  2:  county_nor32              Akershus                 AKR
#>  3:  county_nor33              Buskerud                 BUS
#>  4:  county_nor56   Finnmark-Finnmárkku                 FNM
#>  5:  county_nor34   Innlandet-Sisdajven                 INN
#>  6:  county_nor15       Møre og Romsdal                 MRO
#>  7:  county_nor18   Nordland-Nordlándda                 NLD
#>  8:  county_nor03           Oslo-Oslove                 OSL
#>  9:  county_nor11              Rogaland                 ROG
#> 10:  county_nor40              Telemark                 TEL
#> 11:  county_nor55          Troms-Romssa                 TRO
#> 12:  county_nor50 Trøndelag-Trööndelage                 TRØ
#> 13:  county_nor39              Vestfold                 VFO
#> 14:  county_nor46              Vestland                 VLD
#> 15:  county_nor31               Østfold                 ØFO
#>      location_name_description_nb   location_name_file_nb_utf
#>                            <char>                      <char>
#>  1:                 Agder (fylke)                 Agder_fylke
#>  2:              Akershus (fylke)              Akershus_fylke
#>  3:              Buskerud (fylke)              Buskerud_fylke
#>  4:   Finnmark-Finnmárkku (fylke)   Finnmark_Finnmárkku_fylke
#>  5:   Innlandet-Sisdajven (fylke)   Innlandet_Sisdajven_fylke
#>  6:       Møre og Romsdal (fylke)       Møre_og_Romsdal_fylke
#>  7:   Nordland-Nordlándda (fylke)   Nordland_Nordlándda_fylke
#>  8:           Oslo-Oslove (fylke)           Oslo_Oslove_fylke
#>  9:              Rogaland (fylke)              Rogaland_fylke
#> 10:              Telemark (fylke)              Telemark_fylke
#> 11:          Troms-Romssa (fylke)          Troms_Romssa_fylke
#> 12: Trøndelag-Trööndelage (fylke) Trøndelag_Trööndelage_fylke
#> 13:              Vestfold (fylke)              Vestfold_fylke
#> 14:              Vestland (fylke)              Vestland_fylke
#> 15:               Østfold (fylke)               Østfold_fylke
#>     location_name_file_nb_ascii location_order granularity_geo
#>                          <char>          <int>          <char>
#>  1:                 agder_fylke              7          county
#>  2:              akershus_fylke              8          county
#>  3:              buskerud_fylke              9          county
#>  4:   finnmark_finnmarkku_fylke             10          county
#>  5:   innlandet_sisdajven_fylke             11          county
#>  6:       more_og_romsdal_fylke             12          county
#>  7:   nordland_nordlandda_fylke             13          county
#>  8:           oslo_oslove_fylke             14          county
#>  9:              rogaland_fylke             15          county
#> 10:              telemark_fylke             16          county
#> 11:          troms_romssa_fylke             17          county
#> 12: trondelag_troondelage_fylke             18          county
#> 13:              vestfold_fylke             19          county
#> 14:              vestland_fylke             20          county
#> 15:               ostfold_fylke             21          county
```
