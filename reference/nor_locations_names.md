# All names in Norway

All names in Norway

## Usage

``` r
nor_locations_names(border = csdata::config$border_nor)
```

## Source

<https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer>

## Arguments

- border:

  The year in which Norwegian geographical boundaries were designated
  (2020, 2024).

## Value

- location_code:

  Location code.

- location_name:

  Location name.

- location_name_short:

  3 letter location name for nation and county. A shorter location name
  for wardoslo and extrawardoslo.

- location_name_description_nb:

  Location name with additional description.

- location_name_file_nb_utf:

  Location name that should be used in file names, with Norwegian
  characters.

- location_name_file_nb_ascii:

  Location name that should be used in file names, without Norwegian
  characters.

- location_order:

  The preferred presentation order.

- granularity_geo:

  nation, county, municip, wardoslo, wardbergen, wardstavanger,
  wardtrondheim, baregion, lab.

## Examples

``` r
nor_locations_names()
#> Indices: <granularity_geo>, <location_code>
#>       location_code                 location_name location_name_short
#>              <char>                        <char>              <char>
#>   1:     nation_nor            Norge-Noreg-Norway                   L
#>   2: georegion_nor1      Nord-Norge-Davvi-Norggas                   N
#>   3: georegion_nor2         Trøndelag-Trööndelage                   T
#>   4: georegion_nor3                    Vestlandet                   V
#>   5: georegion_nor4                         Agder                   S
#>  ---                                                                 
#> 612:  lab_nor084467    Laboratoriet Bærum sykehus                <NA>
#> 613:  lab_nor085217         Folkehelseinstituttet                <NA>
#> 614:  lab_nor095148 Lovisenberg diakonale sykehus                <NA>
#> 615:  lab_nor107883      Diakonhjemmet Sykehus AS                <NA>
#> 616:  lab_nor000012        Helgelandssykehuset HF                <NA>
#>             location_name_description_nb         location_name_file_nb_utf
#>                                   <char>                            <char>
#>   1:                  Norge-Noreg-Norway                Norge_Noreg_Norway
#>   2: Nord-Norge-Davvi-Norggas (landsdel) Nord_Norge_Davvi_Norggas_landsdel
#>   3:    Trøndelag-Trööndelage (landsdel)    Trøndelag_Trööndelage_landsdel
#>   4:               Vestlandet (landsdel)               Vestlandet_landsdel
#>   5:                    Agder (landsdel)                    Agder_landsdel
#>  ---                                                                      
#> 612:    Laboratoriet Bærum sykehus (lab)    Laboratoriet_Bærum_sykehus_lab
#> 613:         Folkehelseinstituttet (lab)         Folkehelseinstituttet_lab
#> 614: Lovisenberg diakonale sykehus (lab) Lovisenberg_diakonale_sykehus_lab
#> 615:      Diakonhjemmet Sykehus AS (lab)      Diakonhjemmet_Sykehus_AS_lab
#> 616:        Helgelandssykehuset HF (lab)        Helgelandssykehuset_HF_lab
#>            location_name_file_nb_ascii location_order granularity_geo
#>                                 <char>          <int>          <char>
#>   1:                norge_noreg_norway              1          nation
#>   2: nord_norge_davvi_norggas_landsdel              2       georegion
#>   3:    trondelag_troondelage_landsdel              3       georegion
#>   4:               vestlandet_landsdel              4       georegion
#>   5:                    agder_landsdel              5       georegion
#>  ---                                                                 
#> 612:    laboratoriet_barum_sykehus_lab            612             lab
#> 613:         folkehelseinstituttet_lab            613             lab
#> 614: lovisenberg_diakonale_sykehus_lab            614             lab
#> 615:      diakonhjemmet_sykehus_as_lab            615             lab
#> 616:        helgelandssykehuset_hf_lab            616             lab
```
