
<!-- README.md is generated from README.Rmd. Please edit that file -->

# katws

<!-- badges: start -->
<!-- badges: end -->

# Überblick

## Installation

Sie können die Entwicklungsversion von katws wie folgt installieren:

``` r
remotes::install_gitlab(repo="ge47zot/katws", 
                        auth_token = "glpat-aEDEgsXeapej98uEMQmz",
                        host = "gitlab.lrz.de")
```

## Explorative Analyse

``` r
library(katws)
```

### Numerische Zusammenfassung:

``` r
contingency_table(euro_startups, country_code, status)
#> # A tibble: 8 × 4
#>   country_code acquired closed operating
#>   <fct>           <int>  <int>     <int>
#> 1 AUT                 8     13       106
#> 2 BEL                15     12       170
#> 3 CHE                20     21       273
#> 4 DEU                88     69       864
#> 5 FRA                70     87       933
#> 6 GBR               222    268      3077
#> 7 IRL                29     28       375
#> 8 SWE                28     42       349
```

### Graphische Zusammenfassung:

## Induktive Analyse

Nehmen wir zum Beispiel an, wir denken darüber nach, ein
Start-up-Unternehmen zu eröffnen, und möchten anhand früherer Daten
wissen, ob wir in einem bestimmten Land bessere Erfolgschancen hätten.

Dazu testen wir die Hypothese, ob das Land und der Status von
Unternehmen nach einer bestimmten Zeit (noch in Betrieb, geschlossen
oder gekauft) abhängig sind oder nicht.

H<sub>0</sub>: `country_code` und `status` sind unabhängig  
H<sub>1</sub>: sie sind abhängig

``` r
test_independence(euro_startups, country_code, status, 1000, 1000, TRUE);
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />
