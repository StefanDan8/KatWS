
<!-- README.md is generated from README.Rmd. Please edit that file -->

# katws

<!-- badges: start -->
<!-- badges: end -->

# Überblick

Zusätzlich enthält `katws` Funktionen, welche Plots erstellen:

- `BARPLOT1()` erzeugt ein klassisches Säulendiagramm.
- `BARPLOT2()` erzeugt ein Säulendiagramm mit zusätzlichen Facets.
- `PIECHART()` erzeugt ein Kreisdiagramm.
- `COUNTPLOT()` erzeugt ein Countplot mit möglichen Parametern `prop`
  oder `count`
- `HEATMAPS()` erzeugt eine Heatmap.

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

``` r
BARPLOT1(euro_startups, status, country_code)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

``` r

BARPLOT2(euro_startups, status, country_code)
```

<img src="man/figures/README-unnamed-chunk-5-2.png" width="100%" />

``` r

PIECHART(euro_startups, status)
```

<img src="man/figures/README-unnamed-chunk-5-3.png" width="100%" />

``` r

COUNTPLOT(euro_startups, status, country_code, "prop")
```

<img src="man/figures/README-unnamed-chunk-5-4.png" width="100%" />

``` r

COUNTPLOT(euro_startups, status, country_code, "count")
```

<img src="man/figures/README-unnamed-chunk-5-5.png" width="100%" />

``` r

HEATMAPS(euro_startups, status, country_code)
```

<img src="man/figures/README-unnamed-chunk-5-6.png" width="100%" />

## Induktive Analyse

Angenommen wir denken darüber nach, ein Start-up-Unternehmen zu gründen.
Dabei möchten wir anhand früherer Daten wissen, ob wir in einem
bestimmten Land bessere Erfolgschancen hätten.

Dazu testen wir die Hypothese, ob das Land und der Status von
Unternehmen nach einer bestimmten Zeit (noch in Betrieb, geschlossen
oder gekauft) abhängig sind oder nicht.

H<sub>0</sub>: `country_code` und `status` sind unabhängig  
H<sub>1</sub>: sie sind abhängig

``` r
test_independence(euro_startups, country_code, status, 1000, 1000, TRUE);
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
