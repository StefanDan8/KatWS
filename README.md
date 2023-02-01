
<!-- README.md is generated from README.Rmd. Please edit that file -->

# katws

<!-- badges: start -->
<!-- badges: end -->

# Überblick

`katws` ist ein Paket, das Funktionen zur explorativen Analyse von
(kategorischen) Daten und zum Testen von Hypothesen (Unabhängigkeit
Test) bereitstellt. Diese Funktionen können an zwei im Paket enthaltenen
Datensätzen getested werden:

- `accidents21` – Daten über die Autounfälle in Großbritannien im Jahr
  2021
- `euro_startups` – Daten über die Start-ups in den Top 8 europäischen
  Ländern nach Anzahl der Startups.

Zusätzlich enthält `katws` Funktionen, welche Plots erstellen:

- `BARPLOT1()` erzeugt ein klassisches Säulendiagramm.
- `BARPLOT2()` erzeugt ein Säulendiagramm mit zusätzlichen Facets.
- `PIECHART()` erzeugt ein Kreisdiagramm.
- `COUNTPLOT()` erzeugt ein Countplot mit möglichen Parametern `prop`
  oder `count`
- `HEATMAPS()` erzeugt eine Heatmap.

Um die Funktionalität des Pakets zu veranschaulichen, werden wir den
Datensatz `euro_startups` analysieren.

### `euro_startups`

Die Daten stammen aus einem größeren Datensatz, *Startup Success/Fail
Dataset from Crunchbase*, zu finden auf *kaggle* unter folgendem Link:
<https://www.kaggle.com/datasets/yanmaksi/big-startup-secsees-fail-dataset-from-crunchbase>
Der Datensatz wurde über den folgenden Code generiert:

``` r
startup <- read_csv("data-raw/startup.csv");
startup <- dplyr::select(startup, c("category_list", "status", "country_code"));
euro_startups <- dplyr::filter(startup, !status == "ipo" &
   country_code %in% c("AUT", "BEL", "CHE", "DEU", "FRA", "GBR", "IRL", "SWE") &
   !is.na(category_list));
euro_startups[,2:3] = lapply(euro_startups[, 2:3], as.factor);
```

Bevor wir mit unserer Analyse beginnen, lassen Sie uns sehen, wie wir
unser Paket installieren können.

## Installation

Die neuste Version von `katws` kann wie folgt installiert werden:

``` r
remotes::install_gitlab(repo="ge47zot/katws", 
                        auth_token = "glpat-aEDEgsXeapej98uEMQmz",
                        host = "gitlab.lrz.de")
```

## Explorative Analyse

Jetzt sind wir bereit, die Daten zu untersuchen.

``` r
library(katws)
```

Werfen wir einen ersten Blick auf den Datensatz.

``` r
euro_startups
#> # A tibble: 7,167 × 3
#>    category_list                                       status    country_code
#>    <chr>                                               <fct>     <fct>       
#>  1 Education                                           operating GBR         
#>  2 Local Businesses|Restaurants                        operating FRA         
#>  3 Cosmetics|Marketplaces                              operating FRA         
#>  4 Health and Wellness                                 operating GBR         
#>  5 Local Search|Software|Web Design                    acquired  AUT         
#>  6 Software                                            operating GBR         
#>  7 Publishing|Services                                 operating FRA         
#>  8 Augmented Reality|Computer Vision|Games|iPad|Mobile acquired  SWE         
#>  9 Fitness                                             operating GBR         
#> 10 Games                                               operating FRA         
#> # … with 7,157 more rows
```

`status` und `country_code` sind kategoriale Variablen (Factor).
`category_list`, das die Bereiche darstellt, in denen die Start-ups
tätig sind, kann ebenfalls als kategorisch angesehen werden, obwohl, da
viele nicht nur in einem Bereich tätig sind, es als `chr` gelassen wird,
um Anfragen wie die folgenden einfacher zu verwenden:

``` r
euro_startups %>% dplyr::filter(stringr::str_detect(.data$category_list, "Software"))
#> # A tibble: 1,516 × 3
#>    category_list                                                  status count…¹
#>    <chr>                                                          <fct>  <fct>  
#>  1 Local Search|Software|Web Design                               acqui… AUT    
#>  2 Software                                                       opera… GBR    
#>  3 Software                                                       opera… SWE    
#>  4 Hardware + Software                                            opera… FRA    
#>  5 Business Analytics|E-Commerce|Enterprise Software|Visualizati… opera… IRL    
#>  6 Software                                                       closed IRL    
#>  7 Advertising|Software                                           opera… GBR    
#>  8 3D Technology|Augmented Reality|Computers|Computer Vision|Ent… opera… DEU    
#>  9 Mobile|Software|Web Design|Web Development                     opera… GBR    
#> 10 Accounting|Business Development|Finance|Software|Web Developm… acqui… GBR    
#> # … with 1,506 more rows, and abbreviated variable name ¹​country_code
```

Im Folgenden werden wir uns mehr für die Spalten `status` und
`country_code` interessieren.

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

Wie wir sehen, gibt es weitaus mehr Beobachtungen von Start-ups in
Großbritannien, Frankreich und Deutschland als in den anderen fünf
Ländern. Daher könnte es hilfreich sein, die Prozentsätze jedes Status
in jedem Land zu sehen.

Dafür verwenden wir die `contingency_table_scale` Funktion.

``` r
contingency_table_scale(euro_startups, country_code, status)
#> # A tibble: 8 × 4
#>   country_code acquired closed operating
#>   <fct>           <dbl>  <dbl>     <dbl>
#> 1 AUT            0.0630 0.102      0.835
#> 2 BEL            0.0761 0.0609     0.863
#> 3 CHE            0.0637 0.0669     0.869
#> 4 DEU            0.0862 0.0676     0.846
#> 5 FRA            0.0642 0.0798     0.856
#> 6 GBR            0.0622 0.0751     0.863
#> 7 IRL            0.0671 0.0648     0.868
#> 8 SWE            0.0668 0.100      0.833
```

Bemerkung: In einigen Fällen könnte man Prozentsätze relativ zu allen
beobachteten Daten wünschen. Daher kann man in `contingency_table_scale`
den Parameter `all` auf true setzen:

``` r
contingency_table_scale(euro_startups, country_code, status, all = TRUE)
```

Das gibt uns jetzt jedoch keine neuen Erkenntnisse.

Aus der Tabelle können wir ersehen, dass es eine sehr kleine Varianz in
der `operating` Spalte und eine relativ kleine Varianz in der `acquired`
Spalte. In Österreich und Schweden scheint der Anteil geschlossener
Start-ups jedoch besonders hocher zu sein. Daher werfen wir die Frage
auf: Hängen die Chancen der Start-ups vom Land ab? Ob die beiden
Merkmale `country_code` und `status` voneinander abhängig sind, werden
wir später testen.

### Graphische Zusammenfassung:

``` r
BARPLOT1(euro_startups, status, country_code)
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r

BARPLOT2(euro_startups, status, country_code)
```

<img src="man/figures/README-unnamed-chunk-10-2.png" width="100%" />

``` r

PIECHART(euro_startups, status)
```

<img src="man/figures/README-unnamed-chunk-10-3.png" width="100%" />

``` r

COUNTPLOT(euro_startups, status, country_code, "prop")
```

<img src="man/figures/README-unnamed-chunk-10-4.png" width="100%" />

``` r

COUNTPLOT(euro_startups, status, country_code, "count")
```

<img src="man/figures/README-unnamed-chunk-10-5.png" width="100%" />

``` r

HEATMAPS(euro_startups, status, country_code)
```

<img src="man/figures/README-unnamed-chunk-10-6.png" width="100%" />

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

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />
