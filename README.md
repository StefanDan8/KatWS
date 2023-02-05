
<!-- README.md is generated from README.Rmd. Please edit that file -->

# katws

<!-- badges: start -->
<!-- badges: end -->

# Überblick

`katws` ist ein Paket, das Funktionen zur explorativen Analyse von
(kategorischen) Daten und zum Testen von Hypothesen (Unabhängigkeit
Test) bereitstellt. Diese Funktionen können an zwei im Paket enthaltenen
Datensätzen getestet werden:

- `accidents21` – Daten über die Autounfälle in Großbritannien im Jahr
  2021
- `euro_startups` – Daten über die Start-ups in den Top 8 europäischen
  Ländern nach Anzahl der Startups.

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

Bevor wir mit unserer Analyse beginnen, lasst uns sehen, wie wir unser
Paket installieren können.

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
beobachteten Daten wünschen. Daher kann man in den Parameter `all` auf
true setzen:

``` r
contingency_table_scale(euro_startups, country_code, status, all = TRUE)
```

Das gibt uns jetzt jedoch keine neuen Erkenntnisse.

Aus der Tabelle können wir ersehen, dass es eine sehr kleine Varianz in
der `operating` Spalte und eine relativ kleine Varianz in der `acquired`
Spalte gibt. In Österreich und Schweden scheint der Anteil geschlossener
Start-ups jedoch besonders höher zu sein. Daher werfen wir die Frage
auf: Hängen die Chancen der Start-ups vom Land ab? Ob die beiden
Merkmale `country_code` und `status` voneinander abhängig sind, werden
wir später testen.

### Graphische Zusammenfassung:

`katws` enthält die folgenden Funktionen, welche Plots erstellen:

- `BARPLOT1()` erzeugt ein klassisches Säulendiagramm.
- `BARPLOT2()` erzeugt ein Säulendiagramm mit zusätzlichen Facets.
- `PIECHART()` erzeugt ein Kreisdiagramm.
- `COUNTPLOT()` erzeugt ein Countplot mit möglichen Parametern `prop`
  oder `count`
- `HEATMAPS()` erzeugt eine Heatmap.

``` r
BARPLOT1(euro_startups, status, country_code)
```

<img src="man/figures/README-barplot_simple-1.png" width="100%" />

Dieses Säulendiagramm zeigt uns die Anzahl aller erworbenen
(`acquired`), aller geschlossenen (`closed`) sowie aller laufenden
(`operating`) Start-ups verteilt auf Österreich (`AUT`), Belgien ( `BEL`
), der Schweiz (`CHE`), Deutschland (`DE`), Frankreich (`FRA`),
Großbritannien (`GBR`), Irland (`IRL`) und Schweden (`SWE`). Man erkennt
insbesondere, dass `GBR` in allen Kategorien von `status` dominiert.
Außerdem ist es aus dem Diagramm ersichtlich, dass sehr viele Start-ups
weiterhin im Betrieb sind.

``` r
BARPLOT2(euro_startups, status, country_code)
```

<img src="man/figures/README-barplot_faceted-1.png" width="100%" />

In diesem Diagramm erhalten wir neben den gleichen Informationen vom
ersten Diagramm außerdem Zusatzinformation: Zuerst sind die Prozentsätze
von `acquired`, `closed` und `operating` in jedem Land nahezu gleich.
Außnahmen sind `DEU` und `BEL`, wo sogar einwenig mehr Start-ups den
Status `acquired` haben als `closed`.

``` r
PIECHART(euro_startups, status)
```

<img src="man/figures/README-piechart-1.png" width="100%" />

Hier sind nochmal klar die Prozentsätze von `acquired`, `closed` und
`operating` dargestellt.

``` r
COUNTPLOT(euro_startups, status, country_code, "prop")
```

<img src="man/figures/README-countplot_prop-1.png" width="100%" />

Dieses Diagramm ist die grafische Darstellung von
`contingency_table_scale()`. Es stellt genau die Prozentsätze aller
`status`-Werte in jedem Land dar.

``` r
COUNTPLOT(euro_startups, status, country_code, "count")
```

<img src="man/figures/README-countplot_count-1.png" width="100%" />

Hier erhalten wir eine grafische Variante, welches uns auch `BARPLOT1()`
liefert. In diesem Fall kann man auch nur die Informationen aus
`BARPLOT1()` herausnehmen, obwohl es visuell sogar schlechter ist.

``` r
HEATMAPS(euro_startups, status, country_code)
```

<img src="man/figures/README-heatmap-1.png" width="100%" />

Heatmaps sind eine visuelle Variante von `COUNTPLOT()`, die ähnliche
Resultate darstellt. Hier ist das Diagramm nur von wenig Nutzen, da der
Großteil an Fläche mit der fast selben Farbe gefärbt ist.

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
#> Theoretical approach for the data set:
#>   X-squared value: 15.290
#>   degrees of freedom: 14
#>   p-value: 0.359
```

<img src="man/figures/README-startups_independence_plot-1.png" width="100%" />

    #> Response: country_code (factor)
    #> Explanatory: status (factor)
    #> Null Hypothesis: independence
    #> # A tibble: 1,000 × 2
    #>    replicate  stat
    #>        <int> <dbl>
    #>  1         1  7.97
    #>  2         2 20.3 
    #>  3         3 10.2 
    #>  4         4  6.66
    #>  5         5 17.8 
    #>  6         6 11.0 
    #>  7         7  8.54
    #>  8         8  5.36
    #>  9         9  6.46
    #> 10        10  8.05
    #> # … with 990 more rows

Wir sehen, dass der berechnete Chi-Quadrat-Wert unter der Annahme der
Nullhypothese ziemlich nahe an der approximierten Verteilung liegt. Der
p-Wert liegt weit über dem 0,05-Signifikanzniveau. Daher können wir die
Nullhypothese nicht verwerfen.

Um ein weiteres Beispiel zu sehen, sehen wir uns den Datensatz
`accidents21` an. Wir wollen testen, ob die Unfallschwere von dem
Wochentag abhängt, an dem sie sich ereignen.

H<sub>0</sub>: `accident_severity` und `day_of_week` sind unabhängig  
H<sub>1</sub>: sie sind abhängig

``` r
test_independence(accidents21, accident_severity, day_of_week, 1000, 1000, TRUE);
#> Theoretical approach for the data set:
#>   X-squared value: 118.731
#>   degrees of freedom: 12
#>   p-value: 1.11e-19
```

<img src="man/figures/README-accidents_independence_plot-1.png" width="100%" />

    #> Response: accident_severity (factor)
    #> Explanatory: day_of_week (factor)
    #> Null Hypothesis: independence
    #> # A tibble: 1,000 × 2
    #>    replicate  stat
    #>        <int> <dbl>
    #>  1         1  9.61
    #>  2         2  7.29
    #>  3         3 10.8 
    #>  4         4  5.90
    #>  5         5 12.2 
    #>  6         6 14.4 
    #>  7         7  9.74
    #>  8         8 12.2 
    #>  9         9  9.99
    #> 10        10 10.6 
    #> # … with 990 more rows

In diesem Fall sehen wir, dass der berechnete Chi-Quadrat-Wert weit von
der Verteilung unter der Nullhypothese entfernt ist und, dass der p-Wert
extrem klein ist. Also ist die Wahrscheinlichkeit eines Fehlers 1. Art
auch extrem klein und daher können wir die Null-Hypothese verwerfen.

Aus den Daten erhalten wir also, dass die Unfallschwere und der
Wochentag abhängig zu sein scheinen. Eine mögliche Erklärung kann sein,
dass am Wochenende in den Städten viel weniger Verkehr herrscht und die
leichten Unfälle meist zur Hauptverkehrszeit passieren.
