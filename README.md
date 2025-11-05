stp25tools2
================

Tools for data wrangling and statistical transformations

``` r
library(stp25tools2)
#> stp25tools2 geladen. 
#> Pfad: C:/Users/wpete/AppData/Local/R/win-library/4.5
#> 
#> I changed the contrasts from contr.treatment, contr.poly
#> to contr.Treatment, contr.poly!
#devtools::install("stp4/stp25tools2")
```

## Datenimport und -Transformation

Dieses Paket bietet erweiterte Funktionen für den Import, das Umformen
und Kombinieren von Datensätzen. Es dient als flexible Sammlung von
Tools, die häufige Datentransformationen in der Analyse vereinfachen.

Enthalten sind unter anderem:

- get_data() – Ein flexibler Datenimporter (CSV, XLSX, SAV oder Text)

- Long() – Ein intelligenter Wrapper für pivot_longer() mit Formelsyntax

- Wide() – Ein intuitiver Wrapper für pivot_wider() mit Formelsyntax

- Combine() – Eine komfortable Join-Funktion zum Zusammenführen mehrerer
  Data Frames

## Funktionen

### get_data()

Importiert Datensätze aus verschiedenen Formaten (.xlsx, .csv, .sav)
oder direkt aus Text.

``` r
dat <- get_data("
sex treatment control
m  2 3
f  3 4
",
tabel_expand = TRUE,
id.vars = 1)

xtabs(~ sex + value, dat)
#>    value
#> sex control treatment
#>   f       4         3
#>   m       3         2
```

| Format  | Funktion aus Paket     | Beispielparameter          |
|:--------|:-----------------------|:---------------------------|
| `.xlsx` | `readxl::read_excel()` | `sheet`, `skip`, `range`   |
| `.csv`  | `read.table()`         | `sep`, `dec`, `na.strings` |
| `.sav`  | `haven::read_sav()`    | `encoding`, `user_na`      |
| Text    | `read.text2()`         | `dec`                      |

### Long()

Erweitert tidyr::pivot_longer() um:

Formelsyntax (A + B ~ month)

Label-Unterstützung

Einfache Angabe von ID-Variablen

``` r

df <- data.frame(
  month = rep(month.abb[1:3], 2),
  student = rep(c("Amy", "Bob"), each = 3),
  A = c(9, 7, 6, 8, 6, 9),
  B = c(6, 7, 8, 5, 6, 7),
  C = c(1, 3, 6, 3, 4, 7)
)

# Variable Namen
Long(df, A, B, by = ~month)
#> # A tibble: 12 × 3
#>    month variable value
#>    <chr> <fct>    <dbl>
#>  1 Jan   A            9
#>  2 Jan   B            6
#>  3 Feb   A            7
#>  4 Feb   B            7
#>  5 Mar   A            6
#>  6 Mar   B            8
#>  7 Jan   A            8
#>  8 Jan   B            5
#>  9 Feb   A            6
#> 10 Feb   B            6
#> 11 Mar   A            9
#> 12 Mar   B            7

# Formelsyntax
Long(A + B ~ month, df, key = "student", value = "grade")
#> # A tibble: 12 × 3
#>    month student grade
#>    <chr> <fct>   <dbl>
#>  1 Jan   A           9
#>  2 Jan   B           6
#>  3 Feb   A           7
#>  4 Feb   B           7
#>  5 Mar   A           6
#>  6 Mar   B           8
#>  7 Jan   A           8
#>  8 Jan   B           5
#>  9 Feb   A           6
#> 10 Feb   B           6
#> 11 Mar   A           9
#> 12 Mar   B           7
```

### Wide()

Ein benutzerfreundlicher Wrapper um pivot_wider().

Unterstützt:

Formelsyntax (month ~ student)

Mehrere Value-Spalten

Steuerung über names_sep, names_vary und values_fill

``` r
Wide(df, month ~ student, A, B, C)
#> NULL
#> # A tibble: 3 × 7
#>   month Amy_A Amy_B Amy_C Bob_A Bob_B Bob_C
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Jan       9     6     1     8     5     3
#> 2 Feb       7     7     3     6     6     4
#> 3 Mar       6     8     6     9     7     7
```

### Combine()

Kombiniert mehrere Data Frames mithilfe einer frei wählbaren
Join-Funktion. Standardmäßig wird dplyr::full_join verwendet.

``` r
n <- 10
df1 <- data.frame(id = 1:n, x = rnorm(n))
df2 <- data.frame(id = 1:n, y = runif(n))
df3 <- data.frame(id = 1:n, z = rpois(n, 2))

# Full Join (Standard)
Combine(df1, df2, df3, by = "id")
#>    id           x         y z
#> 1   1 -0.10819912 0.9245359 5
#> 2   2  0.06131535 0.4197466 2
#> 3   3 -1.41616031 0.7410953 1
#> 4   4  0.83649793 0.1411611 4
#> 5   5  0.86794613 0.5929701 1
#> 6   6  0.68280771 0.7874687 5
#> 7   7 -1.51526443 0.1402075 1
#> 8   8  0.14071489 0.7447269 2
#> 9   9 -0.46815902 0.8075659 2
#> 10 10  0.91272580 0.5524534 1

# Left Join
Combine(df1, df2, df3, by = "id", merge_fun = dplyr::left_join)
#>    id           x         y z
#> 1   1 -0.10819912 0.9245359 5
#> 2   2  0.06131535 0.4197466 2
#> 3   3 -1.41616031 0.7410953 1
#> 4   4  0.83649793 0.1411611 4
#> 5   5  0.86794613 0.5929701 1
#> 6   6  0.68280771 0.7874687 5
#> 7   7 -1.51526443 0.1402075 1
#> 8   8  0.14071489 0.7447269 2
#> 9   9 -0.46815902 0.8075659 2
#> 10 10  0.91272580 0.5524534 1
```

### Beispiel-Workflow

``` r
# 1. Daten importieren
dat <- get_data("
sex treatment  neg  pos
f   KG          3   3
f   UG          4   5
m   KG          5   4
m   UG          4   2
", tabel_expand = TRUE, id.vars = 1:2, value = "befund")

#dat$id<-  seq_along(dat)
#dat
# # 2. Daten umformen
# dat_long <- Long(dat, neg, pos, by = ~sex + treatment)
# 
# # 3. Daten kombinieren
# df_extra <- data.frame(sex = c("f", "m"), group = c("A", "B"))
# final <- Combine(dat_long, df_extra, by = "sex")
# 
# # 4. Ergebnisse prüfen
# head(final)
```
