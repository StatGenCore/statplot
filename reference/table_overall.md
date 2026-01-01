# Create a table that summarizes the entire cohort.

Convenience wrapper that builds a gtsummary::tbl_summary for the overall
dataset.

## Usage

``` r
table_overall(d)
```

## Arguments

- d:

  data.frame Dataset containing variables to summarize.

## Value

A gtsummary::tbl_summary object for the overall cohort.

## Examples

``` r
df <- data.frame(age = rnorm(20, 50, 10), sex = sample(c("M","F"), 20, TRUE))
table_overall(df)
#> Setting theme "language: en"
#> Setting theme "Compact"


  

Characteristic
```

**Overall**  
N = 20

age

  

    Median (Q1, Q3)

52.4 (44.1, 58.8)

    Mean +/- SD

50.6 +/- 10.3

    Min, Max

24.7, 63.1

sex

  

    F

14 (70%)

    M

6 (30%)
