# Create overall and by-group summary table

The Overall column reports percentages for the overall cohort. The group
columns report row-wise percentages.

## Usage

``` r
table_overall_and_group(d, name_groupvar, group_header = "**Group**")
```

## Arguments

- d:

  data.frame. Dataset containing variables to summarize.

- name_groupvar:

  character(1). Name of the grouping variable (column name) in `d` used
  to produce the grouped summary.

- group_header:

  character(1). Label used as the tab spanner for the grouped columns in
  the merged table.

## Value

A gtsummary table object

## Details

The function builds two tbl_summary objects: one for the overall dataset
(with the grouping column removed) and one stratified by the specified
grouping variable. It then merges them into a single table with a custom
tab spanner for the grouped columns.

## Examples

``` r
df <- data.frame(
  grp = rep(c("A","B"), each = 10),
  age = rnorm(20, 50, 10),
  sex = sample(c("M","F"), 20, TRUE)
)
table_overall_and_group(df, "grp")
#> Setting theme "language: en"
#> Setting theme "Compact"


  


Characteristic
```

**Overall**  
N = 20

**Group**

**A**  
n = 10

**B**  
n = 10

age

  

  

  

    Median (Q1, Q3)

54.2 (43.5, 61.2)

56.4 (54.1, 63.7)

45.7 (41.6, 58.6)

    Min, Max

32.0, 68.7

41.4, 68.7

32.0, 66.4

sex

  

  

  

    F

8 (40%)

2 (25%)

6 (75%)

    M

12 (60%)

8 (67%)

4 (33%)
