# Format numbers with comma as thousands separator

A lightweight helper that formats numeric values with commas and
disables scientific notation.

## Usage

``` r
comma(x)
```

## Arguments

- x:

  Numeric vector to format.

## Value

Character vector with numbers formatted using `,` as the thousands
separator.

## Examples

``` r
comma(1234567)
#> [1] "1,234,567"
```
