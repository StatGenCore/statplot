# Violin + boxplot of a numeric variable by a grouping factor

Create a violin plot with an overlaid boxplot showing the distribution
of a numeric variable split by a factor. The function expects the
grouping column to be a factor and the outcome to be numeric.

## Usage

``` r
plot_numeric_by_group(
  yvar,
  group,
  d,
  yvar_label = NULL,
  group_label = NULL,
  title = NULL,
  group_colors = NULL,
  show_effect_size = TRUE,
  n_boot = 1000,
  xaxis_labels_nchar_wrap = 20
)
```

## Arguments

- yvar:

  Character scalar. Column name (in `d`) of the numeric variable to
  plot. Must be length 1 and refer to a numeric column in `d`.

- group:

  Character scalar. Column name (in `d`) of the grouping factor. Must be
  length 1 and refer to a factor column in `d`.

- d:

  A data.frame containing the variables.

- yvar_label:

  Optional character scalar for the y-axis label. Defaults to `yvar`. If
  supplied, must be length 1.

- group_label:

  Optional character scalar for the x-axis label. Defaults to `group`.
  If supplied, must be length 1.

- title:

  Optional character scalar for the plot title. Defaults to
  `"<yvar> by <group>"`. If supplied, must be length 1.

- group_colors:

  Optional character vector of colours to use for the grouping levels.
  Must be length equal to `nlevels(d[[group]])` when supplied. Use
  colour names or hex codes.

- show_effect_size:

  Logical scalar. If TRUE the subtitle will include epsilon-squared and
  p-value.

- n_boot:

  Integer-like numeric scalar. Number of bootstrap samples for
  estimating CI of epsilon-squared. Default is 2000. Must be a single
  numeric value \>= 100.

- xaxis_labels_nchar_wrap:

  Numeric scalar. Maximum line width (number of characters) used to wrap
  group labels on the x-axis before appending the sample size. Defaults
  to 20. Must be a single positive numeric value.

## Value

A list containing the ggplot2 object, epsilon-squared estimate and 95%
CI, p-value.

## Examples

``` r
data(mtcars)
mtcars$gear <- factor(mtcars$gear)
p <- plot_numeric_by_group(
  yvar = "mpg",
  group = "gear",
  d = mtcars
)
p$ggplot
```
