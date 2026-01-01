#' example.function
#'
#' Compute the sum of two numeric inputs. This function expects numeric
#' scalars for both arguments and returns their sum.
#'
#' @param x Numeric scalar.
#' @param y Numeric scalar.
#' @return Numeric scalar. The sum of \code{x} and \code{y}.
#' @examples
#' example.function(
#'     x = 1,
#'     y = 2
#'     );
#' @export
example.function <- function(x, y) {
    # checks to make sure input arguments are valid:
    stopifnot(
        is.numeric(x),
        is.numeric(y),
        length(x) == 1,
        length(y) == 1
    )
    res <- x + y
    return(res)
}
