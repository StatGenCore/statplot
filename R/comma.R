#' Format numbers with comma as thousands separator
#'
#' A lightweight helper that formats numeric values with commas and
#' disables scientific notation.
#'
#' @param x Numeric vector to format.
#' @return Character vector with numbers formatted using \code{,} as the thousands separator.
#' @examples
#' comma(1234567)
#' @export
comma <- function(x) format(x, big.mark = ',', scientific = FALSE)
