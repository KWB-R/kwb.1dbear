#' Helper function: calculate e
#'
#' @param k k
#' @param D D
#' @param retardation retardation
#' @param v v
#' @param t t
#'
#' @return ???
#' @keywords internal
#'
calculate_e <- function(k, D, retardation, v, t) {
  sqrt(1 + 4 * k * D * retardation / v^2)
}

#' Helper function: calculate f
#'
#' @param D D
#' @param retardation retardation
#' @param t t
#'
#' @return ???
#' @keywords internal
#'
calculate_f <- function(D, retardation, t) {
  2 * sqrt((D / retardation) * t)
}

#' Helper function: calculate g
#'
#' @param x x
#' @param v v
#' @param D D
#'
#' @return ???
#' @keywords internal
#'
calculate_g <- function(x, v, D) {
  x * v / (2 * D)
}

#' Calculate Cx
#'
#' @param x x
#' @param t t
#' @param v v
#' @param D D
#' @param retardation retardation
#' @param C0 initial concentration
#' @param k k
#'
#' @return ????
#' @export
#' @importFrom pracma erfc
#'
calculate_Cx <- function(x, t, v, D, retardation, C0, k) {
  e <- calculate_e(k, D, retardation, v, t)
  f <- calculate_f(D, retardation, t)
  g <- calculate_g(x, v, D)
  C0 / 2 * (exp(g * (1 - e)) * pracma::erfc((x - v * t / retardation * e) / f) +
              exp(g * (1 + e)) * pracma::erfc((x + v * t / retardation * e) / f))
}
