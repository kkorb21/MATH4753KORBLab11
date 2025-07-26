#' Density Difference Function for Beta HDI with Graph Output
#'
#' Calculates the density difference between two Beta quantiles given a tail probability.
#' Also displays a diagnostic plot of the density difference curve across possible `a1` values,
#' aiding in visual root-finding to identify HDIs.
#'
#' @name dendiff
#' @param a1 Numeric. Left-tail exclusion probability.
#' @param shape1 Numeric. First shape parameter of the Beta distribution.
#' @param shape2 Numeric. Second shape parameter of the Beta distribution.
#' @param alpha Numeric. Total tail probability for the interval.
#'
#' @return Numeric value representing the difference in density.
#' Also displays a plot of density difference vs. a1 with intersecting lines.
#'
#' @importFrom stats dbeta qbeta
#' @importFrom graphics abline curve
#' @export
#'
#' @examples
#' dendiff(a1 = 0.02, shape1 = 3, shape2 = 10, alpha = 0.05)
utils::globalVariables("x")

dendiff <- function(a1, shape1 = 3, shape2 = 10, alpha = 0.05) {
  a2 <- alpha - a1
  qa1 <- qbeta(a1, shape1, shape2)
  qa2 <- qbeta(1 - a2, shape1, shape2)
  dens_diff <- dbeta(qa1, shape1, shape2) - dbeta(qa2, shape1, shape2)

  # Diagnostic plot
  curve(
    expr = {
      a2_temp <- alpha - x
      qa1_temp <- qbeta(x, shape1, shape2)
      qa2_temp <- qbeta(1 - a2_temp, shape1, shape2)
      dbeta(qa1_temp, shape1, shape2) - dbeta(qa2_temp, shape1, shape2)
    },
    from = 0, to = alpha,
    xlab = "a1", ylab = "Difference in density",
    main = "Korb: Find a1 and a2", col = "blue", lwd = 2
  )
  abline(h = 0, col = "red", lty = 2)
  abline(v = a1, col = "red", lty = 2)

  return(dens_diff)
}
