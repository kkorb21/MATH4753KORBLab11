#' Compute the Highest Density Interval (HDI) and Equal Tail Interval (ETI)
#'
#' Calculates the HDI and ETI for a given distribution using quantile and density matching.
#' This is especially helpful for asymmetric distributions (e.g. Beta, Gamma), where HDIs are preferred
#' over equal tail intervals due to their shorter length and alignment with maximum density.
#'
#' @name hdicalc
#' @param stem Character string. Distribution prefix (e.g., `"norm"`, `"beta"`, `"gamma"`).
#' @param args List of named arguments passed to the distribution functions.
#'   For example, `list(mean = 10, sd = 5)` for normal, or `list(shape1 = 3, shape2 = 10)` for beta.
#' @param alpha Numeric. Total significance level (e.g. 0.05).
#'
#' @returns A named list containing:
#' \item{alpha1}{Lower tail probability for HDI, where density at both endpoints matches.}
#' \item{alpha2}{Upper tail probability for HDI (i.e., `alpha - alpha1`).}
#' \item{HDI}{Numeric vector of lower and upper bounds for the HDI.}
#' \item{RHDI}{Width of the HDI (difference between bounds).}
#' \item{ETI}{Numeric vector of equal-tail bounds.}
#' \item{RETI}{Width of the ETI.}
#'
#' @importFrom glue glue
#' @importFrom stats uniroot
#' @export
#'
#' @examples
#' hdicalc(stem = "beta", args = list(shape1 = 3, shape2 = 10), alpha = 0.05)
hdicalc <- function(stem = "norm", args = list(mean = 10, sd = 5), alpha = 0.05) {

  dendiff <- function(x, nr = TRUE) {
    a2 <- alpha - x
    f <- glue::glue("d{stem}")
    q <- glue::glue("q{stem}")

    qa1 <- do.call(q, c(list(p = x), args))
    qa2 <- do.call(q, c(list(p = 1 - a2), args))

    if (nr) {
      do.call(f, c(list(x = qa1), args)) - do.call(f, c(list(x = qa2), args))
    } else {
      c(qa1, qa2)
    }
  }

  # Find root where densities match (HDI logic)
  l <- uniroot(dendiff, interval = c(0, alpha))
  hdi_bounds <- dendiff(x = l$root, nr = FALSE)

  # For comparison: equal-tail interval
  eti_bounds <- dendiff(x = alpha / 2, nr = FALSE)

  list(
    alpha = alpha,
    alpha1 = l$root,
    alpha2 = alpha - l$root,
    HDI = hdi_bounds,
    RHDI = diff(hdi_bounds),
    ETI = eti_bounds,
    RETI = diff(eti_bounds)
  )
}
