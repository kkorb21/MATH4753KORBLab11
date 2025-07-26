#' Compute and Plot a Non-Symmetric Confidence Interval
#'
#' Constructs a non-symmetric confidence interval for the population mean using specified tail risks,
#' generates a histogram of the data, and tests whether a ball around a hypothesized mean lies entirely within the CI.
#'
#' @name ciNonSym
#' @param x Numeric vector. Sample data to analyze.
#' @param alpha Numeric. Total significance level (e.g., 0.05).
#' @param alpha2 Numeric. Significance level allocated to the upper tail.
#'        Lower tail level is computed as \code{alpha1 = alpha - alpha2}.
#' @param mu0 Numeric. Hypothesized population mean under the null.
#' @param epsilon Numeric. Radius of the ball around \code{mu0} to test against the CI.
#' @param bins Integer. Number of bins to use in the histogram (default is 30).
#'
#' @returns A named list containing:
#' \item{ci}{Numeric vector of lower and upper bounds of the non-symmetric CI.}
#' \item{ball}{Numeric vector representing the interval \code{mu0 ± epsilon}.}
#' \item{contains_ball}{Logical. TRUE if the ball lies entirely within the CI, otherwise FALSE.}
#' \item{alpha}{Total significance level.}
#' \item{alpha1}{Lower tail significance level.}
#' \item{alpha2}{Upper tail significance level.}
#' \item{mu0}{Hypothesized mean value.}
#' \item{epsilon}{Ball radius around \code{mu0}.}
#' \item{sample_mean}{Sample mean of \code{x}.}
#' \item{sample_sd}{Sample standard deviation of \code{x}.}
#' \item{n}{Sample size.}
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs scale_fill_manual
#' @importFrom dplyr filter
#' @importFrom stats qt sd
#' @export
#'
#' @examples
#' \dontrun{
#' df <- ddt %>% filter(SPECIES == "CCATFISH")
#'
#' ciNonSym(
#'   x = df$LENGTH,
#'   alpha = 0.05,
#'   alpha2 = 0.02,
#'   mu0 = 45,
#'   epsilon = 4,
#'   bins = 25
#' )
#' }
ciNonSym <- function(x, alpha = 0.05, alpha2 = 0.02, mu0, epsilon, bins = 30) {
  # Sample statistics
  n <- length(x)
  xbar <- mean(x)
  s <- sd(x)
  alpha1 <- alpha - alpha2
  df <- n - 1

  # t critical values
  t_upper <- qt(1 - alpha1, df)
  t_lower <- qt(alpha2, df)

  # Store t-values
  t <- c(t_lower, t_upper)

  # Non-symmetric CI
  ci_lower <- xbar + t_lower * s / sqrt(n)
  ci_upper <- xbar + t_upper * s / sqrt(n)
  ci <- c(ci_lower, ci_upper)

  # Ball about mu0: [mu0 - epsilon, mu0 + epsilon]
  ball <- c(mu0 - epsilon, mu0 + epsilon)
  contains_ball <- (ci[1] <= ball[1]) & (ci[2] >= ball[2])

  # Create data for histogram
  status <- ifelse(x >= ci[1] & x <= ci[2], "in", "out")
  plot_df <- data.frame(x = x, status = status)

  # Plot
  p <- ggplot(plot_df, aes(x = x, fill = status)) +
    geom_histogram(bins = bins) +
    labs(title = "Korb: Lab 11", x = "x", y = "Count", fill = "CI")

  print(p)

  # Return output
  return(list(
    ci = ci,
    t = t,
    ball = ball,
    test = contains_ball
  ))
}
