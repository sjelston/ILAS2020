#' Confidence interval function
#'
#' This function uses a given set of values from a single sample
#' to calculate a 95 percent confidence interval for mean (mu)
#'
#' @param x The vector containing data to calculate the ci
#'
#' @return A 95 percent confidence interval for the given data set
#' @export
#'
#' @examples
#' myci(rnorm(30,mean=10,sd=12))
myci <- function(x){
  # Getting the length of the sample set
  n <- length(x)

  # Creating the T statistic
  t <- qt(1 - 0.05/2, (n - 1))
  mp <- c(-1, 1)  # Will be used for + or - operations

  # Calculating the confidence interval
  ci <- mean(x) + mp*t*sd(x)/sqrt(n)
  ci  # Printing the confidence interval
}
