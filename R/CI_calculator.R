#' Lower CI function
#'
#' @param value Input parameter for the function
#' @param sd Standard deviation of the sample
#' @param prob The probability for the confidence interval
#'
#' @return A numeric value for the lower CI
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' my_CI_lower(10,100)


my_CI_lower <- function(value, sd, prob=0.95) {
  two_tail_prob <- 1 - ((1-prob)/2)
  ci_lower <- value - (qnorm(two_tail_prob, mean=0, sd=1,lower.tail = TRUE)*sd)
  return(ci_lower)
}

#' Upper CI function
#'
#' @param value Input parameter for the function
#' @param sd Standard deviation of the sample
#' @param prob The probability for the confidence interval
#'
#' @return A numeric value for the upper CI
#' @importFrom stats qnorm
#' @export
#'
#' @examples
#' my_CI_upper(10,100)


my_CI_upper <- function(value, sd, prob=0.95) {
  two_tail_prob <- 1 - ((1-prob)/2)
  ci_upper <- value - (qnorm(two_tail_prob, mean=0, sd=1,lower.tail = FALSE)*sd)
  return(ci_upper)
}


