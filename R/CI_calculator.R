#' CI function(Normal)
#'
#' @param vect Input parameter for the function
#' @param conf.int The desired confidence interval
#' @return A numeric value for the lower CI
#' @importFrom stats qnorm
#' @export
#' @examples
#' norm_ci(sample(1:10,20, replace=TRUE))


norm_ci <- function(vect, conf.int=0.95) {
  v = var(vect)
  xbar = mean(vect)
  marginOfError <- qnorm(1-(1-conf.int)/2) * sqrt(v/length(vect))
  return(c(xbar - marginOfError, xbar+ marginOfError))
}


