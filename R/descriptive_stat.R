#' Creates histogram, boxplots and numeric summary
#' @export
#' @param x numerical variable

ds <- function(x) {
  # 1 row two columns for plots
  par(mfrow=c(1,2))
  # histogram
  hist(x, col=rainbow(30))
  # box plot
  boxplot(x, col= 'red')
  par(mfrow=c(1,1))
  # numeric summary
  data.frame(
    minimum = min(x),
    maximum = max(x),
    median = median(x)
  )
}
