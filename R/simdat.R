#' Simulate correlated data
#'
#' @param n Number of datapoints
#' @param r Rho. Correlation value to be input into 2x2 positive-definite correlation matrix
#' @return Returns data.frame with 'y' dependent variable and 'x1' and 'x2' as independent variables
#'
#' @examples
#' simdat(n = 100, r = 0.4)
#'
#' @export

simdat <- function (n, r){
  cm <- rbind(c(1,   r), # correlation matrix
                  c(r, 1))
  d <- MASS::mvrnorm( # creates data
    n = n,
    mu = c(0,0),
    Sigma = cm
  )

  # Create dependent variable
  B <- c(0, 1, 2)
  y <- B[1] + B[2]*d[, 1] + B[3]*d[, 2] + rnorm(100, sd=2)

  # Combine
  df <- as.data.frame(cbind(y, d))
  colnames(df) <- c("y", "x1", "x2")

  return(df)
}
