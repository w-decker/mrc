#' Calculate delta R squared for two linear models
#'
#' @param fullm The full model formula
#' @param restrim The restricted model formula
#' @param data data frame of data
#' @return Returns a numeric vector of F test
#'
#' @examples
#' d <- mrc::simdat(n = 100, r = 0.4)
#' deltaR2(fullm = y ~ x1 + x2, restrim = y ~ x2, data = d)
#'
#' @export

deltaR2 <- function (fullm, restrim, data) {

  # standardize data
  data <- data.frame(scale(data, center = TRUE))

  # get some values
  N <- nrow(data)
  K <- length(labels(terms(fullm)))
  df <- N - K - 1
  diff <- K - (length(labels(terms(restrim))))

  # get R2
  fullm_R2 <- summary(lm(fullm, data=data))$r.squared
  restrim_R2 <- summary(lm(restrim, data=data))$r.squared

  # compute f test
  DR2 <- (fullm_R2 - restrim_R2)
  ms <- (diff * (1 - fullm_R2))/df
  f <- DR2 / ms

  return(c("D" = diff, "df's" = df, "Delta R2" = DR2, "F Value" = f))
}
