#' Function to compute standard error from model
#'
#' @param f Formula for model
#' @param data Data.frame of data
#'
#' @return Standard error value
#'
#' @examples
#' d <- mrc::simdat(n = 100, r = 0.4)
#' se(y ~ x1 + x2, data = d)
#'
#' @export
#'

se <- function(f, data){

  N <- nrow(data)
  K <- length(labels(terms(f)))
  df <- N - K - 1
  r2 <- summary(lm(f, data = data))$r.squared
  se <- sqrt((1 - r2)/df)

  return(c("Standard error" = se))
}

#' Function that calculates t-value for semi-partial correlation
#'
#' @param f Formula for entire model in which label[1] is your independent variable of interest.
#' @param data Data.frame of data
#'
#' @return T-test with t, semi-partial correlation and standard error
#'
#' @examples
#' d <- mrc::simdat(n = 100, r = 0.4)
#' tsr(y ~ x1 + x2, data = d)
#'
#' @export

tsr <- function(f, data){

  s <- mrc::se(f = f, data = data)
  r <- mrc::sprf(f = f, data = data)
  t <- s/r
  names(t) <- "t"

  return(c(t, s, r))
}






