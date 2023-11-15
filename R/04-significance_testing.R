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

#' Model mean squared
#' @param f Formula for model
#' @param data Data.frame of data
#'
#' @return MS value
#'
#' @examples
#' d <- mrc::simdat(n = 100, r = 0.4)
#' mms(y ~ x1 + x2, data = d)
#'
#' @export
#'

mms <- function(f, data){

  N <- nrow(data)
  K <- length(labels(terms(f)))
  df <- N - K - 1
  r2 <- summary(lm(f, data = data))$r.squared
  numerator <- K * (1 - r2)
  denom <- df
  ms <- numerator/denom

  return(c("MS" = ms))

}

#' Full model F-test on multiple regression model
#'
#' @param f Formula for model
#' @param data Data.frame of data
#'
#' @return F-test
#'
#' @examples
#' d <- mrc::simdat(n = 100, r = 0.4)
#' fmr(y ~ x1 + x2, data = d)
#'
#' @export

fmr <- function(f, data){

  N <- nrow(data)
  K <- length(labels(terms(f)))
  df <- N - K - 1
  r2 <- summary(lm(f, data = data))$r.squared
  ms <- unname(mms(f = f, data = data))

  f <- r2/ms

  return(c("df1" = K, "df2" = df, "MS" = ms, "F" = f))


}

#' Takes full model f-test and spits out a written version of the test.
#'
#' @param test Output from `fmr()`
#'
#' @return Prints out statement in R Console
#'
#' @export

fwrite <- function(test){
  return(sprintf("F(%d, %d) = %.5f", test["df1"], test["df2"], test["F"]))
}
