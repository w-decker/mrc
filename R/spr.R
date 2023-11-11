#' Calculate semi-partial correlations from data
#'
#' @param dep Dependent variable
#' @param ivi Independent variable of interest
#' @param ivn Independent variable of NO interest
#' @return Semi-partial correlation value
#'
#' @export

spr <- function(dep, ivi, ivn){

  x <- cor(dep, resid(lm(ivi ~ ivn)))

  return(c("Semi-partial correlation" = x))
}

#' Calculate semi-partial correlations from data by specifying a formula.
#'
#' @param f Formula for entire model in which label[1] is your independent variable of interest.
#' @param data Data.frame of data
#' @return Semi-partial correlation value
#'
#' @export

sprf <- function(f, data){

  y <- data[as.character(formula.tools::lhs(f))]
  indep <- labels(terms(f))
  ivi <- as.matrix(data[indep[1]])
  ivn <- as.matrix(data[indep[2]])

  x <- (cor(y, ivi) - (cor(ivi, ivn) * cor(ivn, y)))/sqrt(1-cor(ivi, ivn)^2)

  return(c("Semi-partial correlation" = x))
}
