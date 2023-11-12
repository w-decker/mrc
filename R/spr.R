#' Calculate semi-partial correlations from data
#'
#' @param dep Dependent variable
#' @param ivi Independent variable of interest
#' @param ivn Independent variable of NO interest
#' @return Semi-partial correlation value
#'
#' @examples
#' d <- mrc::simdata(n = 100, r = 0.4)
#' spr(dep = d$y, ivi = d$x1, ivn = d$x2)
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
#' @examples
#' d <- mrc::simdat(n = 100, r = 0.4)
#' sprf(y ~ x1 + x2, data = d)
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

#' Calculate semi-partial correlations from a 3x3 correlation matrix
#'
#' @param f Formula. Two variables to compute semi-partial correlation. LHS MUST be dep. variable.
#' RHS must be ind. variable of interest
#' @param data Correlation matrix
#'
#' @return Semi-partial correlation value
#'
#' @examples
#' cm <- matrix(data=c(1, 0.2, 0.3, 0.2, 1, -0.6, 0.3, -0.6, 1), nrow=3, ncol=3)
#' colnames(cm) <- c("y", "x1", "x2")
#' rownames(cm) <- c("y", "x1", "x2")
#' covspr(y ~ x1, data = cm)
#'
#' @export

covspr <- function(f, data){

  y <- as.character(formula.tools::lhs(f))
  x <- as.character(labels(terms(f)))[1]
  n <- colnames(data)
  vars <- c(y, x)
  ni <- setdiff(n, vars)


  numerator <- (data[y, x] - (data[y, ni] * data[x, ni]))
  denom <- sqrt(1 - data[x, ni]^2)
  s <- numerator/denom


  return(c("Semi-partial correlation" = s))
}


