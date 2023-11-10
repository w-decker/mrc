#' Calculate semi-partial correlations from data
#'
#' @param f Formula for full model
#' @param vint Independent variable of interest
#' @param data Data frame correlation matrix
#' @return Semi-partial correlation value
#'
#' @export

spr <- function(f, vint, data ){
  x <- cor(f[[2]],
      resid(lm(labels(terms(f))[1] ~ labels(terms(f))[1]),
            data = data))

  return(x)
}
