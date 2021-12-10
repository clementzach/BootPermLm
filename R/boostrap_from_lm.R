#' Bootstrap Confidence Intervals for Regression Coefficients
#' 
#' @param input_lm A lm in which your model is fit. 
#' @param alpha the level of the confidence interval. Default is 0.05
#' @param B The number of bootstrapped samples to conduct. Default is 1000.
#' 
#' @return A table with coefficient estimates and confidence intervals
#' 
#' @references https://en.wikipedia.org/wiki/Bootstrapping_(statistics)

#' 
#' @examples 
#' 
#' require(BootPermLm)
#' require(stats)
#' 
#' x1 <- rnorm(100)
#' x2 <- rnorm(100) + x1
#' x3 <- rnorm(100)
#' y <- rnorm(100) + x1 + 2*x2
#' my_lm <- lm(y ~ x1 + x2 + x3)
#' 
#' bootstrap_from_lm(my_lm)
#'
#'
#'
#' @export
bootstrap_from_lm <- function(input_lm, alpha = 0.05,  B = 1000) {
 
  input_lm <- transform_lm_obj(input_lm)
  y <- input_lm$y
  
  x <- input_lm$x

  bootstrap_from_matrix(y, x, alpha = alpha, B = B)
  
}
