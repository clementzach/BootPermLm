#' Permutation Test Regression Coefficients
#' 
#' @param input_lm A lm in which your model is fit. 
#' @param B The number of permutations to conduct. Default is 1000.
#' 
#' @return A matrix with coefficient estimates and permutation p-values.
#' 
#' @references https://en.wikipedia.org/wiki/Permutation_test
#' 
#' @examples 
#' 
#' require(BootPermLm)
#' require(stats)
#' x1 <- rnorm(100)
#' x2 <- rnorm(100) + x1
#' x3 <- rnorm(100)
#' y <- rnorm(100) + x1 + 2*x2
#' my_lm <- lm(y ~ x1 + x2 + x3)
#' 
#' permutation_test_lm(my_lm)
#' 
#' 
#'
#'
#'
#' @export
permutation_test_lm <- function(input_lm,  B = 1000) {
  
  if (!("model" %in% names(input_lm))){
    stop("You must specify model = T when fitting your lm")
  }
  
  y <- input_lm$model[,1]
  
  x <- input_lm$model[,-1]
  if("(Intercept)" %in% names(input_lm$coefficients)){
    x <- cbind("(Intercept)" = 1, x)
  }
  
  permutation_test_matrix(y, x, B = B)
  
}