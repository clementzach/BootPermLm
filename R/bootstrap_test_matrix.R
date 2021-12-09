#' Bootstrap Confidence Intervals for Regression Coefficients
#' 
#' @param y The response variables (as a matrix)
#' @param x The predictor variables (as a matrix) 
#' @param alpha the level of the confidence interval. Default is 0.05
#' @param B The number of permutations to conduct. Default is 1000.
#' 
#' @return A table with coefficient estimates and confidence intervals
#' 
#' @references https://en.wikipedia.org/wiki/Bootstrapping_(statistics)
#' 
#' @useDynLib BootPermLm
#' @importFrom Rcpp sourceCpp
#' @importFrom stats quantile
#' 
#' @examples 
#' 
#' require(BootPermLm)
#' require(stats)
#' x1 <- rnorm(100)
#' x2 <- rnorm(100) + x1
#' x3 <- rnorm(100)
#' y <- rnorm(100) + x1 + 2*x2
#' x <- cbind(x1, x2, x3)
#' 
#' 
#' bootstrap_test_matrix(y, x)
#'
#'
#'
#' @export
bootstrap_test_matrix <- function(y, x, alpha = 0.05,  B = 1000) {
  x <- as.matrix(x)
  
  if(is.null(colnames(x))){
    colnames(x) <- paste0("V", 1:ncol(x))
  }
  design_matrix <- cbind(y, 1, x) |> as.matrix()
  
  observed_vals <- get_betas(design_matrix)
  
  dist <- bootstrap_distribution(0:(nrow(design_matrix)-1), design_matrix, B)
  
  ci <- sapply(1:(ncol(x)+ 1), function(x){
    quantile(dist[,x], prob = c(alpha/2, 1-alpha/2))
  }) |> 
    t()
  
  cbind(observed_vals, ci) |> 
    `colnames<-`(c("Observed Value", colnames(ci))) |>
    `rownames<-`(c("(Intercept)", colnames(x)))
  
}
