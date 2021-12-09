#' Permutation Test Regression Coefficients
#' 
#' @param y The response variables (As a matrix)
#' @param x The predictor variables  (As a matrix)
#' @param B The number of permutations to conduct. Default is 1000.
#' 
#' @return A table with coefficient estimates and permutation p-values.
#' 
#' @references https://en.wikipedia.org/wiki/Permutation_test
#' 
#' @useDynLib BootPermLm
#' @importFrom Rcpp sourceCpp
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
#' permutation_test_matrix(y, x)
#'
#'
#'
#' @export
permutation_test_matrix <- function(y, x, B = 1000) {
  x <- as.matrix(x)
  
  if(is.null(colnames(x))){
    colnames(x) <- paste0("V", 1:ncol(x))
  }
  design_matrix <- cbind(y, 1, x) |> as.matrix()
  
  observed_vals <- get_betas(design_matrix)
  
  perms <- permutation_distribution(0:(nrow(design_matrix)-1), design_matrix, B)
  
  p_vals <- sapply(1:(ncol(x)+ 1), function(x){
    mean(
      (abs(observed_vals[x]) < perms[,x]) | ## 2 sided 
           (-abs(observed_vals[x]) > perms[,x])
      )
    })
    
  cbind(observed_vals, p_vals) |> 
    `colnames<-`(c("Observed Value", "P")) |>
    `rownames<-`(c("(Intercept)", colnames(x)))

}
