#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS

#include <RcppEigen.h>


// [[Rcpp::depends(RcppEigen)]]

using Eigen::MatrixXd;                  // variable size matrix, double precision

Eigen::MatrixXd get_betas_perm(Eigen::MatrixXd full) {
  
  int q = full.cols() - 1; //finds number of columns minus 1
  Eigen::MatrixXd x = full.rightCols(q); //all cols to the left of the first one
  Eigen::MatrixXd y = full.leftCols(1); //first col
  
  Eigen::MatrixXd xtx = x.transpose() * x;
  Eigen::MatrixXd b = xtx.inverse() * x.transpose()* y;
  
  return b;
}
 

Eigen::MatrixXd perm_obs(Rcpp::NumericVector indices, Eigen::MatrixXd full) {

  indices = Rcpp::sample(indices, indices.size(), FALSE); //sample without replacement
  
  Eigen::MatrixXd out_mat = full; 

  
  for (int i = 0; i < full.rows(); i++){ //go through every row
    //under null, ys have no relationship to xs
    out_mat(i, 0) = full(indices.at(i), 0); //assign the scrambled y value to the scrambled
  }
  return(out_mat);
}

// [[Rcpp::export]]
Eigen::MatrixXd permutation_distribution(Rcpp::NumericVector  indices, Eigen::MatrixXd full, int num_samples){

  Eigen::MatrixXd beta_matrix(num_samples, full.cols() - 1); //1 row for each rep, 1 col for each beta
  Eigen::MatrixXd current_betas(1, full.cols() - 1);
  Eigen::MatrixXd current_full(full.rows(), full.cols());
  
  for(int i = 0; i < num_samples; i++){
    current_full = perm_obs(indices, full);
 //      std::cout << current_full << std::endl;
//       std::cout <<  std::endl;
    current_betas = get_betas_perm(current_full);
    //    std::cout << current_betas << std::endl;
    //    std::cout <<  std::endl;
    beta_matrix.row(i) = current_betas.transpose();
  }
  return(beta_matrix);
}

