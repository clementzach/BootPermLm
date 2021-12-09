#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS

#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]

using Eigen::MatrixXd;                  // variable size matrix, double precision

// [[Rcpp::export]]
Eigen::MatrixXd get_betas( Eigen::MatrixXd full) {
  
  int q = full.cols() - 1; //finds number of columns minus 1
  Eigen::MatrixXd x = full.rightCols(q); //all cols to the left of the first one
  Eigen::MatrixXd y = full.leftCols(1); //first col
  
  Eigen::MatrixXd xtx = x.transpose() * x;
  Eigen::MatrixXd b = xtx.inverse() * x.transpose()* y;
  
  return b;
}


Eigen::MatrixXd bootstrap_obs(Rcpp::NumericVector indices, Eigen::MatrixXd full) {
  
  /* //need to change the types in the function definitions to use this
  std::random_device rd;
  std::default_random_engine rng(rd());
  shuffle(indices.begin(), indices.end(), rng);
  */

  indices = Rcpp::sample(indices, indices.size(), TRUE);

  
  Eigen::MatrixXd out_mat(full.rows(), full.cols()); //matrix with same dimensions as input
  //int index;
  //probably a bottleneck here but not sure how to do it faster
  
  for (int i = 0; i < full.rows(); i++){ //go through every row
    //index = indices(i);
    out_mat.row(i) = full.row(indices.at(i)); //grab the index row of the input matrix
  }
  return(out_mat);
}
 

// [[Rcpp::export]]
Eigen::MatrixXd bootstrap_distribution(Rcpp::NumericVector  indices, Eigen::MatrixXd full, int num_samples){

  Eigen::MatrixXd beta_matrix(num_samples, full.cols() - 1); //1 row for each rep, 1 col for each beta
  Eigen::MatrixXd current_betas(1, full.cols() - 1);
  Eigen::MatrixXd current_full(full.rows(), full.cols());
  
  for(int i = 0; i < num_samples; i++){
    current_full = bootstrap_obs(indices, full);
    
 //   std::cout << current_full << std::endl;
    
//    std::cout <<  std::endl;
    
    current_betas = get_betas(current_full);
    
//    std::cout << current_betas << std::endl;
    
//    std::cout <<  std::endl;
    
    beta_matrix.row(i) = current_betas.transpose();
  }
  
  return(beta_matrix);
}









