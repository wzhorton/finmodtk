#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

/////////////////////////////
// SPLINE HELPER FUNCTIONS //
/////////////////////////////

double basis_repeat4(double u){
  double out;
  if(u > 1.0){
    out = 0.0;
  } else if (u >= 0.0){
    out = (1.0-u)*(1.0-u)*(1.0-u);
  } else {
    out = 0.0;
  }
  return out;
}

double basis_repeat3(double u){
  double out;
  if(u > 2.0){
    out = 0;
  } else if (u > 1.0){
    out = 2.0 - 3.0*u + 3.0/2.0*u*u - 1.0/4.0*u*u*u; 
  } else if (u > 0){
    out = 3.0*u - 9.0/2.0*u*u + 7.0/4.0*u*u*u;
  } else {
    out = 0.0;
  }
  return out;
}

double basis_repeat2(double u){
  double out;
  if(u > 3.0){
    out = 0.0;
  } else if(u > 2.0){
    out = 9.0/2.0 - 9.0/2.0*u + 3.0/2.0*u*u - 1.0/6.0*u*u*u;
  } else if(u > 1.0){
    out = -3.0/2.0 + 9.0/2.0*u - 3.0*u*u + 7.0/12.0*u*u*u;
  } else if(u > 0.0){
    out = 3.0/2.0*u*u - 11.0/12.0*u*u*u;
  } else {
    out = 0.0;
  }
  return out;
}

double basis_unique(double u){
  double out;
  if(u > 4.0){
    out = 0.0;
  } else if(u > 3.0){
    out = -1.0/6.0 * (u-4.0)*(u-4.0)*(u-4.0);
  } else if(u > 2.0){
    out = -22.0/3.0 + 10.0*u - 4.0*u*u + 1.0/2.0*u*u*u;
  } else if(u > 1.0){
    out = 2.0/3.0 - 2.0*u + 2.0*u*u - 1.0/2.0*u*u*u;
  } else if(u > 0.0){
    out = 1.0/6.0*u*u*u;
  } else {
    out = 0.0;
  }
  return out;
}

////////////////////////////////////////////
// EVALUATE BSPLINE ON AN INDICATOR VALUE //
////////////////////////////////////////////
// Note: requires indicators to be normalized to the [0,1] interval

// [[Rcpp::export(".bs_even_C")]]
arma::vec bs_eval(double ind_val, int nbasis){
  double u = ind_val*(nbasis - 3);
  arma::vec out(nbasis);
  out(0) = basis_repeat4(u);
  out(1) = basis_repeat3(u);
  out(2) = basis_repeat2(u);
  for(int j = 0; j < nbasis - 6; j++){
    out(j + 3) = basis_unique(u - j);
  }
  out(nbasis - 3) = basis_repeat2(nbasis - 3 - u);
  out(nbasis - 2) = basis_repeat3(nbasis - 3 - u);
  out(nbasis - 1) = basis_repeat4(nbasis - 3 - u);
  return out;
}

///////////////////////////////////////////////////////
// EVALUATE BSPLINE ON ALL OBSERVED INDICATOR VALUES //
///////////////////////////////////////////////////////
// Note: evaluations for a given indicator value are stored as a row vector


arma::mat bspline_evals(arma::vec ind_vec, int nbasis){
  arma::mat out(ind_vec.n_elem, nbasis);
  for(int i = 0; i < ind_vec.n_elem; i++){
    out.row(i) = bs_eval(ind_vec(i), nbasis).t(); //.t() takes transpose
  }
  return out;
}

/////////////////////////////////////////
// EVALUATE BSPLINE ON INDICATOR PAIRS //
/////////////////////////////////////////
// Note: the value of 10 was discussed previously. Can be modified with care.

arma::mat bspline_pairs_evals(arma::vec ind1_vec, arma::vec ind2_vec){
  arma::mat out(ind1_vec.n_elem, 10*10);
  arma::mat H1 = bspline_evals(ind1_vec, 10);
  arma::mat H2 = bspline_evals(ind2_vec, 10);
  for(int i = 0; i < 10; i++){
    for(int j = 0; j < 10; j++){
      out.col(10*i + j) = H1.col(i) % H2.col(j); //element-wise multiplication
    }
  }
  return out;
}

////////////////////////////////////////////////////////
// ESTIMATE BASIS FUNCTION COEFFICIENTS USING RETURNS //
////////////////////////////////////////////////////////
// Note: requires moderate non-sparsity. Can be modified (with effort)
// Note: penalization can be used to further control smoothing in this step

arma::vec est_spline_coefs(arma::vec return_vec, arma::vec ind1_vec, arma::vec ind2_vec){
  arma::mat X = bspline_pairs_evals(ind1_vec, ind2_vec);
  return solve(X.t()*X, X.t()*return_vec);
}

/////////////////////////////////////////////////////////
// SIMULTANEOUS PREDICTED RETURN GIVEN INDICATOR PAIRS //
/////////////////////////////////////////////////////////
// Note: save the estimated coefficients beforehand (as part of calibration)
// Note: Fully give each individual pairing/combination desired
// Note: predictions for indicators outside of [0,1] will return zero

arma::vec est_return(arma::vec new_ind1_vec, arma::vec new_ind2_vec, arma::vec est_coefs){
  arma::mat X = bspline_pairs_evals(new_ind1_vec, new_ind2_vec);
  return X*est_coefs;
}

