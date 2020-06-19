// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// bs_eval
arma::vec bs_eval(double ind_val, int nbasis);
RcppExport SEXP _finmodtk_bs_eval(SEXP ind_valSEXP, SEXP nbasisSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type ind_val(ind_valSEXP);
    Rcpp::traits::input_parameter< int >::type nbasis(nbasisSEXP);
    rcpp_result_gen = Rcpp::wrap(bs_eval(ind_val, nbasis));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_finmodtk_bs_eval", (DL_FUNC) &_finmodtk_bs_eval, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_finmodtk(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
