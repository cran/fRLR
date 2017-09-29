// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// frlr1
Rcpp::List frlr1(SEXP R_X, SEXP R_Y, SEXP R_COV);
RcppExport SEXP _fRLR_frlr1(SEXP R_XSEXP, SEXP R_YSEXP, SEXP R_COVSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_X(R_XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_Y(R_YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_COV(R_COVSEXP);
    rcpp_result_gen = Rcpp::wrap(frlr1(R_X, R_Y, R_COV));
    return rcpp_result_gen;
END_RCPP
}
// frlr2
Rcpp::List frlr2(SEXP R_X, SEXP R_idx1, SEXP R_idx2, SEXP R_Y, SEXP R_COV);
RcppExport SEXP _fRLR_frlr2(SEXP R_XSEXP, SEXP R_idx1SEXP, SEXP R_idx2SEXP, SEXP R_YSEXP, SEXP R_COVSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type R_X(R_XSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_idx1(R_idx1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_idx2(R_idx2SEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_Y(R_YSEXP);
    Rcpp::traits::input_parameter< SEXP >::type R_COV(R_COVSEXP);
    rcpp_result_gen = Rcpp::wrap(frlr2(R_X, R_idx1, R_idx2, R_Y, R_COV));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fRLR_frlr1", (DL_FUNC) &_fRLR_frlr1, 3},
    {"_fRLR_frlr2", (DL_FUNC) &_fRLR_frlr2, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_fRLR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
