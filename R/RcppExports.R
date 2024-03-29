# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' Fit Repeated Linear Regressions with One Variable
#'
#' Fit a set of linear regressions which differ only in one variable.
#'
#' @param R_X the observation matrix
#' @param R_Y the response
#' @param R_COV common variables
#' @param num_threads number of threads for openmp. If it is -1 (default), it will use all possible threads.
#' @return the fitting results for each regression.
#' @examples
#' set.seed(123)
#' X = matrix(rnorm(50), 10, 5)
#' Y = rnorm(10)
#' COV = matrix(rnorm(40), 10, 4)
#' frlr1(X, Y, COV)
#' @export
frlr1 <- function(R_X, R_Y, R_COV, num_threads = -1L) {
    .Call('_fRLR_frlr1', PACKAGE = 'fRLR', R_X, R_Y, R_COV, num_threads)
}

#' Fit Repeated Linear Regressions with Two Variables
#'
#' Fit a set of linear regressions which differ only in two variables.
#'
#' @param R_X the observation matrix
#' @param R_idx1 the first identical feature
#' @param R_idx2 the second identical feature
#' @param R_Y the response variable
#' @param R_COV common variables
#' @param num_threads number of threads for openmp. If it is -1 (default), it will use all possible threads.
#' @return the fitting results for each regression.
#' @examples
#' set.seed(123)
#' X = matrix(rnorm(50), 10, 5)
#' Y = rnorm(10)
#' COV = matrix(rnorm(40), 10, 4)
#' idx1 = c(1, 2, 3, 4, 1, 1, 1, 2, 2, 3)
#' idx2 = c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5)
#' frlr2(X, idx1, idx2, Y, COV)
#' @export
frlr2 <- function(R_X, R_idx1, R_idx2, R_Y, R_COV, num_threads = -1L) {
    .Call('_fRLR_frlr2', PACKAGE = 'fRLR', R_X, R_idx1, R_idx2, R_Y, R_COV, num_threads)
}

