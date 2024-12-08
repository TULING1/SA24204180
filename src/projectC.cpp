#include <Rcpp.h>
using namespace Rcpp;
//' @title A Linear regression using Rcpp
//' @name lrC
//' @description A Linear regression using Rcpp
//' @importFrom stats rnorm rgamma
//' @useDynLib SA24204180
//' @param x A numeric vector of independent variable values.
//' @param y A numeric vector of dependent variable values.
//' @return A list containing the intercept and slope of the regression line.
//' @examples
//' \dontrun{
//' y=ax+b
//' result<-lrC(x,y)
//' result["intercept"] = b
//' result["slope"] = a
//' }
//' @export
// [[Rcpp::export]]
Rcpp::List lrC(const Rcpp::NumericVector& x, const Rcpp::NumericVector& y) {
  int n = x.length();
  double sum_x = std::accumulate(x.begin(), x.end(), 0.0);
  double sum_y = std::accumulate(y.begin(), y.end(), 0.0);
  double sum_xy = std::inner_product(x.begin(), x.end(), y.begin(), 0.0);
  double sum_x2 = std::inner_product(x.begin(), x.end(), x.begin(), 0.0);
  double x_mean = sum_x / n;
  double y_mean = sum_y / n;

  double beta1 = (sum_xy - sum_x * y_mean) / (sum_x2 - sum_x * x_mean);
  double beta0 = y_mean - beta1 * x_mean;

  Rcpp::List result;
  result["intercept"] = beta0;
  result["slope"] = beta1;
  return result;
}
