#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame countNaValuesRcpp(NumericVector column, std::string columnName) {
  int missingCount = sum(is_na(column));

  return DataFrame::create(_["Column"] = columnName, _["MissingCount"] = missingCount);
}
