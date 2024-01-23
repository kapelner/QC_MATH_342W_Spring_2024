#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix compute_distance_matrix_cpp(NumericMatrix X) {
int n = X.nrow();
int p = X.ncol();
NumericMatrix D(n, n);
std::fill(D.begin(), D.end(), NA_REAL);

for (int i_1 = 0; i_1 < (n - 1); i_1++){
  for (int i_2 = i_1 + 1; i_2 < n; i_2++){
    int sqd_diff = 0;
    for (int j = 0; j < p; j++){
      sqd_diff += pow(X(i_1, j) - X(i_2, j), 2); //by default the cmath library in std is loaded
    }
    D(i_1, i_2) = sqrt(sqd_diff); //by default the cmath library in std is loaded
  }
}
return D;
}