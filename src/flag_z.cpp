#include <Rcpp.h>
using namespace Rcpp;
using std::sort;
using std::vector;

// [[Rcpp::export]]
SEXP flag_z(NumericVector x, int width = 5, double threshold = 3.0, bool return_z = false) {
  int n = x.size();
  int half = width / 2;
  NumericVector z(n, NA_REAL);
  CharacterVector flag(n, NA_STRING);
  
  if (n < width) {
    return List::create(Named("z") = z, Named("flag") = flag);
  }
  
  for (int i = 0; i < n; ++i) {
    int start = std::max(0, i - half);
    int end = std::min(n - 1, i + half);
    std::vector<double> window;
    
    for (int j = start; j <= end; ++j) {
      if (!NumericVector::is_na(x[j])) {
        window.push_back(x[j]);
      }
    }
    
    if (window.size() % 2 == 0 && window.size() >= 2) {
      int drop = std::ceil(0.5 * window.size()) - 1;
      window.erase(window.begin() + drop);
    }
    
    if (window.size() < 3) continue;
    
    // Median
    sort(window.begin(), window.end());
    double med = (window.size() % 2 == 0) ?
    (window[window.size() / 2 - 1] + window[window.size() / 2]) / 2.0 :
      window[window.size() / 2];
    
    // Residuals
    std::vector<double> resid(window.size());
    for (size_t k = 0; k < window.size(); ++k) {
      resid[k] = window[k] - med;
    }
    
    // MAD
    std::vector<double> abs_resid = resid;
    for (double& val : abs_resid) val = std::abs(val);
    sort(abs_resid.begin(), abs_resid.end());
    double mad = (abs_resid.size() % 2 == 0) ?
    (abs_resid[abs_resid.size() / 2 - 1] + abs_resid[abs_resid.size() / 2]) / 2.0 :
      abs_resid[abs_resid.size() / 2];
    if (mad == 0 || NumericVector::is_na(mad)) continue;
    
    // Tukey's biweight scale
    double s = mad;
    double sum_w2_r2 = 0.0;
    double sum_w2 = 0.0;
    for (size_t k = 0; k < resid.size(); ++k) {
      double u = resid[k] / (4.685 * s);
      if (std::abs(u) < 1.0) {
        double w_i = pow(1 - u * u, 2);
        sum_w2_r2 += pow(w_i, 2) * resid[k] * resid[k];
        sum_w2 += pow(w_i, 2);
      }
    }
    if (sum_w2 == 0) continue;
    
    double scale = sqrt(sum_w2_r2 / sum_w2) * 1.4826;
    if (scale == 0 || !std::isfinite(scale)) continue;
    
    // Z-score
    if (!NumericVector::is_na(x[i])) {
      z[i] = (x[i] - med) / scale;
      if (std::isfinite(z[i]) && std::abs(z[i]) > threshold) {
        flag[i] = "Z";
      }
    }
  }
  
  if (return_z) {
    return List::create(Named("z") = z, Named("flag") = flag);
  } else {
    return flag;
  }
}
