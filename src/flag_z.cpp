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
    
    // Tukey biweight midvariance (Mosteller & Tukey 1977; Lax 1985).
    //   uᵢ = rᵢ / (c · MAD), c = 9 is the standard scale tuning constant.
    //   s² = n · Σ rᵢ² (1−uᵢ²)⁴ / [ Σ (1−uᵢ²)(1−5uᵢ²) ]²
    // Sums are taken over indices with |uᵢ| < 1; points outside the window
    // are downweighted to zero.
    const double c_tune = 9.0;
    double num = 0.0;
    double den = 0.0;
    int n_in = 0;
    for (size_t k = 0; k < resid.size(); ++k) {
      double u = resid[k] / (c_tune * mad);
      if (std::abs(u) < 1.0) {
        double one_minus_u2 = 1.0 - u * u;
        num += resid[k] * resid[k] * pow(one_minus_u2, 4);
        den += one_minus_u2 * (1.0 - 5.0 * u * u);
        ++n_in;
      }
    }
    if (n_in == 0 || den == 0.0) continue;

    double scale = std::sqrt(static_cast<double>(resid.size()) * num) /
      std::abs(den);
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
