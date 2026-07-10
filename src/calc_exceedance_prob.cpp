#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

struct FlowValue {
  double value;
  int output_index;
};

// [[Rcpp::export]]
NumericVector cpp_calc_exceedance_prob(NumericVector flow, bool rm_zero = false, double alpha = 0.0) {
  int n_flow = flow.size();
  NumericVector result(n_flow, NA_REAL);
  std::vector<FlowValue> values;
  values.reserve(n_flow);

  for (int i = 0; i < n_flow; ++i) {
    double value = flow[i];
    if (NumericVector::is_na(value)) {
      continue;
    }
    if (rm_zero && value <= 0) {
      continue;
    }
    values.push_back({value, i});
  }

  int n = values.size();
  if (n == 0) {
    return result;
  }

  std::sort(
    values.begin(),
    values.end(),
    [](const FlowValue& a, const FlowValue& b) {
      return a.value > b.value;
    }
  );

  int i = 0;
  while (i < n) {
    int j = i + 1;
    while (j < n && values[j].value == values[i].value) {
      ++j;
    }

    double rank = (static_cast<double>(i + 1) + static_cast<double>(j)) / 2.0;
    double exceedance_probability =
      (rank - alpha) / (static_cast<double>(n) + 1.0 - 2.0 * alpha);

    for (int k = i; k < j; ++k) {
      result[values[k].output_index] = exceedance_probability;
    }

    i = j;
  }

  return result;
}
