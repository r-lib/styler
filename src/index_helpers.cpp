#include <cpp11.hpp>
#include <cpp11/sexp.hpp>
#include <string>
#include <vector>
using namespace cpp11;

[[cpp11::register]]
std::vector<int> odd_index(const sexp x)
{
  std::vector<int> odd_indices;

  for (int i = 1; i < Rf_length(x); i += 2)
  {
    odd_indices.push_back(i);
  }

  return odd_indices;
}

[[cpp11::register]]
std::vector<int> even_index(const sexp x)
{
  std::vector<int> even_indices;
  for (int i = 2; i < Rf_length(x); i += 2)
  {
    even_indices.push_back(i);
  }

  return even_indices;
}
