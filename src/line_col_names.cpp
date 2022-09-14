#include <cpp11.hpp>
#include <string>
#include <vector>
using namespace cpp11;
[[cpp11::register]]
std::vector<std::string> line_col_names()
{
  std::vector<std::string> x{"line1", "line2", "col1", "col2"};
  return x;
}
