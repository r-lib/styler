#include <cpp11.hpp>
#include <string>
#include <vector>
#include <algorithm>
using namespace cpp11;

//' Ensure there is one (and only one) blank line at the end of a vector
//' @examples
//' styler:::ensure_last_n_empty("")
//' styler:::ensure_last_n_empty(letters)
//' styler:::ensure_last_n_empty(c(letters, "", "", ""))
//' @keywords internal
[[cpp11::register]]
std::vector<std::string> ensure_last_n_empty(std::vector<std::string> x, int n = 1)
{

  // Return early if all elements are empty
  // There should be only single empty element, no matter the value of `n`
  if (std::all_of(x.begin(), x.end(), [](std::string s) { return s == ""; }))
  {
    std::vector<std::string> x{""};
    return x;
  }

  // If an empty element isn't found at the end, add `n` of them
  if (x.back() != "" && n > 0)
  {
    std::fill_n(std::back_inserter(x), n, "");
    return x;
  }

  auto it = std::find_if(x.rbegin(), x.rend(), [](std::string s) { return s != ""; });
  x.erase(it.base(), x.end());
  std::fill_n(std::back_inserter(x), n, "");
  return x;
}
