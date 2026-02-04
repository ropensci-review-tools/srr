
#include <vector>
#include <fstream>
#include <regex>

#include <Rcpp.h>

namespace c {

bool is_c_cmt (const std::string &line);
void replace_c_cmt (std::string &line);

} // end namespace rs
