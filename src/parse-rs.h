
#include <vector>
#include <fstream>
#include <regex>

#include <Rcpp.h>


namespace rs {

bool is_rs_cmt (const std::string &line);
void replace_rs_cmt (std::string &line);

} // end namespace rs
