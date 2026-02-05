
#include <vector>
#include <fstream>
#include <regex>

#include <Rcpp.h>


namespace rs {

bool is_rs_cmt (const std::string &line);
bool is_empty_rs_cmt (const std::string &line);
bool is_rs_cmt_with_srr (const std::string &line);
void replace_rs_cmt (std::string &line);

} // end namespace rs
