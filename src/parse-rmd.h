
#include <vector>
#include <fstream>
#include <regex>

#include <Rcpp.h>


namespace rmd {

void strip_leading_white (std::string &line);

bool chunk_start (const std::string &line);

bool chunk_end (const std::string &line);

} // end namespace rmd
