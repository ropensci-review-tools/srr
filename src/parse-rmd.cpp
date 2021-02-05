
#include <vector>

#include <Rcpp.h>

//' parse_rmd
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::CharacterVector rcpp_parse_rmd (const Rcpp::CharacterVector rmd)
{

    std::vector <std::string> out;

    return Rcpp::wrap (out);
}
