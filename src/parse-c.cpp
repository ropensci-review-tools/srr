#include "parse-rmd.h"
#include "parse-c.h"

//' parse_c
//'
//' @noRd
// [[Rcpp::export]]
void rcpp_parse_c (const std::string filename,
        const std::string tempfile)
{
    std::ifstream in_file;
    std::ofstream out_file;
    std::string linetxt;

    in_file.open (filename.c_str (), std::ifstream::in);
    if (in_file.fail ()) {
        Rcpp::stop ("Reading file failed");
    }
    out_file.open (tempfile.c_str (), std::ofstream::out);
    if (out_file.fail ()) {
        Rcpp::stop ("Writing file failed");
    }

    in_file.clear ();
    in_file.seekg (0); 

    bool in_cmts = false;

    while (getline (in_file, linetxt, '\n')) { 
        
        rmd::strip_leading_white (linetxt);

        if (c::is_c_cmt (linetxt)) {
            in_cmts = true;
            c::replace_c_cmt (linetxt);
            out_file << linetxt << std::endl;
        } else {
            if (in_cmts) {
                out_file << "NULL" << std::endl;
                in_cmts = false;
            } else {
                out_file << "" << std::endl;
            }
        }
    }

    in_file.close();
    out_file.close();
}

bool c::is_c_cmt (const std::string &line)
{
    const std::string c_cmt_def = "//'";

    return (line.compare (0, 3, c_cmt_def) == 0);
}

void c::replace_c_cmt (std::string &line)
{
    const std::string c_cmt_def = "//'";
    const std::string r_cmt_def = "#'";
    
    if (line.compare (0, 3, c_cmt_def) == 0) {
        line = r_cmt_def + line.substr(3);
    }
}
