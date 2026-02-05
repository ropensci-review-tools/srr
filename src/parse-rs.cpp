#include "parse-rmd.h"
#include "parse-rs.h"

//' parse_rs
//'
//' @noRd
// [[Rcpp::export]]
void rcpp_parse_rs (const std::string filename,
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
    bool in_srr = false;

    while (getline (in_file, linetxt, '\n')) { 
        
        rmd::strip_leading_white (linetxt);

        if (rs::is_rs_cmt (linetxt)) {
            in_cmts = true;
            bool start_srr = rs::is_rs_cmt_with_srr (linetxt);
            bool line_is_empty = rs::is_empty_rs_cmt (linetxt);
            if (start_srr) {
                in_srr = true;
            }
            if (in_srr) {
                if (!line_is_empty) {
                    rs::replace_rs_cmt (linetxt);
                } else {
                    in_srr = false;
                    linetxt = "#'";
                }
            } else {
                linetxt = "#'";
            }
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

bool rs::is_rs_cmt (const std::string &line)
{
    const std::string ptn = "///";

    return (line.compare (0, 3, ptn) == 0);
}

bool rs::is_empty_rs_cmt (const std::string &line)
{
    const std::string ptn = "///";

    if (line.compare (0, 3, ptn) != 0) {
        return false;
    }

    return std::all_of(line.begin() + 3, line.end(),
        [](unsigned char c) { return std::isspace(c); });
}

bool rs::is_rs_cmt_with_srr (const std::string &line)
{
    const std::string ptn = "/// @srrstats";

    return (line.compare (0, 13, ptn) == 0);
}

void rs::replace_rs_cmt (std::string &line)
{
    const std::string rs_cmt_def = "///";
    const std::string r_cmt_def = "#'";
    
    if (line.compare (0, 3, rs_cmt_def) == 0) {
        line = r_cmt_def + line.substr(3);
    }
}
