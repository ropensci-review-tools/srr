#include "parse-rmd.h"

//' parse_rmd
//'
//' @noRd
// [[Rcpp::export]]
void rcpp_parse_rmd (const std::string filename,
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


    int chunk_len = 0;
    bool in_chunk = false;
    while (getline (in_file, linetxt, '\n')) { 
        
        rmd::strip_leading_white (linetxt);

        if (rmd::chunk_start (linetxt))
        {
            in_chunk = true;
        }

        if (!in_chunk)
        {
            out_file << "" << std::endl;
        } else {

            bool end_chunk = (chunk_len > 0 && rmd::chunk_end (linetxt));

            if (chunk_len == 0)
            {
                out_file << "" << std::endl;
            } else if (!end_chunk)
            {
                out_file << linetxt << std::endl;
            } else
            {
                out_file << "NULL" << std::endl;
            }
            chunk_len++;

            if (end_chunk) {
                chunk_len = 0;
                in_chunk = false;
            }
        }
    }

    in_file.close();
    out_file.close();
}

void rmd::strip_leading_white (std::string &line)
{
    std::regex_replace (line, std::regex("^\\s+"), std::string(""));
}

bool rmd::chunk_start (const std::string &line)
{
    const std::string chunk_start_def = "```{";

    return (line.compare (0, 4, chunk_start_def) == 0);
}

bool rmd::chunk_end (const std::string &line)
{
    const std::string chunk_end_def = "```";

    return (line.compare (0, 3, chunk_end_def) == 0);
}
