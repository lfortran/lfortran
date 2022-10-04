std::string
CPreprocessor::preprocess_include(const std::string& input,
                                  LocationManager& lm,
                                  bool fixed_form) const
{
    LFORTRAN_ASSERT(input[input.size()] == '\0');
    unsigned char* string_start = (unsigned char*) (&input[0]);
    unsigned char* cur = string_start;
    std::string output;
    lm.preprocessor = true;
    get_newlines(input, lm.in_newlines0);
	lm.out_start0.clear();
    lm.in_start0.clear();
    lm.interval_type0.clear();
    lm.in_size0.clear();
    lm.out_start0.push_back(0);
    lm.in_start0.push_back(0);

    auto process = [&](std::string& include_filename, unsigned char* tok) {
        std::string current_filename = lm.in_filename;
        std::string include_root = std::filesystem::path(include_filename).root_name();
        std::string include_path_str;
        if (include_root.empty()) {
            std::filesystem::path include_path = std::filesystem::path(current_filename).parent_path();
            include_path.append(include_filename);
            include_path_str = include_path.string();
        } else {
            include_path_str = include_filename;
        }

        std::string include;
        if (!read_file(include_path_str, include)) {
            throw LCompilersException("Include file '" + include_filename
                                      + "' cannot be opened");
        }

        include = fix_continuation(include, lm, fixed_form);
        LocationManager lm_tmp;
        lm_tmp.in_filename = include_path_str;
        include = preprocess_include(include, lm_tmp, fixed_form);

        // Prepare the start of the interval
        interval_end_type_0(lm, output.size(), tok - string_start);

        // Include
        output.append(include);

        // Prepare the end of the interval
        interval_end(lm, output.size(), cur - string_start, token(tok, cur).size() - 1, 1);
    };

    for (;;) {
        unsigned char* tok = cur;
        unsigned char* mar;
        unsigned char *t1, *t2;
        /*!stags:re2c format = 'unsigned char *@@;\n'; */
        /*!re2c
            * {
                output.append(token(tok, cur));
                continue;
            }
            end {
                break;
            }
            [!#] [^\n\x00]* newline {
                output.append(token(tok, cur));
                continue;
            }
            'include' whitespace "'" @t1 [^'\x00]* @t2 "'" [^\n\x00]* newline {
                std::string filename = token(t1, t2);
                process(filename, tok);
                continue;
            }
            'include' whitespace '"' @t1 [^"\x00]* @t2 '"' [^\n\x00]* newline {
                std::string filename = token(t1, t2);
                process(filename, tok);
                continue;
            }
        */
    }
    lm.out_start0.push_back(output.size());
    lm.in_start0.push_back(input.size());
    // The just created interval ID:
    size_t N = lm.out_start0.size() - 2;
    lm.in_size0.push_back(lm.out_start0[N + 1] - lm.out_start0[N]);
    lm.interval_type0.push_back(0);

    std::cout << "INCLUDE" << std::endl;
    std::cout << "============" << std::endl;
    std::cout << "in_start0: ";
    for (auto A : lm.in_start0) { std::cout << A << " "; }
    std::cout << std::endl;
    std::cout << "in_size0: ";
    for (auto A : lm.in_size0) { std::cout << A << " "; }
    std::cout << std::endl;
    std::cout << "interval_type0: ";
    for (auto A : lm.interval_type0) { std::cout << A << " "; }
    std::cout << std::endl;
    std::cout << "out_start0: ";
    for (auto A : lm.out_start0) { std::cout << A << " "; }
    std::cout << std::endl;
    std::cout << "============" << std::endl;

    return output;
}
