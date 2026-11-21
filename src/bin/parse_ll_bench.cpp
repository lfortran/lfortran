// Benchmark of the prototype LL parser against the Bison GLR parser.
//
//     parse_ll_bench <repetitions> file.f90...
//
// Each file is prescanned and parsed by both parsers. A file takes part in
// the benchmark if the LL parser supports everything in it and both parsers
// return the same AST (compared as JSON, including the locations). The time
// of a file is the best of the repetitions.
#include <algorithm>
#include <chrono>
#include <fstream>
#include <iostream>
#include <sstream>
#include <lfortran/parser/parser.h>
#include <lfortran/parser/ll_parser.h>
#include <lfortran/parser/source_offset.h>
#include <lfortran/pickle.h>
#include <lfortran/utils.h>

using namespace LCompilers;
typedef std::chrono::high_resolution_clock Clock;

static double us(Clock::time_point a, Clock::time_point b) {
    return std::chrono::duration_cast<std::chrono::nanoseconds>(b-a).count()/1000.0;
}

// Replaces every run of 19 or more digits by `#`
static std::string mask_long_numbers(const std::string &s) {
    std::string r;
    size_t i = 0;
    while (i < s.size()) {
        size_t j = i;
        while (j < s.size() && s[j] >= '0' && s[j] <= '9') j++;
        if (j - i >= 19) {
            r += '#';
            i = j;
        } else if (j > i) {
            r.append(s, i, j - i);
            i = j;
        } else {
            r += s[i];
            i++;
        }
    }
    return r;
}

int main(int argc, char **argv) {
    int reps = std::atoi(argv[1]);
    const char *mode_env = getenv("BMODE");
    std::string mode = mode_env ? mode_env : "all";
    const char *list = getenv("LL_LIST");
    double t_glr = 0, t_ll = 0, t_tok = 0;
    size_t files = 0, unsupported = 0, mismatch = 0, failed = 0, bytes = 0,
        lines = 0, tokens = 0;
    LFortran::LLTokens toks;
    std::ofstream list_out;
    if (list) list_out.open(list);
    for (int i = 2; i < argc; i++) {
        std::ifstream f(argv[i]);
        std::stringstream ss;
        ss << f.rdbuf();
        std::string src = ss.str();
        if (src.empty() || src.back() != '\n') src.push_back('\n');
        diag::Diagnostics d;
        LocationManager lm;
        LocationManager::FileLocations fl;
        fl.in_filename = argv[i];
        lm.files.push_back(fl);
        lm.file_ends.push_back(LFortran::source_offset(src.size()));
        std::vector<std::filesystem::path> inc;
        auto pr = LFortran::prescan(src, lm, false, false, inc, d);
        if (!pr.ok) { failed++; continue; }
        const std::string &pre = pr.result;
        CompilerOptions co;
        std::string a, b;
        {
            Allocator al(64*1024);
            auto res = LFortran::parse(al, pre, d, co);
            if (!res.ok) { failed++; continue; }
            a = LFortran::pickle_json(*res.result, lm, false);
        }
        try {
            Allocator al(64*1024);
            diag::Diagnostics d2;
            LFortran::AST::TranslationUnit_t *t = LFortran::ll_parse(al, pre, d2, toks);
            b = LFortran::pickle_json(*t, lm, false);
        } catch (const LFortran::LLUnsupported &e) {
            unsupported++;
            if (getenv("LL_WHY")) {
                size_t a = pre.rfind('\n', e.pos == 0 ? 0 : e.pos - 1);
                a = (a == std::string::npos) ? 0 : a + 1;
                size_t b = pre.find('\n', e.pos);
                std::string line = pre.substr(a, b - a);
                size_t c = line.find_first_not_of(" \t");
                if (c != std::string::npos) line = line.substr(c);
                std::cerr << "UNSUP " << e.line << " "
                    << LFortran::token2text(e.token) << " | " << argv[i]
                    << " | " << line.substr(0, 90) << "\n";
            }
            continue;
        }
        if (a != b) {
            // The JSON of an integer too large for 62 bits is a pointer;
            // compare such trees without their long numbers, and in the
            // other AST format, which prints the integers
            Allocator al1(64*1024), al2(64*1024);
            diag::Diagnostics d3;
            auto r1 = LFortran::parse(al1, pre, d3, co);
            LFortran::AST::TranslationUnit_t *r2 = LFortran::ll_parse(al2, pre,
                d3, toks);
            if (mask_long_numbers(a) == mask_long_numbers(b)
                    && LFortran::pickle(*r1.result) == LFortran::pickle(*r2)) {
                b = a;
            }
        }
        if (a != b) {
            mismatch++;
            if (mismatch <= 100000) std::cerr << "AST mismatch: " << argv[i] << "\n";
            if (getenv("LL_DUMP")) {
                std::ofstream("glr" + std::to_string(mismatch) + ".json") << a;
                std::ofstream("ll" + std::to_string(mismatch) + ".json") << b;
            }
            continue;
        }
        if (list) list_out << argv[i] << "\n";
        double b_glr = 1e30, b_ll = 1e30, b_tok = 1e30;
        for (int r = 0; r < reps; r++) {
            if (mode != "ll") {
                diag::Diagnostics d2;
                Allocator al(64*1024);
                auto t1 = Clock::now();
                auto res = LFortran::parse(al, pre, d2, co);
                auto t2 = Clock::now();
                (void)res;
                b_glr = std::min(b_glr, us(t1, t2));
            }
            if (mode != "glr") {
                diag::Diagnostics d2;
                Allocator al(64*1024);
                auto t1 = Clock::now();
                LFortran::ll_parse(al, pre, d2, toks);
                auto t2 = Clock::now();
                b_ll = std::min(b_ll, us(t1, t2));
            }
            if (mode == "all") {
                diag::Diagnostics d2;
                Allocator al(64*1024);
                auto t1 = Clock::now();
                LFortran::ll_tokenize(al, pre, d2, toks);
                auto t2 = Clock::now();
                b_tok = std::min(b_tok, us(t1, t2));
            }
        }
        files++;
        bytes += pre.size();
        tokens += toks.n;
        lines += std::count(src.begin(), src.end(), '\n');
        t_glr += b_glr; t_ll += b_ll; t_tok += b_tok;
    }
    std::cout << "files " << files << "  unsupported " << unsupported
        << "  AST mismatch " << mismatch << "  failed " << failed << "\n";
    std::cout << "lines " << lines << "  bytes " << bytes << "  tokens "
        << tokens << "\n";
    std::cout << "GLR  tokenize+parse: " << t_glr/1000 << " ms  "
        << t_glr*1000/tokens << " ns/token  " << lines/t_glr << " Mlines/s\n";
    std::cout << "LL   tokenize+parse: " << t_ll/1000 << " ms  "
        << t_ll*1000/tokens << " ns/token  " << lines/t_ll << " Mlines/s\n";
    std::cout << "LL   tokenize only:  " << t_tok/1000 << " ms  "
        << t_tok*1000/tokens << " ns/token\n";
    std::cout << "speedup (GLR/LL): " << t_glr/t_ll << "\n";
    return 0;
}
