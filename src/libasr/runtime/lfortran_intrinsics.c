#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <complex.h>
#include <string.h>
#include <inttypes.h>
#include <time.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include <errno.h>  
#include <limits.h>
#include <stdint.h>

#define PI 3.14159265358979323846
#if defined(_WIN32)
#  include <winsock2.h>
#  include <io.h>
#  define ftruncate _chsize_s
#else
#  include <unistd.h>
#endif

#if defined(__APPLE__)
#  include <sys/time.h>
#endif

#include <libasr/runtime/lfortran_intrinsics.h>
#include <libasr/config.h>

#if defined (WITH_LFORTRAN_ASSERT)
    #define lfortran_assert(cond, msg)\
        if(!(cond)){\
            lfortran_error(msg);\
        }
#else
    #define lfortran_assert(cond, msg) ((void)0);
#endif

#ifdef HAVE_RUNTIME_STACKTRACE

#ifdef COMPILE_TO_WASM
    #undef HAVE_LFORTRAN_MACHO
    #undef HAVE_LFORTRAN_LINK
#endif

#ifdef HAVE_LFORTRAN_LINK
// For dl_iterate_phdr() functionality
#  include <link.h>
struct dl_phdr_info {
    ElfW(Addr) dlpi_addr;
    const char *dlpi_name;
    const ElfW(Phdr) *dlpi_phdr;
    ElfW(Half) dlpi_phnum;
};
extern int dl_iterate_phdr (int (*__callback) (struct dl_phdr_info *,
    size_t, void *), void *__data);
#endif

#ifdef HAVE_LFORTRAN_UNWIND
// For _Unwind_Backtrace() function
#  include <unwind.h>
#endif

#ifdef HAVE_LFORTRAN_MACHO
#  include <mach-o/dyld.h>
#endif

// Runtime Stacktrace
#define LCOMPILERS_MAX_STACKTRACE_LENGTH 200
char *source_filename;
char *binary_executable_path = "/proc/self/exe";

struct Stacktrace {
    uintptr_t pc[LCOMPILERS_MAX_STACKTRACE_LENGTH];
    uint64_t pc_size;
    uintptr_t current_pc;

    uintptr_t local_pc[LCOMPILERS_MAX_STACKTRACE_LENGTH];
    char *binary_filename[LCOMPILERS_MAX_STACKTRACE_LENGTH];
    uint64_t local_pc_size;

    uint64_t addresses[LCOMPILERS_MAX_STACKTRACE_LENGTH];
    uint64_t line_numbers[LCOMPILERS_MAX_STACKTRACE_LENGTH];
    uint64_t stack_size;
};

// Styles and Colors
#define DIM "\033[2m"
#define BOLD "\033[1m"
#define S_RESET "\033[0m"
#define MAGENTA "\033[35m"
#define C_RESET "\033[39m"

#endif // HAVE_RUNTIME_STACKTRACE

#ifdef HAVE_LFORTRAN_MACHO
    #define INT64 "%lld"
#elif HAVE_BUILD_TO_WASM
    #define INT64 "%lld"
#else
    #define INT64 "%ld"
#endif

// This function performs case insensitive string comparison
bool streql(const char *s1, const char* s2) {
#if defined(_MSC_VER)
    return _stricmp(s1, s2) == 0;
#else
    return strcasecmp(s1, s2) == 0;
#endif
}

LFORTRAN_API double _lfortran_sum(int n, double *v)
{
    int i, r;
    r = 0;
    for (i=0; i < n; i++) {
        r += v[i];
    }
    return r;
}

LFORTRAN_API void _lfortran_random_number(int n, double *v)
{
    int i;
    for (i=0; i < n; i++) {
        v[i] = rand() / (double) RAND_MAX;
    }
}

LFORTRAN_API int _lfortran_init_random_seed(unsigned seed)
{
    srand(seed);
    return seed;
}

LFORTRAN_API void _lfortran_init_random_clock()
{
    unsigned int count;
#if defined(_WIN32)
    count = (unsigned int)clock();
#else
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        count = (unsigned int)(ts.tv_nsec);
    } else {
        count = (unsigned int)clock();
    }
#endif
    srand(count);
}

LFORTRAN_API double _lfortran_random()
{
    return (rand() / (double) RAND_MAX);
}

LFORTRAN_API int _lfortran_randrange(int lower, int upper)
{
    int rr = lower + (rand() % (upper - lower));
    return rr;
}

LFORTRAN_API int _lfortran_random_int(int lower, int upper)
{
    int randint = lower + (rand() % (upper - lower + 1));
    return randint;
}

LFORTRAN_API void _lfortran_printf(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    char* str = va_arg(args, char*);
    char* end = va_arg(args, char*);
    if(str == NULL){
        str = " "; // dummy output
    }
    // Detect "\b" to raise error
    if(str[0] == '\b'){
        str = str+1;
        fprintf(stderr, "%s", str);
        exit(1);
    }
    fprintf(stdout, format, str, end);
    fflush(stdout);
    va_end(args);
}

char* substring(const char* str, int start, int end) {
    int len = end - start;
    char* substr = (char*)malloc((len + 1) * sizeof(char));
    strncpy(substr, str + start, len);
    substr[len] = '\0';
    return substr;
}

char* append_to_string(char* str, const char* append) {
    int len1 = strlen(str);
    int len2 = strlen(append);
    str = (char*)realloc(str, (len1 + len2 + 1) * sizeof(char));
    strcat(str, append);
    return str;
}

void handle_integer(char* format, int64_t val, char** result, bool is_signed_plus) {
    int width = 0, min_width = 0;
    char* dot_pos = strchr(format, '.');
    int len;
    int sign_width = (val < 0) ? 1 : 0;
    bool sign_plus_exist = (is_signed_plus && val >= 0);
    if (val == 0) {
        len = 1;
    } else if (val == INT64_MIN) {
        len = 19;
    } else {
        len = (int)log10(llabs(val)) + 1;
    }
    if (dot_pos != NULL) {
        dot_pos++;
        width = atoi(format + 1);
        min_width = atoi(dot_pos);
        if (min_width > width && width != 0) {
            perror("Minimum number of digits cannot be more than the specified width for format.\n");
        }
    } else {
        width = atoi(format + 1);
        if (width == 0) {
            width = len + sign_width + sign_plus_exist;
        }
    }
    if (width >= len + sign_width + sign_plus_exist || width == 0) {
        if (min_width > len) {
            for (int i = 0; i < (width - min_width - sign_width - sign_plus_exist); i++) {
                *result = append_to_string(*result, " ");
            }
            
            if (val < 0) {
                *result = append_to_string(*result, "-");
            } else if(sign_plus_exist){
                *result = append_to_string(*result, "+");
            }

            for (int i = 0; i < (min_width - len); i++) {
                *result = append_to_string(*result, "0");
            }
        } else {
            for (int i = 0; i < (width - len - sign_width - sign_plus_exist); i++) {
                *result = append_to_string(*result, " ");
            }
            if (val < 0) {
                *result = append_to_string(*result, "-");
            } else if (sign_plus_exist){
                *result = append_to_string(*result, "+");
            }
        }
        char str[20];
        if (val == INT64_MIN) {
            sprintf(str, "9223372036854775808");
        } else {
            sprintf(str, "%lld", llabs(val));
        }
        *result = append_to_string(*result, str);
    } else {
        for (int i = 0; i < width; i++) {
            *result = append_to_string(*result, "*");
        }
    }
}

void handle_logical(char* format, bool val, char** result) {
    int width = atoi(format + 1);
    for (int i = 0; i < width - 1; i++) {
        *result = append_to_string(*result, " ");
    }
    if (val) {
        *result = append_to_string(*result, "T");
    } else {
        *result = append_to_string(*result, "F");
    }
}

void handle_float(char* format, double val, int scale, char** result, bool use_sign_plus) {
    val = val * pow(10, scale); // scale the value
    if (strcmp(format,"f-64") == 0) { //use c formatting.
        char* float_str = (char*)malloc(50 * sizeof(char));
        sprintf(float_str,"%23.17e",val);
        *result = append_to_string(*result,float_str);
        free(float_str);
        return;
    } else if(strcmp(format,"f-32") == 0){ //use c formatting.
        char* float_str = (char*)malloc(40 * sizeof(char));
        sprintf(float_str,"%13.8e",val);
        *result = append_to_string(*result,float_str);
        free(float_str);
        return;
    }
    int width = 0, decimal_digits = 0;
    long integer_part = (long)fabs(val);
    double decimal_part = fabs(val) - integer_part;

    int sign_width = (val < 0) ? 1 : 0; // Negative sign
    bool sign_plus_exist = (use_sign_plus && val>=0); // Positive sign
    int integer_length = (integer_part == 0) ? 1 : (int)log10(integer_part) + 1;

    // parsing the format
    char* dot_pos = strchr(format, '.');
    if (dot_pos != NULL) {
        decimal_digits = atoi(dot_pos + 1);
        width = atoi(format + 1);
    }

    double rounding_factor = pow(10, -decimal_digits);
    decimal_part = round(decimal_part / rounding_factor) * rounding_factor;

    if (decimal_part >= 1.0) {
        integer_part += 1;
        decimal_part -= 1.0;
    }

    char int_str[64];
    sprintf(int_str, "%ld", integer_part);

    // TODO: This will work for up to `F65.60` but will fail for:
    // print "(F67.62)", 1.23456789101112e-62_8
    char dec_str[64];
    sprintf(dec_str, "%.*f", decimal_digits, decimal_part);
    // removing the leading "0." from the formatted decimal part
    memmove(dec_str, dec_str + 2, strlen(dec_str)-1);

    // Determine total length needed
    int total_length =  sign_width      + 
                        integer_length  +
                        1 /*dot `.`*/   +
                        decimal_digits  +
                        sign_plus_exist ;
    
    if (width == 0) {
        width = total_length;
    }

    char formatted_value[128] = "";

    int spaces = width - total_length;
    for (int i = 0; i < spaces; i++) {
        strcat(formatted_value, " ");
    }
    if(sign_plus_exist){
        strcat(formatted_value, "+");
    }
    if (val < 0) {
        strcat(formatted_value, "-");
    }
    if (integer_part == 0 && decimal_part!= 0 && format[1] == '0') {
        strcat(formatted_value, "");
    } else {
        strcat(formatted_value, int_str);
    }
    strcat(formatted_value, ".");
    if (decimal_digits != 0){
        strcat(formatted_value, dec_str);
    }

    // checking for overflow
    if (strlen(formatted_value) > width) {
        for (int i = 0; i < width; i++) {
            *result = append_to_string(*result, "*");
        }
    } else {
        *result = append_to_string(*result, formatted_value);
    }
}

void parse_decimal_or_en_format(char* format, int* width_digits, int* decimal_digits, int* exp_digits) {
    *width_digits = -1;
    *decimal_digits = -1;
    *exp_digits = -1;

    char *width_digits_pos = format;
    while (!isdigit(*width_digits_pos)) {
        width_digits_pos++;
    }
    *width_digits = atoi(width_digits_pos);

    // dot_pos exists, we previously checked for it in `parse_fortran_format`
    char *dot_pos = strchr(format, '.');
    *decimal_digits = atoi(++dot_pos);

    char *exp_pos = strchr(dot_pos, 'e');
    if (exp_pos == NULL) {
        exp_pos = strchr(dot_pos, 'E');
    }
    if(exp_pos != NULL) {
        *exp_digits = atoi(++exp_pos);
    }
}

/*
`handle_en` - Formats a floating-point number using a Fortran-style "EN" format.

NOTE: The function allocates memory for the formatted result, which is returned via
the `result` parameter. It is the responsibility of the caller to free this memory
using `free(*result)` after it is no longer needed.
*/
void handle_en(char* format, double val, int scale, char** result, char* c, bool is_signed_plus) {
    int width, decimal_digits, exp_digits;
    parse_decimal_or_en_format(format, &width, &decimal_digits, &exp_digits);

    // Default fallback if 0
    bool is_g0_like = (width == 0 && decimal_digits == 0 && exp_digits == 0);
    if (decimal_digits <= 0) decimal_digits = 9;
    if (exp_digits == 0) exp_digits = 2;
    else if (exp_digits == -1) exp_digits = 3;

    bool sign_plus_exist = (is_signed_plus && val >= 0); // SP specifier

    char formatted_value[256];
    double abs_val = fabs(val);
    if (is_g0_like) {
        if (abs_val == 0.0 || (abs_val >= 1.0 && abs_val < 1000.0)) {
            snprintf(formatted_value, sizeof(formatted_value), "%.9f", val);
        } else {
            // Engineering notation: scale exponent to multiple of 3
            int exponent = (int)floor(log10(abs_val));
            int remainder = exponent % 3;
            if (remainder < 0) remainder += 3;
            exponent -= remainder;
            double scaled_val = val / pow(10, exponent);

            char val_str[128];
            snprintf(val_str, sizeof(val_str), "%.9f", scaled_val);
            snprintf(formatted_value, sizeof(formatted_value),
                    "%s%s%+d", val_str, c, exponent);  // no padding, plain exponent
        }
    } else {
        int exponent = 0;
        double scaled_val = val;
        if (abs_val != 0.0) {
            exponent = (int)floor(log10(abs_val));
            int remainder = exponent % 3;
            if (remainder < 0) remainder += 3;
            exponent -= remainder;
            scaled_val = val / pow(10, exponent);
        }
        char val_str[128];
        snprintf(val_str, sizeof(val_str), "%.*f", decimal_digits, scaled_val);
        snprintf(formatted_value, sizeof(formatted_value),
                "%s%s%+0*d", val_str, c, exp_digits, exponent);
    }

    // Width == 0, no padding
    if (width == 0) {
        if (sign_plus_exist) {
            char* temp = malloc(strlen(formatted_value) + 2);
            temp[0] = '+';
            strcpy(temp + 1, formatted_value);
            *result = append_to_string(*result, temp);
            free(temp);
        } else {
            *result = append_to_string(*result, formatted_value);
        }
        return;
    }

    // Check for overflow
    int total_len = strlen(formatted_value);
    if (sign_plus_exist) total_len += 1;
    
    if (total_len > width) {
        // Overflow: fill with '*'
        char* final_result = malloc(width + 1);
        memset(final_result, '*', width);
        final_result[width] = '\0';
        *result = append_to_string(*result, final_result);
        free(final_result);
        return;
    }

    // Allocate and pad properly
    char* final_result = malloc(width + 1);
    int padding = width - strlen(formatted_value) - sign_plus_exist;
    if (padding > 0) {
        memset(final_result, ' ', padding);
        if (sign_plus_exist) final_result[padding] = '+';
        strcpy(final_result + padding + sign_plus_exist, formatted_value);
    } else {
        if (sign_plus_exist) final_result[0] = '+';
        strncpy(final_result + is_signed_plus, formatted_value, width);
        final_result[width] = '\0';
    }

    *result = append_to_string(*result, final_result);
    free(final_result);
}

void handle_decimal(char* format, double val, int scale, char** result, char* c, bool is_signed_plus) {
    // Consider an example: write(*, "(es10.2)") 1.123e+10
    // format = "es10.2", val = 11230000128.00, scale = 0, c = "E"

    int width_digits, decimal_digits, exp_digits;
    parse_decimal_or_en_format(format, &width_digits, &decimal_digits, &exp_digits);
    int width = width_digits;
    int digits = decimal_digits;
    int sign_width = (val < 0) ? 1 : 0;
    bool sign_plus_exist = (is_signed_plus && val>=0); // Positive sign
    // sign_width = 0
    double integer_part = trunc(val);
    int integer_length = (integer_part == 0) ? 1 : (int)log10(fabs(integer_part)) + 1;
    // integer_part = 11230000128, integer_length = 11
    // width = 10, digits = 2

    #define MAX_SIZE 512
    char val_str[MAX_SIZE] = "";
    int avail_len_decimal_digits = MAX_SIZE - integer_length - sign_width - 2 /* 0.*/;
    sprintf(val_str, "%.*lf", avail_len_decimal_digits, val);
    // val_str = "11230000128.00..."
    int i = strlen(val_str) - 1;
    if (val != 0.0) {
        while (val_str[i] == '0') {
            val_str[i] = '\0';
            i--;
        }
    }
    // val_str = "11230000128."

    char* ptr = strchr(val_str, '.');
    if (ptr != NULL) {
        memmove(ptr, ptr + 1, strlen(ptr));
    }
    // val_str = "11230000128"

    if (val < 0) {
        // removes `-` (negative) sign
        memmove(val_str, val_str + 1, strlen(val_str));
    }

    int decimal = 1;
    while (val_str[0] == '0') {
        // Used for the case: 1.123e-10
        memmove(val_str, val_str + 1, strlen(val_str));
        decimal--;
        // loop end: decimal = -9
    }
    bool is_s_format = false;
    if (tolower(format[1]) == 's') {
        is_s_format = true;
        scale = 1;
    }
    int exponent_value = 0;
    if (val == 0.0) {
        exponent_value = 0;
        memset(val_str, '0', digits + scale + 1); // +1 in case scale=0
        val_str[digits + scale] = '\0';
        integer_length = 1;
    } else {
        exponent_value = (int)floor(log10(fabs(val))) - scale + 1;
    }

    int exp = 2;
    if (exp_digits > 0) {
        exp = exp_digits;
    } else if (is_s_format && abs(exponent_value) >= 10) {
        int abs_exp = abs(exponent_value);
        exp = (abs_exp == 0) ? 2 : (int)log10(abs_exp) + 1;
    } else if (abs(exponent_value >= 100)) {
        exp = 3;
    }
    // exp = 2;
    if (exp != -1 && exponent_value >= (pow(10, exp))) {
        goto overflow;
    }

    char exponent[12];
    if (width_digits == 0) {
        sprintf(exponent, "%+02d", exponent_value);
    } else {
        int exp_width = exp + 1;
        if (exp_width > 10) exp_width = 10;

        int len = snprintf(exponent, sizeof(exponent), "%+0*d", exp_width, exponent_value);
        if (len < 0 || len >= sizeof(exponent)) {
            goto overflow;
        }
        // exponent = "+10"
    }

    int FIXED_CHARS_LENGTH = 1 + 1 + 1; // digit, ., E
    int exp_length = strlen(exponent);

    if (width == 0) {
        if (digits == 0) {
            digits = 9;
        }
        width = sign_width + digits + FIXED_CHARS_LENGTH + exp_length;
    }
    if (digits > width - FIXED_CHARS_LENGTH) {
        goto overflow;
    }
    int val_str_len = strlen(val_str);
    int zeroes_needed = digits - (val_str_len - integer_length);
    if (zeroes_needed < 0) zeroes_needed = 0;
    if (zeroes_needed > MAX_SIZE - val_str_len - 1) zeroes_needed = MAX_SIZE - val_str_len - 1;

    for(int i = 0; i < zeroes_needed && val_str_len + i < MAX_SIZE - 1; i++) {
        val_str[val_str_len + i] = '0';
    }
    val_str[val_str_len + zeroes_needed] = '\0';

    char formatted_value[512] = "";
    int spaces = width - (sign_width + digits + FIXED_CHARS_LENGTH + exp_length + sign_plus_exist);
    // spaces = 2
    for (int i = 0; i < spaces; i++) {
        strcat(formatted_value, " ");
    }

    if (scale > 1) {
        digits -= scale - 1;
    }

    if (sign_width == 1) {
        // adds `-` (negative) sign
        strcat(formatted_value, "-");
    } else if(sign_plus_exist){ // `SP specifier`
        strcat(formatted_value, "+");
    }

    if (scale <= 0) {
        strcat(formatted_value, "0.");
        for (int k = 0; k < abs(scale); k++) {
            strcat(formatted_value, "0");
        }
        int zeros = 0;
        while(val_str[zeros] == '0') zeros++;
        // TODO: figure out a way to round decimals with value < 1e-15
        if (digits + scale < strlen(val_str) && val != 0 && digits + scale - zeros<= 15) {
            val_str[15] = '\0';
            long long t = (long long)round((long double)atoll(val_str) / (long long)pow(10, (strlen(val_str) - digits - scale)));
            sprintf(val_str, "%lld", t);
            int index = zeros;
            while(index--) strcat(formatted_value, "0");
        }
        strncat(formatted_value, val_str, digits);
    } else {
        char* temp = substring(val_str, 0, scale);
        strcat(formatted_value, temp);
        strcat(formatted_value, ".");
        // formatted_value = "  1."
        char* new_str = substring(val_str, scale, strlen(val_str));
        // new_str = "1230000128" case:  1.123e+10
        int zeros = 0;
        if (digits < strlen(new_str) && digits + scale <= 15) {
            new_str[15] = '\0';
            zeros = strspn(new_str, "0");
            long long t = (long long)round((long double)atoll(new_str) / (long long) pow(10, (strlen(new_str) - digits)));
            sprintf(new_str, "%lld", t);
            // new_str = 12
            int index = zeros;
            while(index--) {
                memmove(new_str + 1, new_str, strlen(new_str)+1);
                new_str[0] = '0';
            }
        }
        new_str[digits] = '\0';
        strcat(formatted_value, new_str);
        // formatted_value = "  1.12"
        free(new_str);
        free(temp);
    }

    if (!(width_digits == 0 && decimal_digits == 0 && exponent_value == 0)) {
        if (abs(exponent_value) < 100 || exp_length < 4 || width_digits == 0) {
            strcat(formatted_value, c);
        }
        // formatted_value = "  1.12E"
        strcat(formatted_value, exponent);
        // formatted_value = "  1.12E+10"
    }
    if (strlen(formatted_value) > width) {
        if (strlen(formatted_value) - width == 1 && formatted_value[0] == '0') {
            memmove(formatted_value, formatted_value + 1, strlen(formatted_value));
            *result = append_to_string(*result, formatted_value);
            return;
        } else {
            goto overflow;
        }
    } else {
        *result = append_to_string(*result, formatted_value);
        return;
        // result = "  1.12E+10"
    }

    overflow:
    for (int i = 0; i < width; i++) {
        *result = append_to_string(*result, "*");
    }
    return;
}

void handle_SP_specifier(char** result, bool is_positive_value){
    char positive_sign_string[] = "+";
    if(is_positive_value) append_to_string(*result, positive_sign_string);
}
/*
Ignore blank space characters within format specification, except
within character string edit descriptor

E.g.; "('Number : ', I 2, 5 X, A)" becomes '('Number : ', I2, 5X, A)'
*/
char* remove_spaces_except_quotes(const fchar* format, const int64_t len, int* cleaned_format_len) {
    char* cleaned_format = malloc(len + 1);   // +1 for optional '\0' at end

    int j = 0;
    // don't remove blank spaces from within character
    // string editor descriptor
    bool in_quotes = false;
    char current_quote = '\0';

    for (int i = 0; i < len; i++) {
        char c = format[i];
        if (c == '"' || c == '\'') {
            if (i == 0 || format[i - 1] != '\\') {
                // toggle in_quotes and set current_quote on entering or exiting quotes
                if (!in_quotes) {
                    in_quotes = true;
                    current_quote = c;
                } else if (current_quote == c) {
                    in_quotes = false;
                }
            }
        }

        if (!isspace(c) || in_quotes) {
            cleaned_format[j++] = c; // copy non-space characters or any character within quotes
        }
    }

    cleaned_format[j] = '\0';       // optional for printing or debugging
    *cleaned_format_len = j;
    return cleaned_format;
}

int find_matching_parentheses(const fchar* format, const int64_t format_len, int index){
    int parenCount = 0;
    while (index < format_len) {
        if (format[index] == '(') {
            parenCount++;
        } else if (format[index] == ')'){
            parenCount--;
        }
        index++;
        if (parenCount == 0)
            break;
    }
    if (parenCount != 0) {
        fprintf(stderr, "Error: Unbalanced paranthesis in format string\n");
        exit(1);
    }
    return index;
}

/**
 * parse fortran format string by extracting individual 'format specifiers'
 * (e.g. 'i', 't', '*' etc.) into an array of strings
 *
 * `fchar* format`: the fortran string we need to split into format specifiers (it should be null-independant)
 * `int* count`  : store count of format specifiers (passed by reference from caller)
 * `item_start`  :
 *
 * e.g. "(I5, F5.2, T10)" is split separately into "I5", "F5.2", "T10" as
 * format specifiers
*/
char** parse_fortran_format(const fchar* format, const int64_t format_len, int64_t *count, int64_t *item_start) {
    const char* cformat = (const char*)format;
    char** format_values_2 = (char**)malloc((*count + 1) * sizeof(char*));
    int format_values_count = *count;
    int index = 0 , start = 0;

    while (index < format_len) {
        char** ptr = (char**)realloc(format_values_2, (format_values_count + 1) * sizeof(char*));
        if (ptr == NULL) {
            perror("Memory allocation failed.\n");
            free(format_values_2);
        } else {
            format_values_2 = ptr;
        }
        switch (tolower(cformat[index])) {
            case ',' :
                break;
            case '/' :
            case ':' :
                format_values_2[format_values_count++] = substring(cformat, index, index+1);
                break;
            case '*' :
                format_values_2[format_values_count++] = substring(cformat, index, index+1);
                break;
            case '"' :
                start = index++;
                while (cformat[index] != '"') {
                    index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index+1);

                break;
            case '\'' :
                start = index++;
                while (cformat[index] != '\'') {
                    index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index+1);
                break;
            case 'a' :
                start = index++;
                while (isdigit(cformat[index])) {
                    index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index);
                index--;
                break;
            case 'e' :
                start = index++;
                bool edot = false;
                if (tolower(cformat[index]) == 'n') index++;
                if (tolower(cformat[index]) == 's') index++;
                while (isdigit(cformat[index])) index++;
                if (cformat[index] == '.') {
                    edot = true;
                    index++;
                } else {
                    printf("Error: Period required in format specifier\n");
                    exit(1);
                }
                while (isdigit(cformat[index])) index++;
                if (edot && (tolower(cformat[index]) == 'e' || tolower(cformat[index]) == 'n')) {
                    index++;
                    while (isdigit(cformat[index])) index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index);
                index--;
                break;
            case 'i' :
            case 'd' :
            case 'f' :
            case 'l' :
            case 'b' :
            case 'g' :
                start = index++;
                bool dot = false;
                if(tolower(cformat[index]) == 's') index++;
                while (isdigit(cformat[index])) index++;
                if (cformat[index] == '.') {
                    dot = true;
                    index++;
                }
                while (isdigit(cformat[index])) index++;
                if (dot && tolower(cformat[index]) == 'e') {
                    index++;
                    while (isdigit(cformat[index])) index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index);
                index--;
                break;
            case 's': 
                start = index++;
                if( cformat[index] == ','          || // 'S'  (default sign)
                    tolower(cformat[index]) == 'p' || // 'SP' (sign plus)
                    tolower(cformat[index]) == 's'    // 'SS' (sign suppress)
                    ){
                    if(cformat[index] == ',') --index; // Don't consume
                    format_values_2[format_values_count++] = substring(cformat, start, index+1);
                } else {
                    fprintf(stderr, "Error: Invalid format specifier. After 's' specifier\n");
                    exit(1);
                }
                break;
            case '(' :
                start = index;
                index = find_matching_parentheses(format, format_len, index);
                format_values_2[format_values_count++] = substring(cformat, start, index);
                *item_start = format_values_count;
                break;
            case 't' :
                // handle 'T', 'TL' & 'TR' editing see section 13.8.1.2 in 24-007.pdf
                start = index++;
                if (tolower(cformat[index]) == 'l' || tolower(cformat[index]) == 'r') {
                     index++;  // move past 'L' or 'R'
                }
                // raise error when "T/TL/TR" is specified itself or with
                // non-positive width
                if (!isdigit(cformat[index])) {
                    // TODO: if just 'T' is specified the error message will print 'T,', fix it
                    printf("Error: Positive width required with '%c%c' descriptor in format string\n",
                        cformat[start], cformat[start + 1]);
                    exit(1);
                }
                while (isdigit(cformat[index])) {
                    index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index);
                index--;
                break;
            default :
                if (
                    (cformat[index] == '-' && isdigit(cformat[index + 1]) && tolower(cformat[index + 2]) == 'p')
                    || ((isdigit(cformat[index])) && tolower(cformat[index + 1]) == 'p')) {
                    start = index;
                    index = index + 1 + (cformat[index] == '-');
                    format_values_2[format_values_count++] = substring(cformat, start, index + 1);
                } else if (isdigit(cformat[index])) {
                    start = index;
                    while (isdigit(cformat[index])) index++;
                    char* repeat_str = substring(cformat, start, index);
                    int repeat = atoi(repeat_str);
                    free(repeat_str);
                    format_values_2 = (char**)realloc(format_values_2, (format_values_count + repeat + 1) * sizeof(char*));
                    if (cformat[index] == '(') {
                        start = index;
                        index = find_matching_parentheses(format, format_len, index);
                        *item_start = format_values_count+1;
                        for (int i = 0; i < repeat; i++) {
                            format_values_2[format_values_count++] = substring(cformat, start, index);
                        }
                    } else {
                        start = index;
                        while (isalpha(cformat[index])) index++; 
                        if (isdigit(cformat[index])) {
                            while (isdigit(cformat[index])) index++;
                            if (cformat[index] == '.') index++;
                            while (isdigit(cformat[index])) index++;
                            if (cformat[index] == 'e' || cformat[index] == 'E') index++;
                            while (isdigit(cformat[index])) index++;
                        }
                        for (int i = 0; i < repeat; i++) {
                            format_values_2[format_values_count++] = substring(cformat, start, index);
                        }
                        index--;
                    }
                } else if (cformat[index] != ' ') {
                    fprintf(stderr, "Unsupported or unrecognized `%c` in format string\n", cformat[index]);
                    break;
                }
        }
        index++;
    }
    *count = format_values_count;
    return format_values_2;
}

typedef enum primitive_types{
    INTEGER_64_TYPE = 0,
    INTEGER_32_TYPE = 1,
    INTEGER_16_TYPE = 2,
    INTEGER_8_TYPE = 3,
    FLOAT_64_TYPE = 4,
    FLOAT_32_TYPE = 5,
    CHAR_PTR_TYPE = 6,
    LOGICAL_TYPE = 7,
    CPTR_VOID_PTR_TYPE = 8,
    NONE_TYPE = 9,
    STRING_DESCRIPTOR_TYPE = 10,
    UNSIGNED_INTEGER_64_TYPE = 11,
    UNSIGNED_INTEGER_32_TYPE = 12,
    UNSIGNED_INTEGER_16_TYPE = 13,
    UNSIGNED_INTEGER_8_TYPE = 14,
} Primitive_Types;


char primitive_enum_to_format_specifier(Primitive_Types primitive_enum){
    switch(primitive_enum){
        case INTEGER_8_TYPE:
        case INTEGER_16_TYPE:
        case INTEGER_32_TYPE:
        case INTEGER_64_TYPE:
            return 'i';
            break;
        case FLOAT_32_TYPE:
        case FLOAT_64_TYPE:
            return 'f';
            break;
        case CHAR_PTR_TYPE:
        case STRING_DESCRIPTOR_TYPE:
            return 'a';
            break;
        case LOGICAL_TYPE:
            return 'l';
            break;
        case CPTR_VOID_PTR_TYPE:
            return 'P'; // Not actual fortran specifier.
            break;
        default:
            fprintf(stderr,"Compiler Error : Unidentified Type %d\n", primitive_enum);
            exit(0);
    }

}

bool is_format_match(char format_value, Primitive_Types current_arg_type){
    char current_arg_correct_format = primitive_enum_to_format_specifier(current_arg_type);

    char lowered_format_value = tolower(format_value);
    if (lowered_format_value == 'g') return true;
    if(lowered_format_value == 'd' || lowered_format_value == 'e'){
        lowered_format_value = 'f';
    }
    // Special conditions that are allowed by gfortran.
    bool special_conditions = (lowered_format_value == 'l' && current_arg_correct_format == 'a') ||
                               (lowered_format_value == 'a' && current_arg_correct_format == 'l') ||
                               (lowered_format_value == 'b' && (current_arg_correct_format == 'i' || current_arg_correct_format == 'f'));

    if(lowered_format_value != current_arg_correct_format && !special_conditions){
        return false;
    } else {
        return true;
    }
}

typedef struct stack {
    int64_t* p;
    int32_t stack_size;
    int32_t top_index;
} Stack;  

Stack* create_stack(){
    Stack* s = (Stack*)malloc(sizeof(Stack));
    s->stack_size = 10; // intial value
    s->p = (int64_t*)malloc(s->stack_size * sizeof(int64_t));
    s->top_index = -1;
    return s;
}

void push_stack(Stack* x, int64_t val){
    if(x->top_index == x->stack_size - 1){ // Check if extending is needed.
        x->stack_size *= 2;
        x->p = (int64_t*)realloc(x->p, x->stack_size * sizeof(int64_t));
    }
    x->p[++x->top_index] = val;
}

void pop_stack(Stack* x){
    if(x->top_index == -1){
        fprintf(stderr,"Compiler Internal Error.\n");
        exit(1);
    }
    --x->top_index;
}

static inline int64_t get_stack_top(struct stack* s){
    return s->p[s->top_index];
}

static inline bool stack_empty(Stack* s){
    return s->top_index == -1;
}
void free_stack(Stack* x){
    free(x->p);
    free(x);
}



typedef struct serialization_info{
    const char* serialization_string;
    int32_t current_stop; // current stop index in the serialization_string.
    Stack* array_sizes_stack; // Holds the sizes of the arrays (while nesting).
    Stack* array_serialiation_start_index; // Holds the index of '[' char in serialization
    Primitive_Types current_element_type;
    struct current_arg_info{
        va_list* args;
        void* current_arg; // holds pointer to the arg being printed (Scalar or Vector) 
        bool is_complex;
        int64_t current_string_len; // Holds string length 'If Exist'
    } current_arg_info;
    struct runtime_sizes_lengths{ // Passed array sizes or string legnths.
        int64_t* ptr;
        int32_t current_index;
    } array_sizes, string_lengths;
    bool just_peeked; // Flag to indicate if we just peeked the next element.
    char* temp_char_pp; // Dummy container (Should be removed)
} Serialization_Info;



// Transforms serialized size (array-size, string-size)
int64_t transform_string_size_into_int(struct serialization_info* s_info){
    int64_t array_size = 0;
    while(isdigit(s_info->serialization_string[s_info->current_stop])){
        array_size = array_size * 10 + (s_info->serialization_string[s_info->current_stop++] - '0');
    }
    return array_size;
}

// Sets `current_string_length` either from the serialization string or passed run-time length
void set_string_length(Serialization_Info* s_info){
    if(s_info->serialization_string[s_info->current_stop] == '-'){
        s_info->current_stop++;
        s_info->current_arg_info.current_string_len = 
            transform_string_size_into_int(s_info);
    } else {
        if(s_info->current_element_type == CHAR_PTR_TYPE ||
            s_info->current_element_type == STRING_DESCRIPTOR_TYPE ) return; // Array. length already set (Consumed from array of lengths).
        s_info->current_arg_info.current_string_len = 
            s_info->string_lengths.ptr[s_info->string_lengths.current_index++];
    }
}
// Deserialize to know the physical type of string
Primitive_Types get_string_primitive_type(Serialization_Info* s_info){
    if(s_info->serialization_string[s_info->current_stop++] != '-') {
        fprintf(stderr, "RunTime - compiler internal error"
            " : Unidentified Print Types Serialization for strings --> %s\n",
                s_info->serialization_string);
        exit(1);
    }
    if(s_info->serialization_string[s_info->current_stop++] == 'D'){
        bool DESC = 
            s_info->serialization_string[s_info->current_stop++] == 'E' &&
            s_info->serialization_string[s_info->current_stop++] == 'S' && 
            s_info->serialization_string[s_info->current_stop++] == 'C';
        if(!DESC){fprintf(stderr, "%s", "ERROR: string serializatino\n");exit(1);}
        return STRING_DESCRIPTOR_TYPE;
    } else if (s_info->serialization_string[s_info->current_stop++] == 'C'){
        bool CCHAR = 
            s_info->serialization_string[s_info->current_stop++] == 'C' &&
            s_info->serialization_string[s_info->current_stop++] == 'H' && 
            s_info->serialization_string[s_info->current_stop++] == 'A' &&
            s_info->serialization_string[s_info->current_stop++] == 'R';
        if(!CCHAR){fprintf(stderr, "%s", "ERROR: string serializatino\n");exit(1);}
        return CHAR_PTR_TYPE;
    } else {
        fprintf(stderr, "RunTime - compiler internal error"
            " : Unidentified Print Types Serialization for strings --> %s\n",
                s_info->serialization_string);
        exit(1);
    }  
}
// A Fortran Type (struct) has an array member of strings ==>`([s])`
bool array_of_string_special_case(Serialization_Info* s_info){ // {string_descriptor*}
    bool in_struct;
    {   
        if(!stack_empty(s_info->array_sizes_stack)){
            int64_t tmp = get_stack_top(s_info->array_sizes_stack);
            pop_stack(s_info->array_sizes_stack);
            in_struct = !stack_empty(s_info->array_sizes_stack) && (get_stack_top(s_info->array_sizes_stack) < 0) /*means a struct*/;
            push_stack(s_info->array_sizes_stack, tmp); // Push back the size of the array.
        } else {
            in_struct = false;
        }
    }
    if(in_struct && (get_stack_top(s_info->array_sizes_stack) > 0) &&
        (s_info->current_element_type == CHAR_PTR_TYPE ||
        s_info->current_element_type == STRING_DESCRIPTOR_TYPE)){ 
        return true;
    } else {
        return false;
    }
}


// Moves a containing pointer (struct, array) to the next the element
void move_containing_ptr_next(Serialization_Info* s_info){
    // Ordering of types is crucial (Matched with enum `Primitive_Types`)
    static const int primitive_type_sizes[] = 
        {sizeof(int64_t), sizeof(int32_t), sizeof(int16_t),
        sizeof(int8_t) , sizeof(double), sizeof(float), 
        sizeof(char*), sizeof(bool), sizeof(void*), 0 /*Important to be zero*/,
        sizeof(char*) + sizeof(int64_t)/*String Descriptor*/ };
    if( !stack_empty(s_info->array_sizes_stack) && 
        (get_stack_top(s_info->array_sizes_stack) > 0) && 
        (s_info->current_element_type == CHAR_PTR_TYPE ||
            s_info->current_element_type == STRING_DESCRIPTOR_TYPE)){ // Array of strings (Single Ptr)
        char* arr_str_ptr = *(char**)s_info->current_arg_info.current_arg;
        s_info->temp_char_pp =  arr_str_ptr + s_info->current_arg_info.current_string_len + 1 /*\0*/;
        s_info->current_arg_info.current_arg = (void*)&s_info->temp_char_pp;
    } else {
        s_info->current_arg_info.current_arg = 
            (void*)
            ((char*)s_info->current_arg_info.current_arg +
                primitive_type_sizes[s_info->current_element_type]); // char* cast needed for windows MinGW.
    }
        
}

/* Sets primitive type for the current argument
 by parsing through the serialization string. */
void set_current_PrimitiveType(Serialization_Info* s_info){
    Primitive_Types *PrimitiveType = &s_info->current_element_type;
    switch (s_info->serialization_string[s_info->current_stop++])
    {
    case 'I':
        switch (s_info->serialization_string[s_info->current_stop++])
        {
        case '8':
            *PrimitiveType = INTEGER_64_TYPE;
            break;
        case '4':
            *PrimitiveType = INTEGER_32_TYPE;
            break;
        case '2':
            *PrimitiveType = INTEGER_16_TYPE;
            break;
        case '1':
            *PrimitiveType = INTEGER_8_TYPE;
            break;
        default:
            fprintf(stderr, "RunTime - compiler internal error"
                " : Unidentified Print Types Serialization --> %s\n",
                    s_info->serialization_string);
            exit(1);
            break;
        }
        break;
    case 'U':
        switch (s_info->serialization_string[s_info->current_stop++])
        {
        case '8':
            *PrimitiveType = UNSIGNED_INTEGER_64_TYPE;
            break;
        case '4':
            *PrimitiveType = UNSIGNED_INTEGER_32_TYPE;
            break;
        case '2':
            *PrimitiveType = UNSIGNED_INTEGER_16_TYPE;
            break;
        case '1':
            *PrimitiveType = UNSIGNED_INTEGER_8_TYPE;
            break;
        default:
            fprintf(stderr, "RunTime - compiler internal error"
                " : Unidentified Print Types Serialization --> %s\n",
                    s_info->serialization_string);
            exit(1);
            break;
        }
        break;
    case 'R':
        switch (s_info->serialization_string[s_info->current_stop++])
        {
        case '8':
            *PrimitiveType = FLOAT_64_TYPE;
            break;
        case '4':
            *PrimitiveType = FLOAT_32_TYPE;
            break;
        default:
            fprintf(stderr, "RunTime - compiler" 
            "internal error : Unidentified Print Types Serialization --> %s\n",
                    s_info->serialization_string);
            exit(1);
            break;
        }
        break;
    case 'L':
        *PrimitiveType = LOGICAL_TYPE;
        break;
    case 'S':{
        Primitive_Types str_phys_type = get_string_primitive_type(s_info);
        set_string_length(s_info);
        *PrimitiveType = str_phys_type;
        break;
    }
    case 'C':
        ASSERT_MSG(s_info->serialization_string[s_info->current_stop++] == 'P' &&
            s_info->serialization_string[s_info->current_stop++] == 't' &&
            s_info->serialization_string[s_info->current_stop++] == 'r',
            "%s\n",s_info->serialization_string);
        *PrimitiveType = CPTR_VOID_PTR_TYPE;
        break;
    default:
        fprintf(stderr, "RunTime - compiler internal error"
            " : Unidentified Print Types Serialization --> %s\n",
                s_info->serialization_string);
        exit(1);
        break;
    }
}

/*
    Fetches the type of next element that will get printed + 
    sets the `current_arg` to the desired pointer.
    Return `false` if no elements remaining.
*/
bool move_to_next_element(struct serialization_info* s_info, bool peek){
    // Handle `peek` flag logic
    if(s_info->just_peeked && peek ||
        (s_info->just_peeked && !peek)){ // already `s_info` is set with current element.
        s_info->just_peeked = peek;
        return s_info->current_element_type != NONE_TYPE;
    } else{ // Get next.
        s_info->just_peeked = peek;
    }

    while(true){
        char cur = s_info->serialization_string[s_info->current_stop];
        // Zero-size array OR inside zero-size array --> e.g. (0[(I8,4[S])])
        bool zero_size = (!stack_empty(s_info->array_sizes_stack) &&
                            (get_stack_top(s_info->array_sizes_stack) == 0)); 
        if(isdigit(cur)){ // ArraySize --> `50[I4]`
            int64_t array_size = transform_string_size_into_int(s_info);
            ASSERT_MSG(s_info->serialization_string[s_info->current_stop] == '[',
                "RunTime - Compiler Internal error "
                ": Wrong serialization for print statement --> %s\n",
                s_info->serialization_string);
            zero_size ? push_stack(s_info->array_sizes_stack, 0):
                        push_stack(s_info->array_sizes_stack, array_size);
        } else if (cur == '['){ //ArrayStart
            if(stack_empty(s_info->array_sizes_stack)){// Runtime-NonStringSerialized arraysize.
                push_stack(s_info->array_sizes_stack,
                    s_info->array_sizes.ptr[s_info->array_sizes.current_index++]); 
            }
            // Remeber where the array starts to handle this nested case --> `10[(10[I4]))]`
            ++s_info->current_stop;
            push_stack(s_info->array_serialiation_start_index, s_info->current_stop);
        } else if (cur == ']'){ // ArrayEnd
            if(!zero_size &&  (get_stack_top(s_info->array_sizes_stack) - 1 == 0) || 
                (zero_size &&  (get_stack_top(s_info->array_sizes_stack) - 1 == -1))){ // Move to the next element.
                pop_stack(s_info->array_sizes_stack);
                pop_stack(s_info->array_serialiation_start_index);
                ++s_info->current_stop;
            } else { // Decrement Size + Move to the begining of the array.
                ASSERT_MSG(!zero_size,
                    "%s\n", "Zero-size vector shouldn't go back to the begining.");
                int64_t arr_size_decremented = get_stack_top(s_info->array_sizes_stack) - 1;
                pop_stack(s_info->array_sizes_stack);
                push_stack(s_info->array_sizes_stack, arr_size_decremented);
                s_info->current_stop = get_stack_top(s_info->array_serialiation_start_index);
            }
        } else if (cur == '(' || cur == '{') { // StructStart, complexStart
            s_info->current_arg_info.is_complex = (cur == '{');
            push_stack(s_info->array_sizes_stack, -1000000); // dummy size. needed to know when to move from arg* to another.
            ++s_info->current_stop;
        } else if (cur == ')' || cur == '}') { // StructEnd, ComplexEnd
            s_info->current_arg_info.is_complex = false;
            pop_stack(s_info->array_sizes_stack);
            ++s_info->current_stop;
        } else if (cur == ','){ // Separator between scalars or in compound type --> `I4,R8`, (I4,R8)`.
            ++s_info->current_stop;
            // Only move from passed arg to another in the `va_list` when we don't have struct or array in process.
            if(stack_empty(s_info->array_sizes_stack)){ 
                ASSERT(stack_empty(s_info->array_serialiation_start_index));
                s_info->current_arg_info.current_arg = va_arg(*s_info->current_arg_info.args, void*);
                s_info->current_element_type = NONE_TYPE; // Important to set type to none when moving from whole argument to another
            } 
        } else if(cur == '\0'){ // End of Serialization.
            ASSERT( stack_empty(s_info->array_sizes_stack) && 
                    stack_empty(s_info->array_serialiation_start_index));
            s_info->current_arg_info.current_arg = NULL;
            s_info->current_element_type = NONE_TYPE;
            return false;
        } else { // Type
            if(zero_size) {
                set_current_PrimitiveType(s_info);/*Keep deserializing*/
                continue;
            }
            move_containing_ptr_next(s_info); // Moves containing pointer to the next element
            set_current_PrimitiveType(s_info);
            return true;
        }
    }
}


void print_into_string(Serialization_Info* s_info,  char* result){
    void* arg = s_info->current_arg_info.current_arg;
    switch (s_info->current_element_type){
        case INTEGER_64_TYPE:
            sprintf(result, "%"PRId64, *(int64_t*)arg);
            break;
        case INTEGER_32_TYPE:
            sprintf(result, "%d", *(int32_t*)arg);
            break;
        case INTEGER_16_TYPE:
            sprintf(result, "%hi", *(int16_t*)arg);
            break;
        case INTEGER_8_TYPE:
            sprintf(result, "%hhi", *(int8_t*)arg);
            break;
        case UNSIGNED_INTEGER_64_TYPE:
            sprintf(result, "%"PRIu64, *(uint64_t*)arg);
            break;
        case UNSIGNED_INTEGER_32_TYPE:
            sprintf(result, "%u", *(uint32_t*)arg);
            break;
        case UNSIGNED_INTEGER_16_TYPE:
            sprintf(result, "%hu", *(uint16_t*)arg);
            break;
        case UNSIGNED_INTEGER_8_TYPE:
            sprintf(result, "%hhu", *(uint8_t*)arg);
            break;
        case FLOAT_64_TYPE:
            if(s_info->current_arg_info.is_complex){
                double real = *(double*)arg;
                move_to_next_element(s_info, false);
                double imag = *(double*)s_info->current_arg_info.current_arg;
                sprintf(result, "(%23.17e, %23.17e)", real, imag);
            } else {
                sprintf(result, "%23.17e", *(double*)arg);
            }
            break;
        case FLOAT_32_TYPE:
            if(s_info->current_arg_info.is_complex){
                float real = *(float*)arg;
                move_to_next_element(s_info, false);
                double imag = *(float*)s_info->current_arg_info.current_arg;
                sprintf(result, "(%13.8e, %13.8e)", real, imag);
            } else {
                sprintf(result, "%13.8e", *(float*)arg);
            }
            break;
        case LOGICAL_TYPE:
            sprintf(result, "%c", (*(bool*)arg)? 'T' : 'F');
            break;
        case CHAR_PTR_TYPE:
        case STRING_DESCRIPTOR_TYPE:{
            if(array_of_string_special_case(s_info)){fprintf(stderr,"ERROR : implement this\n");exit(1);}
            char* char_ptr = *(char**)arg;
                if(char_ptr == NULL){
                    sprintf(result, "%s", " ");
                } else {
                    memcpy(result,
                        char_ptr,
                        s_info->current_arg_info.current_string_len);
                    *(result + s_info->current_arg_info.current_string_len) = '\0';
                }
                break;
        }
        case CPTR_VOID_PTR_TYPE:
            sprintf(result, "%p",*(void**)arg);
            break;
        default :
            fprintf(stderr, "Unknown type");
            exit(1);
    }

}

void strip_outer_parenthesis(const char* str, int len, char* output) {
    if (len >= 2 && str[0] == '(' && str[len - 1] == ')') {
        int nest = 0;
        int i;
        // Check balance: if the outermost '(' is properly closed by the last character
        for (i = 0; i < len; i++) {
            if (str[i] == '(') {
                nest++;
            } else if (str[i] == ')') {
                nest--;
                // If the nesting level reaches 0 before the end, the outermost ')' isn't at len-1
                if (nest == 0) {
                    break;
                }
            }
        }
        
        if (nest == 0) {
            // Copy the string without outer parentheses
            memmove(output, output + 1, len);
            output[i - 1] = '\0';
        } else {
            memmove(output, output + 1, len);
            output[len - 2] = '\0';
        }
    }
}

void default_formatting(char** result, struct serialization_info* s_info){
    int64_t result_capacity = 100;
    int64_t result_size = 0;
    const int default_spacing_len = 4;
    const char* default_spacing = "    ";
    ASSERT(default_spacing_len == strlen(default_spacing));
    *result = realloc(*result, result_capacity + 1 /*Null Character*/ );

    while(move_to_next_element(s_info, false)){
        int size_to_allocate;
        if((s_info->current_element_type == CHAR_PTR_TYPE ||
            s_info->current_element_type == STRING_DESCRIPTOR_TYPE) && 
            *(char**)s_info->current_arg_info.current_arg != NULL){
            size_to_allocate = (s_info->current_arg_info.current_string_len
                                 + default_spacing_len + 1) * sizeof(char);
        } else {
            size_to_allocate = (60 + default_spacing_len) * sizeof(char);
        }
        int64_t old_capacity = result_capacity;
        while(result_capacity <= size_to_allocate + result_size){ // Check if string extension is needed.
            if(result_size + size_to_allocate > result_capacity*2){
                result_capacity = size_to_allocate + result_size ;
            } else {
                result_capacity *=2;
            }
        }
        if(result_capacity != old_capacity){*result = (char*)realloc(*result, result_capacity + 1);}
        if(result_size > 0){
            strcpy((*result)+result_size, default_spacing);
            result_size+=default_spacing_len;
        }
        print_into_string(s_info,  (*result) + result_size);
        int64_t printed_arg_size = strlen((*result) + result_size);
        result_size += printed_arg_size;
    }
}
void free_serialization_info(Serialization_Info* s_info){
    free(s_info->array_sizes.ptr);
    free(s_info->string_lengths.ptr);
    free_stack(s_info->array_sizes_stack);
    free_stack(s_info->array_serialiation_start_index);
    va_end(*s_info->current_arg_info.args);
}

LFORTRAN_API char* _lcompilers_string_format_fortran(const char* format, int64_t format_len, const char* serialization_string, 
    int32_t array_sizes_cnt, int32_t string_lengths_cnt, ...)
{
    va_list args;
    va_start(args, string_lengths_cnt);
    char* result = (char*)malloc(sizeof(char)); //TODO : the consumer of this string needs to free it.
    result[0] = '\0';

    // Setup s_info
    struct serialization_info s_info;
    s_info.serialization_string = serialization_string;
    s_info.array_serialiation_start_index = create_stack();
    s_info.array_sizes_stack = create_stack();
    s_info.current_stop = 0;
    s_info.current_arg_info.args = &args;
    s_info.current_element_type = NONE_TYPE;
    s_info.current_arg_info.is_complex = false;
    s_info.array_sizes.current_index = 0;
    s_info.string_lengths.current_index = 0;
    s_info.just_peeked = false;

    int64_t* array_sizes = (int64_t*) malloc(array_sizes_cnt * sizeof(int64_t));
    for(int i=0; i<array_sizes_cnt; i++){
        array_sizes[i] = va_arg(args, int64_t);
    }
    s_info.array_sizes.ptr = array_sizes;

    int64_t* string_lengths = (int64_t*) malloc(string_lengths_cnt * sizeof(int64_t));
    for(int i=0; i<string_lengths_cnt; i++){
        string_lengths[i] = va_arg(args, int64_t);
    }
    s_info.string_lengths.ptr = string_lengths;

    s_info.current_arg_info.current_arg = va_arg(args, void*);

    if(!s_info.current_arg_info.current_arg && 
        s_info.serialization_string[s_info.current_stop] !='\0')
    {fprintf(stderr,"Internal Error : default formatting error\n");exit(1);}

    if(format == NULL){
        default_formatting(&result, &s_info);
        free_serialization_info(&s_info);
        return result;
    }

    int64_t format_values_count = 0,item_start_idx=0;
    char** format_values;
    char* modified_input_string;
    int len = 0;
    char* cleaned_format = remove_spaces_except_quotes((const fchar*)format, format_len, &len);
    if (!cleaned_format) {
        free_serialization_info(&s_info);
        return NULL;
    }
    modified_input_string = (char*)malloc((len+1) * sizeof(char));
    strncpy(modified_input_string, cleaned_format, len);
    modified_input_string[len] = '\0';
    strip_outer_parenthesis(cleaned_format, len, modified_input_string);
    format_values = parse_fortran_format((const fchar*)modified_input_string, strlen(modified_input_string), &format_values_count, &item_start_idx);
    /*
    is_SP_specifier = false  --> 'S' OR 'SS'
    is_SP_specifier = true  --> 'SP'
    */
    bool is_SP_specifier = false;
    int item_start = 0;
    bool array = false;
    bool BreakWhileLoop= false;
    while (1) {
        int scale = 0;
        bool is_array = false;
        bool array_looping = false;
        for (int i = item_start; i < format_values_count; i++) {
            char* value;
            if(format_values[i] == NULL) continue;
            value = format_values[i];
            int64_t value_len = strlen(value);
            if (value_len >= 2 && value[0] == '(' && value[value_len - 1] == ')') {
                value[value_len - 1] = '\0';
                int64_t new_fmt_val_count = 0;
                value += 1;
                char** new_fmt_val = parse_fortran_format((const fchar*)value, value_len - 2, &new_fmt_val_count, &item_start_idx);
                char** ptr = (char**)realloc(format_values, (format_values_count + new_fmt_val_count + 1) * sizeof(char*));
                if (ptr == NULL) {
                    perror("Memory allocation failed.\n");
                    free(format_values);
                } else {
                    format_values = ptr;
                }
                for (int k = format_values_count - 1; k >= i+1; k--) {
                    format_values[k + new_fmt_val_count] = format_values[k];
                }
                for (int k = 0; k < new_fmt_val_count; k++) {
                    format_values[i + 1 + k] = new_fmt_val[k];
                }
                format_values_count = format_values_count + new_fmt_val_count;
                free(format_values[i]);
                format_values[i] = NULL;
                free(new_fmt_val);
                continue;
            }

            if (value[0] == ':') {
                if (!move_to_next_element(&s_info, true)) break;
                continue;
            } else if (value[0] == '/') {
                result = append_to_string(result, "\n");
            } else if (value[0] == '*') {
                array = true;
            } else if (isdigit(value[0]) && tolower(value[1]) == 'p') {
                // Scale Factor nP
                scale = atoi(&value[0]);
            } else if (value[0] == '-' && isdigit(value[1]) && tolower(value[2]) == 'p') {
                char temp[3] = {value[0],value[1],'\0'};
                scale = atoi(temp);
            } else if ((value[0] == '\"' && value[strlen(value) - 1] == '\"') ||
                (value[0] == '\'' && value[strlen(value) - 1] == '\'')) {
                // String
                value = substring(value, 1, strlen(value) - 1);
                result = append_to_string(result, value);
                free(value);
            } else if (tolower(value[strlen(value) - 1]) == 'x') {
                result = append_to_string(result, " ");
            } else if (tolower(value[0]) == 's') {
                is_SP_specifier = ( strlen(value) == 2 /*case 'S' specifier*/ &&
                                    tolower(value[1]) == 'p'); 
            } else if (tolower(value[0]) == 't') {
                if (tolower(value[1]) == 'l') {
                    // handle "TL" format specifier
                    int tab_left_pos = atoi(value + 2);
                    int current_length = strlen(result);
                    if (tab_left_pos > current_length) {
                        result[0] = '\0';
                    } else {
                        result[current_length - tab_left_pos] = '\0';
                    }
                } else if (tolower(value[1]) == 'r') {
                    // handle "TR" format specifier
                    int tab_right_pos = atoi(value + 2);
                    int current_length = strlen(result);
                    int spaces_needed = tab_right_pos;
                    if (spaces_needed > 0) {
                        char* spaces = (char*)malloc((spaces_needed + 1) * sizeof(char));
                        memset(spaces, ' ', spaces_needed);
                        spaces[spaces_needed] = '\0';
                        result = append_to_string(result, spaces);
                        free(spaces);
                    }
                } else {
                    if (!move_to_next_element(&s_info, true)) break;
                    int tab_position = atoi(value + 1);
                    int current_length = strlen(result);
                    int spaces_needed = tab_position - current_length - 1;
                    if (spaces_needed > 0) {
                        char* spaces = (char*)malloc((spaces_needed + 1) * sizeof(char));
                        memset(spaces, ' ', spaces_needed);
                        spaces[spaces_needed] = '\0';
                        result = append_to_string(result, spaces);
                        free(spaces);
                    } else if (spaces_needed < 0) {
                        // Truncate the string to the length specified by Tn
                        // if the current position exceeds it
                        if (tab_position < current_length) {
                            // Truncate the string at the position specified by Tn
                            result[tab_position] = '\0';
                        }
                    }
                }
            } else {
                if (!move_to_next_element(&s_info, false)) break;
                if (!is_format_match(
                        tolower(value[0]), s_info.current_element_type)) {
                    char* type; // For better error message.
                    switch (primitive_enum_to_format_specifier(s_info.current_element_type))
                    {
                        case 'i':
                            type = "INTEGER";
                            break;
                        case 'f':
                            type = "REAL";
                            break;
                        case 'l':
                            type = "LOGICAL";
                            break;
                        case 'a':
                            type = "CHARACTER";
                            break;
                    }
                    free(result);
                    result = (char*)malloc(150 * sizeof(char));
                    sprintf(result, " Runtime Error : Got argument of type (%s), while the format specifier is (%c)\n", type, value[0]);
                    // Special indication for error --> "\b" to be handled by `lfortran_print` or `lfortran_file_write`
                    result[0] = '\b';
                    BreakWhileLoop = true;
                    break;
                }
            
                // All formatting functions uses int64 and double.
                // We have to cast the pointers to int64 or double to avoid accessing beyond bounds.
                int64_t integer_val = 0;
                double double_val = 0;
                char* char_val = NULL;
                bool bool_val = false;
                switch(s_info.current_element_type ){
                    case  INTEGER_64_TYPE:
                        integer_val = *(int64_t*)s_info.current_arg_info.current_arg; 
                        break;
                    case  INTEGER_32_TYPE:
                        integer_val = (int64_t)*(int32_t*)s_info.current_arg_info.current_arg; 
                        break;
                    case  INTEGER_16_TYPE:
                        integer_val = (int64_t)*(int16_t*)s_info.current_arg_info.current_arg; 
                        break;
                    case  INTEGER_8_TYPE:
                        integer_val = (int64_t)*(int8_t*)s_info.current_arg_info.current_arg; 
                        break;
                    case  FLOAT_64_TYPE:
                        double_val = *(double*)s_info.current_arg_info.current_arg; 
                        break;
                    case  FLOAT_32_TYPE:
                        double_val = (double)*(float*)s_info.current_arg_info.current_arg; 
                        break;
                    case CHAR_PTR_TYPE:
                    case STRING_DESCRIPTOR_TYPE:
                        char_val = *(char**)s_info.current_arg_info.current_arg;
                        break;
                    case LOGICAL_TYPE:
                        bool_val = *(bool*)s_info.current_arg_info.current_arg;
                        break;
                    default:
                        break;
                }
                if (tolower(value[0]) == 'a') {
                    // Handle if argument is actually logical (allowed in Fortran).
                    if(s_info.current_element_type==LOGICAL_TYPE){
                        handle_logical("l",*(bool*)s_info.current_arg_info.current_arg, &result);
                        continue;
                    }
                    char* arg = *(char**)s_info.current_arg_info.current_arg;
                    if (arg == NULL) continue;
                    if (strlen(value) == 1) {
                        result = append_to_string(result, arg);
                    } else {
                        char* str = (char*)malloc((strlen(value)) * sizeof(char));
                        memmove(str, value+1, strlen(value));
                        int buffer_size = 20;
                        char* s = (char*)malloc(buffer_size * sizeof(char));
                        snprintf(s, buffer_size, "%%%s.%ss", str, str);
                        char* string = (char*)malloc((atoi(str) + 1) * sizeof(char));
                        sprintf(string,s, arg);
                        result = append_to_string(result, string);
                        free(str);
                        free(s);
                        free(string);
                    }
                } else if (tolower(value[0]) == 'i') {
                    // Integer Editing ( I[w[.m]] )
                    handle_integer(value, integer_val, &result, is_SP_specifier);
                } else if (tolower(value[0]) == 'b') {
                    int width = 0;
                    int min_digit_cnt = 0;
                    if (strlen(value) > 1) {
                        width = atoi(value + 1); // Get width after 'B'
                    }
                    const char *dot = strchr(value + 1, '.'); // Look for '.' after 'b'
                    if (dot != NULL) {
                        min_digit_cnt = atoi(dot + 1); // get digits after '.'
                    }
                    int bit_size = 0;
                    uint64_t uval = 0;
                    char fmt_type = primitive_enum_to_format_specifier(s_info.current_element_type);
                    if (fmt_type == 'i') {
                        if (s_info.current_element_type == INTEGER_8_TYPE) {
                            bit_size = 8;
                        } else if (s_info.current_element_type == INTEGER_16_TYPE) {
                            bit_size = 16;
                        } else if (s_info.current_element_type == INTEGER_32_TYPE) {
                            bit_size = 32;
                        } else if (s_info.current_element_type == INTEGER_64_TYPE) {
                            bit_size = 64;
                        }
                        uint64_t mask = (bit_size == 64) ? UINT64_MAX : ((1ULL << bit_size) - 1);
                        uval = ((uint64_t)integer_val) & mask;
                    } else if (fmt_type == 'f') {
                        if (s_info.current_element_type == FLOAT_32_TYPE) {
                            float f = (float)double_val;
                            uint32_t bits;
                            memcpy(&bits, &f, sizeof(float));
                            uval = (uint64_t)bits;
                            bit_size = 32;
                        } else if (s_info.current_element_type == FLOAT_64_TYPE) {
                            double d = double_val;
                            memcpy(&uval, &d, sizeof(double));
                            bit_size = 64;
                        }
                    } else {
                        result = append_to_string(result, "<unsupported>");
                        break;
                    }

                    char binary_str[65]; // max 64 bits + '\0'
                    int idx = 0;
                    bool started = false;

                    for (int bit = bit_size - 1; bit >= 0; bit--) {
                        if ((uval >> bit) & 1) {
                            started = true;
                        }
                        if (started) {
                            binary_str[idx++] = ((uval >> bit) & 1) ? '1' : '0';
                        }
                    }

                    if (idx == 0) {
                        binary_str[idx++] = '0'; // If number is 0
                    }
                    binary_str[idx] = '\0';

                    int bin_len = strlen(binary_str);

                    if (width == 0) {
                        result = append_to_string(result, binary_str);
                    } else if (bin_len > width) {
                        for (int i = 0; i < width; i++) {
                            result = append_to_string(result, "*");
                        }
                    } else {
                        int bin_len = strlen(binary_str);
                        // Step 1: Pad with zeros to meet min_digit_cnt
                        if (min_digit_cnt > bin_len) {
                            int zero_padding = min_digit_cnt - bin_len;
                            char* zeros = (char*)malloc((zero_padding + 1) * sizeof(char));
                            memset(zeros, '0', zero_padding);
                            zeros[zero_padding] = '\0';
                            char* tmp = (char*)malloc((min_digit_cnt + 1) * sizeof(char));
                            strcpy(tmp, zeros);
                            strcat(tmp, binary_str);
                            strcpy(binary_str, tmp);
                            free(tmp);
                            free(zeros);
                            bin_len = strlen(binary_str);
                        }
                        // Step 2: Pad with spaces to meet width
                        int padding_needed = width - bin_len;
                        if (padding_needed > 0) {
                            char* pad = (char*)malloc((padding_needed + 1) * sizeof(char));
                            memset(pad, ' ', padding_needed);
                            pad[padding_needed] = '\0';
                            result = append_to_string(result, pad);
                            free(pad);
                        }
                        result = append_to_string(result, binary_str);
                    }
                } else if (tolower(value[0]) == 'g') {
                    int width = 0;
                    int precision = 0;
                    if (strlen(value) > 1) {
                        width = atoi(value + 1); // Get width after 'g'
                    } 
                    const char *dot = strchr(value + 1, '.'); // Look for '.' after 'b'
                    if (dot != NULL) {
                        precision = atoi(dot + 1); // get digits after '.'
                    }
                    char buffer[100];
                    char formatted[100];
                    if (s_info.current_element_type == FLOAT_32_TYPE || s_info.current_element_type == FLOAT_64_TYPE) {
                        if (double_val == 0.0 || (fabs(double_val) >= 0.1 && fabs(double_val) < pow(10.0, precision))) {
                            char format_spec[20];
                            snprintf(format_spec, sizeof(format_spec), "%%#.%dG", precision);
                            snprintf(formatted, sizeof(formatted), format_spec, double_val);
                        } else {
                            int exp = 0;
                            double abs_val = fabs(double_val);
                            if (abs_val > 0.0) {
                                exp = (int)floor(log10(abs_val)) + 1;
                            }
                            double scale = pow(10.0, -exp);
                            double final_val = double_val * scale;
                            char mantissa[64], exponent[16];
                            snprintf(mantissa, sizeof(mantissa), "%.*f", precision, final_val);
                            if (width > 0) {
                                snprintf(exponent, sizeof(exponent), "E%+03d", exp); 
                            } else {
                                snprintf(exponent, sizeof(exponent), "E%+d", exp);
                            }
                            snprintf(formatted, sizeof(formatted), "%s%s", mantissa, exponent);
                        }
                        int len = strlen(formatted);
                        if (width > len) {
                            int padding = width - len;
                            snprintf(buffer, sizeof(buffer), "%*s", width, formatted);
                        } else {
                            strcpy(buffer, formatted);
                        }
                        result = append_to_string(result, buffer);
                    } else if (s_info.current_element_type == INTEGER_8_TYPE ||
                               s_info.current_element_type == INTEGER_16_TYPE ||
                               s_info.current_element_type == INTEGER_32_TYPE ||
                               s_info.current_element_type == INTEGER_64_TYPE) {
                        snprintf(result, sizeof(buffer), "%"PRId64, integer_val);
                    } else if (s_info.current_element_type == CHAR_PTR_TYPE ||
                        s_info.current_element_type == STRING_DESCRIPTOR_TYPE) {
                        result = append_to_string(result, char_val);
                    } else if (s_info.current_element_type == LOGICAL_TYPE) {
                        result = append_to_string(result, bool_val ? "T" : "F");
                    } else {
                        result = append_to_string(result, "<unsupported>");
                    }
                } else if (tolower(value[0]) == 'd') {
                    // D Editing (D[w[.d]])
                    double val = *(double*)s_info.current_arg_info.current_arg;
                    handle_decimal(value, double_val, scale, &result, "D", is_SP_specifier);
                } else if (tolower(value[0]) == 'e') {
                    // check for decimal presence before passing, else leads to segmentation fault
                    // as FORTRAN seeks for E<width>.<number of digits>
                    if (strchr(value, '.') == NULL) {
                        fprintf(stderr, "Error: Invalid format descriptor E - Proper Format is E<width>.<number of digits>\n");
                        fprintf(stderr, "Period required in format specifier\n");   
                        exit(1);
                    }
                    // Check if the next character is 'N' for EN format
                    char format_type = tolower(value[1]);
                    if (format_type == 'n') {
                        handle_en(value, double_val, scale, &result, "E", is_SP_specifier);
                    } else {
                        handle_decimal(value, double_val, scale, &result, "E", is_SP_specifier);
                    }
                } else if (tolower(value[0]) == 'f') {
                    handle_float(value, double_val, scale, &result, is_SP_specifier);
                } else if (tolower(value[0]) == 'l') {
                    bool val = *(bool*)s_info.current_arg_info.current_arg;
                    handle_logical(value, val, &result);
                } else if (strlen(value) != 0) {
                    printf("Printing support is not available for %s format.\n",value);
                }

            }
        }
        if(BreakWhileLoop) break;
        if (move_to_next_element(&s_info, true)) {
            if (!array) {
                result = append_to_string(result, "\n");
            }
            item_start = item_start_idx;
        } else {
            break;
        }
    }
    free(modified_input_string);
    for (int i = 0;(i<format_values_count);i++) {
            free(format_values[i]);
    }
    va_end(args);
    free(format_values);
    free_serialization_info(&s_info);
    return result;
}

LFORTRAN_API void _lcompilers_print_error(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    fflush(stderr);
    va_end(args);
}

LFORTRAN_API void _lfortran_complex_add_32(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result)
{
    result->re = a->re + b->re;
    result->im = a->im + b->im;
}

LFORTRAN_API void _lfortran_complex_add_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result)
{
    result->re = a->re + b->re;
    result->im = a->im + b->im;
}

LFORTRAN_API void _lfortran_complex_sub_32(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result)
{
    result->re = a->re - b->re;
    result->im = a->im - b->im;
}

LFORTRAN_API void _lfortran_complex_sub_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result)
{
    result->re = a->re - b->re;
    result->im = a->im - b->im;
}

LFORTRAN_API void _lfortran_complex_mul_32(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result)
{
    float p = a->re, q = a->im;
    float r = b->re, s = b->im;
    result->re = (p*r - q*s);
    result->im = (p*s + q*r);
}

LFORTRAN_API void _lfortran_complex_mul_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result)
{
    double p = a->re, q = a->im;
    double r = b->re, s = b->im;
    result->re = (p*r - q*s);
    result->im = (p*s + q*r);
}

LFORTRAN_API void _lfortran_complex_div_32(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result)
{
    float p = a->re, q = a->im;
    float r = b->re, s = -(b->im);
    float mod_b = r*r + s*s;
    result->re = (p*r - q*s)/mod_b;
    result->im = (p*s + q*r)/mod_b;
}

LFORTRAN_API void _lfortran_complex_div_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result)
{
    double p = a->re, q = a->im;
    double r = b->re, s = -(b->im);
    double mod_b = r*r + s*s;
    result->re = (p*r - q*s)/mod_b;
    result->im = (p*s + q*r)/mod_b;
}

#undef CMPLX
#undef CMPLXF
#undef CMPLXL
#undef _Imaginary_I

#define _Imaginary_I (I)
#define CMPLX(x, y) ((double complex)((double)(x) + _Imaginary_I * (double)(y)))
#define CMPLXF(x, y) ((float complex)((float)(x) + _Imaginary_I * (float)(y)))
#define CMPLXL(x, y) ((long double complex)((long double)(x) + \
                      _Imaginary_I * (long double)(y)))
#define BITS_32 32
#define BITS_64 64

LFORTRAN_API void _lfortran_complex_pow_32(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result)
{
    #ifdef _MSC_VER
        _Fcomplex ca = _FCOMPLEX_(a->re, a->im);
        _Fcomplex cb = _FCOMPLEX_(b->re, b->im);
        _Fcomplex cr = cpowf(ca, cb);
    #else
        float complex ca = CMPLXF(a->re, a->im);
        float complex cb = CMPLXF(b->re, b->im);
        float complex cr = cpowf(ca, cb);
    #endif
        result->re = crealf(cr);
        result->im = cimagf(cr);

}

LFORTRAN_API void _lfortran_complex_pow_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result)
{
    #ifdef _MSC_VER
        _Dcomplex ca = _DCOMPLEX_(a->re, a->im);
        _Dcomplex cb = _DCOMPLEX_(b->re, b->im);
        _Dcomplex cr = cpow(ca, cb);
    #else
        double complex ca = CMPLX(a->re, a->im);
        double complex cb = CMPLX(b->re, b->im);
        double complex cr = cpow(ca, cb);
    #endif
        result->re = creal(cr);
        result->im = cimag(cr);

}

int64_t _lfortran_integer_pow_64(int64_t base, int64_t exponent){ // Binary Exponentiation
    int64_t res = 1;
    int64_t temp = base;
    if( exponent < 0 ) return 0;
    while(exponent){
        if(exponent%2){
            exponent--;
            res *= temp;
        } else {
            temp *= temp;
            exponent/=2;
        }
    }
    return res;
}

// sqrt ------------------------------------------------------------------------

LFORTRAN_API float_complex_t _lfortran_csqrt(float_complex_t x)
{
    return csqrtf(x);
}

LFORTRAN_API double_complex_t _lfortran_zsqrt(double_complex_t x)
{
    return csqrt(x);
}

// aimag -----------------------------------------------------------------------

LFORTRAN_API void _lfortran_complex_aimag_32(struct _lfortran_complex_32 *x, float *res)
{
    *res = x->im;
}

LFORTRAN_API void _lfortran_complex_aimag_64(struct _lfortran_complex_64 *x, double *res)
{
    *res = x->im;
}

// exp -------------------------------------------------------------------------

LFORTRAN_API float _lfortran_sexp(float x)
{
    return expf(x);
}

LFORTRAN_API double _lfortran_dexp(double x)
{
    return exp(x);
}

LFORTRAN_API float_complex_t _lfortran_cexp(float_complex_t x)
{
    return cexpf(x);
}

LFORTRAN_API double_complex_t _lfortran_zexp(double_complex_t x)
{
    return cexp(x);
}

// log -------------------------------------------------------------------------

LFORTRAN_API float _lfortran_slog(float x)
{
    return logf(x);
}

LFORTRAN_API double _lfortran_dlog(double x)
{
    return log(x);
}

LFORTRAN_API bool _lfortran_sis_nan(float x) {
    return isnan(x);
}

LFORTRAN_API bool _lfortran_dis_nan(double x) {
    return isnan(x);
}

LFORTRAN_API float_complex_t _lfortran_clog(float_complex_t x)
{
    return clogf(x);
}

LFORTRAN_API double_complex_t _lfortran_zlog(double_complex_t x)
{
    return clog(x);
}

// erf -------------------------------------------------------------------------

LFORTRAN_API float _lfortran_serf(float x)
{
    return erff(x);
}

LFORTRAN_API double _lfortran_derf(double x)
{
    return erf(x);
}

// erfc ------------------------------------------------------------------------

LFORTRAN_API float _lfortran_serfc(float x)
{
    return erfcf(x);
}

LFORTRAN_API double _lfortran_derfc(double x)
{
    return erfc(x);
}

// log10 -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_slog10(float x)
{
    return log10f(x);
}

LFORTRAN_API double _lfortran_dlog10(double x)
{
    return log10(x);
}

// gamma -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_sgamma(float x)
{
    return tgammaf(x);
}

LFORTRAN_API double _lfortran_dgamma(double x)
{
    return tgamma(x);
}

// gamma -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_slog_gamma(float x)
{
    return lgammaf(x);
}

LFORTRAN_API double _lfortran_dlog_gamma(double x)
{
    return lgamma(x);
}

// besselj0 --------------------------------------------------------------------

LFORTRAN_API double _lfortran_dbessel_j0( double x ) {
    return j0(x);
}

LFORTRAN_API float _lfortran_sbessel_j0( float x ) {
    return j0(x);
}

// besselj1 --------------------------------------------------------------------

LFORTRAN_API double _lfortran_dbessel_j1( double x ) {
    return j1(x);
}

LFORTRAN_API float _lfortran_sbessel_j1( float x ) {
    return j1(x);
}

// besseljn --------------------------------------------------------------------

LFORTRAN_API double _lfortran_dbesseljn( int n, double x ) {
    return jn(n, x);
}

LFORTRAN_API float _lfortran_sbesseljn( int n, float x ) {
    return jn(n, x);
}

// bessely0 --------------------------------------------------------------------

LFORTRAN_API double _lfortran_dbessel_y0( double x ) {
    return y0(x);
}

LFORTRAN_API float _lfortran_sbessel_y0( float x ) {
    return y0(x);
}

// bessely1 --------------------------------------------------------------------

LFORTRAN_API double _lfortran_dbessel_y1( double x ) {
    return y1(x);
}

LFORTRAN_API float _lfortran_sbessel_y1( float x ) {
    return y1(x);
}

// besselyn --------------------------------------------------------------------

LFORTRAN_API double _lfortran_dbesselyn( int n, double x ) {
    return yn(n, x);
}

LFORTRAN_API float _lfortran_sbesselyn( int n, float x ) {
    return yn(n, x);
}

uint64_t cutoff_extra_bits(uint64_t num, uint32_t bits_size, uint32_t max_bits_size) {
    if (bits_size == max_bits_size) {
        return num;
    }
    return (num & ((1lu << bits_size) - 1lu));
}

LFORTRAN_API int _lfortran_sishftc(int val, int shift_signed, int bits_size) {
    uint32_t max_bits_size = 64;
    bool negative_shift = (shift_signed < 0);
    uint32_t shift = abs(shift_signed);

    uint64_t val1 = cutoff_extra_bits((uint64_t)val, (uint32_t)bits_size, max_bits_size);
    uint64_t result;
    if (negative_shift) {
        result = (val1 >> shift) | cutoff_extra_bits(val1 << (bits_size - shift), bits_size, max_bits_size);
    } else {
        result = cutoff_extra_bits(val1 << shift, bits_size, max_bits_size) | ((val1 >> (bits_size - shift)));
    }
    return result;
}

LFORTRAN_API int64_t _lfortran_dishftc(int64_t val, int64_t shift_signed, int64_t bits_size) {
    uint32_t max_bits_size = 64;
    bool negative_shift = (shift_signed < 0);
    uint32_t shift = llabs(shift_signed);

    uint64_t val1 = cutoff_extra_bits((uint64_t)val, (uint32_t)bits_size, max_bits_size);
    uint64_t result;
    if (negative_shift) {
        result = (val1 >> shift) | cutoff_extra_bits(val1 << (bits_size - shift), bits_size, max_bits_size);
    } else {
        result = cutoff_extra_bits(val1 << shift, bits_size, max_bits_size) | ((val1 >> (bits_size - shift)));
    }
    return result;
}

// sin -------------------------------------------------------------------------

LFORTRAN_API float _lfortran_ssin(float x)
{
    return sinf(x);
}

LFORTRAN_API double _lfortran_dsin(double x)
{
    return sin(x);
}

LFORTRAN_API float_complex_t _lfortran_csin(float_complex_t x)
{
    return csinf(x);
}

LFORTRAN_API double_complex_t _lfortran_zsin(double_complex_t x)
{
    return csin(x);
}

LFORTRAN_API float _lfortran_ssind(float x)
{
    float radians = (x * PI) / 180.0;
    return sin(radians);
}

LFORTRAN_API double _lfortran_dsind(double x)
{
    double radians = (x * PI) / 180.0;
    return sin(radians);
}

// cos -------------------------------------------------------------------------

LFORTRAN_API float _lfortran_scos(float x)
{
    return cosf(x);
}

LFORTRAN_API double _lfortran_dcos(double x)
{
    return cos(x);
}

LFORTRAN_API float_complex_t _lfortran_ccos(float_complex_t x)
{
    return ccosf(x);
}

LFORTRAN_API double_complex_t _lfortran_zcos(double_complex_t x)
{
    return ccos(x);
}

LFORTRAN_API float _lfortran_scosd(float x)
{
    float radians = (x * PI) / 180.0;
    return cos(radians);
}

LFORTRAN_API double _lfortran_dcosd(double x)
{
    double radians = (x * PI) / 180.0;
    return cos(radians);
}

// tan -------------------------------------------------------------------------

LFORTRAN_API float _lfortran_stan(float x)
{
    return tanf(x);
}

LFORTRAN_API double _lfortran_dtan(double x)
{
    return tan(x);
}

LFORTRAN_API float_complex_t _lfortran_ctan(float_complex_t x)
{
    return ctanf(x);
}

LFORTRAN_API double_complex_t _lfortran_ztan(double_complex_t x)
{
    return ctan(x);
}

LFORTRAN_API float _lfortran_stand(float x)
{
    float radians = (x * PI) / 180.0;
    return tan(radians);
}

LFORTRAN_API double _lfortran_dtand(double x)
{
    double radians = (x * PI) / 180.0;
    return tan(radians);
}

// sinh ------------------------------------------------------------------------

LFORTRAN_API float _lfortran_ssinh(float x)
{
    return sinhf(x);
}

LFORTRAN_API double _lfortran_dsinh(double x)
{
    return sinh(x);
}

LFORTRAN_API float_complex_t _lfortran_csinh(float_complex_t x)
{
    return csinhf(x);
}

LFORTRAN_API double_complex_t _lfortran_zsinh(double_complex_t x)
{
    return csinh(x);
}

// cosh ------------------------------------------------------------------------


LFORTRAN_API float _lfortran_scosh(float x)
{
    return coshf(x);
}

LFORTRAN_API double _lfortran_dcosh(double x)
{
    return cosh(x);
}

LFORTRAN_API float_complex_t _lfortran_ccosh(float_complex_t x)
{
    return ccoshf(x);
}

LFORTRAN_API double_complex_t _lfortran_zcosh(double_complex_t x)
{
    return ccosh(x);
}

// tanh ------------------------------------------------------------------------

LFORTRAN_API float _lfortran_stanh(float x)
{
    return tanhf(x);
}

LFORTRAN_API double _lfortran_dtanh(double x)
{
    return tanh(x);
}

LFORTRAN_API float_complex_t _lfortran_ctanh(float_complex_t x)
{
    return ctanhf(x);
}

LFORTRAN_API double_complex_t _lfortran_ztanh(double_complex_t x)
{
    return ctanh(x);
}

// asin ------------------------------------------------------------------------

LFORTRAN_API float _lfortran_sasin(float x)
{
    return asinf(x);
}

LFORTRAN_API double _lfortran_dasin(double x)
{
    return asin(x);
}

LFORTRAN_API float_complex_t _lfortran_casin(float_complex_t x)
{
    return casinf(x);
}

LFORTRAN_API double_complex_t _lfortran_zasin(double_complex_t x)
{
    return casin(x);
}

LFORTRAN_API float _lfortran_sasind(float x)
{
    return (asin(x)*180)/PI;
}

LFORTRAN_API double _lfortran_dasind(double x)
{
    return (asin(x)*180)/PI;
}

// acos ------------------------------------------------------------------------

LFORTRAN_API float _lfortran_sacos(float x)
{
    return acosf(x);
}

LFORTRAN_API double _lfortran_dacos(double x)
{
    return acos(x);
}

LFORTRAN_API float_complex_t _lfortran_cacos(float_complex_t x)
{
    return cacosf(x);
}

LFORTRAN_API double_complex_t _lfortran_zacos(double_complex_t x)
{
    return cacos(x);
}

LFORTRAN_API float _lfortran_sacosd(float x)
{
    return (acos(x)*180)/PI;
}

LFORTRAN_API double _lfortran_dacosd(double x)
{
    return (acos(x)*180)/PI;
}

// atan ------------------------------------------------------------------------

LFORTRAN_API float _lfortran_satan(float x)
{
    return atanf(x);
}

LFORTRAN_API double _lfortran_datan(double x)
{
    return atan(x);
}

LFORTRAN_API float_complex_t _lfortran_catan(float_complex_t x)
{
    return catanf(x);
}

LFORTRAN_API double_complex_t _lfortran_zatan(double_complex_t x)
{
    return catan(x);
}

LFORTRAN_API float _lfortran_satand(float x)
{
    return (atan(x)*180)/PI;
}

LFORTRAN_API double _lfortran_datand(double x)
{
    return (atan(x)*180)/PI;
}

// atan2 -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_satan2(float y, float x)
{
    return atan2f(y, x);
}

LFORTRAN_API double _lfortran_datan2(double y, double x)
{
    return atan2(y, x);
}

// asinh -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_sasinh(float x)
{
    return asinhf(x);
}

LFORTRAN_API double _lfortran_dasinh(double x)
{
    return asinh(x);
}

LFORTRAN_API float_complex_t _lfortran_casinh(float_complex_t x)
{
    return casinhf(x);
}

LFORTRAN_API double_complex_t _lfortran_zasinh(double_complex_t x)
{
    return casinh(x);
}

// acosh -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_sacosh(float x)
{
    return acoshf(x);
}

LFORTRAN_API double _lfortran_dacosh(double x)
{
    return acosh(x);
}

LFORTRAN_API float_complex_t _lfortran_cacosh(float_complex_t x)
{
    return cacoshf(x);
}

LFORTRAN_API double_complex_t _lfortran_zacosh(double_complex_t x)
{
    return cacosh(x);
}

// fmod -----------------------------------------------------------------------

LFORTRAN_API double _lfortran_dfmod(double x, double y)
{
    return fmod(x, y);
}

// atanh -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_satanh(float x)
{
    return atanhf(x);
}

LFORTRAN_API double _lfortran_datanh(double x)
{
    return atanh(x);
}

LFORTRAN_API float_complex_t _lfortran_catanh(float_complex_t x)
{
    return catanhf(x);
}

LFORTRAN_API double_complex_t _lfortran_zatanh(double_complex_t x)
{
    return catanh(x);
}

// trunc -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_strunc(float x)
{
    return truncf(x);
}

LFORTRAN_API double _lfortran_dtrunc(double x)
{
    return trunc(x);
}

// fix -----------------------------------------------------------------------

LFORTRAN_API float _lfortran_sfix(float x)
{
    if (x > 0.0) {
        return floorf(x);
    } else {
        return ceilf(x);
    }
}

LFORTRAN_API double _lfortran_dfix(double x)
{
    if (x > 0.0) {
        return floor(x);
    } else {
        return ceil(x);
    }
}

// phase --------------------------------------------------------------------

LFORTRAN_API float _lfortran_cphase(float_complex_t x)
{
    return atan2f(cimagf(x), crealf(x));
}

LFORTRAN_API double _lfortran_zphase(double_complex_t x)
{
    return atan2(cimag(x), creal(x));
}

// strcat  --------------------------------------------------------------------

LFORTRAN_API char* _lfortran_strcat(
    char* s1, int64_t s1_len,
    char* s2, int64_t s2_len)
{  
    int cntr = 0;
    char trmn = '\0';
    int trmn_size = sizeof(trmn);
    char* dest_char = (char*)malloc(s1_len+s2_len+trmn_size);
    for (int i = 0; i < s1_len; i++) {
        dest_char[cntr] = s1[i];
        cntr++;
    }
    for (int i = 0; i < s2_len; i++) {
        dest_char[cntr] = s2[i];
        cntr++;
    }
    dest_char[s1_len+s2_len+trmn_size-1] = trmn;
    return dest_char;
}

/*  
    Copy from RHS into LHS and,
    pad right if LHS is longer than RHS
*/
LFORTRAN_API void _lfortran_copy_str_and_pad(
    char* lhs, int64_t lhs_len,
    char* rhs, int64_t rhs_len){

    lfortran_assert(lhs != NULL, "Run-time Error : Copying into unallocated LHS string")
    if(rhs == NULL) lfortran_error("Run-time Error : Copying from unallocated RHS string");

    for (int64_t i = 0; i < lhs_len; i++) {
        if (i < rhs_len) {
            lhs[i] = rhs[i];
        } else {
            lhs[i] = ' ';
        }
    }
    lhs[lhs_len] = '\0'; // Null-terminate the string (TODO::remove)
}
// TODO : split them into three functions instead of making compile-time choices at runtime
LFORTRAN_API void _lfortran_strcpy(
    char** lhs, int64_t* lhs_len,
    bool is_lhs_allocatable, bool is_lhs_deferred,
    char* rhs, int64_t rhs_len){
    int null_terminated_string = 1; //TODO : remove this
    if(!is_lhs_deferred && !is_lhs_allocatable){
        if(*lhs == NULL){lfortran_error("Runtime Error : Non-allocatable string isn't allocted");}
        _lfortran_copy_str_and_pad(*lhs, *lhs_len, rhs, rhs_len);
    } else if (!is_lhs_deferred && is_lhs_allocatable){ // Automatic Allocation
        if(*lhs == NULL) *lhs = (char*)malloc((*lhs_len + null_terminated_string) * sizeof(char));
        _lfortran_copy_str_and_pad(*lhs, *lhs_len, rhs, rhs_len);
    } else if (is_lhs_deferred && is_lhs_allocatable) { // Automatic Reallocation
        if (rhs == NULL)
            return;
        *lhs = (char*)realloc(*lhs, (rhs_len + null_terminated_string) * sizeof(char));
        *lhs_len = rhs_len;
        for(int64_t i = 0; i < rhs_len; i++) {(*lhs)[i] = rhs[i];}
        if(null_terminated_string)(*lhs)[rhs_len] = '\0';
    } else if(is_lhs_deferred && !is_lhs_allocatable) {
        if(*lhs == NULL){lfortran_error("Runtime Error : Non-allocatable string isn't allocted");}
        _lfortran_copy_str_and_pad(*lhs, *lhs_len, rhs, rhs_len);
    }
}


#define MIN(x, y) ((x < y) ? x : y)

int strlen_without_trailing_space(char *str, int64_t len) {
    int end = len - 1;
    while(end >= 0 && str[end] == ' ') end--;
    return end + 1;
}

int str_compare(char *s1, int64_t s1_len, char *s2, int64_t s2_len){
    int s1_len_ = strlen_without_trailing_space(s1, s1_len);
    int s2_len_ = strlen_without_trailing_space(s2, s2_len);
    int lim = MIN(s1_len_, s2_len_);
    int res = 0;
    int i ;
    for (i = 0; i < lim; i++) {
        if (s1[i] != s2[i]) {
            res = s1[i] - s2[i];
            break;
        }
    }
    res = (i == lim)? s1_len_ - s2_len_ : res;
    return res;
}

LFORTRAN_API char* _lfortran_float_to_str4(float num)
{
    char* res = (char*)malloc(40);
    sprintf(res, "%f", num);
    return res;
}

LFORTRAN_API char* _lfortran_float_to_str8(double num)
{
    char* res = (char*)malloc(40);
    sprintf(res, "%f", num);
    return res;
}

LFORTRAN_API char* _lfortran_int_to_str1(int8_t num)
{
    char* res = (char*)malloc(40);
    sprintf(res, "%d", num);
    return res;
}

LFORTRAN_API char* _lfortran_int_to_str2(int16_t num)
{
    char* res = (char*)malloc(40);
    sprintf(res, "%d", num);
    return res;
}

LFORTRAN_API char* _lfortran_int_to_str4(int32_t num)
{
    char* res = (char*)malloc(40);
    sprintf(res, "%d", num);
    return res;
}

LFORTRAN_API char* _lfortran_int_to_str8(int64_t num)
{
    char* res = (char*)malloc(40);
    long long num2 = num;
    sprintf(res, "%lld", num2);
    return res;
}

LFORTRAN_API int32_t _lpython_bit_length1(int8_t num)
{
    int32_t res = 0;
    num = abs((int)num);
    while (num > 0) {
        num = num >> 1;
        res++;
    }
    return res;
}

LFORTRAN_API int32_t _lpython_bit_length2(int16_t num)
{
    int32_t res = 0;
    num = abs((int)num);
    while (num > 0) {
        num = num >> 1;
        res++;
    }
    return res;
}

LFORTRAN_API int32_t _lpython_bit_length4(int32_t num)
{
    int32_t res = 0;
    num = abs((int)num);
    while (num > 0) {
        num = num >> 1;
        res++;
    }
    return res;
}

LFORTRAN_API int32_t _lpython_bit_length8(int64_t num)
{
    int32_t res = 0;
    num = llabs(num);
    while (num > 0) {
        num = num >> 1;
        res++;
    }
    return res;
}

//repeat str for n time
LFORTRAN_API void _lfortran_strrepeat(char** s, int32_t n, char** dest)
{
    int cntr = 0;
    char trmn = '\0';
    int s_len = strlen(*s);
    int trmn_size = sizeof(trmn);
    int f_len = s_len*n;
    if (f_len < 0)
        f_len = 0;
    char* dest_char = (char*)malloc(f_len+trmn_size);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < s_len; j++) {
            dest_char[cntr] = (*s)[j];
            cntr++;
        }
    }
    dest_char[cntr] = trmn;
    *dest = &(dest_char[0]);
}

LFORTRAN_API char* _lfortran_strrepeat_c(char* s, int32_t n)
{
    int cntr = 0;
    char trmn = '\0';
    int s_len = strlen(s);
    int trmn_size = sizeof(trmn);
    int f_len = s_len*n;
    if (f_len < 0)
        f_len = 0;
    char* dest_char = (char*)malloc(f_len+trmn_size);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < s_len; j++) {
            dest_char[cntr] = s[j];
            cntr++;
        }
    }
    dest_char[cntr] = trmn;
    return dest_char;
}

// idx starts from 1
LFORTRAN_API char* _lfortran_str_item(char* s, int64_t s_len, int64_t idx) {

    // TODO: Check at compile time instead of runtime. if out of bound due to runtime values, it's user's problem.
    int64_t original_idx = idx - 1;
    if (idx < 1) idx += s_len;
    if (idx < 1 || idx >= s_len + 1) {
        printf("String index: %" PRId64 "is out of Bounds\n", original_idx);
        exit(1);
    }
    char* res = (char*)malloc(2);
    res[0] = s[idx-1];
    res[1] = '\0';
    return res;
}

// Specific For Fortran Strings
LFORTRAN_API char* _lfortran_str_slice_fortran(char* s, int64_t start /*1-Based index*/, int64_t end){
    if( (start<=0) || (end <=0) ){
        char* empty = malloc(1*sizeof(char));
        empty = "";
        return empty;
    }
    char* return_str = (char*)malloc(end - start + 1 + 1 /*Null Char*/);
    memcpy(return_str, s + start - 1, end - start + 1);
    return_str[end - start + 1] = '\0';
    return return_str;

}

LFORTRAN_API char* _lfortran_str_slice(char* s, int64_t s_len, int64_t idx1, int64_t idx2, int64_t step,
                        bool idx1_present, bool idx2_present) {    
    if (step == 0) {
        printf("slice step cannot be zero\n");
        exit(1);
    }
    idx1 = idx1 < 0 ? idx1 + s_len : idx1;
    idx2 = idx2 < 0 ? idx2 + s_len : idx2;
    if (!idx1_present) {
        if (step > 0) {
            idx1 = 0;
        } else {
            idx1 = s_len - 1;
        }
    }
    if (!idx2_present) {
        if (step > 0) {
            idx2 = s_len;
        } else {
            idx2 = -1;
        }
    }
    if (idx1 == idx2 ||
        (step > 0 && (idx1 > idx2 || idx1 >= s_len)) ||
        (step < 0 && (idx1 < idx2 || idx2 >= s_len-1)))
        return "";
    int dest_len = 0;
    if (step > 0) {
        idx2 = idx2 > s_len ? s_len : idx2;
        dest_len = (idx2-idx1+step-1)/step + 1;
    } else {
        idx1 = idx1 >= s_len ? s_len-1 : idx1;
        dest_len = (idx2-idx1+step+1)/step + 1;
    }

    char* dest_char = (char*)malloc(dest_len);
    int s_i = idx1, d_i = 0;
    while((step > 0 && s_i >= idx1 && s_i < idx2) ||
        (step < 0 && s_i <= idx1 && s_i > idx2)) {
        dest_char[d_i++] = s[s_i];
        s_i+=step;
    }
    dest_char[d_i] = '\0';
    return dest_char;
}

LFORTRAN_API char* _lfortran_str_slice_assign(char* s, int64_t s_len, char *r, int64_t r_len, int32_t idx1 /*1-Index-based*/, int32_t idx2/*1-Index-based*/, int32_t step,
                        bool idx1_present, bool idx2_present) {
    idx1--;
    if (step == 0) {
        printf("slice step cannot be zero\n");
        exit(1);
    }
    s_len = (s_len < r_len) ? r_len : s_len;
    idx1 = idx1 < 0 ? idx1 + s_len : idx1;
    idx2 = idx2 < 0 ? idx2 + s_len : idx2;
    if (!idx1_present) {
        if (step > 0) {
            idx1 = 0;
        } else {
            idx1 = s_len - 1;
        }
    }
    if (!idx2_present) {
        if (step > 0) {
            idx2 = s_len;
        } else {
            idx2 = -1;
        }
    }
    if (idx1 == idx2 ||
        (step > 0 && (idx1 > idx2 || idx1 >= s_len)) ||
        (step < 0 && (idx1 < idx2 || idx2 >= s_len-1))) {
        return s;
    }

    char* dest_char = (char*)malloc(s_len + 1 /* \0 */);
    memcpy(dest_char, s, s_len + 1 /* \0 */);
    int s_i = idx1, d_i = 0;
    while((step > 0 && s_i >= idx1 && s_i < idx2) ||
        (step < 0 && s_i <= idx1 && s_i > idx2)) {
        dest_char[s_i] = r[d_i++];
        s_i += step;
    }
    return dest_char;
}

LFORTRAN_API int64_t _lfortran_str_len(char* s)
{
    return strlen(s);
}

LFORTRAN_API int _lfortran_str_to_int(char** s)
{
    char *ptr;
    return strtol(*s, &ptr, 10);
}

LFORTRAN_API int _lfortran_str_ord(char** s)
{
    return (*s)[0];
}

LFORTRAN_API int _lfortran_str_ord_c(char* s)
{
    return s[0];
}

LFORTRAN_API char* _lfortran_str_chr(uint8_t val)
{
    char* dest_char = (char*)malloc(2);
    uint8_t extended_ascii = val;
    dest_char[0] = extended_ascii;
    dest_char[1] = '\0';
    return dest_char;
}

LFORTRAN_API void _lfortran_memset(void* s, int32_t c, int32_t size) {
    memset(s, c, size);
}

LFORTRAN_API void* _lfortran_malloc(int64_t size) {
    return malloc((size_t)size);
}

LFORTRAN_API int8_t* _lfortran_realloc(int8_t* ptr, int64_t size) {
    return (int8_t*) realloc(ptr, (size_t)size);
}

LFORTRAN_API int8_t* _lfortran_calloc(int32_t count, int32_t size) {
    return (int8_t*) calloc(count, size);
}

LFORTRAN_API void _lfortran_free(char* ptr) {
    free((void*)ptr);
}


//Remove once we stop depending on null
LFORTRAN_API void _lfortran_string_init(int64_t size_plus_one /*\0 included*/, char *s) {
    int size = size_plus_one-1;
    // memset(s, ' ', size);
    s[size] = '\0';
}

// bit  ------------------------------------------------------------------------

LFORTRAN_API int32_t _lfortran_mvbits32(int32_t from, int32_t frompos,
                                        int32_t len, int32_t to, int32_t topos) {
    uint32_t all_ones = ~0;
    uint32_t ufrom = from;
    uint32_t uto = to;
    all_ones <<= (BITS_32 - frompos - len);
    all_ones >>= (BITS_32 - len);
    all_ones <<= topos;
    ufrom <<= (BITS_32 - frompos - len);
    ufrom >>= (BITS_32 - len);
    ufrom <<= topos;
    return (~all_ones & uto) | ufrom;
}

LFORTRAN_API int64_t _lfortran_mvbits64(int64_t from, int32_t frompos,
                                        int32_t len, int64_t to, int32_t topos) {
    uint64_t all_ones = ~0;
    uint64_t ufrom = from;
    uint64_t uto = to;
    all_ones <<= (BITS_64 - frompos - len);
    all_ones >>= (BITS_64 - len);
    all_ones <<= topos;
    ufrom <<= (BITS_64 - frompos - len);
    ufrom >>= (BITS_64 - len);
    ufrom <<= topos;
    return (~all_ones & uto) | ufrom;
}

LFORTRAN_API int32_t _lfortran_ibits32(int32_t i, int32_t pos, int32_t len) {
    uint32_t ui = i;
    return ((ui << (BITS_32 - pos - len)) >> (BITS_32 - len));
}

LFORTRAN_API int64_t _lfortran_ibits64(int64_t i, int32_t pos, int32_t len) {
    uint64_t ui = i;
    return ((ui << (BITS_64 - pos - len)) >> (BITS_64 - len));
}

// cpu_time  -------------------------------------------------------------------

LFORTRAN_API double _lfortran_d_cpu_time() {
    return ((double) clock()) / CLOCKS_PER_SEC;
}

LFORTRAN_API float _lfortran_s_cpu_time() {
    return ((float) clock()) / CLOCKS_PER_SEC;
}

// system_time -----------------------------------------------------------------

LFORTRAN_API int32_t _lfortran_i32sys_clock_count() {
#if defined(_WIN32)
    return - INT_MAX;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        return (int32_t)(ts.tv_nsec / 1000000) + ((int32_t)ts.tv_sec * 1000);
    } else {
        return - INT_MAX;
    }
#endif
}

LFORTRAN_API int32_t _lfortran_i32sys_clock_count_rate() {
#if defined(_WIN32)
    return - INT_MAX;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        return 1e3; // milliseconds
    } else {
        return 0;
    }
#endif
}

LFORTRAN_API int32_t _lfortran_i32sys_clock_count_max() {
#if defined(_WIN32)
    return 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        return INT_MAX;
    } else {
        return 0;
    }
#endif
}

LFORTRAN_API uint64_t _lfortran_i64sys_clock_count() {
#if defined(_WIN32)
    return 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        return (uint64_t)(ts.tv_nsec) + ((uint64_t)ts.tv_sec * 1000000000);
    } else {
        return - LLONG_MAX;
    }
#endif
}

LFORTRAN_API int64_t _lfortran_i64sys_clock_count_rate() {
#if defined(_WIN32)
    return 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        // FIXME: Rate can be in microseconds or nanoseconds depending on
        //          resolution of the underlying platform clock.
        return 1e9; // nanoseconds
    } else {
        return 0;
    }
#endif
}

LFORTRAN_API int64_t _lfortran_i64sys_clock_count_max() {
#if defined(_WIN32)
    return 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        return LLONG_MAX;
    } else {
        return 0;
    }
#endif
}

LFORTRAN_API float _lfortran_i32r32sys_clock_count_rate() {
#if defined(_WIN32)
    return 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        return 1e3; // milliseconds
    } else {
        return 0;
    }
#endif
}

LFORTRAN_API double _lfortran_i64r64sys_clock_count_rate() {
#if defined(_WIN32)
    return 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        return 1e9; // nanoseconds
    } else {
        return 0;
    }
#endif
}

// result format is -> "(+|-)hhmm\0" = 5 + 1
LFORTRAN_API void _lfortran_zone(char* result) {
#if defined(_WIN32)
    // Windows doesn't provide timezone offset directly, so we calculate it
    TIME_ZONE_INFORMATION tzinfo;
    DWORD retval = GetTimeZoneInformation(&tzinfo);
    
    // Calculate the total offset in minutes
    int offset_minutes = -tzinfo.Bias; // Bias is in minutes; negative for UTC+
    
    if (retval == TIME_ZONE_ID_DAYLIGHT) {
        offset_minutes -= tzinfo.DaylightBias; // Apply daylight saving if applicable
    } else if (retval == TIME_ZONE_ID_STANDARD) {
        offset_minutes -= tzinfo.StandardBias; // Apply standard bias if applicable
    }

#elif defined(__APPLE__) && !defined(__aarch64__)
    // For non-ARM-based Apple platforms
    time_t t = time(NULL);
    struct tm* ptm = localtime(&t);

    // The tm_gmtoff field holds the time zone offset in seconds
    long offset_seconds = ptm->tm_gmtoff;
    int offset_minutes = offset_seconds / 60;

#else
    // For Linux and other platforms
    time_t t = time(NULL);
    struct tm* ptm = localtime(&t);

    // The tm_gmtoff field holds the time zone offset in seconds
    long offset_seconds = ptm->tm_gmtoff;
    int offset_minutes = offset_seconds / 60;
#endif
    char sign = offset_minutes >= 0 ? '+' : '-';
    int offset_hours = abs(offset_minutes / 60);
    int remaining_minutes = abs(offset_minutes % 60);
    snprintf(result, 12, "%c%02d%02d", sign, offset_hours, remaining_minutes);
}

// Result Format Is -> "hhmmss.sss\0" = 12 + 1
LFORTRAN_API void _lfortran_time(char* result) {
#if defined(_WIN32)
    SYSTEMTIME st;
    GetLocalTime(&st); // Gets the current local time
    sprintf(result, "%02d%02d%02d.%03d", st.wHour, st.wMinute, st.wSecond, st.wMilliseconds);
#elif defined(__APPLE__) && !defined(__aarch64__)
    // For non-ARM-based Apple platforms, use current time functions
    struct timeval tv;
    gettimeofday(&tv, NULL);
    struct tm* ptm = localtime(&tv.tv_sec);
    int milliseconds = tv.tv_usec / 1000;
    sprintf(result, "%02d%02d%02d.%03d", ptm->tm_hour, ptm->tm_min, ptm->tm_sec, milliseconds);
#else
    // For Linux and other platforms
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    struct tm* ptm = localtime(&ts.tv_sec);
    int milliseconds = ts.tv_nsec / 1000000;
    sprintf(result, "%02d%02d%02d.%03d", ptm->tm_hour, ptm->tm_min, ptm->tm_sec, milliseconds);
#endif
}

//Result Format Is -> "ccyymmdd\0" = 8 + 1
LFORTRAN_API void _lfortran_date(char* result) {
    // Allocate memory for the output string (8 characters minimum)
#if defined(_WIN32)
    SYSTEMTIME st;
    GetLocalTime(&st); // Get the current local date
    sprintf(result, "%04d%02d%02d", st.wYear, st.wMonth, st.wDay);
#elif defined(__APPLE__) && !defined(__aarch64__)
    // For non-ARM-based Apple platforms
    time_t t = time(NULL);
    struct tm* ptm = localtime(&t);
    sprintf(result, "%04d%02d%02d", ptm->tm_year + 1900, ptm->tm_mon + 1, ptm->tm_mday);
#else
    // For Linux and other platforms
    time_t t = time(NULL);
    struct tm* ptm = localtime(&t);
    snprintf(result, 32, "%04d%02d%02d", ptm->tm_year + 1900, ptm->tm_mon + 1, ptm->tm_mday);
#endif
}

LFORTRAN_API int32_t _lfortran_values(int32_t n)
{   int32_t result = 0;
#if defined(_WIN32)
    SYSTEMTIME st;
    GetLocalTime(&st); // Get the current local date
    if (n == 1) result = st.wYear;
    else if (n == 2) result = st.wMonth;
    else if (n == 3) result = st.wDay;
    else if (n == 4) result = 330;
    else if (n == 5) result = st.wHour;
    else if (n == 6) result = st.wMinute;
    else if (n == 7) result = st.wSecond;
    else if (n == 8) result = st.wMilliseconds;
#elif defined(__APPLE__) && !defined(__aarch64__)
    // For non-ARM-based Apple platforms
    struct timeval tv;
    gettimeofday(&tv, NULL);
    struct tm* ptm = localtime(&tv.tv_sec);
    int milliseconds = tv.tv_usec / 1000;
    if (n == 1) result = ptm->tm_year + 1900;
    else if (n == 2) result = ptm->tm_mon + 1;
    else if (n == 3) result = ptm->tm_mday;
    else if (n == 4) result = 330;
    else if (n == 5) result = ptm->tm_hour;
    else if (n == 6) result = ptm->tm_min;
    else if (n == 7) result = ptm->tm_sec;
    else if (n == 8) result = milliseconds;
#else
    // For Linux and other platforms
    time_t t = time(NULL);
    struct tm* ptm = localtime(&t);
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    if (n == 1) result = ptm->tm_year + 1900;
    else if (n == 2) result = ptm->tm_mon + 1;
    else if (n == 3) result = ptm->tm_mday;
    else if (n == 4) result = 330;
    else if (n == 5) result = ptm->tm_hour;
    else if (n == 6) result = ptm->tm_min;
    else if (n == 7) result = ptm->tm_sec;
    else if (n == 8) result = ts.tv_nsec / 1000000;
#endif
    return result;
}

LFORTRAN_API float _lfortran_sp_rand_num() {
    return rand() / (float) RAND_MAX;
}

LFORTRAN_API double _lfortran_dp_rand_num() {
    return rand() / (double) RAND_MAX;
}

LFORTRAN_API int32_t _lfortran_int32_rand_num() {
    return rand();
}

LFORTRAN_API int64_t _lfortran_int64_rand_num() {
    return rand();
}

LFORTRAN_API bool _lfortran_random_init(bool repeatable, bool image_distinct) {
    if (repeatable) {
            srand(0);
    } else {
        srand(time(NULL));
    }
    return false;
}

LFORTRAN_API int64_t _lfortran_random_seed(unsigned seed)
{
    srand(seed);
    // The seed array size is typically 8 elements because Fortran's RNG often uses a seed with a fixed length of 8 integers to ensure sufficient randomness and repeatability in generating sequences of random numbers.
    return 8;

}

LFORTRAN_API int64_t _lpython_open(char *path, char *flags)
{
    FILE *fd;
    fd = fopen(path, flags);
    if (!fd)
    {
        printf("Error in opening the file!\n");
        perror(path);
        exit(1);
    }
    return (int64_t)fd;
}

#define MAXUNITS 1000

struct UNIT_FILE {
    int32_t unit;
    char* filename;
    FILE* filep;
    bool unit_file_bin;
    int access_id;
    bool read_access;
    bool write_access;
};

int32_t last_index_used = -1;

struct UNIT_FILE unit_to_file[MAXUNITS];

void store_unit_file(int32_t unit_num, char* filename, FILE* filep, bool unit_file_bin, int access_id, bool read_access, bool write_access) {
    for( int i = 0; i <= last_index_used; i++ ) {
        if( unit_to_file[i].unit == unit_num ) {
            unit_to_file[i].unit = unit_num;
            unit_to_file[i].filep = filep;
            unit_to_file[i].unit_file_bin = unit_file_bin;
            unit_to_file[i].access_id = access_id;
            unit_to_file[i].read_access = read_access;
            unit_to_file[i].write_access = write_access;
        }
    }
    last_index_used += 1;
    if( last_index_used >= MAXUNITS ) {
        printf("Only %d units can be opened for now\n.", MAXUNITS);
        exit(1);
    }
    unit_to_file[last_index_used].unit = unit_num;
    unit_to_file[last_index_used].filename = filename;
    unit_to_file[last_index_used].filep = filep;
    unit_to_file[last_index_used].unit_file_bin = unit_file_bin;
    unit_to_file[last_index_used].access_id = access_id;
    unit_to_file[last_index_used].read_access = read_access;
    unit_to_file[last_index_used].write_access = write_access;
}

FILE* get_file_pointer_from_unit(int32_t unit_num, bool *unit_file_bin, int *access_id, bool *read_access, bool *write_access) {
    if (unit_file_bin) *unit_file_bin = false;
    for( int i = 0; i <= last_index_used; i++ ) {
        if( unit_to_file[i].unit == unit_num ) {
            if (unit_file_bin) *unit_file_bin = unit_to_file[i].unit_file_bin;
            if (access_id) *access_id = unit_to_file[i].access_id;
            if (read_access) *read_access = unit_to_file[i].read_access;
            if (write_access) *write_access = unit_to_file[i].write_access;
            return unit_to_file[i].filep;
        }
    }
    return NULL;
}

char* get_file_name_from_unit(int32_t unit_num, bool *unit_file_bin) {
    *unit_file_bin = false;
    for (int i = 0; i <= last_index_used; i++) {
        if (unit_to_file[i].unit == unit_num) {
            *unit_file_bin = unit_to_file[i].unit_file_bin;
            return unit_to_file[i].filename;
        }
    }
    return NULL;
}

void remove_from_unit_to_file(int32_t unit_num) {
    int index = -1;
    for( int i = 0; i <= last_index_used; i++ ) {
        if( unit_to_file[i].unit == unit_num ) {
            index = i;
            break;
        }
    }
    if( index == -1 ) {
        return ;
    }
    for( int i = index; i < last_index_used; i++ ) {
        unit_to_file[i].unit = unit_to_file[i + 1].unit;
        unit_to_file[i].filename = unit_to_file[i + 1].filename;
        unit_to_file[i].filep = unit_to_file[i + 1].filep;
        unit_to_file[i].unit_file_bin = unit_to_file[i + 1].unit_file_bin;
    }
    last_index_used -= 1;
}

// Note: The length 25 was chosen to be at least as good as UUID
//       which has 32 hex digits (36^24 < 16^32 < 36^25).
#define ID_LEN 25
void get_unique_ID(char buffer[ID_LEN + 1]) {
    const char v[36] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    for (int i = 0; i < ID_LEN; i++) {
        int index = rand() % 36;
        buffer[i] = v[index];
    }
    buffer[ID_LEN] = '\0';
}

static void
trim_trailing_spaces(char** str, int64_t* len, bool init)
{
    if (!init)
        return;  // If the string is initialized manually, we do not trim it
    int64_t i = 0;
    int64_t last_non_space = -1;
    while (i < *len && (*str)[i] != '\0') {
        if (!isspace((unsigned char) (*str)[i])) {
            last_non_space = i;
        }
        i++;
    }
    *len = last_non_space + 1;
    // Null terminate if there's room
    if (*len < i) {
        (*str)[*len] = '\0';
    }
}


static char*
to_c_string(char* src, int64_t len)
{
    char* buf = (char*) malloc(len + 1);
    if (!buf)
        return NULL;
    memcpy(buf, src, len);
    buf[len] = '\0';
    return buf;
}

static void
pad_with_spaces(char* dest, int64_t orig_len, int64_t total_len)
{
    for (int64_t i = orig_len; i < total_len; i++) {
        dest[i] = ' ';
    }
    dest[total_len] = '\0';
}

LFORTRAN_API int64_t
_lfortran_open(int32_t unit_num,
               char* f_name,
               int64_t f_name_len,
               char* status,
               int64_t status_len,
               char* form,
               int64_t form_len,
               char* access,
               int64_t access_len,
               char* iomsg,
               int64_t iomsg_len,
               int32_t* iostat,
               char* action,
               int64_t action_len)
{
    if (iostat != NULL) {
        *iostat = 0;
    }
    bool ini_file = true;
    if (f_name == NULL) {  // Not Provided
        char *prefix = "_lfortran_generated_file", *format = "txt";
        char unique_id[ID_LEN + 1];
        get_unique_ID(unique_id);
        int length = ID_LEN + strlen(prefix) + strlen(format) + 3;
        f_name = (char*) malloc(length);
        snprintf(f_name, length, "%s_%s.%s", prefix, unique_id, format);
        ini_file = false;
    }
    bool ini_status = true;
    if (status == NULL) {
        status = "unknown";
        status_len = 7;
        ini_status = false;
    }
    bool ini_form = true;
    if (form == NULL) {
        form = "formatted";
        form_len = 9;
        ini_form = false;
    }
    bool ini_access = true;
    if (access == NULL) {
        access = "sequential";
        access_len = 10;
        ini_access = false;
    }
    bool ini_action = true;
    if (action == NULL) {
        action = "readwrite";
        action_len = 9;
        ini_action = false;
    }
    bool file_exists[1] = { false };
    FILE* already_open = get_file_pointer_from_unit(unit_num, NULL, NULL, NULL, NULL);

    trim_trailing_spaces(&f_name, &f_name_len, ini_file);
    trim_trailing_spaces(&status, &status_len, ini_status);
    trim_trailing_spaces(&form, &form_len, ini_form);
    trim_trailing_spaces(&action, &action_len, ini_action);

    // Prepare null-terminated names for C APIs
    char* f_name_c = to_c_string(f_name, f_name_len);
    char* status_c = to_c_string(status, status_len);
    char* form_c = to_c_string(form, form_len);
    char* access_c = to_c_string(access, access_len);
    char* action_c = to_c_string(action, action_len);

    _lfortran_inquire(
        f_name, f_name_len, file_exists, -1, NULL, NULL, NULL, NULL, 0, NULL, 0, NULL, 0);
    char* access_mode = NULL;
    /*
     STATUS=`specifier` in the OPEN statement
     The following are the available specifiers:
     * "old"     (file must already exist)
     * "new"     (file does not exist and will be created)
     * "scratch" (temporary file will be deleted when closed)
     * "replace" (file will be created, replacing any existing file)
     * "unknown" (it is not known whether the file exists)
     */
    if (streql(status, "old")) {
        if (!*file_exists) {
            if (iostat != NULL) {
                *iostat = 2;
                if ((iomsg != NULL) && (iomsg_len > 0)) {
                    char* temp
                        = "File `%s` does not exists! Cannot open a file with the `status=old`";
                    snprintf(iomsg, iomsg_len + 1, temp, f_name);
                    pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
                }
            } else {
                printf("Runtime error: File `%s` does not exists!\nCannot open a "
                       "file with the `status=old`\n",
                       f_name);
                exit(1);
            }
        }
        access_mode = "r+";
    } else if (streql(status, "new")) {
        if (*file_exists) {
            if (iostat != NULL) {
                *iostat = 17;
                if ((iomsg != NULL) && (iomsg_len > 0)) {
                    char* temp = "File `%s` exists! Cannot open a file with the `status=new`";
                    snprintf(iomsg, iomsg_len + 1, temp, f_name);
                    pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
                }
            } else {
                printf("Runtime error: File `%s` exists!\nCannot open a file with "
                       "the `status=new`\n",
                       f_name);
                exit(1);
            }
        }
        access_mode = "w+";
    } else if (streql(status, "replace") || streql(status, "scratch")) {
        access_mode = "w+";
    } else if (streql(status, "unknown")) {
        if (!*file_exists && !already_open) {
            FILE* fd = fopen(f_name, "w");
            if (fd) {
                fclose(fd);
            }
        }
        access_mode = "r+";
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "STATUS specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len + 1, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: STATUS specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   status);
            exit(1);
        }
    }

    bool unit_file_bin;
    int access_id;
    bool read_access = true;
    bool write_access = true;
    if (streql(form, "formatted")) {
        unit_file_bin = false;
    } else if (streql(form, "unformatted")) {
        unit_file_bin = true;
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "FORM specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len + 1 /*\0*/, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: FORM specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   form);
            exit(1);
        }
    }

    if (streql(access, "stream")) {
        access_id = 1;
    } else if (streql(access, "sequential")) {
        access_id = 0;
    } else if (streql(access,
                      "direct")) {  // TODO: Handle 'direct' as access while reading or writing
        access_id = 2;
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "ACCESS specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len + 1 /*\0*/, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: ACCESS specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   access);
            exit(1);
        }
    }
    if (streql(action, "readwrite")) {
    } else if (streql(action, "write")) {
        read_access = false;
    } else if (streql(action, "read")) {
        write_access = false;
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "ACTION specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len + 1 /*\0*/, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: ACTION specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   action);
            exit(1);
        }
    }
    if (streql(action, "readwrite")) {
    } else if (streql(action, "write")) {
        read_access = false;
    } else if (streql(action, "read")) {
        write_access = false;
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "ACTION specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len + 1 /*\0*/, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: ACTION specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   action);
            exit(1);
        }
    }

    if (access_mode == NULL
        && iostat != NULL) {  // Case: when iostat is present we don't want to terminate
        access_mode = "r";
    }

    if (iostat == NULL || (*iostat == 0)) {
        if (already_open) {
            return (int64_t) already_open;
        }
        FILE* fd = fopen(f_name, access_mode);
        if (!fd && iostat == NULL) {
            printf("Runtime error: Error in opening the file!\n");
            perror(f_name);
            exit(1);
        }
        store_unit_file(unit_num, f_name, fd, unit_file_bin, access_id, read_access, write_access);
        return (int64_t) fd;
    }
    free(f_name_c);
    free(status_c);
    free(form_c);
    free(access_c);
    free(action_c);
    return 0;
}

LFORTRAN_API void _lfortran_flush(int32_t unit_num)
{
    // special case: flush all open units
    if (unit_num == -1) {
        for (int i = 0; i <= last_index_used; i++) {
            if (unit_to_file[i].filep != NULL) {
                fflush(unit_to_file[i].filep);
            }
        }
    } else {
        bool unit_file_bin;
        FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
        if( filep == NULL ) {
            if ( unit_num == 6 ) {
                // special case: flush OUTPUT_UNIT
                fflush(stdout);
                return;
            } else if ( unit_num == 5 ) {
                // special case: flush INPUT_UNIT
                fflush(stdin);
                return;
            } else if ( unit_num == 0 ) {
                // special case: flush ERROR_UNIT
                fflush(stderr);
                return;
            }
            printf("Specified UNIT %d in FLUSH is not connected.\n", unit_num);
            exit(1);
        }
        fflush(filep);
    }
}

LFORTRAN_API void _lfortran_inquire(char* f_name_data, int64_t f_name_len, bool *exists, int32_t unit_num,
                                    bool *opened, int32_t *size, int32_t *pos,
                                    char *write, int64_t write_len,
                                    char *read, int64_t read_len,
                                    char *readwrite, int64_t readwrite_len) {
    if (f_name_data && unit_num != -1) {
        printf("File name and file unit number cannot be specified together.\n");
        exit(1);
    }
    if (f_name_data != NULL) {
        FILE *fp = fopen(f_name_data, "r");
        if (fp != NULL) {
            *exists = true;
            if (size != NULL) {
                fseek(fp, 0, SEEK_END);
                *size = ftell(fp);
            }
            fclose(fp); // close the file
            return;
        }
        *exists = false;
    }
    if (unit_num != -1) {
        bool unit_file_bin;
        bool read_access;
        bool write_access;
        FILE *fp = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, &read_access, &write_access);
        if (write != NULL) {
            if (write_access) {
                _lfortran_copy_str_and_pad(write, write_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(write, write_len, "NO", 2);
            }
        } if (read != NULL) {
            if (read_access) {
                _lfortran_copy_str_and_pad(read, read_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(read, read_len, "NO", 2);
            }
        } if (readwrite != NULL) {
            if (read_access && write_access) {
                _lfortran_copy_str_and_pad(readwrite, readwrite_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(readwrite, readwrite_len, "NO", 2);
            }
        }
        *opened = (fp != NULL);
        if (pos != NULL && fp != NULL) {
            long p = ftell(fp);
            *pos = (int32_t)p + 1;
        }
    }
}

LFORTRAN_API void _lfortran_rewind(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if( filep == NULL ) {
        printf("Specified UNIT %d in REWIND is not created or connected.\n", unit_num);
        exit(1);
    }
    rewind(filep);
}

LFORTRAN_API void _lfortran_backspace(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* fd = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (fd == NULL) {
        fprintf(stderr, "Specified UNIT %d in BACKSPACE is not created or connected.\n", unit_num);
        exit(1);
    }

    fflush(fd);
    long pos = ftell(fd);
    if (pos <= 0) {
        rewind(fd);
        return;
    }

    int ch;
    pos--;  // Step back from EOF
    while (pos > 0) {
        fseek(fd, --pos, SEEK_SET);
        ch = fgetc(fd);
        if (ch == '\n') {
            fseek(fd, pos + 1, SEEK_SET);  // Move to just after the previous newline
            return;
        }
    }

    // If no newline found, rewind to beginning
    rewind(fd);
}

LFORTRAN_API void _lfortran_read_int16(int16_t *p, int32_t unit_num)
{
    if (unit_num == -1) {
        char buffer[100];   // Long enough buffer to fit any 16 bit integer
        if (!fgets(buffer, sizeof(buffer), stdin)) {
            fprintf(stderr, "Error: Failed to read input.\n");
            exit(1);
        }

        // Use strtok() to extract only the first token before any whitespace
        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            fprintf(stderr, "Error: Invalid input for int16_t.\n");
            exit(1);
        }

        char *endptr = NULL;
        errno = 0;
        long long_val = strtol(token, &endptr, 10);

        if (endptr == token || *endptr != '\0') {
            fprintf(stderr, "Error: Invalid input for int16_t.\n");
            exit(1);
        }

        // check for overflow (when input value is more than the int16 limit)
        if (errno == ERANGE || long_val < INT16_MIN || long_val > INT16_MAX) {
            fprintf(stderr, "Error: Value %ld is out of integer(2) range.\n", long_val);
            exit(1);
        }

        // once we checked its a proper integer, and that, it's within range, we convert it to int16
        *p = (int16_t)long_val;
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            fprintf(stderr, "Error: Failed to read int16_t from binary file.\n");
            exit(1);
        }
    } else {
        long temp;
        if (fscanf(filep, "%ld", &temp) != 1) {
            fprintf(stderr, "Error: Invalid input for int16_t from file.\n");
            exit(1);
        }

        if (temp < INT16_MIN || temp > INT16_MAX) {
            fprintf(stderr, "Error: Value %ld is out of integer(2) range (file).\n", temp);
            exit(1);
        }

        *p = (int16_t)temp;
    }
}

// Improved input validation for integer reading
// - Prevents auto-casting of invalid inputs to integers
// NOTE:- More changes need to be implemented for advanced error detection and check
LFORTRAN_API void _lfortran_read_int32(int32_t *p, int32_t unit_num)
{
    if (unit_num == -1) {
        char buffer[100];   // Long enough buffer to fit any 32 bit integer
        if (!fgets(buffer, sizeof(buffer), stdin)) {
            fprintf(stderr, "Error: Failed to read input.\n");
            exit(1);
        }

        // Use strtok() to extract only the first token before any whitespace
        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            fprintf(stderr, "Error: Invalid input for int32_t.\n");
            exit(1);
        }

        char *endptr = NULL;
        errno = 0;
        long long_val = strtol(token, &endptr, 10);

        if (endptr == token || *endptr != '\0') {
            fprintf(stderr, "Error: Invalid input for int32_t.\n");
            exit(1);
        }

        // check for overflow (when input value is more than the int32 limit)
        if (errno == ERANGE || long_val < INT32_MIN || long_val > INT32_MAX) {
            fprintf(stderr, "Error: Value %ld is out of integer(4) range.\n", long_val);
            exit(1);
        }

        // once we checked its a proper integer, and that, it's within range, we convert it to int32
        *p = (int32_t)long_val;
        return;
    }

    bool unit_file_bin;
    int access_mode; // 0 = sequential, 1 = stream
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_mode, NULL, NULL);
    if (!filep) {
        fprintf(stderr, "Internal Compiler Error: No file found with given unit number %d.\n", unit_num);
        exit(1);
    }

    if (unit_file_bin) {
        if (access_mode == 0) {
            // Sequential unformatted: read with record markers
            int32_t record_start = 0, record_end = 0;
            if (fread(&record_start, sizeof(int32_t), 1, filep) != 1 ||
                fread(p, sizeof(int32_t), 1, filep) != 1 ||
                fread(&record_end, sizeof(int32_t), 1, filep) != 1) {
                fprintf(stderr, "Internal Compiler Error: Failed to read int32_t from sequential binary file.\n");
                exit(1);
            }
            if (record_start != sizeof(int32_t) || record_end != sizeof(int32_t)) {
                fprintf(stderr, "Internal Compiler Error: Invalid record marker while reading int32_t.\n");
                exit(1);
            }
        } else {
            // Stream unformatted: direct read
            if (fread(p, sizeof(int32_t), 1, filep) != 1) {
                fprintf(stderr, "Internal Compiler Error: Failed to read int32_t from stream file.\n");
                exit(1);
            }
        }
    } else {
        long temp;
        if (fscanf(filep, "%ld", &temp) != 1) {
            fprintf(stderr, "Error: Invalid input for int32_t from file.\n");
            exit(1);
        }

        if (temp < INT32_MIN || temp > INT32_MAX) {
            fprintf(stderr, "Error: Value %ld is out of integer(4) range (file).\n", temp);
            exit(1);
        }

        *p = (int32_t)temp;
    }
}

LFORTRAN_API void _lfortran_read_int64(int64_t *p, int32_t unit_num)
{
    if (unit_num == -1) {
        char buffer[100];   // Long enough buffer to fit any 64 bit integer
        if (!fgets(buffer, sizeof(buffer), stdin)) {
            fprintf(stderr, "Error: Failed to read input.\n");
            exit(1);
        }

        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            fprintf(stderr, "Error: Invalid input for int64_t.\n");
            exit(1);
        }

        errno = 0;
        char *endptr = NULL;
        long long long_val = strtoll(token, &endptr, 10);

        if (endptr == token || *endptr != '\0') {
            fprintf(stderr, "Error: Invalid input for int64_t.\n");
            exit(1);
        }

        if (errno == ERANGE || long_val < INT64_MIN || long_val > INT64_MAX) {
            fprintf(stderr, "Error: Value %lld is out of integer(8) range.\n", long_val);
            exit(1);
        }

        *p = (int64_t)long_val;
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            fprintf(stderr, "Error: Failed to read int64_t from binary file.\n");
            exit(1);
        }
    } else {
        int64_t temp;
        if (fscanf(filep, "%" PRId64, &temp) != 1) {
            fprintf(stderr, "Error: Invalid input for int64_t from file.\n");
            exit(1);
        }
        if (temp < INT64_MIN || temp > INT64_MAX) {
            fprintf(stderr, "Error: Value %" PRId64 " is out of integer(8) range (file).\n", temp);
            exit(1);
        }

        *p = (int64_t)temp;
    }
}

// boolean read implementation is in process
// Implementing a Logical read API (starting with the basic input of just logical-further, logicalArray also needed)
// changes for the same are in: asr_to_llvm.cpp (line 8210 onwards)
LFORTRAN_API void _lfortran_read_logical(bool *p, int32_t unit_num)
{
    if (unit_num == -1) {
        // Reading from standard input (console)
        char buffer[100];   // Long enough buffer
        if (!fgets(buffer, sizeof(buffer), stdin)) {
            fprintf(stderr, "Error: Failed to read input.\n");
            exit(1);
        }

        // Tokenize input (by whitespace)
        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            fprintf(stderr, "Error: Invalid input for logical.\n");
            exit(1);
        }

        // converting token to lowecase
        for (int i = 0; token[i]; ++i) token[i] = tolower((unsigned char) token[i]);

        // Check for logical values
        if (strcmp(token, "true") == 0 || strcmp(token, ".true.") == 0 || strcmp(token, ".true") == 0) *p = true;
        else if (strcmp(token, "false") == 0 || strcmp(token, ".false.") == 0 || strcmp(token, ".false") == 0) *p = false;
        else {
            fprintf(stderr, "Error: Invalid logical input '%s'. Use .true., .false., true, false\n", token);
            exit(1);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        // Read logical from binary file
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            fprintf(stderr, "Error: Failed to read logical from binary file.\n");
            exit(1);
        }
    } 
    else {
        // Read logical from text file (fscanf handles the logical format)
        char token[100] = {0};    // Initialize token to avoid garbage values
        if (fscanf(filep, "%99s", token) != 1) {
            fprintf(stderr, "Error: Invalid logical input from file.\n");
            printf("Read token: '%s'\n", token);  // debugging purpose
            exit(1);
        }

        // Sanitize the token (removes trailing \r or \n characters)
        int len = strlen(token);
        while (len > 0 && (token[len-1] == '\r' || token[len-1] == '\n')) {
            token[len-1] = '\0';
            len--;
        }

        // updated fix: Convert to lowercase for consistent comparison
        for (int i = 0; token[i]; ++i) {
            token[i] = tolower((unsigned char) token[i]);
        }

        // comparing once
        if (strcmp(token, "true") == 0 || strcmp(token, ".true.") == 0 || strcmp(token, ".true") == 0) *p = true;
        else if (strcmp(token, "false") == 0 || strcmp(token, ".false.") == 0 || strcmp(token, ".false") == 0) *p = false;
        else {
            fprintf(stderr, "Error: Invalid logical input '%s'. Use .true., .false., true, false\n", token);
            exit(1);
        }
    }
}


LFORTRAN_API void _lfortran_read_array_int8(int8_t *p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%s", &p[i]);
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id != 1) {
            // Read record marker first
            int32_t record_marker_start;
            (void)!fread(&record_marker_start, sizeof(int32_t), 1, filep);
        }
        (void)!fread(p, sizeof(int8_t), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%s", &p[i]);
        }
    }
}

LFORTRAN_API void _lfortran_read_array_int16(int16_t *p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%hd", &p[i]);
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id != 1) {
            // Read record marker first
            int32_t record_marker_start;
            (void)!fread(&record_marker_start, sizeof(int32_t), 1, filep);
        }
        (void)!fread(p, sizeof(int16_t), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%hd", &p[i]);
        }
    }
}

LFORTRAN_API void _lfortran_read_array_int32(int32_t *p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%d", &p[i]);
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id != 1) {
            // Read record marker first
            int32_t record_marker_start;
            (void)!fread(&record_marker_start, sizeof(int32_t), 1, filep);
        }
        (void)!fread(p, sizeof(int32_t), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%d", &p[i]);
        }
    }
}

LFORTRAN_API void _lfortran_read_array_int64(int64_t *p, int array_size, int32_t unit_num) {
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%" SCNd64, &p[i]);
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id != 1) {
            // Read record marker first
            int32_t record_marker_start;
            (void)!fread(&record_marker_start, sizeof(int32_t), 1, filep);
        }
        (void)!fread(p, sizeof(int64_t), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%" SCNd64, &p[i]);
        }
    }
}

LFORTRAN_API void _lfortran_read_char(char **p, int64_t p_len, int32_t unit_num)
{
    const char SPACE = ' ';
    if (unit_num == -1) {
        // Read from stdin
        (void)!fgets(*p, p_len + 1, stdin);
        (*p)[strcspn(*p, "\n")] = 0;
        size_t input_length = strlen(*p);
        while (input_length < p_len) {
            strncat(*p, &SPACE, 1);
            input_length++;
        }
        (*p)[p_len] = '\0';
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        int32_t data_length;
        // Only read header if not access=stream and is at start of file
        if (access_id != 1 && ftell(filep) == 0 &&               
                fread(&data_length, sizeof(int32_t), 1, filep) != 1) {   
            printf("Error reading data length from file.\n");
            exit(1);
        }

        long current_pos = ftell(filep);
        fseek(filep, 0L, SEEK_END);
        long end_pos = ftell(filep);
        fseek(filep, current_pos, SEEK_SET);

        if (access_id != 1) {
            data_length = end_pos - current_pos - 4;  // leave last 4 bits as record marker
        } else {
            data_length = end_pos - current_pos;  // For access=stream read till last
        }


        data_length = data_length > p_len ? p_len : data_length;

        // read the actual data
        if (fread(*p, sizeof(char), data_length, filep) != data_length) {
            printf("Error reading data from file.\n");
            exit(1);
        }
        (*p)[p_len] = '\0';
    } else {
        char *tmp_buffer = (char*)malloc((p_len + 1) * sizeof(char));
        (void)!fscanf(filep, "%s", tmp_buffer);
        size_t input_length = strlen(tmp_buffer);
        strcpy(*p, tmp_buffer);
        free(tmp_buffer);
        while (input_length < p_len) {
            strncat(*p, &SPACE, 1);
            input_length++;
        }
        (*p)[p_len] = '\0';
    }
    if (streql(*p, "")) {
        printf("Runtime error: End of file!\n");
        exit(1);
    }
}


// Improved input validation for float reading
// - Prevents auto-casting of invalid inputs to float/real
// NOTE:- More changes need to be implemented for advanced error detection and check
LFORTRAN_API void _lfortran_read_float(float *p, int32_t unit_num)
{
    if (unit_num == -1) {
        char buffer[100];   // Long enough buffer to fit any 64 bit integer
        if (!fgets(buffer, sizeof(buffer), stdin)) {
            fprintf(stderr, "Error: Failed to read input.\n");
            exit(1);
        }

        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            fprintf(stderr, "Error: Invalid input for float.\n");
            exit(1);
        }

        char *endptr;
        *p = strtof(token, &endptr);

        if (*endptr != '\0') {
            fprintf(stderr, "Error: Invalid input for float.\n");
            exit(1);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            fprintf(stderr, "Error: Failed to read float from binary file.\n");
            exit(1);
        }
    } else {
        if (fscanf(filep, "%f", p) != 1) {
            fprintf(stderr, "Error: Invalid input for float from file.\n");
            exit(1);
        }
    }
}

LFORTRAN_API void _lfortran_read_array_complex_float(struct _lfortran_complex_32 *p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%f %f", &p[i].re, &p[i].im);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(struct _lfortran_complex_32), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            // check if `(` is present, if yes, then we strip spaces for each line
            // and then read (1.0, 2.0) (3.0, 4.0) etc.
            char buffer[100];   // Long enough buffer to fit any complex float
            if (fscanf(filep, "%s", buffer) != 1) {
                fprintf(stderr, "Error: Invalid input for complex float from file.\n");
                exit(1);
            }
            // Remove parentheses and split by comma
            char *start = strchr(buffer, '(');
            char *end = strchr(buffer, ')');
            if (start && end && end > start) {
                *end = '\0';  // Replace ')' with null terminator
                start++;      // Move past '('
                char *comma = strchr(start, ',');
                if (comma) {
                    *comma = '\0';  // Replace ',' with null terminator
                    // strip spaces from start and end
                    while (isspace((unsigned char)*start)) start++;
                    while (isspace((unsigned char)*(end - 1))) end--;
                    p[i].re = strtof(start, NULL);
                    p[i].im = strtof(comma + 1, NULL);
                } else {
                    fprintf(stderr, "Error: Invalid complex float format '%s'.\n", buffer);
                    exit(1);
                }
            } else {
                // If no parentheses, read as two separate floats
                (void)!fscanf(filep, "%f %f", &p[i].re, &p[i].im);
                // Check if the read was successful
                if (ferror(filep)) {
                    fprintf(stderr, "Error: Failed to read complex float from file.\n");
                    exit(1);
                }
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_complex_double(struct _lfortran_complex_64 *p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%lf %lf", &p[i].re, &p[i].im);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(struct _lfortran_complex_64), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            // check if `(` is present, if yes, then we strip spaces for each line
            // and then read (1.0, 2.0) (3.0, 4.0) etc.
            char buffer[100];   // Long enough buffer to fit any complex double
            if (fscanf(filep, "%s", buffer) != 1) {
                fprintf(stderr, "Error: Invalid input for complex double from file.\n");
                exit(1);
            }
            // Remove parentheses and split by comma
            char *start = strchr(buffer, '(');
            char *end = strchr(buffer, ')');
            if (start && end && end > start) {
                *end = '\0';  // Replace ')' with null terminator
                start++;      // Move past '('
                char *comma = strchr(start, ',');
                if (comma) {
                    *comma = '\0';  // Replace ',' with null terminator
                    // strip spaces from start and end
                    while (isspace((unsigned char)*start)) start++;
                    while (isspace((unsigned char)*(end - 1))) end--;
                    p[i].re = strtod(start, NULL);
                    p[i].im = strtod(comma + 1, NULL);
                } else {
                    fprintf(stderr, "Error: Invalid complex double format '%s'.\n", buffer);
                    exit(1);
                }
            } else {
                // If no parentheses, read as two separate doubles
                (void)!fscanf(filep, "%lf %lf", &p[i].re, &p[i].im);
                // Check if the read was successful
                if (ferror(filep)) {
                    fprintf(stderr, "Error: Failed to read complex double from file.\n");
                    exit(1);
                }
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_float(float *p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%f", &p[i]);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(float), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%f", &p[i]);
        }
    }
}

LFORTRAN_API void _lfortran_read_array_double(double *p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            (void)!scanf("%lf", &p[i]);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(double), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%lf", &p[i]);
        }
    }
}

LFORTRAN_API void _lfortran_read_array_char(char *p, int64_t length, int array_size, int32_t unit_num)
{
    if(p == NULL) {fprintf(stderr, "%s\n", "Runtime Error : Unallocted array memory");exit(1);}

    bool unit_file_bin; // Unformatted

    /*
        * TODO :: Check formatting + unit_file_bin
        * (`!unit_file_bin && !Formatting` => Raise RuntimeError)
        * (`unit_file_bin && Formatting` => Raise RuntimeError)
        * We'll need to have `formatting` as a function parameter.
    */

    // Fetch stream
    FILE* filep;
    if(unit_num == -1){
        filep = stdin;
        unit_file_bin = false;
    } else {
        filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
        if (!filep) {printf("No file found with given unit\n"); exit(1);}
    }

    #define null_c_len 1
    if(unit_file_bin){ // Unformatted
        char* tmp_buffer = (char*) malloc((array_size * length) * sizeof(char));
        int max_read = fread(tmp_buffer, sizeof(char), length*array_size, filep);
        for (int i = 0; i < array_size && (i*length < max_read); i++) {
            int l = length;
            if(max_read - (i*length) < l) l = max_read - (i*length);
            memcpy(p + (i*(length + null_c_len)), tmp_buffer+(i*length), l);
            (p+(i*(length+ null_c_len)))[length] = '\0';
        }
        free(tmp_buffer);
        return;
    //TODO : remove the code above and use code below after we remove dependency on null char
        int max_read_chars = fread(p, sizeof(char), length*array_size, filep);
        int remaning = (length*array_size) - max_read_chars; if(remaning < 0) remaning = 0;
        memset(p+max_read_chars, ' ', remaning);
    } else { // Formatted
        char length_format[23] /* '%' + MaxDigits + 's'*/;
        sprintf(length_format, "%%%"PRId64, length);
        strcat(length_format, "s");
        for(int i = 0; i < array_size; i++){
            {
                int scan_ret = 0;
                scan_ret = fscanf(filep, length_format, p+(i*(length+null_c_len))); // Read exactly length 
                if(scan_ret != 1) {lfortran_error("Invalid read (scan)");}
            }
            (void)!fscanf(filep, "%*[^\n \t]"); // Consume the rest of character but ( '\n', ' ', '\t' )
        }
    }

}

LFORTRAN_API void _lfortran_read_double(double *p, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        (void)!scanf("%lf", p);
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(*p), 1, filep);
    } else {
        (void)!fscanf(filep, "%lf", p);
    }
}

LFORTRAN_API void _lfortran_formatted_read(int32_t unit_num, int32_t* iostat, int32_t* chunk, char* advance, int64_t advance_length, char* fmt, int32_t no_of_args, ...)
{
    int width = -1; // default is -1, if length not mentioned

    // Supported format are (a) and (aw)
    if (streql(fmt, "(a)")) {
        width = -1;
    }
    else if(fmt[0] == '(' && (fmt[1] == 'a' || fmt[1] == 'A')) {
        int i = 2;
        while (isdigit(fmt[i])) i++;

        if (fmt[i] == ')' && i > 2) {
            char width_str[16];
            strncpy(width_str, fmt + 2, i - 2);
            width_str[i - 2] = '\0';
            width = atoi(width_str);

            if (width <= 0) {
                printf("Invalid format width in '%s'\n", fmt);
                exit(1);
            }
        } else {
            printf("Only (a) and (aw) are supported.\n");
            exit(1);
        }
    }
    else{
        printf("Only (a) and (aw) are supported.\n");
        exit(1);
    }

    // For now, this supports reading a single argument of type string
    // TODO: Support more arguments and other types

    va_list args;
    va_start(args, no_of_args);
    char* str_data = va_arg(args, char*);
    int64_t str_len = va_arg(args, int64_t);

    if (width == -1) width = str_len;

    const char SPACE = ' ';

    if (unit_num == -1) {
        // Block for reading from standard input (stdin)
        char *buffer = (char*)malloc((width + 1) * sizeof(char));
        if (fgets(buffer, width + 1, stdin) == NULL) {
            *iostat = -1;
            va_end(args);
            free(buffer);
            return;
        } else {
            if (streql(buffer, "\n")) {
                *iostat = -2;
            } else {
                *iostat = 0;
            }

            size_t input_length = strcspn(buffer, "\n");
            *chunk = input_length;

            char *output = (char*)malloc(str_len + 1);
            memset(output, SPACE, str_len); // Initialize with spaces
            output[str_len] = '\0';
            
            if (width > 0) {
                char *padded_buffer = (char*)malloc(width + 1);
                strncpy(padded_buffer, buffer, input_length);
                for (size_t i = input_length; i < width; ++i) {
                    padded_buffer[i] = SPACE;
                }
                padded_buffer[width] = '\0';

                if (width > str_len) {
                    if (input_length >= width) {
                        strncpy(output, padded_buffer + (width - str_len), str_len);
                    } else if (input_length >= str_len) {
                        strncpy(output, buffer + (input_length - str_len), str_len);
                    } else {
                        strncpy(output, buffer, input_length);
                    }
                } else { // width <= n
                    strncpy(output, padded_buffer, width);
                    for(size_t i = width; i < str_len; ++i) {
                        output[i] = SPACE;
                    }
                    output[str_len] = '\0';
                }
                free(padded_buffer);
            } else { // For (a) format (width == str_len)
                strncpy(output, buffer, str_len);
            }

            strncpy(str_data, output, str_len);         

            free(output);
            va_end(args);
            free(buffer);
            return;
        }
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    } else {
        // Block for reading from a file
        char *buffer = (char*)malloc((width + 1) * sizeof(char));
        if (fgets(buffer, width + 1, filep) == NULL) {
            *iostat = -1;
            *chunk=0;
            va_end(args);
            free(buffer);
            return;
        } else {
            // If we have advance="no" specified and we also have '\n' in buffer, iostat = -2 (end of record)
            // (strcspn(buffer, "\n") != n) checks if '\n' is present in buffer or not
            if (streql(buffer, "\n") || (streql(advance, "no") && strcspn(buffer, "\n") != str_len)) {
                *iostat = -2;
            } else {
                *iostat = 0;
            }

            (buffer)[strcspn(buffer, "\n")] = 0;

            size_t input_length = strlen(buffer);
            *chunk = input_length;

            char *output = (char*)malloc(str_len + 1);
            memset(output, SPACE, str_len);
            output[str_len] = '\0';

            if (width > 0) { // For (aw) format
                char *padded_buffer = (char*)malloc(width + 1);
                strncpy(padded_buffer, buffer, width);
                for (size_t i = input_length; i < width; ++i) {
                    padded_buffer[i] = SPACE;
                }
                padded_buffer[width] = '\0';

                if (width > str_len) {
                    if (input_length >= width) {
                        strncpy(output, padded_buffer + (width - str_len), str_len);
                    } else if (input_length >= str_len) {
                        strncpy(output, buffer + (input_length - str_len), str_len);
                    } else {
                        strncpy(output, buffer, input_length);
                    }
                } else { // width <= str_len
                    strncpy(output, padded_buffer, width);
                    for(size_t i = width; i < str_len; ++i) {
                        output[i] = SPACE;
                    }
                    output[str_len] = '\0';
                }
            } else { // For (a) format
                strncpy(output, buffer, str_len);
            }
            
            strncpy(str_data, output, str_len);

            free(output);
            va_end(args);
            free(buffer);
        }
    }
}

LFORTRAN_API void _lfortran_empty_read(int32_t unit_num, int32_t* iostat) {
    if (unit_num == -1) {
        // Read from stdin
        return;
    }

    bool unit_file_bin;
    FILE* fp = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!fp) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (!unit_file_bin) {
        // The contents of `c` are ignored
        char c = fgetc(fp);
        while (c != '\n' && c != EOF) {
            c = fgetc(fp);
        }

        if (feof(fp)) {
            *iostat = -1;
        } else if (ferror(fp)) {
            *iostat = 1;
        } else {
            *iostat = 0;
        }
    }
}

LFORTRAN_API char* _lpython_read(int64_t fd, int64_t n)
{
    char *c = (char *) calloc(n, sizeof(char));
    if (fd < 0)
    {
        printf("Error in reading the file!\n");
        exit(1);
    }
    int x = fread(c, 1, n, (FILE*)fd);
    c[x] = '\0';
    return c;
}

LFORTRAN_API void _lfortran_file_write(int32_t unit_num, int32_t* iostat, const char* format_data, int64_t format_len, ...)
{
    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access);
    if (!filep) {
        filep = stdout;
    }
    if (unit_file_bin) {
        fseek(filep, 0, SEEK_END);
        va_list args;
        va_start(args, format_len);
        size_t total_size = 0;
        struct {
            void *ptr;
            int32_t len;
        } data[100];  // support max 100 args

        int count = 0;

        // Read (size, ptr) until len == -1
        while (1) {
            int32_t len = va_arg(args, int32_t);
            if (len == -1) break; // sentinel
            void* ptr = va_arg(args, void*);

            data[count].ptr = ptr;
            if (data[count].ptr == NULL) {
                printf("Internal Compiler Error: NULL pointer passed to _lfortran_file_write.\n");
                exit(1);
            }
            data[count].len = len;
            total_size += len;
            count++;
        }
        va_end(args);

        // Write record marker
        int32_t record_marker = (int32_t)total_size;
        if (access_id != 1) fwrite(&record_marker, sizeof(record_marker), 1, filep);
        size_t written = 0;
        // Write all data chunks
        for (int i = 0; i < count; i++) {
            written += fwrite(data[i].ptr, 1, data[i].len, filep);
        }
        // Write record marker again
        if (access_id != 1) fwrite(&record_marker, sizeof(record_marker), 1, filep);

        if (written != total_size) {
            printf("Error writing data to file.\n");
            // TODO: not sure what is the right value of "iostat" in this case
            // it should be a positive value unique from other predefined iostat values
            // like IOSTAT_INQUIRE_INTERNAL_UNIT, IOSTAT_END, and IOSTAT_EOR.
            // currently, I've set it to 11
            if(iostat != NULL) *iostat = 11;
            exit(1);
        } else {
            if(iostat != NULL) *iostat = 0;
        }
    } else {
        va_list args;
        va_start(args, format_len);
        char* str = va_arg(args, char*);
        // int64_t str_len = va_arg(args, int64_t); >>>>> TODO : pass length
        // Detect "\b" to raise error
        if(str[0] == '\b'){
            if(iostat == NULL){
                str = str+1;
                fprintf(stderr, "%s",str);
                exit(1);
            } else { // Delegate error handling to the user.
                *iostat = 11;
                return;
            }
        }
        if(strcmp(format_data, "%s%s") == 0){
            char* end = va_arg(args, char*);
            int64_t end_len = va_arg(args, int64_t);
            fprintf(filep, format_data, str, end);
        } else {
            fprintf(filep, format_data, str);
        }
        if(iostat != NULL) *iostat = 0;
        va_end(args);
    }
    (void)!ftruncate(fileno(filep), ftell(filep));
}

LFORTRAN_API void _lfortran_string_write(char **str_holder, bool is_allocatable, bool is_deferred, int64_t* len, int32_t* iostat, const char* format,
    int64_t format_len, ...) {
    va_list args;
    va_start(args, format_len);
    char* str;
    // int64_t str_len; >> TODO: Pass length
    char* end_data = ""; int64_t end_len;
    if(strcmp(format, "%s%s") == 0){
        str = va_arg(args, char*);
        // str_len >> Should pass rhs_len
        end_data = va_arg(args, char*);
        end_len = va_arg(args, int64_t); 
    } else if(strcmp(format, "%s") == 0){
        str = va_arg(args, char*);
    } else {
        fprintf(stderr,"Compiler Error : Undefined Format");
        exit(1);
    }

    // Detect "\b" to raise error
    if(str[0] == '\b'){
        if(iostat == NULL){
            str = str+1;
            fprintf(stderr, "%s",str);
            exit(1);
        } else { // Delegate error handling to the user.
            *iostat = 11;
            return;
        }
    }

    char *s = (char *) malloc(strlen(str)*sizeof(char) + strlen(end_data)*sizeof(char) + 1);
    sprintf(s, format, str, end_data);

    _lfortran_strcpy(str_holder, len, is_allocatable, is_deferred, str, strlen(str));
    free(s);
    va_end(args);
    if(iostat != NULL) *iostat = 0;
}

LFORTRAN_API void _lfortran_string_read_i32(char *str, int64_t len, char *format, int32_t *i) {
    char *buf = (char*)malloc(len + 1);
    if (!buf) return; // handle allocation failure if needed
    memcpy(buf, str, len);
    buf[len] = '\0';
    sscanf(buf, format, i);
    free(buf);
}


LFORTRAN_API void _lfortran_string_read_i64(char *str, int64_t len, char *format, int64_t *i) {
    char *buf = (char*)malloc(len + 1);
    if (!buf) return; // allocation failure
    memcpy(buf, str, len);
    buf[len] = '\0';
    sscanf(buf, format, i);
    free(buf);
}

LFORTRAN_API void _lfortran_string_read_f32(char *str, int64_t len, char *format, float *f) {
    char *buf = (char*)malloc(len + 1);
    if (!buf) return;
    memcpy(buf, str, len);
    buf[len] = '\0';
    sscanf(buf, format, f);
    free(buf);
}

LFORTRAN_API void _lfortran_string_read_f64(char *str, int64_t len, char *format, double *f) {
    char *buf = (char*)malloc(len + 1);
    if (!buf) return;
    memcpy(buf, str, len);
    buf[len] = '\0';
    sscanf(buf, format, f);
    free(buf);
}

char *remove_whitespace(char *str) {
    if (str == NULL || str[0] == '\0') {
        return "(null)";
    }
    char *end;
    // remove leading space
    while(isspace((unsigned char)*str)) str++;
    if(*str == 0) // All spaces?
        return str;
    // remove trailing space
    end = str + strlen(str) - 1;
    while(end > str && isspace((unsigned char)*end)) end--;
    // Write new null terminator character
    end[1] = '\0';
    return str;
}

LFORTRAN_API void _lfortran_string_read_str(char *src_data, int64_t src_len, char *dest_data, int64_t dest_len) {
    _lfortran_copy_str_and_pad(
        dest_data, dest_len,
        src_data, src_len);
}

LFORTRAN_API void _lfortran_string_read_bool(char *str, int64_t len, char *format, int32_t *i) {
    sscanf(str, format, i);
    printf("%s\n", str);
}

void lfortran_error(const char *message) {
    fprintf(stderr, "LFORTRAN ERROR: %s\n", message);
    exit(EXIT_FAILURE);
}

// TODO: add support for reading comma separated string, into `_arr` functions
// by accepting array size as an argument as well
LFORTRAN_API void _lfortran_string_read_i32_array(char *str, int64_t len, char *format, int32_t *arr) {
    printf("\nHERE--------------->%s\n", format);
    lfortran_error("Reading into an array of int32_t is not supported.");
}

LFORTRAN_API void _lfortran_string_read_i64_array(char *str, int64_t len, char *format, int64_t *arr) {
    lfortran_error("Reading into an array of int64_t is not supported.");
}

LFORTRAN_API void _lfortran_string_read_f32_array(char *str, int64_t len, char *format, float *arr) {
    lfortran_error("Reading into an array of float is not supported.");
}

LFORTRAN_API void _lfortran_string_read_f64_array(char *str, int64_t len, char *format, double *arr) {
    lfortran_error("Reading into an array of double is not supported.");
}

LFORTRAN_API void _lfortran_string_read_str_array(char *str, int64_t len, char *format, char **arr) {
    lfortran_error("Reading into an array of strings is not supported.");
}

LFORTRAN_API void _lpython_close(int64_t fd)
{
    if (fclose((FILE*)fd) != 0)
    {
        printf("Error in closing the file!\n");
        exit(1);
    }
}

LFORTRAN_API void _lfortran_close(int32_t unit_num, char* status, int64_t status_len)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL);
    if (!filep) {
        return;
    }
    if (fclose(filep) != 0) {
        printf("Error in closing the file!\n");
        exit(1);
    }
    // TODO: Support other `status` specifiers
    char * file_name = get_file_name_from_unit(unit_num, &unit_file_bin);
    char* scratch_file = "_lfortran_generated_file";
    bool is_temp_file = strncmp(file_name, scratch_file, strlen(scratch_file)) == 0;
    if ((status && strcmp(status, "delete") == 0) || is_temp_file) {
        if (remove(file_name) != 0) {
            printf("Error in deleting file!\n");
            exit(1);
        }
    }
    if (is_temp_file) free(file_name);
    remove_from_unit_to_file(unit_num);
}

LFORTRAN_API int32_t _lfortran_ichar(char *c) {
     return (int32_t) c[0];
}

LFORTRAN_API int32_t _lfortran_iachar(char *c) {
    return (int32_t) (uint8_t)(c[0]);
}

// Command line arguments
int32_t _argc;
char **_argv;

LFORTRAN_API void _lpython_set_argv(int32_t argc_1, char *argv_1[]) {
    _argv = malloc(argc_1 * sizeof(char *));
    for (size_t i = 0; i < argc_1; i++) {
        _argv[i] = strdup(argv_1[i]);
    }
    _argc = argc_1;
}

LFORTRAN_API void _lpython_free_argv() {
    if (_argv != NULL) {
        for (size_t i = 0; i < _argc; i++) {
            free(_argv[i]);
        }
        free(_argv);
        _argv = NULL;
    }
}

LFORTRAN_API int32_t _lpython_get_argc() {
    return _argc;
}

LFORTRAN_API int32_t _lfortran_get_argc() {
    return _argc - 1;
}

LFORTRAN_API char *_lpython_get_argv(int32_t index) {
    return _argv[index];
}

// get_command_argument
LFORTRAN_API void _lfortran_get_command_argument_value(int n, char* receiver) {
    if (n >= 0 && n < _argc) {
        int32_t arg_len = strlen(_argv[n]);
        memcpy(receiver, _argv[n], arg_len); 
        receiver[arg_len] = '\0';
    } else {
        receiver[0] = '\0';
    }
}

LFORTRAN_API int32_t _lfortran_get_command_argument_length(int n) {
    if (n >= 0 && n < _argc) {
        return strlen(_argv[n]);
    } else {
        return 0;
    }
}

LFORTRAN_API int32_t _lfortran_get_command_argument_status() {
    return 0;
}

// get_command
#define sep_space " "

LFORTRAN_API void _lfortran_get_command_command(char* receiver) {
    int32_t receiver_idx = 0; // Current index to start writing into
    for(int i=0; i<_argc; i++) {

        int32_t arg_len = strlen(_argv[i]); 
        memcpy(receiver + receiver_idx, _argv[i], arg_len);
        receiver_idx += arg_len;

        if( i == _argc - 1) break; // Don't add a separator

        memcpy(receiver + receiver_idx, sep_space, strlen(sep_space));
        receiver_idx += strlen(sep_space);
    }
    receiver[receiver_idx] = '\0';
}

LFORTRAN_API int32_t _lfortran_get_command_length() {
    int32_t total_length = 0;
    for(int i=0; i<_argc; i++){
        total_length += strlen(_argv[i]);
    }
    total_length += (strlen(sep_space) * (_argc - 1));
    return total_length;
}

LFORTRAN_API int32_t _lfortran_get_command_status() {
    return 1;
}

// << Command line arguments << ------------------------------------------------

// Initial setup
LFORTRAN_API void _lpython_call_initial_functions(int32_t argc_1, char *argv_1[]) {
    _lpython_set_argv(argc_1, argv_1);
    _lfortran_init_random_clock();
}

LFORTRAN_API int32_t _lfortran_command_argument_count() {
    return _argc - 1;
}
// << Initial setup << ---------------------------------------------------------

// >> Runtime Stacktrace >> ----------------------------------------------------
#ifdef HAVE_RUNTIME_STACKTRACE
#ifdef HAVE_LFORTRAN_UNWIND
static _Unwind_Reason_Code unwind_callback(struct _Unwind_Context *context,
        void *vdata) {
    struct Stacktrace *d = (struct Stacktrace *) vdata;
    uintptr_t pc = _Unwind_GetIP(context);
    if (pc != 0) {
        pc--;
        if (d->pc_size < LCOMPILERS_MAX_STACKTRACE_LENGTH) {
            d->pc[d->pc_size] = pc;
            d->pc_size++;
        } else {
            printf("The stacktrace length is out of range.\nAborting...");
            abort();
        }
    }
    return _URC_NO_REASON;
}
#endif // HAVE_LFORTRAN_UNWIND

struct Stacktrace get_stacktrace_addresses() {
    struct Stacktrace d;
    d.pc_size = 0;
#ifdef HAVE_LFORTRAN_UNWIND
    _Unwind_Backtrace(unwind_callback, &d);
#endif
    return d;
}

char *get_base_name(char *filename) {
    // Assuming filename always has an extensions
    size_t end = strrchr(filename, '.')-filename-1;
    // Check for directories else start at 0th index
    char *slash_idx_ptr = strrchr(filename, '/');
    size_t start = 0;
    if (slash_idx_ptr) {
        start = slash_idx_ptr - filename+1;
    }
    int nos_of_chars = end - start + 1;
    char *base_name;
    if (nos_of_chars < 0) {
        return NULL;
    }
    base_name = malloc (sizeof (char) * (nos_of_chars + 1));
    base_name[nos_of_chars] = '\0';
    strncpy (base_name, filename + start, nos_of_chars);
    return base_name;
}

#ifdef HAVE_LFORTRAN_LINK
int shared_lib_callback(struct dl_phdr_info *info,
        size_t size, void *_data) {
    struct Stacktrace *d = (struct Stacktrace *) _data;
    for (int i = 0; i < info->dlpi_phnum; i++) {
        if (info->dlpi_phdr[i].p_type == PT_LOAD) {
            ElfW(Addr) min_addr = info->dlpi_addr + info->dlpi_phdr[i].p_vaddr;
            ElfW(Addr) max_addr = min_addr + info->dlpi_phdr[i].p_memsz;
            if ((d->current_pc >= min_addr) && (d->current_pc < max_addr)) {
                d->binary_filename[d->local_pc_size] = (char *)info->dlpi_name;
                if (d->binary_filename[d->local_pc_size][0] == '\0') {
                    d->binary_filename[d->local_pc_size] = binary_executable_path;
                    d->local_pc[d->local_pc_size] = d->current_pc - info->dlpi_addr;
                    d->local_pc_size++;
                }
                // We found a match, return a non-zero value
                return 1;
            }
        }
    }
    // We didn't find a match, return a zero value
    return 0;
}
#endif // HAVE_LFORTRAN_LINK

#ifdef HAVE_LFORTRAN_MACHO
void get_local_address_mac(struct Stacktrace *d) {
    for (uint32_t i = 0; i < _dyld_image_count(); i++) {
        const struct mach_header *header = _dyld_get_image_header(i);
        intptr_t offset = _dyld_get_image_vmaddr_slide(i);
        struct load_command* cmd = (struct load_command*)((char *)header + sizeof(struct mach_header));
        if(header->magic == MH_MAGIC_64) {
            cmd = (struct load_command*)((char *)header + sizeof(struct mach_header_64));
        }
        for (uint32_t j = 0; j < header->ncmds; j++) {
            if (cmd->cmd == LC_SEGMENT) {
                struct segment_command* seg = (struct segment_command*)cmd;
                if (((intptr_t)d->current_pc >= (seg->vmaddr+offset)) &&
                    ((intptr_t)d->current_pc < (seg->vmaddr+offset + seg->vmsize))) {
                    int check_filename = strcmp(get_base_name(
                        (char *)_dyld_get_image_name(i)),
                        get_base_name(source_filename));
                    if ( check_filename != 0 ) return;
                    d->local_pc[d->local_pc_size] = d->current_pc - offset;
                    d->binary_filename[d->local_pc_size] = (char *)_dyld_get_image_name(i);
                    // Resolve symlinks to a real path:
                    char buffer[PATH_MAX];
                    char* resolved;
                    resolved = realpath(d->binary_filename[d->local_pc_size], buffer);
                    if (resolved) d->binary_filename[d->local_pc_size] = resolved;
                    d->local_pc_size++;
                    return;
                }
            }
            if (cmd->cmd == LC_SEGMENT_64) {
                struct segment_command_64* seg = (struct segment_command_64*)cmd;
                if ((d->current_pc >= (seg->vmaddr + offset)) &&
                    (d->current_pc < (seg->vmaddr + offset + seg->vmsize))) {
                    int check_filename = strcmp(get_base_name(
                        (char *)_dyld_get_image_name(i)),
                        get_base_name(source_filename));
                    if ( check_filename != 0 ) return;
                    d->local_pc[d->local_pc_size] = d->current_pc - offset;
                    d->binary_filename[d->local_pc_size] = (char *)_dyld_get_image_name(i);
                    // Resolve symlinks to a real path:
                    char buffer[PATH_MAX];
                    char* resolved;
                    resolved = realpath(d->binary_filename[d->local_pc_size], buffer);
                    if (resolved) d->binary_filename[d->local_pc_size] = resolved;
                    d->local_pc_size++;
                    return;
                }
            }
            cmd = (struct load_command*)((char*)cmd + cmd->cmdsize);
        }
    }
    printf("The stack address was not found in any shared library or"
        " the main program, the stack is probably corrupted.\n"
        "Aborting...\n");
    abort();
}
#endif // HAVE_LFORTRAN_MACHO

// Fills in `local_pc` and `binary_filename`
void get_local_address(struct Stacktrace *d) {
    d->local_pc_size = 0;
    for (int32_t i=0; i < d->pc_size; i++) {
        d->current_pc = d->pc[i];
#ifdef HAVE_LFORTRAN_LINK
        // Iterates over all loaded shared libraries
        // See `stacktrace.cpp` to get more information
        if (dl_iterate_phdr(shared_lib_callback, d) == 0) {
            printf("The stack address was not found in any shared library or"
                " the main program, the stack is probably corrupted.\n"
                "Aborting...\n");
            abort();
        }
#else
#ifdef HAVE_LFORTRAN_MACHO
        get_local_address_mac(& *d);
#else
    d->local_pc[d->local_pc_size] = 0;
    d->local_pc_size++;
#endif // HAVE_LFORTRAN_MACHO
#endif // HAVE_LFORTRAN_LINK
    }
}

uint32_t get_file_size(int64_t fp) {
    FILE *fp_ = (FILE *)fp;
    int prev = ftell(fp_);
    fseek(fp_, 0, SEEK_END);
    int size = ftell(fp_);
    fseek(fp_, prev, SEEK_SET);
    return size;
}

/*
 * `lines_dat.txt` file must be created before calling this function,
 * The file can be created using the command:
 *     ./src/bin/dat_convert.py lines.dat
 * This function fills in the `addresses` and `line_numbers`
 * from the `lines_dat.txt` file.
 */
void get_local_info_dwarfdump(struct Stacktrace *d) {
    // TODO: Read the contents of lines.dat from here itself.
    char *base_name = get_base_name(source_filename);
    char *filename = malloc(strlen(base_name) + 15);
    strcpy(filename, base_name);
    strcat(filename, "_lines.dat.txt");
    int64_t fd = _lpython_open(filename, "r");
    uint32_t size = get_file_size(fd);
    char *file_contents = _lpython_read(fd, size);
    _lpython_close(fd);
    free(filename);

    char s[LCOMPILERS_MAX_STACKTRACE_LENGTH];
    bool address = true;
    uint32_t j = 0;
    d->stack_size = 0;
    for (uint32_t i = 0; i < size; i++) {
        if (file_contents[i] == '\n') {
            memset(s, '\0', sizeof(s));
            j = 0;
            d->stack_size++;
            continue;
        } else if (file_contents[i] == ' ') {
            s[j] = '\0';
            j = 0;
            if (address) {
                d->addresses[d->stack_size] = strtol(s, NULL, 10);
                address = false;
            } else {
                d->line_numbers[d->stack_size] = strtol(s, NULL, 10);
                address = true;
            }
            memset(s, '\0', sizeof(s));
            continue;
        }
        s[j++] = file_contents[i];
    }
}

char *read_line_from_file(char *filename, uint32_t line_number) {
    FILE *fp;
    char *line = NULL;
    size_t len = 0, n = 0;

    fp = fopen(filename, "r");
    if (fp == NULL) exit(1);
    while (n < line_number && (getline(&line, &len, fp) != -1)) n++;
    fclose(fp);

    return line;
}

static inline uint64_t bisection(const uint64_t vec[],
        uint64_t size, uint64_t i) {
    if (i < vec[0]) return 0;
    if (i >= vec[size-1]) return size;
    uint64_t i1 = 0, i2 = size-1;
    while (i1 < i2-1) {
        uint64_t imid = (i1+i2)/2;
        if (i < vec[imid]) {
            i2 = imid;
        } else {
            i1 = imid;
        }
    }
    return i1;
}

#endif // HAVE_RUNTIME_STACKTRACE

LFORTRAN_API void print_stacktrace_addresses(char *filename, bool use_colors) {
#ifdef HAVE_RUNTIME_STACKTRACE
    source_filename = filename;
    struct Stacktrace d = get_stacktrace_addresses();
    get_local_address(&d);
    get_local_info_dwarfdump(&d);

#ifdef HAVE_LFORTRAN_MACHO
    for (int32_t i = d.local_pc_size-1; i >= 0; i--) {
#else
    for (int32_t i = d.local_pc_size-2; i >= 0; i--) {
#endif
        uint64_t index = bisection(d.addresses, d.stack_size, d.local_pc[i]);
        if(use_colors) {
            fprintf(stderr, DIM "  File " S_RESET
                BOLD MAGENTA "\"%s\"" C_RESET S_RESET
#ifdef HAVE_LFORTRAN_MACHO
                DIM ", line %lld\n" S_RESET
#else
                DIM ", line %" PRIu64 "\n" S_RESET
#endif
                "    %s\n", source_filename, d.line_numbers[index],
                remove_whitespace(read_line_from_file(source_filename,
                d.line_numbers[index])));
        } else {
            fprintf(stderr, "  File \"%s\", "
#ifdef HAVE_LFORTRAN_MACHO
                "line %lld\n    %s\n",
#else
                "line %" PRIu64 "\n    %s\n",
#endif
                source_filename, d.line_numbers[index],
                remove_whitespace(read_line_from_file(source_filename,
                d.line_numbers[index])));
        }
#ifdef HAVE_LFORTRAN_MACHO
    }
#else
    }
#endif
#endif // HAVE_RUNTIME_STACKTRACE
}

// << Runtime Stacktrace << ----------------------------------------------------

LFORTRAN_API void _lfortran_get_environment_variable(char *name, char* receiver) {
    // temporary solution, the below function _lfortran_get_env_variable should be used
    if (name == NULL || ! getenv(name)) {
        memcpy(receiver, " ", 1);
        receiver[1] = '\0';
        return;
    } 
    int32_t len = strlen(getenv(name));
    memcpy(receiver, getenv(name), len);
    receiver[len] = '\0';
}

LFORTRAN_API int32_t _lfortran_get_length_of_environment_variable(char *name) {
    // temporary solution, the below function _lfortran_get_env_variable should be used
    if (name == NULL) {
        return 0;
    } else {
        char *value = getenv(name);
        if (value == NULL) {
            return 0; // If the environment variable is not found, return 0
        } else {
            return strlen(value); // Return the length of the environment variable value
        }
    }
}

LFORTRAN_API char *_lfortran_get_env_variable(char *name) {
    return getenv(name);
}

LFORTRAN_API int _lfortran_exec_command(char *cmd) {
    return system(cmd);
}
