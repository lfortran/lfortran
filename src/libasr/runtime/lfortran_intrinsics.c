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

#define PI 3.14159265358979323846
#if defined(_WIN32)
#  include <winsock2.h>
#  include <io.h>
#  define ftruncate _chsize_s
#else
#  include <unistd.h>
#endif

#include <libasr/runtime/lfortran_intrinsics.h>
#include <libasr/config.h>

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

LFORTRAN_API void _lfortran_init_random_seed(unsigned seed)
{
    srand(seed);
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

void handle_integer(char* format, int64_t val, char** result) {
    int width = 0, min_width = 0;
    char* dot_pos = strchr(format, '.');
    int len = (val == 0) ? 1 : (int)log10(llabs(val)) + 1;
    int sign_width = (val < 0) ? 1 : 0;
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
            width = len + sign_width;
        }
    }
    if (width >= len + sign_width || width == 0) {
        if (min_width > len) {
            for (int i = 0; i < (width - min_width - sign_width); i++) {
                *result = append_to_string(*result, " ");
            }
            if (val < 0) {
                *result = append_to_string(*result, "-");
            }
            for (int i = 0; i < (min_width - len); i++) {
                *result = append_to_string(*result, "0");
            }
        } else if (width == 0) {
            if (val < 0) {
                *result = append_to_string(*result, "-");
            }
            for (int i = 0; i < (min_width - len - sign_width); i++) {
                *result = append_to_string(*result, "0");
            }
        } else {
            for (int i = 0; i < (width - len - sign_width); i++) {
                *result = append_to_string(*result, " ");
            }
            if (val < 0) {
                *result = append_to_string(*result, "-");
            }
        }
        char str[20];
        sprintf(str, "%lld", llabs(val));
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

void handle_float(char* format, double val, char** result) {
    if (strcmp(format,"f-64") == 0){ //use c formatting.
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

    int sign_width = (val < 0) ? 1 : 0;
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
    memmove(dec_str, dec_str + 2, strlen(dec_str));

    // Determine total length needed
    int total_length = sign_width + integer_length + 1 + decimal_digits;
    if (width == 0) {
        width = total_length;
    }

    char formatted_value[128] = "";
    int spaces = width - total_length;
    for (int i = 0; i < spaces; i++) {
        strcat(formatted_value, " ");
    }
    if (val < 0) {
        strcat(formatted_value, "-");
    }
    if (integer_part == 0 && format[1] == '0') {
        strcat(formatted_value, "");
    } else {
        strcat(formatted_value, int_str);
    }
    strcat(formatted_value, ".");
    strcat(formatted_value, dec_str);

    // checking for overflow
    if (strlen(formatted_value) > width) {
        for (int i = 0; i < width; i++) {
            *result = append_to_string(*result, "*");
        }
    } else {
        *result = append_to_string(*result, formatted_value);
    }
}

/*
`handle_en` - Formats a floating-point number using a Fortran-style "EN" format.

NOTE: The function allocates memory for the formatted result, which is returned via
the `result` parameter. It is the responsibility of the caller to free this memory
using `free(*result)` after it is no longer needed.
*/
void handle_en(char* format, double val, int scale, char** result, char* c) {
    int width, decimal_digits;
    char *num_pos = format, *dot_pos = strchr(format, '.');
    decimal_digits = atoi(++dot_pos);
    while (!isdigit(*num_pos)) num_pos++;
    width = atoi(num_pos);

    // Calculate exponent
    int exponent = 0;
    if (val != 0.0) {
        exponent = (int)floor(log10(fabs(val)));
        int remainder = exponent % 3;
        if (remainder < 0) remainder += 3;
        exponent -= remainder;
    }

    double scaled_val = val / pow(10, exponent);

    // Prepare value string
    char val_str[128];
    sprintf(val_str, "%.*lf", decimal_digits, scaled_val);

    // Truncate unnecessary zeros
    char* ptr = strchr(val_str, '.');
    if (ptr) {
        char* end_ptr = ptr;
        while (*end_ptr != '\0') end_ptr++;
        end_ptr--;
        while (*end_ptr == '0' && end_ptr > ptr) end_ptr--;
        *(end_ptr + 1) = '\0';
    }

    // Allocate a larger buffer
    char formatted_value[256];  // Increased size to accommodate larger exponent values
    int n = snprintf(formatted_value, sizeof(formatted_value), "%s%s%+03d", val_str, c, exponent);
    if (n >= sizeof(formatted_value)) {
        fprintf(stderr, "Error: output was truncated. Needed %d characters.\n", n);
    }

    // Handle width and padding
    char* final_result = malloc(width + 1);
    int padding = width - strlen(formatted_value);
    if (padding > 0) {
        memset(final_result, ' ', padding);
        strcpy(final_result + padding, formatted_value);
    } else {
        strncpy(final_result, formatted_value, width);
        final_result[width] = '\0';
    }

    // Assign the result to the output parameter
    *result = final_result;
}

void handle_decimal(char* format, double val, int scale, char** result, char* c) {
    // Consider an example: write(*, "(es10.2)") 1.123e+10
    // format = "es10.2", val = 11230000128.00, scale = 0, c = "E"
    int width = 0, decimal_digits = 0;
    int sign_width = (val < 0) ? 1 : 0;
    // sign_width = 0
    double integer_part = trunc(val);
    int integer_length = (integer_part == 0) ? 1 : (int)log10(fabs(integer_part)) + 1;
    // integer_part = 11230000128, integer_length = 11

    char *num_pos = format ,*dot_pos = strchr(format, '.');
    decimal_digits = atoi(++dot_pos);
    while(!isdigit(*num_pos)) num_pos++;
    width = atoi(num_pos);
    // width = 10, decimal_digits = 2

    char val_str[128];
    // TODO: This will work for up to `E65.60` but will fail for:
    // print "(E67.62)", 1.23456789101112e-62_8
    sprintf(val_str, "%.*lf", (60-integer_length), val);
    // val_str = "11230000128.00..."

    int i = strlen(val_str) - 1;
    while (val_str[i] == '0') {
        val_str[i] = '\0';
        i--;
    }
    // val_str = "11230000128."

    int exp = 2;
    char* exp_loc = strchr(num_pos, 'e');
    if (exp_loc != NULL) {
        exp = atoi(++exp_loc);
    }
    // exp = 2;

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
    if (tolower(format[1]) == 's') {
        scale = 1;
        decimal--;
        // decimal = 0,   case:  1.123e+10
        // decimal = -10, case:  1.123e-10
    }

    if (dot_pos != NULL) {
        if (width == 0) {
            if (decimal_digits == 0) {
                width = 14 + sign_width;
                decimal_digits = 9;
            } else {
                width = decimal_digits + 5 + sign_width;
            }
        }
        if (decimal_digits > width - 3) {
            perror("Specified width is not enough for the specified number of decimal digits.\n");
        }
    } else {
        width = atoi(format + 1);
    }
    if (decimal_digits > strlen(val_str)) {
        int k = decimal_digits - (strlen(val_str) - integer_length);
        for(int i=0; i < k; i++) {
            strcat(val_str, "0");
        }
    }

    char formatted_value[64] = "";
    int spaces = width - sign_width - decimal_digits - 6;
    // spaces = 2
    if (scale > 1) {
        decimal_digits -= scale - 1;
    }
    for (int i = 0; i < spaces; i++) {
        strcat(formatted_value, " ");
    }

    if (sign_width == 1) {
        // adds `-` (negative) sign
        strcat(formatted_value, "-");
    }
    if (scale <= 0) {
        strcat(formatted_value, "0.");
        for (int k = 0; k < abs(scale); k++) {
            strcat(formatted_value, "0");
        }
        int zeros = 0;
        while(val_str[zeros] == '0') zeros++;
        // TODO: figure out a way to round decimals with value < 1e-15
        if (decimal_digits + scale < strlen(val_str) && val != 0 && decimal_digits + scale - zeros<= 15) {
            val_str[15] = '\0';
            long long t = (long long)round((long double)atoll(val_str) / (long long)pow(10, (strlen(val_str) - decimal_digits - scale)));
            sprintf(val_str, "%lld", t);
            int index = zeros;
            while(index--) strcat(formatted_value, "0");
        }
        strncat(formatted_value, val_str, decimal_digits + scale - zeros);
    } else {
        char* temp = substring(val_str, 0, scale);
        strcat(formatted_value, temp);
        strcat(formatted_value, ".");
        // formatted_value = "  1."
        char* new_str = substring(val_str, scale, strlen(val_str));
        // new_str = "1230000128" case:  1.123e+10
        int zeros = 0;
        if (decimal_digits < strlen(new_str) && decimal_digits + scale <= 15) {
            new_str[15] = '\0';
            zeros = strspn(new_str, "0");
            long long t = (long long)round((long double)atoll(new_str) / (long long) pow(10, (strlen(new_str) - decimal_digits)));
            sprintf(new_str, "%lld", t);
            // new_str = 12
            int index = zeros;
            while(index--) {
                memmove(new_str + 1, new_str, strlen(new_str)+1);
                new_str[0] = '0';
            }
        }
        new_str[decimal_digits] = '\0';
        strcat(formatted_value, new_str);
        // formatted_value = "  1.12"
        free(new_str);
        free(temp);
    }

    strcat(formatted_value, c);
    // formatted_value = "  1.12E"

    char exponent[12];
    if (atoi(num_pos) == 0) {
        sprintf(exponent, "%+02d", (integer_length > 0 && integer_part != 0 ? integer_length - scale : decimal));
    } else {
        sprintf(exponent, "%+0*d", exp+1, (integer_length > 0 && integer_part != 0 ? integer_length - scale : decimal));
        // exponent = "+10"
    }

    strcat(formatted_value, exponent);
    // formatted_value = "  1.12E+10"

    if (strlen(formatted_value) == width + 1 && scale <= 0) {
        char* ptr = strchr(formatted_value, '0');
        if (ptr != NULL) {
            memmove(ptr, ptr + 1, strlen(ptr));
        }
    }

    if (strlen(formatted_value) > width) {
        for(int i=0; i<width; i++){
            *result = append_to_string(*result,"*");
        }
    } else {
        *result = append_to_string(*result, formatted_value);
        // result = "  1.12E+10"
    }
}

/*
Ignore blank space characters within format specification, except
within character string edit descriptor

E.g.; "('Number : ', I 2, 5 X, A)" becomes '('Number : ', I2, 5X, A)'
*/
char* remove_spaces_except_quotes(const char* format) {
    int len = strlen(format);
    char* cleaned_format = malloc(len + 1);

    int i = 0, j = 0;
    // don't remove blank spaces from within character
    // string editor descriptor
    bool in_quotes = false;
    char current_quote = '\0';

    while (format[i] != '\0') {
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

        i++;
    }

    cleaned_format[j] = '\0';
    return cleaned_format;
}

/**
 * parse fortran format string by extracting individual 'format specifiers'
 * (e.g. 'i', 't', '*' etc.) into an array of strings
 *
 * `char* format`: the string we need to split into format specifiers
 * `int* count`  : store count of format specifiers (passed by reference from caller)
 * `item_start`  :
 *
 * e.g. "(I5, F5.2, T10)" is split separately into "I5", "F5.2", "T10" as
 * format specifiers
*/
char** parse_fortran_format(char* format, int64_t *count, int64_t *item_start) {
    char** format_values_2 = (char**)malloc((*count + 1) * sizeof(char*));
    int format_values_count = *count;
    int index = 0 , start = 0;
    while (format[index] != '\0') {
        char** ptr = (char**)realloc(format_values_2, (format_values_count + 1) * sizeof(char*));
        if (ptr == NULL) {
            perror("Memory allocation failed.\n");
            free(format_values_2);
        } else {
            format_values_2 = ptr;
        }
        switch (tolower(format[index])) {
            case ',' :
                break;
            case '/' :
                format_values_2[format_values_count++] = substring(format, index, index+1);
                break;
            case '*' :
                format_values_2[format_values_count++] = substring(format, index, index+1);
                break;
            case '"' :
                start = index++;
                while (format[index] != '"') {
                    index++;
                }
                format_values_2[format_values_count++] = substring(format, start, index+1);

                break;
            case '\'' :
                start = index++;
                while (format[index] != '\'') {
                    index++;
                }
                format_values_2[format_values_count++] = substring(format, start, index+1);
                break;
            case 'a' :
                start = index++;
                while (isdigit(format[index])) {
                    index++;
                }
                format_values_2[format_values_count++] = substring(format, start, index);
                index--;
                break;
            case 'e' :
                start = index++;
                bool edot = false;
                bool is_en_formatting = false;
                if (tolower(format[index]) == 'n') {
                    index++;  // move past the 'N'
                    is_en_formatting = true;
                }
                if (tolower(format[index]) == 's') index++;
                while (isdigit(format[index])) index++;
                if (format[index] == '.') {
                    edot = true;
                    index++;
                } else {
                    printf("Error: Period required in format specifier\n");
                    exit(1);
                }
                while (isdigit(format[index])) index++;
                if (edot && (tolower(format[index]) == 'e' || tolower(format[index]) == 'n')) {
                    index++;
                    while (isdigit(format[index])) index++;
                }
                format_values_2[format_values_count++] = substring(format, start, index);
                index--;
                break;
            case 'i' :
            case 'd' :
            case 'f' :
            case 'l' :
                start = index++;
                bool dot = false;
                if(tolower(format[index]) == 's') index++;
                while (isdigit(format[index])) index++;
                if (format[index] == '.') {
                    dot = true;
                    index++;
                }
                while (isdigit(format[index])) index++;
                if (dot && tolower(format[index]) == 'e') {
                    index++;
                    while (isdigit(format[index])) index++;
                }
                format_values_2[format_values_count++] = substring(format, start, index);
                index--;
                break;
            case '(' :
                start = index++;
                while (format[index] != ')') index++;
                format_values_2[format_values_count++] = substring(format, start, index+1);
                *item_start = format_values_count;
                break;
            case 't' :
                start = index++;
                // raise error when "T" is specified itself or with non-positive width
                if (!isdigit(format[index])) {
                    printf("Error: Positive width required with 'T' descriptor in format string\n");
                    exit(1);
                }
                while (isdigit(format[index])) {
                    index++;
                }
                format_values_2[format_values_count++] = substring(format, start, index);
                index--;
                break;
            default :
                if (
                    (format[index] == '-' && isdigit(format[index + 1]) && tolower(format[index + 2]) == 'p')
                    || ((isdigit(format[index])) && tolower(format[index + 1]) == 'p')) {
                    start = index;
                    index = index + 1 + (format[index] == '-');
                    format_values_2[format_values_count++] = substring(format, start, index + 1);
                } else if (isdigit(format[index])) {
                    start = index;
                    while (isdigit(format[index])) index++;
                    char* repeat_str = substring(format, start, index);
                    int repeat = atoi(repeat_str);
                    free(repeat_str);
                    format_values_2 = (char**)realloc(format_values_2, (format_values_count + repeat + 1) * sizeof(char*));
                    if (format[index] == '(') {
                        start = index++;
                        while (format[index] != ')') index++;
                        *item_start = format_values_count+1;
                        for (int i = 0; i < repeat; i++) {
                            format_values_2[format_values_count++] = substring(format, start, index+1);
                        }
                    } else {
                        start = index++;
                        if (isdigit(format[index])) {
                            while (isdigit(format[index])) index++;
                            if (format[index] == '.') index++;
                            while (isdigit(format[index])) index++;
                        }
                        for (int i = 0; i < repeat; i++) {
                            format_values_2[format_values_count++] = substring(format, start, index);
                        }
                        index--;
                    }
                } else if (format[index] != ' ') {
                    fprintf(stderr, "Unsupported or unrecognized `%c` in format string\n", format[index]);
                    exit(1);
                }
        }
        index++;
    }
    *count = format_values_count;
    return format_values_2;
}


struct array_iteration_state{
    //Preserve array size and current element index
    int64_t array_size;
    int64_t current_arr_index;
    //Hold array pointers for each type.
    int64_t* arr_ptr_int64;
    int32_t* arr_ptr_int32;
    int16_t* arr_ptr_int16;
    int8_t* arr_ptr_int8;
    float* arr_ptr_float;
    double* arr_ptr_double;
    char** arr_ptr_charPtr;
    bool* arr_ptr_bool;
    //Hold current element (We support array of int64, double, char*, bool)
    int64_t current_arr_element_int64;
    double current_arr_element_double;
    char* current_arr_element_char_ptr;
    bool current_arr_element_bool;
};

bool check_array_iteration(int* count, int* current_arg_type_int, va_list* args,struct array_iteration_state* state){
    bool is_array = true;
    switch (*current_arg_type_int){
        case 9 : //arr[i64]
            if(state->current_arr_index != state->array_size){
                state->current_arr_element_int64 = state->arr_ptr_int64[state->current_arr_index++];
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0; 
                state->arr_ptr_int64 = va_arg(*args,int64_t*);
                state->current_arr_element_int64 = state->arr_ptr_int64[state->current_arr_index++];
                *count+= state->array_size - 2;
            }
            break;
        case 10 : //arr[i32]
            if(state->current_arr_index != state->array_size){
                int32_t temp_val = state->arr_ptr_int32[state->current_arr_index++];
                state->current_arr_element_int64 = (int64_t)temp_val;
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0;
                state->arr_ptr_int32 = va_arg(*args,int32_t*);
                int32_t temp_val = state->arr_ptr_int32[state->current_arr_index++];
                state->current_arr_element_int64 = (int64_t)temp_val;
                *count+= state->array_size - 2;
            }
            break;
        case 11 : //arr[i16]
            if(state->current_arr_index != state->array_size){
                int16_t temp_val = state->arr_ptr_int16[state->current_arr_index++];
                state->current_arr_element_int64 = (int64_t)temp_val;
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0; 
                state->arr_ptr_int16 = va_arg(*args,int16_t*);
                int16_t temp_val = state->arr_ptr_int16[state->current_arr_index++];
                state->current_arr_element_int64 = (int64_t)temp_val;
                *count+= state->array_size - 2;
            }
            break;
        case 12 : //arr[i8]
            if(state->current_arr_index != state->array_size){
                int8_t temp_val = state->arr_ptr_int8[state->current_arr_index++];
                state->current_arr_element_int64 = (int64_t)temp_val;
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0; 
                state->arr_ptr_int8 = va_arg(*args,int8_t*);
                int8_t temp_val = state->arr_ptr_int8[state->current_arr_index++];
                state->current_arr_element_int64 = (int64_t)temp_val;
                *count+= state->array_size - 2;
            }
            break;
        case 13: // arr[f64]
            if(state->current_arr_index != state->array_size){
                state->current_arr_element_double = state->arr_ptr_double[state->current_arr_index++];
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0; 
                state->arr_ptr_double = va_arg(*args,double*);
                state->current_arr_element_double = state->arr_ptr_double[state->current_arr_index++];
                *count+= state->array_size - 2;
            }
            break;
        case 14: // arr[f32]
            if(state->current_arr_index != state->array_size){
                float temp_val = state->arr_ptr_float[state->current_arr_index++];
                state->current_arr_element_double = (double)temp_val;
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0; 
                state->arr_ptr_float = va_arg(*args,float*);
                float temp_val = state->arr_ptr_float[state->current_arr_index++];
                state->current_arr_element_double = (double)temp_val;
                *count+= state->array_size - 2;
            }
            break;
        case 15: //arr[character]
            if(state->current_arr_index != state->array_size){
                state->current_arr_element_char_ptr = state->arr_ptr_charPtr[state->current_arr_index++];
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0; 
                state->arr_ptr_charPtr = va_arg(*args,char**);
                state->current_arr_element_char_ptr = state->arr_ptr_charPtr[state->current_arr_index++];
                *count+= state->array_size - 2;
            }
            break;
        case 16: //arr[logical]
            if(state->current_arr_index != state->array_size){
                state->current_arr_element_bool = state->arr_ptr_bool[state->current_arr_index++];
            } else {
                state->array_size = va_arg(*args,int64_t);
                state->current_arr_index = 0; 
                state->arr_ptr_bool = va_arg(*args,bool*);
                state->current_arr_element_bool = state->arr_ptr_bool[state->current_arr_index++];
                *count+= state->array_size - 2;
            }
            break;
        //To DO : handle --> arr[cptr], arr[enumType] 
        default:
            is_array = false;
            break;
    }
    return is_array;
    
}
char* int_to_format_specifier(int32_t type_as_int){
    switch(type_as_int){
        case 1:
        case 2:
        case 3:
        case 4:
        case 9:
        case 10:
        case 11:
        case 12:
        case 19:
            return "i0";
        case 5:
        case 13:
            return "f-64"; //special handling in `handle_float`
        case 6:
        case 14:
            return "f-32"; //special handling in `handle_float`
        case 7:
        case 15:
            return "a";
        case 8:
        case 16:
            return "l";
        default:
            fprintf(stderr,"Unidentified number %d\n",type_as_int);
            exit(0);
    }
}

bool is_format_match(char format_value, int32_t current_arg_type_int){
    char* current_arg_correct_format = int_to_format_specifier(current_arg_type_int);
    char lowered_format_value = tolower(format_value);
    if(lowered_format_value == 'd' || lowered_format_value == 'e'){
        lowered_format_value = 'f';
    }
    // Special conditions that are allowed by gfortran.
    bool special_conditions = (lowered_format_value == 'l' && current_arg_correct_format[0] == 'a') ||
                              (lowered_format_value == 'a' && current_arg_correct_format[0] == 'l');
    if(lowered_format_value != current_arg_correct_format[0] && !special_conditions){
        return false;
    } else {
        return true;
    }
}

LFORTRAN_API char* _lcompilers_string_format_fortran(int count, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    bool default_formatting = (format == NULL);
    char* default_spacing = "    ";
    int64_t format_values_count = 0,item_start_idx=0;
    char** format_values;
    char* modified_input_string;
    if (default_formatting){
        format_values_count = INT64_MAX; // Termination would depend on count of args, so set to maximum looping.
    } else {
        char* cleaned_format = remove_spaces_except_quotes(format);
        if (!cleaned_format) {
            va_end(args);
            return NULL;
        }
        int len = strlen(cleaned_format);
        modified_input_string = (char*)malloc((len+1) * sizeof(char));
        strncpy(modified_input_string, cleaned_format, len);
        modified_input_string[len] = '\0';
        if (cleaned_format[0] == '(' && cleaned_format[len-1] == ')') {
            memmove(modified_input_string, modified_input_string + 1, strlen(modified_input_string));
            modified_input_string[len-2] = '\0';
        }
        format_values = parse_fortran_format(modified_input_string,&format_values_count,&item_start_idx);
    }
    char* result = (char*)malloc(sizeof(char));
    result[0] = '\0';
    int item_start = 0;
    bool array = false;
    //initialize array_state to hold information about any passed array pointer arg.
    struct array_iteration_state array_state;
    array_state.array_size = -1;
    array_state.current_arr_index = -1;
    int32_t current_arg_type_int = -1; // holds int that represents type of argument.
    while (1) {
        int scale = 0;
        bool is_array = false;
        bool array_looping = false;
        for (int i = item_start; i < format_values_count; i++) {
            char* value;
            array_looping = (array_state.current_arr_index != array_state.array_size);
            if(default_formatting && !array_looping){
                if(count <=0) break;
                current_arg_type_int =  va_arg(args,int32_t);
                count--;
                value = int_to_format_specifier(current_arg_type_int);
            } else if (!default_formatting) {
                if(format_values[i] == NULL) continue;
                value = format_values[i];
            } else {
                // Array is being looped on.
            }

            if (value[0] == '(' && value[strlen(value)-1] == ')') {
                value[strlen(value)-1] = '\0';
                int64_t new_fmt_val_count = 0;
                char** new_fmt_val = parse_fortran_format(++value,&new_fmt_val_count,&item_start_idx);

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

            if (value[0] == '/') {
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
            } else if (tolower(value[0]) == 't') {
                if (count <= 0) break;
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
                    // Truncate the string to the length specified by Tn if the current position exceeds it
                    if (tab_position < current_length) {
                        result[tab_position] = '\0';  // Truncate the string at the position specified by Tn
                    }
                }
            } else {
                if(count <= 0) break;
                if(!array_looping && !default_formatting){ // Fetch type integer when we don't have an array.
                    current_arg_type_int =  va_arg(args,int32_t);
                    count--;
                }
                if(!default_formatting){
                    if (!is_format_match(value[0], current_arg_type_int)){
                        char* type;
                        switch (int_to_format_specifier(current_arg_type_int)[0])
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
                        sprintf(result, " Runtime Error : Got argument of type (%s), while the format specifier is (%c)\n",type ,value[0]);
                        // Special indication for error --> "\b" to be handled by `lfortran_print` or `lfortran_file_write`
                        result[0] = '\b'; 
                        count = 0; // Break while loop.
                        break;
                    }
                }
                is_array = check_array_iteration(&count, &current_arg_type_int, &args,&array_state);
                if (tolower(value[0]) == 'a') {
                    // Character Editing (A[n])
                    count--;
                    char* arg = NULL;
                    if(is_array){
                        arg = array_state.current_arr_element_char_ptr; 
                    } else {
                        arg = va_arg(args, char*);
                    }
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
                    count--;
                    if(is_array){
                        handle_integer(value, array_state.current_arr_element_int64, &result);
                    } else {
                        int64_t val = va_arg(args, int64_t);
                        handle_integer(value, val, &result);
                    }
                } else if (tolower(value[0]) == 'd') {
                    // D Editing (D[w[.d]])
                    count--;
                    if(is_array){
                        handle_decimal(value, array_state.current_arr_element_double, scale, &result, "D");;
                    } else {
                        double val = va_arg(args, double);
                        handle_decimal(value, val, scale, &result, "D");
                    }
                } else if (tolower(value[0]) == 'e') {
                    // Check if the next character is 'N' for EN format
                    char format_type = tolower(value[1]);
                    count--;
                    if (format_type == 'n') {
                        if(is_array){
                            handle_en(value, array_state.current_arr_element_double, scale, &result, "E");
                        } else {
                            double val = va_arg(args, double);
                            handle_en(value, val, scale, &result, "E");
                        }
                    } else {
                        if(is_array){
                            handle_decimal(value, array_state.current_arr_element_double, scale, &result, "E");
                        } else {
                            double val = va_arg(args, double);
                            handle_decimal(value, val, scale, &result, "E");
                        }
                    }
                } else if (tolower(value[0]) == 'f') {
                    count--;
                    if(is_array){
                        handle_float(value,array_state.current_arr_element_double, &result);
                    } else {
                        double val = va_arg(args, double);
                        handle_float(value, val, &result);
                    }
                } else if (tolower(value[0]) == 'l') {
                    count--;
                    if(is_array){
                        bool val = array_state.current_arr_element_bool;
                        handle_logical(value, val, &result);
                    } else {
                        char* val_str = va_arg(args, char*);
                        bool val = (strcmp(val_str, "True") == 0);
                        handle_logical(value, val, &result);
                    }
                } else if (strlen(value) != 0) {
                    count--;
                    printf("Printing support is not available for %s format.\n",value);
                }
                if( default_formatting && (count > 0) ){ //append spacing after each element.
                    result = append_to_string(result,default_spacing);
                }
            }
        }
        if ( count > 0 ) {
            if (!array) {
                result = append_to_string(result, "\n");
            }
            item_start = item_start_idx;
        } else {
            break;
        }
    }
    if(!default_formatting){
        free(modified_input_string);
        for (int i = 0;(i<format_values_count);i++) {
                free(format_values[i]);
        }
        free(format_values);
    }
    va_end(args);
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

LFORTRAN_API void _lfortran_strcat(char** s1, char** s2, char** dest)
{
    int cntr = 0;
    char trmn = '\0';
    int s1_len = strlen(*s1);
    int s2_len = strlen(*s2);
    int trmn_size = sizeof(trmn);
    char* dest_char = (char*)malloc(s1_len+s2_len+trmn_size);
    for (int i = 0; i < s1_len; i++) {
        dest_char[cntr] = (*s1)[i];
        cntr++;
    }
    for (int i = 0; i < s2_len; i++) {
        dest_char[cntr] = (*s2)[i];
        cntr++;
    }
    dest_char[cntr] = trmn;
    *dest = &(dest_char[0]);
}

// strcpy -----------------------------------------------------------

LFORTRAN_API void _lfortran_strcpy(char** x, char *y, int8_t free_target)
{
    if (free_target) {
        if (*x) {
            // We should free `x` here, but cannot due to:
            // https://github.com/lfortran/lfortran/issues/3787
            //free((void *)*x);
        }
        *x = (char*) malloc((strlen(y) + 1) * sizeof(char));
        _lfortran_string_init(strlen(y) + 1, *x);
    }
    if( *x == NULL ) {
        *x = (char*) malloc((strlen(y) + 1) * sizeof(char));
        _lfortran_string_init(strlen(y) + 1, *x);
    }
    for (size_t i = 0; i < strlen(*x); i++) {
        if (i < strlen(y)) {
            x[0][i] = y[i];
        } else {
            x[0][i] = ' ';
        }
    }
}

#define MIN(x, y) ((x < y) ? x : y)

int strlen_without_trailing_space(char *str) {
    int end = strlen(str) - 1;
    while(end >= 0 && str[end] == ' ') end--;
    return end + 1;
}

int str_compare(char **s1, char **s2)
{
    int s1_len = strlen_without_trailing_space(*s1);
    int s2_len = strlen_without_trailing_space(*s2);
    int lim = MIN(s1_len, s2_len);
    int res = 0;
    int i ;
    for (i = 0; i < lim; i++) {
        if ((*s1)[i] != (*s2)[i]) {
            res = (*s1)[i] - (*s2)[i];
            break;
        }
    }
    res = (i == lim)? s1_len - s2_len : res;
    return res;
}
LFORTRAN_API bool _lpython_str_compare_eq(char **s1, char **s2)
{
    return str_compare(s1, s2) == 0;
}

LFORTRAN_API bool _lpython_str_compare_noteq(char **s1, char **s2)
{
    return str_compare(s1, s2) != 0;
}

LFORTRAN_API bool _lpython_str_compare_gt(char **s1, char **s2)
{
    return str_compare(s1, s2) > 0;
}

LFORTRAN_API bool _lpython_str_compare_lte(char **s1, char **s2)
{
    return str_compare(s1, s2) <= 0;
}

LFORTRAN_API bool _lpython_str_compare_lt(char **s1, char **s2)
{
    return str_compare(s1, s2) < 0;
}

LFORTRAN_API bool _lpython_str_compare_gte(char **s1, char **s2)
{
    return str_compare(s1, s2) >= 0;
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
LFORTRAN_API char* _lfortran_str_item(char* s, int32_t idx) {

    int s_len = strlen(s);
    int original_idx = idx - 1;
    if (idx < 1) idx += s_len;
    if (idx < 1 || idx >= s_len + 1) {
        printf("String index: %d is out of Bounds\n", original_idx);
        exit(1);
    }
    char* res = (char*)malloc(2);
    res[0] = s[idx-1];
    res[1] = '\0';
    return res;
}

// idx1 and idx2 both start from 1
LFORTRAN_API char* _lfortran_str_copy(char* s, int32_t idx1, int32_t idx2) {

    int s_len = strlen(s);
    if(idx1 > s_len || idx1 <= (-1*s_len)){
        printf("String index out of Bounds\n");
        exit(1);
    }
    if(idx1 <= 0) {
        idx1 = s_len + idx1;
    }
    if(idx2 <= 0) {
        idx2 = s_len + idx2;
    }
    char* dest_char = (char*)malloc(idx2-idx1+2);
    for (int i=idx1; i <= idx2; i++)
    {
        dest_char[i-idx1] = s[i-1];
    }
    dest_char[idx2-idx1+1] = '\0';
    return dest_char;
}

LFORTRAN_API char* _lfortran_str_slice(char* s, int32_t idx1, int32_t idx2, int32_t step,
                        bool idx1_present, bool idx2_present) {
    int s_len = strlen(s);
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

LFORTRAN_API char* _lfortran_str_slice_assign(char* s, char *r, int32_t idx1, int32_t idx2, int32_t step,
                        bool idx1_present, bool idx2_present) {
    int s_len = strlen(s);
    int r_len = strlen(r);
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

    char* dest_char = (char*)malloc(s_len);
    strcpy(dest_char, s);
    int s_i = idx1, d_i = 0;
    while((step > 0 && s_i >= idx1 && s_i < idx2) ||
        (step < 0 && s_i <= idx1 && s_i > idx2)) {
        dest_char[s_i] = r[d_i++];
        s_i += step;
    }
    return dest_char;
}

LFORTRAN_API int32_t _lfortran_str_len(char** s)
{
    return strlen(*s);
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

LFORTRAN_API char* _lfortran_str_chr(int val)
{
    char* dest_char = (char*)malloc(2);
    uint8_t extended_ascii = (uint8_t)val;
    dest_char[0] = extended_ascii;
    dest_char[1] = '\0';
    return dest_char;
}

LFORTRAN_API void _lfortran_memset(void* s, int32_t c, int32_t size) {
    memset(s, c, size);
}

LFORTRAN_API void* _lfortran_malloc(int32_t size) {
    return malloc(size);
}

LFORTRAN_API int8_t* _lfortran_realloc(int8_t* ptr, int32_t size) {
    return (int8_t*) realloc(ptr, size);
}

LFORTRAN_API int8_t* _lfortran_calloc(int32_t count, int32_t size) {
    return (int8_t*) calloc(count, size);
}

LFORTRAN_API void _lfortran_free(char* ptr) {
    free((void*)ptr);
}

LFORTRAN_API void _lfortran_alloc(char** ptr, int32_t size) {
    *ptr = (char *) malloc(size);
}

// size_plus_one is the size of the string including the null character
LFORTRAN_API void _lfortran_string_init(int size_plus_one, char *s) {
    int size = size_plus_one-1;
    for (int i=0; i < size; i++) {
        s[i] = ' ';
    }
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

LFORTRAN_API void _lfortran_d_cpu_time(double *t) {
    *t = ((double) clock()) / CLOCKS_PER_SEC;
}

LFORTRAN_API void _lfortran_s_cpu_time(float *t) {
    *t = ((float) clock()) / CLOCKS_PER_SEC;
}

// system_time -----------------------------------------------------------------

LFORTRAN_API void _lfortran_i32sys_clock(
        int32_t *count, int32_t *rate, int32_t *max) {
#if defined(_WIN32)
        *count = - INT_MAX;
        *rate = 0;
        *max = 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        *count = (int32_t)(ts.tv_nsec / 1000000) + ((int32_t)ts.tv_sec * 1000);
        *rate = 1e3; // milliseconds
        *max = INT_MAX;
    } else {
        *count = - INT_MAX;
        *rate = 0;
        *max = 0;
    }
#endif
}

LFORTRAN_API void _lfortran_i64sys_clock(
        uint64_t *count, int64_t *rate, int64_t *max) {
#if defined(_WIN32)
        *count = - INT_MAX;
        *rate = 0;
        *max = 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        *count = (uint64_t)(ts.tv_nsec) + ((uint64_t)ts.tv_sec * 1000000000);
        // FIXME: Rate can be in microseconds or nanoseconds depending on
        //          resolution of the underlying platform clock.
        *rate = 1e9; // nanoseconds
        *max = LLONG_MAX;
    } else {
        *count = - LLONG_MAX;
        *rate = 0;
        *max = 0;
    }
#endif
}

LFORTRAN_API void _lfortran_i64r64sys_clock(
        uint64_t *count, double *rate, int64_t *max) {
double ratev;
int64_t maxv;
if( rate == NULL ) {
    rate = &ratev;
}
if( max == NULL ) {
    max = &maxv;
}
#if defined(_WIN32)
        *count = - INT_MAX;
        *rate = 0;
        *max = 0;
#else
    struct timespec ts;
    if(clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
        *count = (uint64_t)(ts.tv_nsec) + ((uint64_t)ts.tv_sec * 1000000000);
        // FIXME: Rate can be in microseconds or nanoseconds depending on
        //          resolution of the underlying platform clock.
        *rate = 1e9; // nanoseconds
        *max = LLONG_MAX;
    } else {
        *count = - LLONG_MAX;
        *rate = 0;
        *max = 0;
    }
#endif
}

LFORTRAN_API double _lfortran_time()
{
#if defined(_WIN32)
    FILETIME ft;
    ULARGE_INTEGER uli;
    GetSystemTimeAsFileTime(&ft);
    uli.LowPart = ft.dwLowDateTime;
    uli.HighPart = ft.dwHighDateTime;
    return (double)uli.QuadPart / 10000000.0 - 11644473600.0;
#elif defined(__APPLE__) && !defined(__aarch64__)
    return 0.0;
#else
    struct timespec ts;
    clock_gettime(CLOCK_REALTIME, &ts);
    return (double)ts.tv_sec + (double)ts.tv_nsec / 1000000000.0;
#endif
}

LFORTRAN_API float _lfortran_sp_rand_num() {
    return rand() / (float) RAND_MAX;
}

LFORTRAN_API double _lfortran_dp_rand_num() {
    return rand() / (double) RAND_MAX;
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
    FILE* filep;
    bool unit_file_bin;
};

int32_t last_index_used = -1;

struct UNIT_FILE unit_to_file[MAXUNITS];

void store_unit_file(int32_t unit_num, FILE* filep, bool unit_file_bin) {
    for( int i = 0; i <= last_index_used; i++ ) {
        if( unit_to_file[i].unit == unit_num ) {
            unit_to_file[i].unit = unit_num;
            unit_to_file[i].filep = filep;
            unit_to_file[i].unit_file_bin = unit_file_bin;
        }
    }
    last_index_used += 1;
    if( last_index_used >= MAXUNITS ) {
        printf("Only %d units can be opened for now\n.", MAXUNITS);
        exit(1);
    }
    unit_to_file[last_index_used].unit = unit_num;
    unit_to_file[last_index_used].filep = filep;
    unit_to_file[last_index_used].unit_file_bin = unit_file_bin;
}

FILE* get_file_pointer_from_unit(int32_t unit_num, bool *unit_file_bin) {
    *unit_file_bin = false;
    for( int i = 0; i <= last_index_used; i++ ) {
        if( unit_to_file[i].unit == unit_num ) {
            *unit_file_bin = unit_to_file[i].unit_file_bin;
            return unit_to_file[i].filep;
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
        unit_to_file[i].filep = unit_to_file[i + 1].filep;
        unit_to_file[i].unit_file_bin = unit_to_file[i + 1].unit_file_bin;
    }
    last_index_used -= 1;
}

LFORTRAN_API int64_t _lfortran_open(int32_t unit_num, char *f_name, char *status, char *form)
{
    if (f_name == NULL) {
        f_name = "_lfortran_generated_file.txt";
    }

    if (status == NULL) {
        status = "unknown";
    }

    if (form == NULL) {
        form = "formatted";
    }
    bool file_exists[1] = {false};
    _lfortran_inquire(f_name, file_exists, -1, NULL);
    char *access_mode = NULL;
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
            printf("Runtime error: File `%s` does not exists!\nCannot open a "
                "file with the `status=old`\n", f_name);
            exit(1);
        }
        access_mode = "r+";
    } else if (streql(status, "new")) {
        if (*file_exists) {
            printf("Runtime error: File `%s` exists!\nCannot open a file with "
                "the `status=new`\n", f_name);
            exit(1);
        }
        access_mode = "w+";
    } else if (streql(status, "replace")) {
        access_mode = "w+";
    } else if (streql(status, "unknown")) {
        if (!*file_exists) {
            FILE *fd = fopen(f_name, "w");
            if (fd) {
                fclose(fd);
            }
        }
        access_mode = "r+";
    } else if (streql(status, "scratch")) {
        printf("Runtime error: Unhandled type status=`scratch`\n");
        exit(1);
    } else {
        printf("Runtime error: STATUS specifier in OPEN statement has "
            "invalid value '%s'\n", status);
        exit(1);
    }

    bool unit_file_bin;
    if (streql(form, "formatted")) {
        unit_file_bin = false;
    } else if (streql(form, "unformatted")) {
        unit_file_bin = true;
    } else {
        printf("Runtime error: FORM specifier in OPEN statement has "
            "invalid value '%s'\n", form);
        exit(1);
    }

    FILE *fd = fopen(f_name, access_mode);
    if (!fd)
    {
        printf("Runtime error: Error in opening the file!\n");
        perror(f_name);
        exit(1);
    }
    store_unit_file(unit_num, fd, unit_file_bin);
    return (int64_t)fd;
}

LFORTRAN_API void _lfortran_flush(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if( filep == NULL ) {
        printf("Specified UNIT %d in FLUSH is not connected.\n", unit_num);
        exit(1);
    }
    fflush(filep);
}

LFORTRAN_API void _lfortran_inquire(char *f_name, bool *exists, int32_t unit_num, bool *opened) {
    if (f_name && unit_num != -1) {
        printf("File name and file unit number cannot be specifed together.\n");
        exit(1);
    }
    if (f_name != NULL) {
        FILE *fp = fopen(f_name, "r");
        if (fp != NULL) {
            *exists = true;
            fclose(fp); // close the file
            return;
        }
        *exists = false;
    }
    if (unit_num != -1) {
        bool unit_file_bin;
        if (get_file_pointer_from_unit(unit_num, &unit_file_bin) != NULL) {
            *opened = true;
        } else {
            *opened = false;
        }
    }
}

LFORTRAN_API void _lfortran_rewind(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if( filep == NULL ) {
        printf("Specified UNIT %d in REWIND is not created or connected.\n", unit_num);
        exit(1);
    }
    rewind(filep);
}

LFORTRAN_API void _lfortran_backspace(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* fd = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if( fd == NULL ) {
        printf("Specified UNIT %d in BACKSPACE is not created or connected.\n",
            unit_num);
        exit(1);
    }
    int n = ftell(fd);
    for(int i = n; i >= 0; i --) {
        char c = fgetc(fd);
        if (i == n) {
            // Skip previous record newline
            fseek(fd, -3, SEEK_CUR);
            continue;
        } else  if (c == '\n') {
            break;
        } else {
            fseek(fd, -2, SEEK_CUR);
        }
    }
}

LFORTRAN_API void _lfortran_read_int32(int32_t *p, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        (void)!scanf("%d", p);
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(*p), 1, filep);
    } else {
        (void)!fscanf(filep, "%d", p);
    }
}

LFORTRAN_API void _lfortran_read_int64(int64_t *p, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        (void)!scanf("%" PRId64, p);
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(*p), 1, filep);
    } else {
        (void)!fscanf(filep, "%" PRId64, p);
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
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(int8_t), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%s", &p[i]);
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
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(int32_t), array_size, filep);
    } else {
        for (int i = 0; i < array_size; i++) {
            (void)!fscanf(filep, "%d", &p[i]);
        }
    }
}

LFORTRAN_API void _lfortran_read_char(char **p, int32_t unit_num)
{
    const char SPACE = ' ';
    int n = strlen(*p);
    if (unit_num == -1) {
        // Read from stdin
        *p = (char*)malloc(n * sizeof(char));
        (void)!fgets(*p, n + 1, stdin);
        (*p)[strcspn(*p, "\n")] = 0;
        size_t input_length = strlen(*p);
        while (input_length < n) {
            strncat(*p, &SPACE, 1);
            input_length++;
        }
        (*p)[n] = '\0';
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        // read the record marker for data length
        int32_t data_length;
        if (fread(&data_length, sizeof(int32_t), 1, filep) != 1) {
            printf("Error reading data length from file.\n");
            exit(1);
        }

        // allocate memory for the data based on data length
        *p = (char*)malloc((data_length + 1) * sizeof(char));
        if (*p == NULL) {
            printf("Memory allocation failed.\n");
            exit(1);
        }

        // read the actual data
        if (fread(*p, sizeof(char), data_length, filep) != data_length) {
            printf("Error reading data from file.\n");
            free(*p);
            exit(1);
        }
        (*p)[data_length] = '\0';

        // read the record marker after data
        int32_t check_length;
        if (fread(&check_length, sizeof(int32_t), 1, filep) != 1) {
            printf("Error reading end data length from file.\n");
            free(*p);
            exit(1);
        }

        // verify that the start and end markers match
        if (check_length != data_length) {
            printf("Data length mismatch between start and end markers.\n");
            free(*p);
            exit(1);
        }
    } else {
        char *tmp_buffer = (char*)malloc((n + 1) * sizeof(char));
        (void)!fscanf(filep, "%s", tmp_buffer);
        size_t input_length = strlen(tmp_buffer);
        strcpy(*p, tmp_buffer);
        free(tmp_buffer);
        while (input_length < n) {
            strncat(*p, &SPACE, 1);
            input_length++;
        }
        (*p)[n] = '\0';
    }
    if (streql(*p, "")) {
        printf("Runtime error: End of file!\n");
        exit(1);
    }
}

LFORTRAN_API void _lfortran_read_float(float *p, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        (void)!scanf("%f", p);
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        (void)!fread(p, sizeof(*p), 1, filep);
    } else {
        (void)!fscanf(filep, "%f", p);
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
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
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
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
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

LFORTRAN_API void _lfortran_read_array_char(char **p, int array_size, int32_t unit_num)
{
    if (unit_num == -1) {
        // Read from stdin
        for (int i = 0; i < array_size; i++) {
            int n = 1; // TODO: Support character length > 1
            p[i] = (char*) malloc(n * sizeof(char));
            (void)!scanf("%s", p[i]);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    for (int i = 0; i < array_size; i++) {
        int n = 1; // TODO: Support character length > 1
        p[i] = (char*) malloc((n + 1) * sizeof(char));
        if (unit_file_bin) {
            (void)!fread(p[i], sizeof(char), n, filep);
            p[i][1] = '\0';
        } else {
            (void)!fscanf(filep, "%c", p[i]);
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
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
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

LFORTRAN_API void _lfortran_formatted_read(int32_t unit_num, int32_t* iostat, int32_t* chunk, char* fmt, int32_t no_of_args, ...)
{
    if (!streql(fmt, "(a)")) {
        printf("Only (a) supported as fmt currently");
        exit(1);
    }

    // For now, this supports reading a single argument of type string
    // TODO: Support more arguments and other types

    va_list args;
    va_start(args, no_of_args);
    char** arg = va_arg(args, char**);

    int n = strlen(*arg);
    *arg = (char*)malloc(n * sizeof(char));

    if (unit_num == -1) {
        // Read from stdin
        *iostat = !(fgets(*arg, n, stdin) == *arg);
        (*arg)[strcspn(*arg, "\n")] = 0;
        va_end(args);
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }

    *iostat = !(fgets(*arg, n+1, filep) == *arg);
    if (streql(*arg, "\n")) {
        *iostat = -2;
    }
    int len = strcspn(*arg, "\n");
    *chunk = len;
    (*arg)[len] = 0;
    va_end(args);
}

LFORTRAN_API void _lfortran_empty_read(int32_t unit_num, int32_t* iostat) {
    if (unit_num == -1) {
        // Read from stdin
        return;
    }

    bool unit_file_bin;
    FILE* fp = get_file_pointer_from_unit(unit_num, &unit_file_bin);
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

LFORTRAN_API void _lfortran_file_write(int32_t unit_num, int32_t* iostat, const char *format, ...)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        filep = stdout;
    }
    va_list args;
    va_start(args, format);
    char* str = va_arg(args, char*);
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
    if (unit_file_bin) {
        // size the size of `str_len` to bytes
        size_t str_len = strlen(str);

        // calculate record marker size
        int32_t record_marker = (int32_t)str_len;

        // write record marker before the data
        fwrite(&record_marker, sizeof(record_marker), 1, filep);

        size_t written = fwrite(str, sizeof(char), str_len, filep); // write as binary data

        // write the record marker after the data
        fwrite(&record_marker, sizeof(record_marker), 1, filep);

        if (written != str_len) {
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
        if(strcmp(format, "%s%s") == 0){
            char* end = va_arg(args, char*);
            fprintf(filep, format, str, end);
        } else {
            fprintf(filep, format, str);
        }
        if(iostat != NULL) *iostat = 0;
    }
    va_end(args);
    (void)!ftruncate(fileno(filep), ftell(filep));
}

LFORTRAN_API void _lfortran_string_write(char **str_holder, int32_t* iostat, const char *format, ...) {
    va_list args;
    va_start(args, format);
    char* str = va_arg(args, char*);
    char *s = (char *) malloc(strlen(*str_holder)*sizeof(char));
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

    if(strcmp(format, "%s%s") == 0){
        char* end = va_arg(args, char*);
        sprintf(s, format, str, end);
    } else {
        sprintf(s, format, str);
    }
    _lfortran_strcpy(str_holder, s, 0);
    free(s);
    va_end(args);
    if(iostat != NULL) *iostat = 0;
}

LFORTRAN_API void _lfortran_string_read(char *str, char *format, int *i) {
    sscanf(str, format, i);
}

LFORTRAN_API void _lpython_close(int64_t fd)
{
    if (fclose((FILE*)fd) != 0)
    {
        printf("Error in closing the file!\n");
        exit(1);
    }
}

LFORTRAN_API void _lfortran_close(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin);
    if (!filep) {
        printf("No file found with given unit\n");
        exit(1);
    }
    if (fclose(filep) != 0) {
        printf("Error in closing the file!\n");
        exit(1);
    }
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
LFORTRAN_API char *_lfortran_get_command_argument_value(int n) {
    if (n >= 0 && n < _argc) {
        return strdup(_argv[n]);  // Return a copy of the nth argument
    } else {
        return "";
    }
}

LFORTRAN_API int32_t _lfortran_get_command_argument_length(int n) {
    char* out = _lfortran_get_command_argument_value(n);
    return strlen(out);
}

LFORTRAN_API int32_t _lfortran_get_command_argument_status() {
    return 0;
}

// get_command
LFORTRAN_API char *_lfortran_get_command_command() {
    char* out;
    for(int i=0; i<_argc; i++) {
        if(i == 0) {
            out = strdup(_argv[i]);
        } else {
            out = realloc(out, strlen(out) + strlen(_argv[i]) + 1);
            strcat(out, " ");
            strcat(out, _argv[i]);
        }
    }
    return out;
}

LFORTRAN_API int32_t _lfortran_get_command_length() {
    char* out = _lfortran_get_command_command();
    return strlen(out);
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
    char *filename = malloc(strlen(base_name) + 14);
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
                DIM ", line %ld\n" S_RESET
#endif
                "    %s\n", source_filename, d.line_numbers[index],
                remove_whitespace(read_line_from_file(source_filename,
                d.line_numbers[index])));
        } else {
            fprintf(stderr, "  File \"%s\", "
#ifdef HAVE_LFORTRAN_MACHO
                "line %lld\n    %s\n",
#else
                "line %ld\n    %s\n",
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

LFORTRAN_API char *_lfortran_get_environment_variable(char *name) {
    // temporary solution, the below function _lfortran_get_env_variable should be used
    if (name == NULL) {
        return NULL;
    } else {
        return getenv("HOME");
    }
}

LFORTRAN_API char *_lfortran_get_env_variable(char *name) {
    return getenv(name);
}

LFORTRAN_API int _lfortran_exec_command(char *cmd) {
    return system(cmd);
}
