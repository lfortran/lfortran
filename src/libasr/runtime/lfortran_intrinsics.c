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
#include <stddef.h>  /* ptrdiff_t */

#define PI 3.14159265358979323846
// Enum for float format types to avoid string comparison
typedef enum {
    FLOAT_FORMAT_F64,     // for default 64-bit float formatting
    FLOAT_FORMAT_F32,     // for default 32-bit float formatting
    FLOAT_FORMAT_CUSTOM   // for custom format strings like "F10.5"
} FloatFormatType;

#if defined(_WIN32)
#  include <winsock2.h>
#  include <io.h>
#  define ftruncate _chsize_s
#else
#  include <unistd.h>
#if !defined(COMPILE_TO_WASM)
#  include <sys/wait.h>
#endif
#endif

static int64_t lfortran_getline(char **lineptr, size_t *n, FILE *stream) {
    if (!lineptr || !n || !stream) {
        errno = EINVAL;
        return -1;
    }
    if (*lineptr == NULL || *n == 0) {
        *n = 128;
        *lineptr = (char *)malloc(*n);
        if (!*lineptr) {
            errno = ENOMEM;
            return -1;
        }
    }
    size_t pos = 0;
    int c = 0;
    while ((c = fgetc(stream)) != EOF) {
        if (pos + 1 >= *n) {
            size_t new_n = (*n) * 2;
            char *new_ptr = (char *)realloc(*lineptr, new_n);
            if (!new_ptr) {
                errno = ENOMEM;
                return -1;
            }
            *lineptr = new_ptr;
            *n = new_n;
        }
        (*lineptr)[pos++] = (char)c;
        if (c == '\n') {
            break;
        }
    }
    if (pos == 0 && c == EOF) {
        return -1;
    }
    (*lineptr)[pos] = '\0';
    return (int64_t)pos;
}

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

// Global flag to control runtime error colors
static int _lfortran_use_runtime_colors = 0;  // disabled by default

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
// This header file does not always contain _Unwind_Backtrace()
//#  include <unwind.h>
// So we define these defines manually
typedef enum {
    _URC_NO_REASON = 0,
    _URC_FOREIGN_EXCEPTION_CAUGHT = 1,
    _URC_FATAL_PHASE2_ERROR = 2,
    _URC_FATAL_PHASE1_ERROR = 3,
    _URC_NORMAL_STOP = 4,
    _URC_END_OF_STACK = 5,
    _URC_HANDLER_FOUND = 6,
    _URC_INSTALL_CONTEXT = 7,
    _URC_CONTINUE_UNWIND = 8,
    _URC_FAILURE = 9
} _Unwind_Reason_Code;

struct _Unwind_Context;  // Opaque type, no need for full definition

typedef _Unwind_Reason_Code (*_Unwind_Trace_Fn)(struct _Unwind_Context *, void *);

extern _Unwind_Reason_Code _Unwind_Backtrace(_Unwind_Trace_Fn callback, void *data);

extern uintptr_t _Unwind_GetIP(struct _Unwind_Context *context);
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

#define MIN(x, y) ((x < y) ? x : y)
#define MAX(x, y) ((x < y) ? y : x)

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

LFORTRAN_API void _lfortran_printf(const char* format, const fchar* str, uint32_t str_len, const fchar* end, uint32_t end_len)
{
    if (str == NULL) {
        str = (fchar*)" "; // dummy output
        str_len = 1; // length of dummy output
    }
    // Detect "\b" to raise error (only if still null-terminated)
    if (str_len > 0 && ((char*)str)[0] == '\b') {
        str++;
        str_len--;
        fwrite((char*)str, 1, str_len, stderr);
        fputc('\n', stderr);
        exit(1);
    }

    // Printing without depending on null termination:
    fwrite((char*)str, sizeof(char), str_len, stdout);
    fwrite((char*)end, sizeof(char), end_len, stdout);
    fflush(stdout);
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

/*
    -- Append String (Null-Termination Independent) --
    * It returns null-terminated string.
*/
char* append_to_string_NTI(char* dest, int64_t dest_len, const char* src, int64_t src_len) {
    dest = (char*)realloc(dest, (dest_len + src_len + 1 /* \0 */) * sizeof(char));
    memcpy(dest + dest_len, src, src_len);
    dest[dest_len + src_len] = '\0';
    return dest;
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

static void format_float_fortran(char* result, float val);
static void format_double_fortran(char* result, double val);

void handle_float(FloatFormatType format_type, char* format, double val, int scale, char** result, bool use_sign_plus, char rounding_mode) {
    val = val * pow(10, scale); // scale the value
    if (format_type == FLOAT_FORMAT_F64) {
        char* float_str = (char*)malloc(64 * sizeof(char));
        format_double_fortran(float_str, val);
        *result = append_to_string(*result,float_str);
        free(float_str);
        return;
    } else if (format_type == FLOAT_FORMAT_F32) {
        char* float_str = (char*)malloc(64 * sizeof(char));
        format_float_fortran(float_str, (float)val);
        *result = append_to_string(*result,float_str);
        free(float_str);
        return;
    }
    // FLOAT_FORMAT_CUSTOM: parse the format string
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

    if (rounding_mode == 'u') {
        decimal_part = ceil(decimal_part / rounding_factor) * rounding_factor;
    } else if (rounding_mode == 'd') {
        decimal_part = floor(decimal_part / rounding_factor) * rounding_factor;
    } else if (rounding_mode == 'z') {
        decimal_part = trunc(decimal_part / rounding_factor) * rounding_factor;
    } else {
        // Default: round to nearest
        decimal_part = round(decimal_part / rounding_factor) * rounding_factor;
    }

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

    bool drop_leading_zero = false;
    if (integer_part == 0 && width > 0 && total_length > width) {
        drop_leading_zero = true;
        total_length -= 1;
    }
    
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
    if (integer_part == 0 && (drop_leading_zero || (decimal_part != 0 && format[1] == '0'))) {
        // Omit the leading zero
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
    
    // Store original exp_digits to know if Ee was explicitly specified
    int original_exp_digits = exp_digits;
    if (exp_digits == 0) exp_digits = 2;
    else if (exp_digits == -1) exp_digits = 2;

    bool sign_plus_exist = (is_signed_plus && val >= 0); // SP specifier

    char formatted_value[256];
    double abs_val = fabs(val);

    // Handle special values (Infinity, NaN) before any log10 calculations
    if (isnan(val)) {
        snprintf(formatted_value, sizeof(formatted_value), "NaN");
    } else if (isinf(val)) {
        snprintf(formatted_value, sizeof(formatted_value), "%sInfinity", (val < 0) ? "-" : "");
    } else if (is_g0_like) {
        // For EN0.0E0, always use engineering notation: scale exponent to multiple of 3
        int exponent = 0;
        double scaled_val = val;
        if (abs_val != 0.0) {
            exponent = (int)floor(log10(abs_val));
            int remainder = exponent % 3;
            if (remainder < 0) remainder += 3;
            exponent -= remainder;
            scaled_val = val / pow(10, exponent);
        }

        // For EN0.0E0, format with 0 decimal digits but keep the decimal point
        char val_str[128];
        snprintf(val_str, sizeof(val_str), "%#.0f", scaled_val);
        snprintf(formatted_value, sizeof(formatted_value),
                "%s%s%+d", val_str, c, exponent);  // no padding, plain exponent
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
        
        // Adjust exp_digits dynamically if no explicit Ee was given
        if (original_exp_digits <= 0) {
            int abs_exp = (exponent < 0 ? -exponent : exponent);
            if (abs_exp >= 100) {
                exp_digits = 3;
            } else if (abs_exp >= 10) {
                exp_digits = 2;
            } else {
                exp_digits = 2;
            }
        }
        
        char val_str[128];
        snprintf(val_str, sizeof(val_str), "%.*f", decimal_digits, scaled_val);
        // exp_digits is the number of exponent digits, but %+0*d width includes the sign
        // So we need exp_digits + 1 for the total width
        snprintf(formatted_value, sizeof(formatted_value),
                "%s%s%+0*d", val_str, c, exp_digits + 1, exponent);
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

    // Handle special values (Infinity, NaN) before any log10 calculations
    if (isnan(val) || isinf(val)) {
        const char* special_str = isnan(val) ? "NaN" : ((val < 0) ? "-Infinity" : "Infinity");
        int special_len = strlen(special_str);
        if (width == 0 || special_len <= width) {
            if (width > special_len) {
                for (int i = 0; i < width - special_len; i++) {
                    *result = append_to_string(*result, " ");
                }
            }
            *result = append_to_string(*result, special_str);
        } else {
            for (int i = 0; i < width; i++) {
                *result = append_to_string(*result, "*");
            }
        }
        return;
    }

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
        // Compute exponent based on decimal (stripped zeros) or integer_length
        // This is more accurate than log10 for values near powers of 10
        // For val >= 1: exponent = integer_length - scale
        // For val < 1:  exponent = decimal - scale
        if (fabs(val) >= 1.0) {
            exponent_value = integer_length - scale;
        } else {
            exponent_value = decimal - scale;
        }
    }

    // For ES format with 0 decimal places, we need to round properly
    // and adjust the exponent if rounding causes overflow
    if (is_s_format && digits == 0 && val != 0.0) {
        // Calculate the mantissa for ES format (scale = 1)
        double abs_val = fabs(val);
        double mantissa = abs_val / pow(10, exponent_value);
        // Round to nearest integer
        double rounded_mantissa = round(mantissa);
        // If rounding causes mantissa >= 10, adjust exponent
        if (rounded_mantissa >= 10.0) {
            exponent_value++;
            rounded_mantissa = 1.0;
        }
        // Reconstruct val_str with the rounded mantissa (unsigned)
        sprintf(val_str, "%d", (int)rounded_mantissa);
        integer_length = strlen(val_str);
    }

    int exp = 2;
    if (exp_digits > 0) {
        exp = exp_digits;
    } else if (is_s_format && (exponent_value < 0 ? -exponent_value : exponent_value) >= 10) {
        int abs_exp = (exponent_value < 0 ? -exponent_value : exponent_value);
        exp = (abs_exp == 0) ? 2 : (int)log10(abs_exp) + 1;
    } else if ((exponent_value < 0 ? -exponent_value : exponent_value) >= 100) {
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

    int exp_length = strlen(exponent);
    // The 'E' is dropped for 3+ digit exponents ONLY when no explicit Ee width is given
    // (i.e., when exp_digits <= 0). When an explicit Ee is specified, 'E' is always kept.
    bool drop_e = (exp_digits <= 0 && (exponent_value < 0 ? -exponent_value : exponent_value) >= 100 && exp_length >= 4 && width_digits != 0);
    int FIXED_CHARS_LENGTH = drop_e ? 2 : 3; // digit, ., [E]

    if (width == 0) {
        // For ES0.0E0 or similar, keep digits = 0 to match gfortran behavior
        if (digits == 0 && (width_digits != 0 || decimal_digits != 0)) {
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
        for (int k = 0; k < (scale < 0 ? -scale : scale); k++) {
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

    // Add 'E' unless dropped for 3+ digit exponents (when no explicit Ee given)
    if (!drop_e) {
        strcat(formatted_value, c);
    }
    // formatted_value = "  1.12E" or "  1.12" (if E dropped)
    strcat(formatted_value, exponent);
    // formatted_value = "  1.12E+10" or "  1.12+100" (if E dropped)

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
    char* cleaned_format = malloc(len + 1);

    int j = 0;
    // don't remove blank spaces from within character
    // string editor descriptor
    bool in_quotes = false;
    char current_quote = '\0';

    for (int i = 0; i < len; i++) {
        char c = format[i];
        if (c == '"' || c == '\'') {
            if (in_quotes && current_quote == c && (i + 1) < len && format[i + 1] == c) {
                cleaned_format[j++] = c;
                cleaned_format[j++] = c;
                i++;
                continue;
            }
            if (i == 0 || format[i - 1] != '\\') {
                // toggle in_quotes and set current_quote on entering or exiting quotes
                if (!in_quotes) {
                    in_quotes = true;
                    current_quote = c;
                } else if (current_quote == c) {
                    in_quotes = false;
                    current_quote = '\0';
                }
            }
        }

        if (!isspace(c) || in_quotes) {
            cleaned_format[j++] = c; // copy non-space characters or any character within quotes
        }
    }

    cleaned_format[j] = '\0';
    *cleaned_format_len = j;
    return cleaned_format;
}

static char* unescape_quoted_literal(const char* value, int64_t value_len, char quote_char, int64_t* out_len) {
    char* result = (char*)malloc(value_len + 1);
    int64_t j = 0;
    for (int64_t i = 0; i < value_len; i++) {
        if (value[i] == quote_char && (i + 1) < value_len && value[i + 1] == quote_char) {
            result[j++] = quote_char;
            i++;
        } else {
            result[j++] = value[i];
        }
    }
    result[j] = '\0';
    *out_len = j;
    return result;
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
 * Check if a character is a data or control descriptor that requires comma separation
 */
static bool is_descriptor_requiring_comma(char c) {
    c = tolower(c);
    return (c == 'a' || c == 'i' || c == 'f' || c == 'e' || c == 'd' || 
            c == 'g' || c == 'l' || c == 'b' || c == 'z' ||
            c == 't' || c == 's' || c == 'r');
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
    bool last_was_descriptor = false;
    bool comma_seen = false;

    while (index < format_len) {
        char** ptr = (char**)realloc(format_values_2, (format_values_count + 1) * sizeof(char*));
        if (ptr == NULL) {
            perror("Memory allocation failed.\n");
            free(format_values_2);
        } else {
            format_values_2 = ptr;
        }
        switch (tolower(cformat[index])) {
            case ' ' :
                break;
            case ',' :
                comma_seen = true;
                break;
            case '/' :
            case ':' :
                format_values_2[format_values_count++] = substring(cformat, index, index+1);
                last_was_descriptor = false;
                comma_seen = false;
                break;
            case '*' :
                format_values_2[format_values_count++] = substring(cformat, index, index+1);
                break;
            case '"' :
                start = index++;
                while (index < format_len) {
                    if (cformat[index] == '"') {
                        if ((index + 1) < format_len && cformat[index + 1] == '"') {
                            index += 2;
                            continue;
                        }
                        break;
                    }
                    index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index+1);

                break;
            case '\'' :
                start = index++;
                while (index < format_len) {
                    if (cformat[index] == '\'') {
                        if ((index + 1) < format_len && cformat[index + 1] == '\'') {
                            index += 2;
                            continue;
                        }
                        break;
                    }
                    index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index+1);
                break;
            case 'a' :
                if (last_was_descriptor && !comma_seen) {
                    fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                    exit(1);
                }
                start = index++;
                while (isdigit(cformat[index])) {
                    index++;
                }
                format_values_2[format_values_count++] = substring(cformat, start, index);
                index--;
                last_was_descriptor = true;
                comma_seen = false;
                break;
            case 'e' :
                if (last_was_descriptor && !comma_seen) {
                    fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                    exit(1);
                }
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
                last_was_descriptor = true;
                comma_seen = false;
                break;
            case 'b' :
                start = index++;
                if (tolower(cformat[index]) == 'n' || tolower(cformat[index]) == 'z') {
                    if (last_was_descriptor && !comma_seen) {
                        fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                        exit(1);
                    }
                    format_values_2[format_values_count++] = substring(cformat, start, index+1);
                    last_was_descriptor = true;
                    comma_seen = false;
                } else {
                    if (last_was_descriptor && !comma_seen) {
                        fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                        exit(1);
                    }
                    bool dot = false;
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
                    last_was_descriptor = true;
                    comma_seen = false;
                }
                break;
            case 'd' :
                start = index++;
                if (tolower(cformat[index]) == 't') {
                    if (last_was_descriptor && !comma_seen) {
                        fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                        exit(1);
                    }
                    format_values_2[format_values_count++] = substring(cformat, start, index+1);
                    last_was_descriptor = true;
                    comma_seen = false;
                } else {
                    if (last_was_descriptor && !comma_seen) {
                        fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                        exit(1);
                    }
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
                    last_was_descriptor = true;
                    comma_seen = false;
                }
                break;
            case 'i' :
            case 'f' :
            case 'l' :
            case 'g' :
            case 'z' :
                if (last_was_descriptor && !comma_seen) {
                    fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                    exit(1);
                }
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
                last_was_descriptor = true;
                comma_seen = false;
                break;
            case 's': 
                if (last_was_descriptor && !comma_seen) {
                    fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                    exit(1);
                }
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
                last_was_descriptor = true;
                comma_seen = false;
                break;
            case '(' :
                start = index;
                index = find_matching_parentheses(format, format_len, index);
                format_values_2[format_values_count++] = substring(cformat, start, index);
                *item_start = format_values_count;
                break;
            case 't' :
                // handle 'T', 'TL' & 'TR' editing see section 13.8.1.2 in 24-007.pdf
                if (last_was_descriptor && !comma_seen) {
                    fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                    exit(1);
                }
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
                last_was_descriptor = true;
                comma_seen = false;
                break;
            case 'r':  // Rounding mode: RU, RD, RN, RZ
                if (last_was_descriptor && !comma_seen) {
                    fprintf(stderr, "Error: Missing comma between descriptors in format string\n");
                    exit(1);
                }
                start = index++;
                if (tolower(cformat[index]) == 'u' || tolower(cformat[index]) == 'd' ||
                    tolower(cformat[index]) == 'n' || tolower(cformat[index]) == 'z' ||
                    tolower(cformat[index]) == 'c' || tolower(cformat[index]) == 'p') {
                    format_values_2[format_values_count++] = substring(cformat, start, index+1);
                    last_was_descriptor = true;
                    comma_seen = false;
                } else {
                    fprintf(stderr, "Error: Invalid rounding mode after 'R'\n");
                    exit(1);
                }
                break;
            default :
                if (
                    ((cformat[index] == '-' || cformat[index] == '+') && isdigit(cformat[index + 1]) && tolower(cformat[index + 2]) == 'p')
                    || ((isdigit(cformat[index])) && tolower(cformat[index + 1]) == 'p')) {
                    start = index;
                    index = index + 1 + (cformat[index] == '-' || cformat[index] == '+');
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
        case UNSIGNED_INTEGER_8_TYPE:
        case UNSIGNED_INTEGER_16_TYPE:
        case UNSIGNED_INTEGER_32_TYPE:
        case UNSIGNED_INTEGER_64_TYPE:
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
    if (lowered_format_value == 'z') {
        return current_arg_correct_format == 'i' ||
               current_arg_correct_format == 'f';
    }
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

void handle_hexadecimal(const char* format, Primitive_Types type,
        void* arg_ptr, char** result) {
    int width = 0;
    int min_digits = 1;
    if (strlen(format) > 1) {
        width = atoi(format + 1);
    }
    const char *dot = strchr(format + 1, '.');
    if (dot != NULL) {
        int parsed = atoi(dot + 1);
        if (parsed > 0) {
            min_digits = parsed;
        }
    }

    int byte_count = 0;
    uint64_t raw = 0;
    switch (type) {
        case INTEGER_8_TYPE:
            byte_count = 1;
            raw = (uint64_t)(uint8_t)(*(int8_t*)arg_ptr);
            break;
        case INTEGER_16_TYPE:
            byte_count = 2;
            raw = (uint64_t)(uint16_t)(*(int16_t*)arg_ptr);
            break;
        case INTEGER_32_TYPE:
            byte_count = 4;
            raw = (uint64_t)(uint32_t)(*(int32_t*)arg_ptr);
            break;
        case INTEGER_64_TYPE:
            byte_count = 8;
            raw = (uint64_t)(*(int64_t*)arg_ptr);
            break;
        case UNSIGNED_INTEGER_8_TYPE:
            byte_count = 1;
            raw = (uint64_t)(*(uint8_t*)arg_ptr);
            break;
        case UNSIGNED_INTEGER_16_TYPE:
            byte_count = 2;
            raw = (uint64_t)(*(uint16_t*)arg_ptr);
            break;
        case UNSIGNED_INTEGER_32_TYPE:
            byte_count = 4;
            raw = (uint64_t)(*(uint32_t*)arg_ptr);
            break;
        case UNSIGNED_INTEGER_64_TYPE:
            byte_count = 8;
            raw = (uint64_t)(*(uint64_t*)arg_ptr);
            break;
        case FLOAT_32_TYPE: {
            byte_count = sizeof(float);
            uint32_t tmp = 0;
            memcpy(&tmp, arg_ptr, sizeof(float));
            raw = (uint64_t)tmp;
            break;
        }
        case FLOAT_64_TYPE: {
            byte_count = sizeof(double);
            uint64_t tmp = 0;
            memcpy(&tmp, arg_ptr, sizeof(double));
            raw = tmp;
            break;
        }
        default:
            fprintf(stderr, "Unsupported type for Z edit descriptor\n");
            exit(1);
    }

    int total_digits = byte_count * 2;
    if (total_digits <= 0) {
        total_digits = 1;
    }

    char *hex_full = (char*)malloc((total_digits + 1) * sizeof(char));
    snprintf(hex_full, total_digits + 1, "%0*llX", total_digits,
        (unsigned long long)raw);

    int start_idx = 0;
    while (start_idx < total_digits - 1 && hex_full[start_idx] == '0') {
        start_idx++;
    }

    int digits_len = total_digits - start_idx;
    if (digits_len <= 0) digits_len = 1;
    if (min_digits < 1) min_digits = 1;

    int output_len = digits_len > min_digits ? digits_len : min_digits;
    char *formatted = (char*)malloc((output_len + 1) * sizeof(char));
    int leading_zeros = output_len - digits_len;
    for (int i = 0; i < leading_zeros; i++) {
        formatted[i] = '0';
    }
    memcpy(formatted + leading_zeros, hex_full + start_idx, digits_len);
    formatted[output_len] = '\0';

    if (width > 0 && width < output_len) {
        for (int i = 0; i < width; i++) {
            *result = append_to_string(*result, "*");
        }
        free(hex_full);
        free(formatted);
        return;
    }

    if (width > output_len) {
        int padding = width - output_len;
        char *spaces = (char*)malloc((padding + 1) * sizeof(char));
        memset(spaces, ' ', padding);
        spaces[padding] = '\0';
        *result = append_to_string(*result, spaces);
        free(spaces);
    }

    *result = append_to_string(*result, formatted);
    free(hex_full);
    free(formatted);
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
            ASSERT_MSG(s_info->current_element_type != CHAR_PTR_TYPE,
                    "ICE:%s\n","Not supported -- Can't deduce length for CCHAR");
            s_info->current_arg_info.current_string_len = 
                *(int64_t*)((char*)s_info->current_arg_info.current_arg + sizeof(char*)); // Get string len.
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
            s_info->current_element_type == STRING_DESCRIPTOR_TYPE)){ // Array of strings (Consecutive memory)
        char* arr_str_ptr = *(char**)s_info->current_arg_info.current_arg;
        s_info->temp_char_pp =  arr_str_ptr + s_info->current_arg_info.current_string_len;
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

static void format_float_fortran(char* result, float val) {
    float abs_val = fabsf(val);
    
    if (abs_val == 0.0f) {
        sprintf(result, "0.00000000");
        return;
    }
    if (abs_val < 0.1f || abs_val >= 1.0e8f) {
        sprintf(result, "%.8E", val);
        return;
    }

    int magnitude = (int)floor(log10f(abs_val)) + 1;
    int decimal_places = 9 - magnitude;
    if (decimal_places < 0) decimal_places = 0;
    
    char format_str[32];
    sprintf(format_str, "%%.%df", decimal_places);
    sprintf(result, format_str, val);
}

static void format_double_fortran(char* result, double val) {
    double abs_val = fabs(val);
    
    if (abs_val == 0.0) {
        sprintf(result, "0.0000000000000000");
        return;
    }

    if (abs_val < 0.1 || abs_val >= 1.0e16) {
        sprintf(result, "%.16E", val);
        char* e_pos = strchr(result, 'E');
        if (e_pos != NULL) {
            char sign = e_pos[1];
            int exp_val = atoi(e_pos + 2);
            sprintf(e_pos, "E%c%03d", sign, (exp_val < 0 ? -exp_val : exp_val));
        }
        return;
    }
    
    int magnitude = (int)floor(log10(abs_val)) + 1;
    int decimal_places = 17 - magnitude;
    if (decimal_places < 0) decimal_places = 0;
    
    char format_str[32];
    sprintf(format_str, "%%.%df", decimal_places);
    sprintf(result, format_str, val);
}

// Returns the length of the string that is printed inside result
int64_t print_into_string(Serialization_Info* s_info,  char* result){
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
                char real_str[64], imag_str[64];
                format_double_fortran(real_str, real);
                format_double_fortran(imag_str, imag);
                sprintf(result, "(%s,%s)", real_str, imag_str);
            } else {
                format_double_fortran(result, *(double*)arg);
            }
            break;
        case FLOAT_32_TYPE:
            if(s_info->current_arg_info.is_complex){
                float real = *(float*)arg;
                move_to_next_element(s_info, false);
                float imag = *(float*)s_info->current_arg_info.current_arg;
                char real_str[64], imag_str[64];
                format_float_fortran(real_str, real);
                format_float_fortran(imag_str, imag);
                sprintf(result, "(%s,%s)", real_str, imag_str);
            } else {
                format_float_fortran(result, *(float*)arg);
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
                    return s_info->current_arg_info.current_string_len;
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

    return strlen(result);
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

void default_formatting(char** result, int64_t *result_size_ptr, struct serialization_info* s_info){
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
        int64_t printed_arg_size = print_into_string(s_info,  (*result) + result_size);
        result_size += printed_arg_size;
    }

    (*result_size_ptr) = result_size;
}
void free_serialization_info(Serialization_Info* s_info){
    free(s_info->array_sizes.ptr);
    free(s_info->string_lengths.ptr);
    free_stack(s_info->array_sizes_stack);
    free_stack(s_info->array_serialiation_start_index);
    va_end(*s_info->current_arg_info.args);
}

FILE* get_file_pointer_from_unit(int32_t unit_num, bool *unit_file_bin, int *access_id, bool *read_access, bool *write_access, int *delim, bool *blank_zero, int32_t *recl, int *sign_mode);

LFORTRAN_API char* _lcompilers_string_format_fortran(const char* format, int64_t format_len, const char* serialization_string,
    int64_t *result_size, int32_t array_sizes_cnt, int32_t string_lengths_cnt, ...)
{
    va_list args;
    va_start(args, string_lengths_cnt);
    char* result = (char*)malloc(sizeof(char)); //TODO : the consumer of this string needs to free it.
    result[0] = '\0';
    (*result_size) = 0;
    int64_t result_len = 0;  // Track actual result length (handles embedded nulls)

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
        default_formatting(&result, result_size, &s_info);
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
    free(cleaned_format);
    format_values = parse_fortran_format((const fchar*)modified_input_string, strlen(modified_input_string), &format_values_count, &item_start_idx);
    /*
    is_SP_specifier = false  --> 'S' OR 'SS'
    is_SP_specifier = true  --> 'SP'
    */
    // Initialize is_SP_specifier based on unit 6's sign mode
    // Unit 6 is stdout, which is what PRINT uses
    int unit6_sign_mode = 0;
    get_file_pointer_from_unit(6, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &unit6_sign_mode);
    bool is_SP_specifier = (unit6_sign_mode == 1);  // 1 = sign='plus'
    int item_start = 0;
    bool array = false;
    bool BreakWhileLoop= false;
    char rounding_mode = 'n';  // 'n'=nearest, 'u'=up, 'd'=down, 'z'=zero
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
                result = append_to_string_NTI(result, result_len, "\n", 1);
                result_len += 1;
            } else if (value[0] == '*') {
                array = true;
            } else if (isdigit(value[0]) && tolower(value[1]) == 'p') {
                // Scale Factor nP
                scale = atoi(&value[0]);
            } else if ((value[0] == '-' || value[0] == '+') && isdigit(value[1]) && tolower(value[2]) == 'p') {
                char temp[3] = {value[0],value[1],'\0'};
                scale = atoi(temp);
            } else if ((value[0] == '\"' && value[strlen(value) - 1] == '\"') ||
                (value[0] == '\'' && value[strlen(value) - 1] == '\'')) {
                // String literal in format
                char quote_char = value[0];
                char* inner_value = substring(value, 1, strlen(value) - 1);
                int64_t val_len = 0;
                char* unescaped_value = unescape_quoted_literal(inner_value, strlen(inner_value), quote_char, &val_len);
                result = append_to_string_NTI(result, result_len, unescaped_value, val_len);
                result_len += val_len;
                free(inner_value);
                free(unescaped_value);
            } else if (tolower(value[strlen(value) - 1]) == 'x') {
                result = append_to_string_NTI(result, result_len, " ", 1);
                result_len += 1;
            } else if (tolower(value[0]) == 's') {
                is_SP_specifier = ( strlen(value) == 2 /*case 'S' specifier*/ &&
                                    tolower(value[1]) == 'p'); 
            } else if (tolower(value[0]) == 'r') {
                // Rounding mode descriptors (RU, RD, RN, RZ)
                if (strlen(value) >= 2) {
                    char mode = tolower(value[1]);
                    if (mode == 'u') {
                        rounding_mode = 'u';
                    } else if (mode == 'd') {
                        rounding_mode = 'd';
                    } else if (mode == 'n') {
                        rounding_mode = 'n';
                    } else if (mode == 'z') {
                        rounding_mode = 'z';
                    }
                }
            } else if (tolower(value[0]) == 't') {
                if (tolower(value[1]) == 'l') {
                    // handle "TL" format specifier - move position left
                    int tab_left_pos = atoi(value + 2);
                    if (tab_left_pos > result_len) {
                        result[0] = '\0';
                        result_len = 0;
                    } else {
                        result_len -= tab_left_pos;
                        result[result_len] = '\0';
                    }
                } else if (tolower(value[1]) == 'r') {
                    // handle "TR" format specifier - move position right (add spaces)
                    int spaces_needed = atoi(value + 2);
                    if (spaces_needed > 0) {
                        result = (char*)realloc(result, result_len + spaces_needed + 1);
                        memset(result + result_len, ' ', spaces_needed);
                        result_len += spaces_needed;
                        result[result_len] = '\0';
                    }
                } else {
                    if (!move_to_next_element(&s_info, true)) break;
                    int tab_position = atoi(value + 1);
                    int spaces_needed = tab_position - (int)result_len - 1;
                    if (spaces_needed > 0) {
                        result = (char*)realloc(result, result_len + spaces_needed + 1);
                        memset(result + result_len, ' ', spaces_needed);
                        result_len += spaces_needed;
                        result[result_len] = '\0';
                    } else if (spaces_needed < 0) {
                        // Truncate the string to the length specified by Tn
                        if (tab_position - 1 < (int)result_len) {
                            result_len = tab_position - 1;
                            result[result_len] = '\0';
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
                    result_len = strlen(result);
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
                        char* temp_buf = (char*)malloc(1); temp_buf[0] = '\0';
                        handle_logical("l",*(bool*)s_info.current_arg_info.current_arg, &temp_buf);
                        int64_t temp_len = strlen(temp_buf);
                        result = append_to_string_NTI(result, result_len, temp_buf, temp_len);
                        result_len += temp_len;
                        free(temp_buf);
                        continue;
                    }
                    char* arg = *(char**)s_info.current_arg_info.current_arg;
                    if (arg == NULL) continue;
                    if (strlen(value) == 1) {
                        // Simple 'A' format - use full string length, preserve embedded nulls
                        result = append_to_string_NTI(result, result_len, arg, s_info.current_arg_info.current_string_len);
                        result_len += s_info.current_arg_info.current_string_len;
                    } else {
                        // 'Aw' format with width - copy exactly w characters, preserving embedded nulls
                        int64_t width = atoi(value + 1);
                        int64_t src_len = s_info.current_arg_info.current_string_len;
                        int64_t copy_len = (width < src_len) ? width : src_len;
                        int64_t pad_len = (width > src_len) ? (width - src_len) : 0;
                        // Reallocate result to fit new content
                        result = (char*)realloc(result, result_len + width + 1);
                        // Right-justify: add leading spaces if padding needed
                        if (pad_len > 0) {
                            memset(result + result_len, ' ', pad_len);
                        }
                        // Copy the string content (preserves embedded nulls)
                        memcpy(result + result_len + pad_len, arg, copy_len);
                        result_len += width;
                        result[result_len] = '\0';
                    }
                } else if (tolower(value[0]) == 'i') {
                    // Integer Editing ( I[w[.m]] )
                    char* temp_buf = (char*)malloc(1); temp_buf[0] = '\0';
                    handle_integer(value, integer_val, &temp_buf, is_SP_specifier);
                    int64_t temp_len = strlen(temp_buf);
                    result = append_to_string_NTI(result, result_len, temp_buf, temp_len);
                    result_len += temp_len;
                    free(temp_buf);
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
                        result = append_to_string_NTI(result, result_len, "<unsupported>", 13);
                        result_len += 13;
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
                        result = append_to_string_NTI(result, result_len, binary_str, bin_len);
                        result_len += bin_len;
                    } else if (bin_len > width) {
                        // Output asterisks for overflow
                        result = (char*)realloc(result, result_len + width + 1);
                        memset(result + result_len, '*', width);
                        result_len += width;
                        result[result_len] = '\0';
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
                            result = (char*)realloc(result, result_len + padding_needed + 1);
                            memset(result + result_len, ' ', padding_needed);
                            result_len += padding_needed;
                            result[result_len] = '\0';
                        }
                        result = append_to_string_NTI(result, result_len, binary_str, bin_len);
                        result_len += bin_len;
                    }
                } else if (tolower(value[0]) == 'z') {
                    char* temp_buf = (char*)malloc(1); temp_buf[0] = '\0';
                    handle_hexadecimal(value, s_info.current_element_type,
                        s_info.current_arg_info.current_arg, &temp_buf);
                    int64_t temp_len = strlen(temp_buf);
                    result = append_to_string_NTI(result, result_len, temp_buf, temp_len);
                    result_len += temp_len;
                    free(temp_buf);
                } else if (tolower(value[0]) == 'g') {
                    int width = 0;
                    int precision = 0;
                    bool has_dot = false;
                    if (strlen(value) > 1) {
                        width = atoi(value + 1); // Get width after 'g'
                    }
                    const char *dot = strchr(value + 1, '.'); // Look for '.' after 'g'
                    if (dot != NULL) {
                        has_dot = true;
                        precision = atoi(dot + 1); // get digits after '.'
                    }
                    char buffer[100];
                    char formatted[100];
                    if (s_info.current_element_type == FLOAT_32_TYPE || s_info.current_element_type == FLOAT_64_TYPE) {
                        if (isnan(double_val)) {
                            snprintf(formatted, sizeof(formatted), "NaN");
                        } else if (isinf(double_val)) {
                            snprintf(formatted, sizeof(formatted), "%sInfinity", (double_val < 0) ? "-" : "");
                        } else if (width == 0 && !has_dot) {
                            if (s_info.current_element_type == FLOAT_32_TYPE) {
                                format_float_fortran(formatted, (float)double_val);
                            } else {
                                format_double_fortran(formatted, double_val);
                            }
                        } else if (double_val == 0.0 || (fabs(double_val) >= 0.1 && fabs(double_val) < pow(10.0, precision))) {
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
                        int64_t buf_len = strlen(buffer);
                        result = append_to_string_NTI(result, result_len, buffer, buf_len);
                        result_len += buf_len;
                    } else if (s_info.current_element_type == INTEGER_8_TYPE ||
                               s_info.current_element_type == INTEGER_16_TYPE ||
                               s_info.current_element_type == INTEGER_32_TYPE ||
                               s_info.current_element_type == INTEGER_64_TYPE) {
                        snprintf(buffer, sizeof(buffer), "%"PRId64, integer_val);
                        int64_t buf_len = strlen(buffer);
                        result = append_to_string_NTI(result, result_len, buffer, buf_len);
                        result_len += buf_len;
                    } else if (s_info.current_element_type == CHAR_PTR_TYPE ||
                        s_info.current_element_type == STRING_DESCRIPTOR_TYPE) {
                        result = append_to_string_NTI(result, result_len, char_val, s_info.current_arg_info.current_string_len);
                        result_len += s_info.current_arg_info.current_string_len;
                    } else if (s_info.current_element_type == LOGICAL_TYPE) {
                        result = append_to_string_NTI(result, result_len, bool_val ? "T" : "F", 1);
                        result_len += 1;
                    } else {
                        result = append_to_string_NTI(result, result_len, "<unsupported>", 13);
                        result_len += 13;
                    }
                } else if (tolower(value[0]) == 'd') {
                    // D Editing (D[w[.d]])
                    char* temp_buf = (char*)malloc(1); temp_buf[0] = '\0';
                    handle_decimal(value, double_val, scale, &temp_buf, "D", is_SP_specifier);
                    int64_t temp_len = strlen(temp_buf);
                    result = append_to_string_NTI(result, result_len, temp_buf, temp_len);
                    result_len += temp_len;
                    free(temp_buf);
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
                    char* temp_buf = (char*)malloc(1); temp_buf[0] = '\0';
                    if (format_type == 'n') {
                        handle_en(value, double_val, scale, &temp_buf, "E", is_SP_specifier);
                    } else {
                        handle_decimal(value, double_val, scale, &temp_buf, "E", is_SP_specifier);
                    }
                    int64_t temp_len = strlen(temp_buf);
                    result = append_to_string_NTI(result, result_len, temp_buf, temp_len);
                    result_len += temp_len;
                    free(temp_buf);
                } else if (tolower(value[0]) == 'f') {
                    char* temp_buf = (char*)malloc(1); temp_buf[0] = '\0';
                    FloatFormatType float_fmt_type;
                    if (strcmp(value, "f-64") == 0) {
                        float_fmt_type = FLOAT_FORMAT_F64;
                    } else if (strcmp(value, "f-32") == 0) {
                        float_fmt_type = FLOAT_FORMAT_F32;
                    } else {
                        float_fmt_type = FLOAT_FORMAT_CUSTOM;
                    }
                    handle_float(float_fmt_type, value, double_val, scale, &temp_buf, is_SP_specifier, rounding_mode);
                    int64_t temp_len = strlen(temp_buf);
                    result = append_to_string_NTI(result, result_len, temp_buf, temp_len);
                    result_len += temp_len;
                    free(temp_buf);
                } else if (tolower(value[0]) == 'l') {
                    bool val = *(bool*)s_info.current_arg_info.current_arg;
                    char* temp_buf = (char*)malloc(1); temp_buf[0] = '\0';
                    handle_logical(value, val, &temp_buf);
                    int64_t temp_len = strlen(temp_buf);
                    result = append_to_string_NTI(result, result_len, temp_buf, temp_len);
                    result_len += temp_len;
                    free(temp_buf);
                } else if (strlen(value) != 0) {
                    printf("Printing support is not available for %s format.\n",value);
                }

            }
        }
        if(BreakWhileLoop) break;
        if (move_to_next_element(&s_info, true)) {
            if (!array) {
                result = append_to_string_NTI(result, result_len, "\n", 1);
                result_len += 1;
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
    // Use tracked length (handles embedded nulls correctly)
    (*result_size) = result_len;
    return result;
}

static int runtime_line_num_width(unsigned int line) {
    if (line >= 10000) {
        return 5;
    } else if (line >= 1000) {
        return 4;
    } else if (line >= 100) {
        return 3;
    } else if (line >= 10) {
        return 2;
    }
    return 1;
}

static void runtime_sanitize_line(char *line) {
    char *p = line;
    char *w = line;
    while (*p) {
        if (*p == '\t') {
            *w++ = ' ';
        } else if (*p != '\r') {
            *w++ = *p;
        }
        p++;
    }
    *w = '\0';
    size_t len = strlen(line);
    if (len > 0 && line[len - 1] == '\n') {
        line[len - 1] = '\0';
    }
}

static char* runtime_read_line(const char *filename, unsigned int line_no) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        return NULL;
    }
    char *line = NULL;
    size_t cap = 0;
    unsigned int current = 1;
    while (lfortran_getline(&line, &cap, file) != -1) {
        if (current == line_no) {
            fclose(file);
            runtime_sanitize_line(line);
            return line;
        }
        current++;
    }
    fclose(file);
    free(line);
    return NULL;
}

static size_t runtime_squiggle_len(const char *line, unsigned int column) {
    size_t len = strlen(line);
    if (len == 0) {
        return 1;
    }
    if (column == 0) {
        column = 1;
    }
    size_t start = column - 1;
    if (start >= len) {
        return 1;
    }
    if (isspace((unsigned char)line[start])) {
        return 1;
    }
    size_t i = start;
    while (i < len && !isspace((unsigned char)line[i])) {
        i++;
    }
    if (i == start) {
        return 1;
    }
    return i - start;
}


static void runtime_render_error(const char *formatted) {
    const char *color_reset = _lfortran_use_runtime_colors ? "\033[0;0m" : "";
    const char *color_bold = _lfortran_use_runtime_colors ? "\033[0;1m" : "";
    const char *color_bold_red = _lfortran_use_runtime_colors ? "\033[0;31;1m" : "";
    const char *color_bold_blue = _lfortran_use_runtime_colors ? "\033[0;34;1m" : "";

    // Check if this is a STOP statement (not an error)
    if (strncmp(formatted, "STOP", 4) == 0 || strncmp(formatted, "ERROR STOP", 10) == 0) {
        fprintf(stderr, "%s", formatted);
        fflush(stderr);
        return;
    }

    // Try to parse "At line:col of file filename\nmessage" format
    const char *prefix = "At ";
    const char *file_marker = " of file ";

    // If the message doesn't have location info, just print it with the runtime error prefix
    if (strncmp(formatted, prefix, strlen(prefix)) != 0) {
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, formatted, color_reset);
        fflush(stderr);
        return;
    }

    // Parse line number
    const char *p = formatted + strlen(prefix);
    char *endptr = NULL;
    unsigned long line = strtoul(p, &endptr, 10);
    if (!endptr || *endptr != ':') {
        // Parsing failed, just print the message
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, formatted, color_reset);
        fflush(stderr);
        return;
    }

    // Parse column number
    p = endptr + 1;
    unsigned long column = strtoul(p, &endptr, 10);
    if (!endptr) {
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, formatted, color_reset);
        fflush(stderr);
        return;
    }

    // Parse filename
    const char *file_pos = strstr(endptr, file_marker);
    if (!file_pos) {
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, formatted, color_reset);
        fflush(stderr);
        return;
    }

    const char *filename_start = file_pos + strlen(file_marker);
    const char *filename_end = strchr(filename_start, '\n');
    if (!filename_end) {
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, formatted, color_reset);
        fflush(stderr);
        return;
    }

    size_t filename_len = (size_t)(filename_end - filename_start);
    char *filename = (char*)malloc(filename_len + 1);
    if (!filename) {
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, formatted, color_reset);
        fflush(stderr);
        return;
    }
    memcpy(filename, filename_start, filename_len);
    filename[filename_len] = '\0';

    const char *message = filename_end + 1;
    if (*message == '\0') {
        message = "Unknown error";
    }

    // Always print the error message
    fprintf(stderr, "%sruntime error%s%s: %s%s\n",
            color_bold_red, color_reset, color_bold, message, color_reset);

    // If line is 0, we don't have a specific location - just show the message
    if (line == 0) {
        free(filename);
        fflush(stderr);
        return;
    }

    // Try to read the source line - if it fails, still show location without source
    char *line_text = runtime_read_line(filename, (unsigned int)line);

    int width = runtime_line_num_width((unsigned int)line);

    // Show location info
    fprintf(stderr, "%*s%s-->%s %s:%lu:%lu\n",
            width, "", color_bold_blue, color_reset, filename, line, column);

    // If we have the source line, show it with the squiggle
    if (line_text) {
        size_t squiggle_len = runtime_squiggle_len(line_text, (unsigned int)column);

        fprintf(stderr, "%*s%s|%s\n", width + 1, "", color_bold_blue, color_reset);
        fprintf(stderr, "%s%*lu |%s %s\n",
                color_bold_blue, width, line, color_reset, line_text);
        fprintf(stderr, "%*s%s|%s ", width + 1, "", color_bold_blue, color_reset);
        if (column > 1) {
            fprintf(stderr, "%*s", (int)(column - 1), "");
        }
        fprintf(stderr, "%s", color_bold_red);
        for (size_t i = 0; i < squiggle_len; i++) {
            fputc('^', stderr);
        }
        fprintf(stderr, " %s\n", color_reset);

        free(line_text);
    }

    fflush(stderr);
    free(filename);
}

static void print_label_span(const Span *span, bool is_primary, 
                              const char *label_msg, bool use_colors) {
    const char *color_reset = use_colors ? "\033[0;0m" : "";
    const char *color_bold_blue = use_colors ? "\033[0;34;1m" : "";
    const char *color_bold_red = use_colors ? "\033[0;31;1m" : "";

    const char *underline_color = is_primary ? color_bold_red : color_bold_blue;
    char underline_char = is_primary ? '^' : '~';

    uint32_t line = span->start_l;
    uint32_t start_col = span->start_c;
    uint32_t end_col = (span->start_l == span->last_l) ? span->last_c : start_col;

    const char *filename = span->filename ? span->filename : "unknown";
    char *line_text = runtime_read_line(filename, line);
    if (!line_text) {
        return;
    }
 
    int width = runtime_line_num_width(line);

    fprintf(stderr, "%*s%s|%s\n", width + 1, "", color_bold_blue, color_reset);
    fprintf(stderr, "%s%*u |%s %s\n",
            color_bold_blue, width, line, color_reset, line_text);
    fprintf(stderr, "%*s%s|%s ", width + 1, "", color_bold_blue, color_reset);

    if (start_col > 1) {
        fprintf(stderr, "%*s", (int)(start_col - 1), "");
    }

    fprintf(stderr, "%s", underline_color);
    size_t underline_len = (end_col >= start_col) ? (end_col - start_col + 1) : 1;
    for (size_t i = 0; i < underline_len; i++) {
        fputc(underline_char, stderr);
    }

    if (label_msg && label_msg[0] != '\0') {
        fprintf(stderr, " %s", label_msg);
    }
    fprintf(stderr, "%s\n", color_reset);

    free(line_text);
}

LFORTRAN_API void _lcompilers_runtime_error(Label *labels, uint32_t n_labels, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);
    if (needed < 0) {
        vfprintf(stderr, format, args);
        fflush(stderr);
        va_end(args);
        return;
    }
    char *error_msg = (char*)malloc((size_t)needed + 1);
    if (!error_msg) {
        vfprintf(stderr, format, args);
        fflush(stderr);
        va_end(args);
        return;
    }
    vsnprintf(error_msg, (size_t)needed + 1, format, args);
    va_end(args);

    bool use_colors = _lfortran_use_runtime_colors;
    const char *color_reset = use_colors ? "\033[0;0m" : "";
    const char *color_bold = use_colors ? "\033[0;1m" : "";
    const char *color_bold_red = use_colors ? "\033[0;31;1m" : "";
    const char *color_bold_blue = use_colors ? "\033[0;34;1m" : "";

    if (n_labels == 0) {
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, error_msg, color_reset);
        fflush(stderr);
        free(error_msg);
        return;
    }

    Label *primary_label = NULL;
    for (uint32_t i = 0; i < n_labels; i++) {
        if (labels[i].primary) {
            primary_label = &labels[i];
            break;
        }
    }

    if (!primary_label || primary_label->n_spans == 0) {
        fprintf(stderr, "%sruntime error%s%s: %s%s\n",
                color_bold_red, color_reset, color_bold, error_msg, color_reset);
        fflush(stderr);
        free(error_msg);
        return;
    }

    fprintf(stderr, "%sruntime error%s%s: %s%s\n",
            color_bold_red, color_reset, color_bold, error_msg, color_reset);

    Span *first_span = &primary_label->spans[0];
    const char *filename = first_span->filename ? first_span->filename : "unknown";

    int width = runtime_line_num_width(first_span->start_l);
    fprintf(stderr, "%*s%s-->%s %s:%u:%u\n",
            width, "", color_bold_blue, color_reset, 
            filename, first_span->start_l, first_span->start_c);

    for (uint32_t i = 0; i < n_labels; i++) {
        Label *label = &labels[i];
        for (uint32_t j = 0; j < label->n_spans; j++) {
            print_label_span(&label->spans[j], label->primary, 
                           label->message, use_colors);
            free(label->message);
        }
    }

    fflush(stderr);
    free(error_msg);
}

LFORTRAN_API char* _lcompilers_snprintf(const char* format, ...) {
    va_list args;
    va_start(args, format);
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);
    if (needed < 0) {
        vfprintf(stderr, format, args);
        fflush(stderr);
        va_end(args);
        return NULL;
    }
    char *formatted = (char*)malloc((size_t)needed + 1);
    if (!formatted) {
        vfprintf(stderr, format, args);
        fflush(stderr);
        va_end(args);
        return NULL;
    }
    vsnprintf(formatted, (size_t)needed + 1, format, args);
    va_end(args);
    return formatted;
}

// TODO: after migrating to llvm_utils->generate_runtime_error2(), remove runtime_render_error() and revert this function to how it was before
LFORTRAN_API void _lcompilers_print_error(const char* format, ...)
{
    va_list args;
    va_start(args, format);
    va_list args_copy;
    va_copy(args_copy, args);
    int needed = vsnprintf(NULL, 0, format, args_copy);
    va_end(args_copy);
    if (needed < 0) {
        vfprintf(stderr, format, args);
        fflush(stderr);
        va_end(args);
        return;
    }
    char *formatted = (char*)malloc((size_t)needed + 1);
    if (!formatted) {
        vfprintf(stderr, format, args);
        fflush(stderr);
        va_end(args);
        return;
    }
    vsnprintf(formatted, (size_t)needed + 1, format, args);
    va_end(args);
    runtime_render_error(formatted);
    free(formatted);
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
    // Use Smith's algorithm for numerical stability
    // This avoids overflow/underflow issues with very large or small numbers
    float p = a->re, q = a->im;
    float r = b->re, s = b->im;
    
    if (fabsf(r) >= fabsf(s)) {
        float ratio = s / r;
        float denom = r + s * ratio;
        result->re = (p + q * ratio) / denom;
        result->im = (q - p * ratio) / denom;
    } else {
        float ratio = r / s;
        float denom = s + r * ratio;
        result->re = (p * ratio + q) / denom;
        result->im = (q * ratio - p) / denom;
    }
}

LFORTRAN_API void _lfortran_complex_div_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result)
{
    // Use Smith's algorithm for numerical stability
    // This avoids overflow/underflow issues with very large or small numbers
    double p = a->re, q = a->im;
    double r = b->re, s = b->im;
    
    if (fabs(r) >= fabs(s)) {
        double ratio = s / r;
        double denom = r + s * ratio;
        result->re = (p + q * ratio) / denom;
        result->im = (q - p * ratio) / denom;
    } else {
        double ratio = r / s;
        double denom = s + r * ratio;
        result->re = (p * ratio + q) / denom;
        result->im = (q * ratio - p) / denom;
    }
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
    uint32_t shift = (shift_signed < 0 ? -shift_signed : shift_signed);

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
    char* dest_char = (char*)malloc(s1_len+s2_len);
    for (int i = 0; i < s1_len; i++) {
        dest_char[cntr] = s1[i];
        cntr++;
    }
    for (int i = 0; i < s2_len; i++) {
        dest_char[cntr] = s2[i];
        cntr++;
    }
    return dest_char;
}

/*  
    Copy from RHS into LHS and,
    pad right if LHS is longer than RHS
*/
LFORTRAN_API void _lfortran_copy_str_and_pad(
    char* lhs, int64_t lhs_len,
    char* rhs, int64_t rhs_len){

    lfortran_assert(lhs != NULL, "Run-time Error : Copying into unallocated LHS string.")
    if(rhs == NULL) lfortran_error("Run-time Error : Copying from unallocated RHS string.");

    int64_t data_amount_to_copy = MIN(lhs_len, rhs_len);
    memcpy(lhs, rhs, data_amount_to_copy * sizeof(char));

    int64_t pad_amount = lhs_len - data_amount_to_copy;
    memset(lhs + data_amount_to_copy, ' ', pad_amount * sizeof(char));
}
// TODO : split them into three functions instead of making compile-time choices at runtime
LFORTRAN_API void _lfortran_strcpy(
    char** lhs, int64_t* lhs_len,
    bool is_lhs_allocatable, bool is_lhs_deferred,
    char* rhs, int64_t rhs_len){
    if(!is_lhs_deferred && !is_lhs_allocatable){
        lfortran_assert(*lhs != NULL, "Runtime Error : Non-allocatable string isn't allocated.")
        _lfortran_copy_str_and_pad(*lhs, *lhs_len, rhs, rhs_len);
    } else if (!is_lhs_deferred && is_lhs_allocatable){ // Automatic Allocation
        if (*lhs == NULL) *lhs = (char*)malloc(MAX((*lhs_len), 1) * sizeof(char));
        _lfortran_copy_str_and_pad(*lhs, *lhs_len, rhs, rhs_len);
    } else if (is_lhs_deferred && is_lhs_allocatable) { // Automatic Reallocation
        if (*lhs != NULL && rhs != NULL) {
            char* lhs_start = *lhs;
            char* lhs_end = lhs_start + (*lhs_len);
            if (rhs >= lhs_start && rhs < lhs_end) {
                if (rhs_len <= *lhs_len) {
                    memmove(*lhs, rhs, rhs_len * sizeof(char));
                    *lhs_len = rhs_len;
                    return;
                } else {
                    char* tmp = (char*)malloc(MAX(rhs_len, 1) * sizeof(char));
                    memcpy(tmp, rhs, rhs_len * sizeof(char));
                    *lhs = (char*)realloc(*lhs, MAX(rhs_len, 1) * sizeof(char));
                    *lhs_len = rhs_len;
                    memcpy(*lhs, tmp, rhs_len * sizeof(char));
                    free(tmp);
                    return;
                }
            }
        }
        *lhs = (char*)realloc(*lhs, MAX(rhs_len, 1) * sizeof(char));
        *lhs_len = rhs_len;
        for(int64_t i = 0; i < rhs_len; i++) {(*lhs)[i] = rhs[i];}
    } else if(is_lhs_deferred && !is_lhs_allocatable) {
        lfortran_assert(*lhs != NULL, "Runtime Error : Non-allocatable string isn't allocated.")
        _lfortran_copy_str_and_pad(*lhs, *lhs_len, rhs, rhs_len);
    }
}



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
    num = (num < 0 ? -num : num);
    while (num > 0) {
        num = num >> 1;
        res++;
    }
    return res;
}

LFORTRAN_API int32_t _lpython_bit_length2(int16_t num)
{
    int32_t res = 0;
    num = (num < 0 ? -num : num);
    while (num > 0) {
        num = num >> 1;
        res++;
    }
    return res;
}

LFORTRAN_API int32_t _lpython_bit_length4(int32_t num)
{
    int32_t res = 0;
    num = (num < 0 ? -num : num);
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

    char* dest_char = (char*)malloc(s_len);
    memcpy(dest_char, s, s_len);
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

/* 
    -- Handles Allocation Of Strings --

 * Makes sure to allocate a minimum length of `1`.

 * USE THIS WHEN LENGTH IS RUNTIME
 * Compile-Time should be handled at compile-time + call `malloc` directly
*/
LFORTRAN_API void* _lfortran_string_malloc(int64_t length) {
    if(length < 0) lfortran_error("Allocating string with length < 0");
    return malloc(MAX((size_t)length, (size_t)1));
}

LFORTRAN_API int8_t* _lfortran_realloc(int8_t* ptr, int64_t size) {
    // When size is 0, realloc(ptr, 0) may return NULL (implementation-defined).
    // For Fortran semantics, a size-0 allocatable array is still "allocated",
    // so we must ensure a non-null pointer by allocating at least 1 byte.
    if (size == 0) {
        size = 1;
    }
    return (int8_t*) realloc(ptr, (size_t)size);
}

LFORTRAN_API int8_t* _lfortran_calloc(int32_t count, int32_t size) {
    return (int8_t*) calloc(count, size);
}

LFORTRAN_API void _lfortran_free(char* ptr) {
    free((void*)ptr);
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
    int offset_hours = (offset_minutes < 0 ? -(offset_minutes / 60) : (offset_minutes / 60));
    int remaining_minutes = (offset_minutes < 0 ? -(offset_minutes % 60) : (offset_minutes % 60));
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
    int delim;
    bool blank_zero;  // true for 'blank="zero"', false for 'blank="null"'
    int32_t record_length;
    int sign_mode;  // 0=processor_defined, 1=plus, 2=suppress
};

int32_t last_index_used = -1;

struct UNIT_FILE unit_to_file[MAXUNITS];

// Pre-connect standard Fortran units at program startup.
// The Fortran standard requires INPUT_UNIT, OUTPUT_UNIT, ERROR_UNIT to be
// pre-connected, but their values are processor-dependent. Units 5/6/0 are
// a widely-used convention (gfortran, ifort, etc.) for legacy compatibility.
static bool _lfortran_standard_units_initialized = false;

static void _lfortran_init_standard_units(void) {
    if (_lfortran_standard_units_initialized) return;
    _lfortran_standard_units_initialized = true;
    // Unit 5: stdin (read-only, formatted, sequential)
    unit_to_file[0].unit = 5;
    unit_to_file[0].filename = NULL;
    unit_to_file[0].filep = stdin;
    unit_to_file[0].unit_file_bin = false;
    unit_to_file[0].access_id = 0;  // sequential
    unit_to_file[0].read_access = true;
    unit_to_file[0].write_access = false;
    unit_to_file[0].delim = 0;
    unit_to_file[0].blank_zero = false;
    unit_to_file[0].sign_mode = 0;  // processor_defined

    // Unit 6: stdout (write-only, formatted, sequential)
    unit_to_file[1].unit = 6;
    unit_to_file[1].filename = NULL;
    unit_to_file[1].filep = stdout;
    unit_to_file[1].unit_file_bin = false;
    unit_to_file[1].access_id = 0;  // sequential
    unit_to_file[1].read_access = false;
    unit_to_file[1].write_access = true;
    unit_to_file[1].delim = 0;
    unit_to_file[1].blank_zero = false;
    unit_to_file[1].sign_mode = 0;  // processor_defined

    // Unit 0: stderr (write-only, formatted, sequential)
    unit_to_file[2].unit = 0;
    unit_to_file[2].filename = NULL;
    unit_to_file[2].filep = stderr;
    unit_to_file[2].unit_file_bin = false;
    unit_to_file[2].access_id = 0;  // sequential
    unit_to_file[2].read_access = false;
    unit_to_file[2].write_access = true;
    unit_to_file[2].delim = 0;
    unit_to_file[2].blank_zero = false;
    unit_to_file[2].sign_mode = 0;  // processor_defined

    last_index_used = 2;
}

void store_unit_file(int32_t unit_num, char* filename, FILE* filep, bool unit_file_bin, int access_id, bool read_access, bool write_access, int delim, bool blank_zero, int32_t record_length, int sign_mode) {
    _lfortran_init_standard_units();
    for( int i = 0; i <= last_index_used; i++ ) {
        if( unit_to_file[i].unit == unit_num ) {
            // Update existing entry - only update filename if explicitly provided (not NULL)
            if (filename != NULL) {
                unit_to_file[i].filename = filename;
            }
            unit_to_file[i].filep = filep;
            unit_to_file[i].unit_file_bin = unit_file_bin;
            unit_to_file[i].access_id = access_id;
            unit_to_file[i].read_access = read_access;
            unit_to_file[i].write_access = write_access;
            unit_to_file[i].delim = delim;
            unit_to_file[i].blank_zero = blank_zero;
            unit_to_file[i].record_length = record_length;
            unit_to_file[i].sign_mode = sign_mode;
            return;  // Don't add duplicate entry
        }
    }
    // Add new entry
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
    unit_to_file[last_index_used].delim = delim;
    unit_to_file[last_index_used].blank_zero = blank_zero;
    unit_to_file[last_index_used].record_length = record_length;
    unit_to_file[last_index_used].sign_mode = sign_mode;
}

FILE* get_file_pointer_from_unit(int32_t unit_num, bool *unit_file_bin, int *access_id, bool *read_access, bool *write_access, int *delim, bool *blank_zero, int32_t *recl, int *sign_mode) {
    _lfortran_init_standard_units();
    // Initialize all output params to safe defaults for unconnected units
    if (unit_file_bin) *unit_file_bin = false;
    if (access_id) *access_id = 0;
    if (read_access) *read_access = true;
    if (write_access) *write_access = true;
    if (delim) *delim = 0;
    if (blank_zero) *blank_zero = false;
    if (recl) *recl = 0;
    if (sign_mode) *sign_mode = 0;
    for( int i = 0; i <= last_index_used; i++ ) {
        if( unit_to_file[i].unit == unit_num ) {
            if (unit_file_bin) *unit_file_bin = unit_to_file[i].unit_file_bin;
            if (access_id) *access_id = unit_to_file[i].access_id;
            if (read_access) *read_access = unit_to_file[i].read_access;
            if (write_access) *write_access = unit_to_file[i].write_access;
            if (delim) *delim =  unit_to_file[i].delim;
            if (blank_zero) *blank_zero = unit_to_file[i].blank_zero;
            if (recl) *recl = unit_to_file[i].record_length;
            if (sign_mode) *sign_mode = unit_to_file[i].sign_mode;
            return unit_to_file[i].filep;
        }
    }
    return NULL;
}

char* get_file_name_from_unit(int32_t unit_num, bool *unit_file_bin) {
    _lfortran_init_standard_units();
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
        unit_to_file[i] = unit_to_file[i + 1];
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

/*
    -- Converts (Fortran-style string) INTO (C-style string). --
    Simply adds a null-character at the end.
*/
static char* to_c_string(const fchar* src, int64_t len)
{   
    lfortran_assert(!(len < 0), "Invalid Length")
    char* buf = (char*) malloc(len + 1);
    if (!buf) lfortran_error("Compiler Internal Error : Couldn't allocate memory");
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
               int64_t action_len,
               char* delim,
               int64_t delim_len,
               char* position,
               int64_t position_len,
               char* blank,
               int64_t blank_len,
               char* encoding,
               int64_t encoding_len,
               int32_t* recl,
               char* sign,
               int64_t sign_len)
{
    if (iostat != NULL) {
        *iostat = 0;
    }
    bool ini_encoding = true;
    if (encoding == NULL) {
        encoding = "default";
        encoding_len = 7;
        ini_encoding = false;
    }

    trim_trailing_spaces(&encoding, &encoding_len, ini_encoding);
    char* encoding_c = to_c_string((const fchar*)encoding, encoding_len);

    // DEFAULT and UTF-8 are currently treated the same
    if (!streql(encoding_c, "default") && !streql(encoding_c, "utf-8")) {
        // Unknown encoding → warning
        fprintf(stderr,
            "Warning: ENCODING='%s' is not recognized, treated as DEFAULT\n",
            encoding_c
        );
    }
    bool ini_sign = true;
    if (sign == NULL) {
        sign = "processor_defined";
        sign_len = 17;
        ini_sign = false;
    }
    trim_trailing_spaces(&sign, &sign_len, ini_sign);
    char* sign_c = to_c_string((const fchar*)sign, sign_len);
    
    // Validate sign specifier
    if (!streql(sign_c, "plus") && !streql(sign_c, "suppress") && !streql(sign_c, "processor_defined")) {
        fprintf(stderr,
            "Warning: SIGN='%s' is not recognized, treated as PROCESSOR_DEFINED\n",
            sign_c
        );
    }
    // Note: The actual sign behavior would be handled during formatted output (WRITE/PRINT)
    // For now, we just validate and store the value but don't use it in this function
    
    bool ini_file = true;
    if (f_name == NULL) {  // Not Provided
        char *prefix = "_lfortran_generated_file", *format = "txt";
        char unique_id[ID_LEN + 1];
        get_unique_ID(unique_id);
        int length = ID_LEN + strlen(prefix) + strlen(format) + 3;
        f_name = (char*) malloc(length);
        snprintf(f_name, length, "%s_%s.%s", prefix, unique_id, format);
        f_name_len = strlen(f_name);
        ini_file = false;
    }
    bool ini_status = true;
    if (status == NULL) {
        status = "unknown";
        status_len = 7;
        ini_status = false;
    }
    bool ini_access = true;
    if (access == NULL) {
        access = "sequential";
        access_len = 10;
        ini_access = false;
    }
    trim_trailing_spaces(&access, &access_len, ini_access);
    char* access_c = to_c_string((const fchar*)access, access_len);
    bool ini_form = true;
    if (form == NULL) {
        bool is_stream_or_direct = false;
        if (access != NULL) {
            char* access_c_temp = (char*)malloc(access_len + 1);
            if (access_c_temp) {
                memcpy(access_c_temp, access, access_len);
                access_c_temp[access_len] = '\0';                
                if (streql(access_c_temp, "stream") || streql(access_c_temp, "direct")) {
                    is_stream_or_direct = true;
                }
                free(access_c_temp);
            }
        }
        if (is_stream_or_direct) {
            form = "unformatted";
            form_len = 11;
        } else {
            form = "formatted";
            form_len = 9;
        }
        ini_form = false;
    }
    bool ini_action = true;
    if (action == NULL) {
        action = "readwrite";
        action_len = 9;
        ini_action = false;
    }
    bool ini_delim = true;
    if (delim == NULL) {
        delim = "none";
        delim_len = 4;
        ini_delim = false;
    }
    bool ini_blank = true;
    if (blank == NULL) {
        blank = "null";
        blank_len = 4;
        ini_blank = false;
    }
    bool file_exists[1] = { false };
    FILE* already_open = get_file_pointer_from_unit(unit_num, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

    trim_trailing_spaces(&f_name, &f_name_len, ini_file);
    trim_trailing_spaces(&status, &status_len, ini_status);
    trim_trailing_spaces(&form, &form_len, ini_form);
    trim_trailing_spaces(&action, &action_len, ini_action);
    trim_trailing_spaces(&delim, &delim_len, ini_delim);
    trim_trailing_spaces(&blank, &blank_len, ini_blank);

    // Prepare null-terminated names for C APIs
    char* f_name_c = to_c_string((const fchar*)f_name, f_name_len);
    char* status_c = to_c_string((const fchar*)status, status_len);
    char* form_c = to_c_string((const fchar*)form, form_len);
    char* action_c = to_c_string((const fchar*)action, action_len);
    char* delim_c = to_c_string((const fchar*)delim, delim_len);
    char* blank_c = to_c_string((const fchar*)blank, blank_len);

    _lfortran_inquire(
        (const fchar*)f_name, f_name_len, file_exists, -1, NULL, NULL, NULL,
        NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL,
        NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, NULL);
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
    if (streql(status_c, "old")) {
        if (!*file_exists) {
            if (iostat != NULL) {
                *iostat = 2;
                if ((iomsg != NULL) && (iomsg_len > 0)) {
                    char* temp
                        = "File `%s` does not exists! Cannot open a file with the `status=old`";
                    snprintf(iomsg, iomsg_len + 1, temp, f_name_c);
                    pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
                }
            } else {
                printf("Runtime error: File `%s` does not exists!\nCannot open a "
                       "file with the `status=old`\n",
                       f_name_c);
                exit(1);
            }
        }
        access_mode = "r+";
    } else if (streql(status_c, "new")) {
        if (*file_exists) {
            if (iostat != NULL) {
                *iostat = 17;
                if ((iomsg != NULL) && (iomsg_len > 0)) {
                    char* temp = "File `%s` exists! Cannot open a file with the `status=new`";
                    snprintf(iomsg, iomsg_len + 1, temp, f_name_c);
                    pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
                }
            } else {
                printf("Runtime error: File `%s` exists!\nCannot open a file with "
                       "the `status=new`\n",
                       f_name_c);
                exit(1);
            }
        }
        access_mode = "w+";
    } else if (streql(status_c, "replace") || streql(status_c, "scratch")) {
        access_mode = "w+";
    } else if (streql(status_c, "unknown")) {
        if (!*file_exists && !already_open) {
            FILE* fd = fopen(f_name_c, "w");
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
                   status_c);
            exit(1);
        }
    }

    bool unit_file_bin;
    int access_id;
    bool read_access = true;
    bool write_access = true;
    bool blank_zero = false;
    int delim_value = 0;  // 0: default, 1: apostrophe, 2: quote
    if (streql(form_c, "formatted")) {
        unit_file_bin = false;
    } else if (streql(form_c, "unformatted")) {
        unit_file_bin = true;
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "FORM specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: FORM specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   form_c);
            exit(1);
        }
    }

    if (streql(access_c, "stream")) {
        access_id = 1;
    } else if (streql(access_c, "sequential")) {
        access_id = 0;
    } else if (streql(access_c,
                      "direct")) {  // TODO: Handle 'direct' as access while reading or writing
        access_id = 2;
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "ACCESS specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: ACCESS specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   access_c);
            exit(1);
        }
    }
    if (streql(action_c, "readwrite")) {
    } else if (streql(action_c, "write")) {
        read_access = false;
    } else if (streql(action_c, "read")) {
        write_access = false;
    } else {
        if (iostat != NULL) {
            *iostat = 5002;
            if ((iomsg != NULL) && (iomsg_len > 0)) {
                char* temp = "ACTION specifier in OPEN statement has invalid value.";
                snprintf(iomsg, iomsg_len, "%s", temp);
                pad_with_spaces(iomsg, strlen(iomsg), iomsg_len);
            }
        } else {
            printf("Runtime error: ACTION specifier in OPEN statement has "
                   "invalid value '%s'\n",
                   action_c);
            exit(1);
        }
    }

    if (streql(delim_c, "none")) {
    } else if (streql(delim_c, "apostrophe")) {
        delim_value = 1;
    } else if (streql(delim_c, "quote")) {
        delim_value = 2;
    } else {
        // TODO: Throw error
    }

    if (streql(blank_c, "zero")) {
        blank_zero = true;
    }

    // Determine sign mode: 0=processor_defined, 1=plus, 2=suppress
    int sign_mode = 0;  // default
    if (streql(sign_c, "plus")) {
        sign_mode = 1;
    } else if (streql(sign_c, "suppress")) {
        sign_mode = 2;
    }

    if (access_mode == NULL
        && iostat != NULL) {  // Case: when iostat is present we don't want to terminate
        access_mode = "r";
    }
    int32_t record_length = 0;
    if (access_id == 2 && recl != NULL) {
        record_length = *recl;
    }
    if (iostat == NULL || (*iostat == 0)) {
        if (already_open) {
            // Unit is already open, but we still need to update its properties like sign_mode
            // Pass NULL for filename to preserve the existing filename
            store_unit_file(unit_num, NULL, already_open, unit_file_bin, access_id, read_access, write_access, delim_value, blank_zero, record_length, sign_mode);
            return (int64_t) already_open;
        }
        FILE* fd = fopen(f_name_c, access_mode);
        if (!fd && iostat == NULL) {
            printf("Runtime error: Error in opening the file!\n");
            perror(f_name_c);
            exit(1);
        }
        // Handle position='append': seek to end of file
        if (fd && position != NULL && position_len > 0) {
            char* position_c = to_c_string((const fchar*)position, position_len);
            if (streql(position_c, "append")) {
                fseek(fd, 0, SEEK_END);
            }
            free(position_c);
        }
        store_unit_file(unit_num, f_name_c, fd, unit_file_bin, access_id, read_access, write_access, delim_value, blank_zero, record_length, sign_mode);
        return (int64_t) fd;
    }
    free(f_name_c);
    free(status_c);
    free(form_c);
    free(access_c);
    free(action_c);
    free(encoding_c);
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
        FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
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

LFORTRAN_API void _lfortran_abort()
{
    abort();
}

LFORTRAN_API void _lfortran_inquire(const fchar* f_name_data, int64_t f_name_len, bool *exists, int32_t unit_num,
                                    bool *opened, int32_t *size, int32_t *pos,
                                    char *write, int64_t write_len,
                                    char *read, int64_t read_len,
                                    char *readwrite, int64_t readwrite_len,
                                    char *access, int64_t access_len,
                                    char *name, int64_t name_len,
                                    char *blank, int64_t blank_len,
                                    int32_t *recl, int32_t *number, bool *named,
                                    char* sequential, int64_t sequential_len,
                                    char* direct, int64_t direct_len,
                                    char* form, int64_t form_len,
                                    char* formatted, int64_t formatted_len,
                                    char* unformatted, int64_t unformatted_len,
                                    int32_t *iostat, int32_t *nextrec) {
    if (f_name_data && unit_num != -1) {
        printf("File name and file unit number cannot be specified together.\n");
        exit(1);
    }
    if (f_name_data != NULL) {
        bool unit_file_bin;
        int access_id = -1;
        bool read_access;
        bool write_access;
        int delim;
        bool blank_zero;
        int32_t unit_recl = 0;
        char *c_f_name_data = to_c_string(f_name_data, f_name_len);
        if (named != NULL) {
            *named = true;
        }
        if (name != NULL) {
            _lfortran_copy_str_and_pad(name, name_len, c_f_name_data, strlen(c_f_name_data));
        }
        FILE *fp = fopen(c_f_name_data, "r");

        if (fp != NULL) {
            *exists = true;
            if (size != NULL) {
                fseek(fp, 0, SEEK_END);
                *size = ftell(fp);
            }
            fclose(fp); // close the file
        } else {
            *exists = false;
        }
        int u_num = -1;
        for(int i=0; i<1000; i++) {
            char* unit_name = get_file_name_from_unit(i, &unit_file_bin);
            if (unit_name != NULL && strcmp(unit_name, c_f_name_data) == 0) {
                u_num = i;
                break;
            }
        }
        free(c_f_name_data);

        if (number != NULL) {
            *number = u_num;
        }
        
        fp = get_file_pointer_from_unit(u_num, &unit_file_bin, &access_id, &read_access, &write_access, &delim, &blank_zero, &unit_recl, NULL);
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
        if (access != NULL) {
            char *access_str = "";
            if (access_id == 0) {
                access_str = "SEQUENTIAL";
            } else if (access_id == 1) {
                access_str = "STREAM";
            } else if (access_id == 2) {
                access_str = "DIRECT";
            }
            _lfortran_copy_str_and_pad(access, access_len, access_str, strlen(access_str));
        }
        if (blank != NULL) {
            // For formatted files only
            if (unit_file_bin) {
                _lfortran_copy_str_and_pad(blank, blank_len, "UNDEFINED", 9);
            } else {
                if (blank_zero) {
                    _lfortran_copy_str_and_pad(blank, blank_len, "ZERO", 4);
                } else {
                    _lfortran_copy_str_and_pad(blank, blank_len, "NULL", 4);
                }
            }
        }
        if (opened != NULL) {
            *opened = (fp != NULL);
        }
        if (pos != NULL && fp != NULL) {
            long p = ftell(fp);
            *pos = (int32_t)p + 1;
        }
        if (size != NULL && fp != NULL) {
            long current_pos = ftell(fp);
            fseek(fp, 0, SEEK_END);
            *size = ftell(fp);
            fseek(fp, current_pos, SEEK_SET);
        }
        if (recl != NULL && access_id == 2) {
            *recl = unit_recl;
        }
        if (sequential != NULL) {
            if (access_id == 0) {
                _lfortran_copy_str_and_pad(sequential, sequential_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(sequential, sequential_len, "NO", 2);
            }
        }
        if (direct != NULL) {
            if (access_id == 2) {
                _lfortran_copy_str_and_pad(direct, direct_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(direct, direct_len, "NO", 2);
            }
        }
        if (form != NULL) {
            if (unit_file_bin) {
                _lfortran_copy_str_and_pad(form, form_len, "UNFORMATTED", 11);
            } else {
                _lfortran_copy_str_and_pad(form, form_len, "FORMATTED", 9);
            }
        }
        if (formatted != NULL) {
            if (!unit_file_bin) {
                _lfortran_copy_str_and_pad(formatted, formatted_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(formatted, formatted_len, "NO", 2);
            }
        }
        if (unformatted != NULL) {
            if (unit_file_bin) {
                _lfortran_copy_str_and_pad(unformatted, unformatted_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(unformatted, unformatted_len, "NO", 2);
            }
        }
        if (nextrec != NULL && access_id == 2 && fp != NULL) {
            long current_pos = ftell(fp);
            *nextrec = (int32_t)(current_pos / unit_recl) + 1;
        }
        if (iostat != NULL) {
            *iostat = 0;
        }
    }
    if (unit_num != -1) {
        bool unit_file_bin;
        int access_id = -1;
        bool read_access;
        bool write_access;
        int delim;
        bool blank_zero;
        int32_t unit_recl = 0;
        FILE *fp = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, &delim, &blank_zero, &unit_recl, NULL);
        if (get_file_name_from_unit(unit_num, &unit_file_bin) != NULL) {
            *exists = true;
        } else {
            *exists = false;
        }
        if (number != NULL) {
            *number = unit_num;
        }
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
        if (access != NULL) {
            char *access_str = "";
            if (access_id == 0) {
                access_str = "SEQUENTIAL";
            } else if (access_id == 1) {
                access_str = "STREAM";
            } else if (access_id == 2) {
                access_str = "DIRECT";
            }
            _lfortran_copy_str_and_pad(access, access_len, access_str, strlen(access_str));
        }
        if (name != NULL) {
            bool dummy_unit_file_bin;
            char *unit_name = get_file_name_from_unit(unit_num, &dummy_unit_file_bin);
            if (unit_name != NULL) {
                _lfortran_copy_str_and_pad(name, name_len, unit_name, strlen(unit_name));
            } else {
                _lfortran_copy_str_and_pad(name, name_len, "", 0);
            }
            if (named != NULL) {
                *named = (unit_name != NULL);
            }
        }
        if (blank != NULL) {
            // For formatted files only
            if (unit_file_bin) {
                _lfortran_copy_str_and_pad(blank, blank_len, "UNDEFINED", 9);
            } else {
                if (blank_zero) {
                    _lfortran_copy_str_and_pad(blank, blank_len, "ZERO", 4);
                } else {
                    _lfortran_copy_str_and_pad(blank, blank_len, "NULL", 4);
                }
            }
        }
        if (opened != NULL) {
            *opened = (fp != NULL);
        }
        if (pos != NULL && fp != NULL) {
            long p = ftell(fp);
            *pos = (int32_t)p + 1;
        }
        if (size != NULL && fp != NULL) {
            long current_pos = ftell(fp);
            fseek(fp, 0, SEEK_END);
            *size = ftell(fp);
            fseek(fp, current_pos, SEEK_SET);
        }
        if (recl != NULL && access_id == 2) {
            *recl = unit_recl;
        }
        if (sequential != NULL) {
            if (access_id == 0) {
                _lfortran_copy_str_and_pad(sequential, sequential_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(sequential, sequential_len, "NO", 2);
            }
        }
        if (direct != NULL) {
            if (access_id == 2) {
                _lfortran_copy_str_and_pad(direct, direct_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(direct, direct_len, "NO", 2);
            }
        }
        if (form != NULL) {
            if (unit_file_bin) {
                _lfortran_copy_str_and_pad(form, form_len, "UNFORMATTED", 11);
            } else {
                _lfortran_copy_str_and_pad(form, form_len, "FORMATTED", 9);
            }
        }
        if (formatted != NULL) {
            if (!unit_file_bin) {
                _lfortran_copy_str_and_pad(formatted, formatted_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(formatted, formatted_len, "NO", 2);
            }
        }
        if (unformatted != NULL) {
            if (unit_file_bin) {
                _lfortran_copy_str_and_pad(unformatted, unformatted_len, "YES", 3);
            } else {
                _lfortran_copy_str_and_pad(unformatted, unformatted_len, "NO", 2);
            }
        }
        if (nextrec != NULL && access_id == 2 && fp != NULL) {
            long current_pos = ftell(fp);
            *nextrec = (int32_t)(current_pos / unit_recl) + 1;
        }
        if (iostat != NULL) {
            *iostat = 0;
        }
    }
}

LFORTRAN_API void _lfortran_rewind(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if( filep == NULL ) {
        printf("Specified UNIT %d in REWIND is not created or connected.\n", unit_num);
        exit(1);
    }
    rewind(filep);
}

LFORTRAN_API void _lfortran_backspace(int32_t unit_num)
{
    bool unit_file_bin;
    FILE* fd = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
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

LFORTRAN_API void _lfortran_seek_record(int32_t unit_num, int32_t rec, int32_t *iostat)
{
    if (iostat != NULL) {
        *iostat = 0;
    }

    bool unit_file_bin;
    int access_id = -1;
    bool read_access = false, write_access = false;
    int delim = 0;
    bool blank_zero = false;
    int32_t unit_recl = 0;

    FILE* fp = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, 
                                          &read_access, &write_access, &delim, 
                                          &blank_zero, &unit_recl, NULL);
    if (fp == NULL) {
        if (iostat != NULL) {
            *iostat = 5001; // unit not connected
            return;
        } else {
            fprintf(stderr, "Runtime Error: Specified UNIT %d is not created or connected.\n", unit_num);
            exit(1);
        }
    }

    if (access_id != 2) {  // 2 = DIRECT access
        if (iostat != NULL) {
            *iostat = 5002; // not a direct access file
            return;
        } else {
            fprintf(stderr, "Runtime Error: REC= specified for non-direct access UNIT %d.\n", unit_num);
            exit(1);
        }
    }

    if (unit_recl <= 0) {
        if (iostat != NULL) {
            *iostat = 5003; // RECL not set or invalid
            return;
        } else {
            fprintf(stderr, "Runtime Error: RECL not set or invalid for UNIT %d.\n", unit_num);
            exit(1);
        }
    }

    if (rec <= 0) {
        if (iostat != NULL) {
            *iostat = 5004; // invalid record number
            return;
        } else {
            fprintf(stderr, "Runtime Error: Invalid REC value %lld for UNIT %d (must be > 0).\n", 
                    (long long)rec, unit_num);
            exit(1);
        }
    }
    long long offset_ll = (long long)(rec - 1) * (long long)unit_recl;


    // Seek to the record position
#if defined(_WIN32)
    int seek_result = fseek(fp, (long)offset_ll, SEEK_SET);
#else
    int seek_result = fseeko(fp, (off_t)offset_ll, SEEK_SET);
#endif

    if (seek_result != 0) {
        if (iostat != NULL) {
            if (ferror(fp)) {
                *iostat = 5005; // seek error
            } else if (feof(fp)) {
                *iostat = 0;
                return;
            } else {
                *iostat = 5006; // unknown seek error
            }
            return;
        } else {
            int err = errno;
            if (err != 0) {
                fprintf(stderr, "Runtime Error: Cannot seek to record %lld on UNIT %d: %s\n",
                        (long long)rec, unit_num, strerror(err));
            } else {
                fprintf(stderr, "Runtime Error: Cannot seek to record %lld on UNIT %d.\n",
                        (long long)rec, unit_num);
            }
            exit(1);
        }
    }
}

static bool read_next_nonblank_stdin_line(char *buffer, size_t bufsize, int32_t *iostat)
{
    while (true) {
        if (!fgets(buffer, bufsize, stdin)) {
            if (iostat) {
                *iostat = -1;
                return false;
            }
            fprintf(stderr, "Error: Failed to read input.\n");
            exit(1);
        }

        bool nonblank = false;
        for (size_t i = 0; buffer[i] != '\0'; i++) {
            if (!isspace((unsigned char)buffer[i])) {
                nonblank = true;
                break;
            }
        }

        if (nonblank) {
            return true;
        }
    }
}

LFORTRAN_API void _lfortran_read_int16(int16_t *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;
    if (unit_num == -1) {
        char buffer[100];
        if (!read_next_nonblank_stdin_line(buffer, sizeof(buffer), iostat)) {
            return;
        }

        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for int16_t.\n");
            exit(1);
        }

        char *endptr = NULL;
        errno = 0;
        long long_val = strtol(token, &endptr, 10);

        if (endptr == token || *endptr != '\0') {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for int16_t.\n");
            exit(1);
        }

        if (errno == ERANGE || long_val < INT16_MIN || long_val > INT16_MAX) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Value %ld is out of integer(2) range.\n", long_val);
            exit(1);
        }

        *p = (int16_t)long_val;
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read int16_t from binary file.\n");
            exit(1);
        }
    } else {
        long temp;
        if (fscanf(filep, "%ld", &temp) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Invalid input for int16_t from file.\n");
            exit(1);
        }

        if (temp < INT16_MIN || temp > INT16_MAX) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Value %ld is out of integer(2) range (file).\n", temp);
            exit(1);
        }

        *p = (int16_t)temp;
    }
}

// Improved input validation for integer reading
// - Prevents auto-casting of invalid inputs to integers
LFORTRAN_API void _lfortran_read_int32(int32_t *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;
    if (unit_num == -1) {
        char buffer[100];
        if (!read_next_nonblank_stdin_line(buffer, sizeof(buffer), iostat)) {
            return;
        }

        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for int32_t.\n");
            exit(1);
        }

        char *endptr = NULL;
        errno = 0;
        long long_val = strtol(token, &endptr, 10);

        if (endptr == token || *endptr != '\0') {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for int32_t.\n");
            exit(1);
        }

        if (errno == ERANGE || long_val < INT32_MIN || long_val > INT32_MAX) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Value %ld is out of integer(4) range.\n", long_val);
            exit(1);
        }

        *p = (int32_t)long_val;
        return;
    }

    bool unit_file_bin;
    int access_mode;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_mode, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        fprintf(stderr, "Internal Compiler Error: No file found with given unit number %d.\n", unit_num);
        exit(1);
    }

    if (unit_file_bin) {
        if (access_mode == 0) {
            int32_t record_start = 0, record_end = 0;
            if (fread(&record_start, sizeof(int32_t), 1, filep) != 1 ||
                fread(p, sizeof(int32_t), 1, filep) != 1 ||
                fread(&record_end, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Internal Compiler Error: Failed to read int32_t from sequential binary file.\n");
                exit(1);
            }
            if (record_start != (int32_t)sizeof(int32_t) || record_end != (int32_t)sizeof(int32_t)) {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Internal Compiler Error: Invalid record marker while reading int32_t.\n");
                exit(1);
            }
        } else {
            if (fread(p, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Internal Compiler Error: Failed to read int32_t from stream file.\n");
                exit(1);
            }
        }
    } else {
        long temp;
        if (fscanf(filep, "%ld", &temp) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Invalid input for int32_t from file.\n");
            exit(1);
        }

        if (temp < INT32_MIN || temp > INT32_MAX) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Value %ld is out of integer(4) range (file).\n", temp);
            exit(1);
        }

        *p = (int32_t)temp;
    }
}

LFORTRAN_API void _lfortran_read_int64(int64_t *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;
    if (unit_num == -1) {
        char buffer[100];
        if (!read_next_nonblank_stdin_line(buffer, sizeof(buffer), iostat)) {
            return;
        }

        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for int64_t.\n");
            exit(1);
        }

        errno = 0;
        char *endptr = NULL;
        long long long_val = strtoll(token, &endptr, 10);

        if (endptr == token || *endptr != '\0') {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for int64_t.\n");
            exit(1);
        }

        if (errno == ERANGE || long_val < INT64_MIN || long_val > INT64_MAX) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Value %lld is out of integer(8) range.\n", long_val);
            exit(1);
        }

        *p = (int64_t)long_val;
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read int64_t from binary file.\n");
            exit(1);
        }
    } else {
        int64_t temp;
        if (fscanf(filep, "%" PRId64, &temp) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Invalid input for int64_t from file.\n");
            exit(1);
        }
        if (temp < INT64_MIN || temp > INT64_MAX) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Value %" PRId64 " is out of integer(8) range (file).\n", temp);
            exit(1);
        }

        *p = (int64_t)temp;
    }
}

// Logical read API
LFORTRAN_API void _lfortran_read_logical(bool *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        char buffer[100];
        if (!read_next_nonblank_stdin_line(buffer, sizeof(buffer), iostat)) {
            return;
        }

        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for logical.\n");
            exit(1);
        }

        for (int i = 0; token[i]; ++i) token[i] = tolower((unsigned char) token[i]);

        if (strcmp(token, "t") == 0 || strcmp(token, "true") == 0 ||
            strcmp(token, ".true.") == 0 || strcmp(token, ".true") == 0) {
            *p = true;
        } else if (strcmp(token, "f") == 0 || strcmp(token, "false") == 0 ||
                   strcmp(token, ".false.") == 0 || strcmp(token, ".false") == 0) {
            *p = false;
        } else {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid logical input '%s'. Use T, F, .true., .false., true, false\n", token);
            exit(1);
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id == 0) {
            int32_t temp = 0;
            int32_t record_start = 0, record_end = 0;
            if (fread(&record_start, sizeof(int32_t), 1, filep) != 1 ||
                fread(&temp, sizeof(int32_t), 1, filep) != 1 ||
                fread(&record_end, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read logical from sequential binary file.\n");
                exit(1);
            }
            if (record_start != (int32_t)sizeof(int32_t) || record_end != (int32_t)sizeof(int32_t)) {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Error: Invalid record marker while reading logical.\n");
                exit(1);
            }
            *p = (temp != 0);
        } else {
            int32_t temp = 0;
            if (fread(&temp, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read logical from binary file.\n");
                exit(1);
            }
            *p = (temp != 0);
        }
    } else {
        char token[100] = {0};
        if (fscanf(filep, "%99s", token) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Invalid logical input from file.\n");
            exit(1);
        }

        int len = strlen(token);
        while (len > 0 && (token[len-1] == '\r' || token[len-1] == '\n')) {
            token[len-1] = '\0';
            len--;
        }

        for (int i = 0; token[i]; ++i) {
            token[i] = tolower((unsigned char) token[i]);
        }

        if (strcmp(token, "t") == 0 || strcmp(token, "true") == 0 ||
            strcmp(token, ".true.") == 0 || strcmp(token, ".true") == 0) {
            *p = true;
        } else if (strcmp(token, "f") == 0 || strcmp(token, "false") == 0 ||
                   strcmp(token, ".false.") == 0 || strcmp(token, ".false") == 0) {
            *p = false;
        } else {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid logical input '%s'. Use T, F, .true., .false., true, false\n", token);
            exit(1);
        }
    }
}


LFORTRAN_API void _lfortran_read_array_int8(int8_t *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%" SCNd8, &p[i]) != 1) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int8_t from stdin.\n");
                exit(1);
            }
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id == 0) {
            int32_t record_marker_start;
            if (fread(&record_marker_start, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read record marker.\n");
                exit(1);
            }
        }
        if (fread(p, sizeof(int8_t), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read int8_t array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            if (fscanf(filep, "%" SCNd8, &p[i]) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int8_t from file.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_logical(bool *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            _lfortran_read_logical(&p[i], unit_num, iostat);
            if (iostat && *iostat != 0) {
                return;
            }
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id == 0) {
            int32_t record_marker_start;
            if (fread(&record_marker_start, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read record marker.\n");
                exit(1);
            }
        }
        // Each logical element is stored as int32_t (4 bytes) in binary files
        for (int i = 0; i < array_size; i++) {
            int32_t temp = 0;
            if (fread(&temp, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read logical array from binary file.\n");
                exit(1);
            }
            p[i] = (temp != 0);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            _lfortran_read_logical(&p[i], unit_num, iostat);
            if (iostat && *iostat != 0) {
                return;
            }
        }
    }
}


LFORTRAN_API void _lfortran_read_array_int16(int16_t *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%hd", &p[i]) != 1) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int16_t from stdin.\n");
                exit(1);
            }
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id == 0) {
            int32_t record_marker_start;
            if (fread(&record_marker_start, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read record marker.\n");
                exit(1);
            }
        }
        if (fread(p, sizeof(int16_t), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read int16_t array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            if (fscanf(filep, "%hd", &p[i]) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int16_t from file.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_int32(int32_t *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%d", &p[i]) != 1) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int32_t from stdin.\n");
                exit(1);
            }
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id == 0) {
            int32_t record_marker_start;
            if (fread(&record_marker_start, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read record marker.\n");
                exit(1);
            }
        }
        if (fread(p, sizeof(int32_t), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read int32_t array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            if (fscanf(filep, "%d", &p[i]) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int32_t from file.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_int64(int64_t *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%" SCNd64, &p[i]) != 1) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int64_t from stdin.\n");
                exit(1);
            }
        }
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id == 0) {
            int32_t record_marker_start;
            if (fread(&record_marker_start, sizeof(int32_t), 1, filep) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read record marker.\n");
                exit(1);
            }
        }
        if (fread(p, sizeof(int64_t), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read int64_t array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            if (fscanf(filep, "%" SCNd64, &p[i]) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read int64_t from file.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_char(char **p, int64_t p_len, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        if (!fgets(*p, p_len + 1, stdin)) {
            if (iostat) { *iostat = -1; return; }
            printf("Runtime error: End of file!\n");
            exit(1);
        }
        size_t len = strcspn(*p, "\n");
        pad_with_spaces(*p, len, p_len);
        return;
    }

    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    int delim_value;
    FILE *filep = get_file_pointer_from_unit(unit_num, &unit_file_bin,
                                             &access_id, &read_access, &write_access, &delim_value, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (access_id == 2 || access_id == 1) {
            int32_t data_length = (int32_t)p_len;
            if (fread(*p, sizeof(char), data_length, filep) != (size_t)data_length) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                printf("Error reading data from file.\n");
                exit(1);
            }
            pad_with_spaces(*p, data_length, p_len);
        } else {
            int32_t data_length = 0;

            if (ftell(filep) == 0) {
                if (fread(&data_length, sizeof(int32_t), 1, filep) != 1) {
                    if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                    printf("Error reading data length from file.\n");
                    exit(1);
                }
            }

            long current_pos = ftell(filep);
            fseek(filep, 0L, SEEK_END);
            long end_pos = ftell(filep);
            fseek(filep, current_pos, SEEK_SET);

            data_length = (int32_t)(end_pos - current_pos - 4);

            if (data_length > p_len) data_length = (int32_t)p_len;

            if (fread(*p, sizeof(char), data_length, filep) != (size_t)data_length) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                printf("Error reading data from file.\n");
                exit(1);
            }

            pad_with_spaces(*p, data_length, p_len);
        }

    } else {
        char *tmp_buffer = (char *)malloc((p_len + 1) * sizeof(char));
        if (!tmp_buffer) {
            if (iostat) { *iostat = 1; return; }
            printf("Memory allocation failed\n");
            exit(1);
        }
        char c;
        size_t len = 0;
        if (delim_value == 1) {
            c = fgetc(filep);
            if (c == EOF) {
                free(tmp_buffer);
                if (iostat) { *iostat = -1; return; }
                printf("Runtime error: End of file!\n");
                exit(1);
            }
            while ((c = fgetc(filep)) != EOF && c != '\'') {
                if (len < (size_t)p_len) tmp_buffer[len++] = (char)c;
            }
            if (c == EOF) {
                free(tmp_buffer);
                if (iostat) { *iostat = -1; return; }
                printf("Runtime error: End of file!\n");
                exit(1);
            }
        } else if (delim_value == 2) {
            c = fgetc(filep);
            if (c == EOF) {
                free(tmp_buffer);
                if (iostat) { *iostat = -1; return; }
                printf("Runtime error: End of file!\n");
                exit(1);
            }
            while ((c = fgetc(filep)) != EOF && c != '"') {
                if (len < (size_t)p_len) tmp_buffer[len++] = (char)c;
            }
            if (c == EOF) {
                free(tmp_buffer);
                if (iostat) { *iostat = -1; return; }
                printf("Runtime error: End of file!\n");
                exit(1);
            }
        } else {
            if (fscanf(filep, "%s", tmp_buffer) != 1) {
                free(tmp_buffer);
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                printf("Runtime error: End of file!\n");
                exit(1);
            }
            len = strlen(tmp_buffer);
        }

        memcpy(*p, tmp_buffer, len);
        pad_with_spaces(*p, len, p_len);
        free(tmp_buffer);
    }
}


// Helper to convert Fortran D exponent notation to E for C parsing
// Modifies buffer in-place, replacing D/d with E
static void convert_fortran_d_exponent(char* buffer) {
    for (int i = 0; buffer[i] != '\0'; i++) {
        if (buffer[i] == 'D' || buffer[i] == 'd') {
            buffer[i] = 'E';
        }
    }
}

// Helper to parse float with D exponent support and error checking
// Returns 1 on success, 0 on failure
static int parse_fortran_float(const char* buffer, float* result) {
    char temp[100];
    strncpy(temp, buffer, 99);
    temp[99] = '\0';
    convert_fortran_d_exponent(temp);
    char* endptr;
    *result = strtof(temp, &endptr);
    // Check if conversion consumed any characters and reached end or whitespace
    return (endptr != temp && (*endptr == '\0' || isspace((unsigned char)*endptr)));
}

// Helper to parse double with D exponent support and error checking
// Returns 1 on success, 0 on failure
static int parse_fortran_double(const char* buffer, double* result) {
    char temp[100];
    strncpy(temp, buffer, 99);
    temp[99] = '\0';
    convert_fortran_d_exponent(temp);
    char* endptr;
    *result = strtod(temp, &endptr);
    // Check if conversion consumed any characters and reached end or whitespace
    return (endptr != temp && (*endptr == '\0' || isspace((unsigned char)*endptr)));
}

// Read a complete complex number expression from file, handling whitespace
// within parentheses. Fortran list-directed format allows arbitrary whitespace
// inside (real, imag) format, e.g., "( 0.1000E+01, 0.2000E+01)".
// Returns 1 on success, 0 on failure (EOF or error).
static int read_complex_expr(FILE *filep, char *buffer, size_t bufsize) {
    int ch;
    size_t i = 0;

    // Skip leading whitespace
    while ((ch = fgetc(filep)) != EOF && isspace(ch));

    if (ch == EOF) return 0;

    if (ch == '(') {
        // Read the entire parenthesized expression
        buffer[i++] = (char)ch;
        while (i < bufsize - 1 && (ch = fgetc(filep)) != EOF) {
            buffer[i++] = (char)ch;
            if (ch == ')') break;
        }
        buffer[i] = '\0';
        return (ch == ')') ? 1 : 0;
    } else {
        // Not a parenthesized expression, read as whitespace-delimited token
        buffer[i++] = (char)ch;
        while (i < bufsize - 1 && (ch = fgetc(filep)) != EOF && !isspace(ch)) {
            buffer[i++] = (char)ch;
        }
        buffer[i] = '\0';
        // Push back the whitespace character if we read one
        if (ch != EOF && isspace(ch)) {
            ungetc(ch, filep);
        }
        return 1;
    }
}

// Improved input validation for float reading
LFORTRAN_API void _lfortran_read_float(float *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        char buffer[100];
        if (!read_next_nonblank_stdin_line(buffer, sizeof(buffer), iostat)) {
            return;
        }

        convert_fortran_d_exponent(buffer);
        char *token = strtok(buffer, " \t\n");
        if (token == NULL) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for float.\n");
            exit(1);
        }

        char *endptr;
        *p = strtof(token, &endptr);

        if (*endptr != '\0') {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input for float.\n");
            exit(1);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read float from binary file.\n");
            exit(1);
        }
    } else {
        // Read as string first to handle Fortran D exponent notation
        char buffer[100];
        if (fscanf(filep, "%99s", buffer) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Invalid input for float from file.\n");
            exit(1);
        }
        if (!parse_fortran_float(buffer, p)) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input from file.\n");
            exit(1);
        }
    }
}

LFORTRAN_API void _lfortran_read_complex_float(struct _lfortran_complex_32 *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        char buf_re[100], buf_im[100];
        if (scanf("%99s %99s", buf_re, buf_im) != 2) {
            if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read complex float from stdin.\n");
            exit(1);
        }
        convert_fortran_d_exponent(buf_re);
        convert_fortran_d_exponent(buf_im);
        p->re = strtof(buf_re, NULL);
        p->im = strtof(buf_im, NULL);
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(struct _lfortran_complex_32), 1, filep) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read complex float from binary file.\n");
            exit(1);
        }
    } else {
        char buffer[200];
        if (!read_complex_expr(filep, buffer, sizeof(buffer))) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Invalid input for complex float from file.\n");
            exit(1);
        }
        convert_fortran_d_exponent(buffer);
        char *start = strchr(buffer, '(');
        char *end = strchr(buffer, ')');
        if (start && end && end > start) {
            *end = '\0';
            start++;
            char *comma = strchr(start, ',');
            if (comma) {
                *comma = '\0';
                while (isspace((unsigned char)*start)) start++;
                char *endptr_re;
                p->re = strtof(start, &endptr_re);
                while (isspace((unsigned char)*endptr_re)) endptr_re++;
                char *im_start = comma + 1;
                while (isspace((unsigned char)*im_start)) im_start++;
                char *endptr_im;
                p->im = strtof(im_start, &endptr_im);
            } else {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Error: Invalid complex float format '%s'.\n", buffer);
                exit(1);
            }
        } else {
            // No parentheses: treat as two whitespace-separated numbers
            p->re = strtof(buffer, NULL);
            if (fscanf(filep, "%f", &p->im) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read imaginary part of complex float.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_complex_double(struct _lfortran_complex_64 *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        char buf_re[100], buf_im[100];
        if (scanf("%99s %99s", buf_re, buf_im) != 2) {
            if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read complex double from stdin.\n");
            exit(1);
        }
        convert_fortran_d_exponent(buf_re);
        convert_fortran_d_exponent(buf_im);
        p->re = strtod(buf_re, NULL);
        p->im = strtod(buf_im, NULL);
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(struct _lfortran_complex_64), 1, filep) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read complex double from binary file.\n");
            exit(1);
        }
    } else {
        char buffer[200];
        if (!read_complex_expr(filep, buffer, sizeof(buffer))) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Invalid input for complex double from file.\n");
            exit(1);
        }
        convert_fortran_d_exponent(buffer);
        char *start = strchr(buffer, '(');
        char *end = strchr(buffer, ')');
        if (start && end && end > start) {
            *end = '\0';
            start++;
            char *comma = strchr(start, ',');
            if (comma) {
                *comma = '\0';
                while (isspace((unsigned char)*start)) start++;
                char *endptr_re;
                p->re = strtod(start, &endptr_re);
                while (isspace((unsigned char)*endptr_re)) endptr_re++;
                char *im_start = comma + 1;
                while (isspace((unsigned char)*im_start)) im_start++;
                char *endptr_im;
                p->im = strtod(im_start, &endptr_im);
            } else {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Error: Invalid complex double format '%s'.\n", buffer);
                exit(1);
            }
        } else {
            // No parentheses: treat as two whitespace-separated numbers
            p->re = strtod(buffer, NULL);
            if (fscanf(filep, "%lf", &p->im) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read imaginary part of complex double.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_complex_float(struct _lfortran_complex_32 *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    char buf_re[100], buf_im[100];

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%99s %99s", buf_re, buf_im) != 2) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read complex float from stdin.\n");
                exit(1);
            }
            convert_fortran_d_exponent(buf_re);
            convert_fortran_d_exponent(buf_im);
            p[i].re = strtof(buf_re, NULL);
            p[i].im = strtof(buf_im, NULL);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(struct _lfortran_complex_32), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read complex float array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            char buffer[200];
            if (!read_complex_expr(filep, buffer, sizeof(buffer))) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Invalid input for complex float from file.\n");
                exit(1);
            }
            convert_fortran_d_exponent(buffer);
            char *start = strchr(buffer, '(');
            char *end = strchr(buffer, ')');
            if (start && end && end > start) {
                *end = '\0';
                start++;
                char *comma = strchr(start, ',');
                if (comma) {
                    *comma = '\0';
                    while (isspace((unsigned char)*start)) start++;
                    char *endptr_re;
                    p[i].re = strtof(start, &endptr_re);
                    while (isspace((unsigned char)*endptr_re)) endptr_re++;
                    char *im_start = comma + 1;
                    while (isspace((unsigned char)*im_start)) im_start++;
                    char *endptr_im;
                    p[i].im = strtof(im_start, &endptr_im);
                } else {
                    if (iostat) { *iostat = 1; return; }
                    fprintf(stderr, "Error: Invalid complex float format '%s'.\n", buffer);
                    exit(1);
                }
            } else {
                // No parentheses: treat as two whitespace-separated numbers
                p[i].re = strtof(buffer, NULL);
                if (fscanf(filep, "%f", &p[i].im) != 1) {
                    if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                    fprintf(stderr, "Error: Failed to read complex float from file.\n");
                    exit(1);
                }
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_complex_double(struct _lfortran_complex_64 *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    char buf_re[100], buf_im[100];

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%99s %99s", buf_re, buf_im) != 2) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read complex double from stdin.\n");
                exit(1);
            }
            convert_fortran_d_exponent(buf_re);
            convert_fortran_d_exponent(buf_im);
            p[i].re = strtod(buf_re, NULL);
            p[i].im = strtod(buf_im, NULL);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(struct _lfortran_complex_64), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read complex double array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            char buffer[200];
            if (!read_complex_expr(filep, buffer, sizeof(buffer))) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Invalid input for complex double from file.\n");
                exit(1);
            }
            convert_fortran_d_exponent(buffer);
            char *start = strchr(buffer, '(');
            char *end = strchr(buffer, ')');
            if (start && end && end > start) {
                *end = '\0';
                start++;
                char *comma = strchr(start, ',');
                if (comma) {
                    *comma = '\0';
                    while (isspace((unsigned char)*start)) start++;
                    char *endptr_re;
                    p[i].re = strtod(start, &endptr_re);
                    while (isspace((unsigned char)*endptr_re)) endptr_re++;
                    char *im_start = comma + 1;
                    while (isspace((unsigned char)*im_start)) im_start++;
                    char *endptr_im;
                    p[i].im = strtod(im_start, &endptr_im);
                } else {
                    if (iostat) { *iostat = 1; return; }
                    fprintf(stderr, "Error: Invalid complex double format '%s'.\n", buffer);
                    exit(1);
                }
            } else {
                // No parentheses: treat as two whitespace-separated numbers
                p[i].re = strtod(buffer, NULL);
                if (fscanf(filep, "%lf", &p[i].im) != 1) {
                    if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                    fprintf(stderr, "Error: Failed to read complex double from file.\n");
                    exit(1);
                }
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_float(float *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    char buffer[100];

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%99s", buffer) != 1) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read float from stdin.\n");
                exit(1);
            }
            if (!parse_fortran_float(buffer, &p[i])) {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Error: Invalid input from stdin.\n");
                exit(1);
            }
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(float), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read float array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            if (fscanf(filep, "%99s", buffer) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read float from file.\n");
                exit(1);
            }
            if (!parse_fortran_float(buffer, &p[i])) {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Error: Invalid input from file.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_double(double *p, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    char buffer[100];

    if (unit_num == -1) {
        for (int i = 0; i < array_size; i++) {
            if (scanf("%99s", buffer) != 1) {
                if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read double from stdin.\n");
                exit(1);
            }
            if (!parse_fortran_double(buffer, &p[i])) {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Error: Invalid input from stdin.\n");
                exit(1);
            }
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(double), array_size, filep) != (size_t)array_size) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read double array from binary file.\n");
            exit(1);
        }
    } else {
        for (int i = 0; i < array_size; i++) {
            if (fscanf(filep, "%99s", buffer) != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Failed to read double from file.\n");
                exit(1);
            }
            if (!parse_fortran_double(buffer, &p[i])) {
                if (iostat) { *iostat = 1; return; }
                fprintf(stderr, "Error: Invalid input from file.\n");
                exit(1);
            }
        }
    }
}

LFORTRAN_API void _lfortran_read_array_char(char *p, int64_t length, int array_size, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (p == NULL) {
        if (iostat) { *iostat = 1; return; }
        fprintf(stderr, "Runtime Error: Unallocated array memory\n");
        exit(1);
    }

    bool unit_file_bin;
    FILE* filep;
    if (unit_num == -1) {
        filep = stdin;
        unit_file_bin = false;
    } else {
        filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
        if (!filep) {
            if (iostat) { *iostat = 1; return; }
            printf("No file found with given unit\n");
            exit(1);
        }
    }

    if (unit_file_bin) {
        size_t want = (size_t)(length * array_size);
        size_t got = fread(p, sizeof(char), want, filep);
        if (got != want) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; }
        }
        if (got < want) {
            memset(p + got, ' ', want - got);
        }
    } else {
        char length_format[23];
        sprintf(length_format, "%%%" PRId64, length);
        strcat(length_format, "s");
        for (int i = 0; i < array_size; i++) {
            int scan_ret = fscanf(filep, length_format, p + (i * length));
            if (scan_ret != 1) {
                if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
                fprintf(stderr, "Error: Invalid read (scan)\n");
                exit(1);
            }
            (void)!fscanf(filep, "%*[^\n \t]");
        }
    }
}

LFORTRAN_API void _lfortran_read_double(double *p, int32_t unit_num, int32_t *iostat)
{
    if (iostat) *iostat = 0;

    if (unit_num == -1) {
        // Read as string to handle Fortran D exponent notation
        char buffer[100];
        if (scanf("%99s", buffer) != 1) {
            if (iostat) { *iostat = feof(stdin) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read double from stdin.\n");
            exit(1);
        }
        if (!parse_fortran_double(buffer, p)) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input from stdin.\n");
            exit(1);
        }
        return;
    }

    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!filep) {
        if (iostat) { *iostat = 1; return; }
        printf("No file found with given unit\n");
        exit(1);
    }

    if (unit_file_bin) {
        if (fread(p, sizeof(*p), 1, filep) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read double from binary file.\n");
            exit(1);
        }
    } else {
        // Read as string to handle Fortran D exponent notation
        char buffer[100];
        if (fscanf(filep, "%99s", buffer) != 1) {
            if (iostat) { *iostat = feof(filep) ? -1 : 1; return; }
            fprintf(stderr, "Error: Failed to read double from file.\n");
            exit(1);
        }
        if (!parse_fortran_double(buffer, p)) {
            if (iostat) { *iostat = 1; return; }
            fprintf(stderr, "Error: Invalid input from file.\n");
            exit(1);
        }
    }
}

/*
    -- Check equality of strings --
- Not case sensitive.
- Not null dependent.
*/
LFORTRAN_API bool is_streql_NCS(char* s1, int64_t s1_len, char* s2, int64_t s2_len){
    if(s1_len != s2_len) return false;
    for(int64_t i = 0; i < s1_len; i++){
        if(tolower(s1[i]) != tolower((s2[i]))) return false;
    }
    return true;
}

typedef enum {
    INPUT_FILE,
    INPUT_STRING
} InputMethod;

typedef struct {
    InputMethod inputMethod;
    union {
        FILE *file;
        struct {
            const fchar *buf;
            const int64_t len;
            size_t pos;
        } str;
    };
    long record_start_pos;
} InputSource;

// Shared buffer parsing functions for formatted reads
// These functions parse already-read data from a buffer, allowing code reuse
// between file-based and string-based formatted reads.

// blank_mode: 0 = BN (blank null - ignore blanks), 1 = BZ (blank zero - treat blanks as zeros)
static void parse_integer_from_buffer(char* buffer, int field_len, 
        void* int_ptr, int32_t type_code, int blank_mode)
{
    // Process buffer according to blank mode
    char* processed = (char*)malloc(field_len + 1);
    int j = 0;
    for (int i = 0; i < field_len; i++) {
        if (buffer[i] == ' ') {
            if (blank_mode == 1) {  // BZ: treat blanks as zeros
                processed[j++] = '0';
            }
            // BN: skip blanks (blank_mode == 0)
        } else {
            processed[j++] = buffer[i];
        }
    }
    processed[j] = '\0';
    
    if (type_code == 2) {
        *((int32_t*)int_ptr) = (int32_t)strtol(processed, NULL, 10);
    } else {
        *((int64_t*)int_ptr) = (int64_t)strtoll(processed, NULL, 10);
    }
    
    free(processed);
}

static void parse_real_from_buffer(char* buffer, int field_len,
        void* real_ptr, int32_t type_code, int scale_factor)
{
    // Handle integer types (type_code 2 = int32, type_code 3 = int64)
    if (type_code == 2 || type_code == 3) {
        char* temp = (char*)malloc(field_len + 1);
        if (temp) {
            memcpy(temp, buffer, field_len);
            temp[field_len] = '\0';
            if (type_code == 2) {
                char* endptr;
                long val = strtol(temp, &endptr, 10);
                *(int32_t*)real_ptr = (int32_t)val;
            } else {
                char* endptr;
                long long val = strtoll(temp, &endptr, 10);
                *(int64_t*)real_ptr = (int64_t)val;
            }
            free(temp);
        }
        return;
    }
    // Replace D/d with E for parsing, check for exponent
    bool has_exponent = false;
    for (int i = 0; i < field_len; i++) {
        if (buffer[i] == 'D' || buffer[i] == 'd') {
            buffer[i] = 'E';
            has_exponent = true;
        } else if (buffer[i] == 'E' || buffer[i] == 'e') {
            has_exponent = true;
        }
    }

    double v = strtod(buffer, NULL);
    if (!has_exponent) {
        v = v / pow(10.0, scale_factor);
    }
    if (type_code == 4) {
        *((float*)real_ptr) = (float)v;
    } else {
        *((double*)real_ptr) = v;
    }
}

static void parse_logical_from_buffer(char* buffer, int field_len,
        int32_t* log_ptr)
{
    *log_ptr = 0;
    for (int i = 0; i < field_len; i++) {
        char c = toupper(buffer[i]);
        if (c == 'T') {
            *log_ptr = 1;
            break;
        } else if (c == 'F') {
            *log_ptr = 0;
            break;
        }
    }
}

static void parse_character_from_buffer(char* buffer, int field_len,
        char* str_data, int64_t str_len, int width)
{
    pad_with_spaces(str_data, 0, str_len);
    if (width > (int)str_len) {
        if (field_len >= width) {
            memcpy(str_data, buffer + (width - (int)str_len), (size_t)str_len);
        } else if (field_len >= (int)str_len) {
            memcpy(str_data, buffer + (field_len - (int)str_len), (size_t)str_len);
        } else if (field_len > 0) {
            memcpy(str_data, buffer, (size_t)field_len);
        }
    } else {
        int copy_len = field_len;
        if (copy_len > width) copy_len = width;
        if (copy_len > (int)str_len) copy_len = (int)str_len;
        if (copy_len > 0) memcpy(str_data, buffer, (size_t)copy_len);
    }
}

static inline char* read_line(char *buf, int size, InputSource *inputSource)
{
    if (size <= 0) {
        return NULL;
    }

    int i = 0;

    switch (inputSource->inputMethod) {
    case INPUT_FILE:
        return fgets(buf, size, inputSource->file);
    case INPUT_STRING:
        while (i < size - 1 && inputSource->str.pos < inputSource->str.len) {
            char c = inputSource->str.buf[inputSource->str.pos++];
            buf[i++] = c;
            if (c == '\n') break;
        }

        if (i == 0) {
            return NULL;
        }

        buf[i] = '\0';
        return buf;
    }

    return NULL;
}

static inline int read_character(InputSource *inputSource)
{
    switch (inputSource->inputMethod) {

    case INPUT_FILE:
        return fgetc(inputSource->file);

    case INPUT_STRING:
        if (inputSource->str.pos < inputSource->str.len) {
            return (unsigned char)inputSource->str.buf[inputSource->str.pos++];
        }
        return EOF;
    }

    return EOF;
}

static bool read_field(InputSource *inputSource, int read_width, bool advance_no,
        int32_t *iostat, int32_t *chunk, bool *consumed_newline,
        char **buffer, int *field_len)
{
    *buffer = (char*)malloc((size_t)read_width + 2);
    if (!*buffer) {
        printf("Memory allocation failed\n");
        exit(1);
    }

    if (read_line(*buffer, read_width + 1, inputSource) == NULL) {
        if (iostat) *iostat = -1;
        if (chunk) *chunk = 0;
        free(*buffer);
        *buffer = NULL;
        *field_len = 0;
        return false;
    }

    char* nl = strchr(*buffer, '\n');
    *field_len = (nl != NULL) ? (int)(nl - *buffer) : (int)strlen(*buffer);
    if (nl != NULL) {
        *nl = '\0';
        *consumed_newline = true;
    }

    if (chunk) *chunk = (int32_t)(*field_len);
    if (advance_no && *consumed_newline && *field_len != read_width) {
        if (iostat) *iostat = -2;
    }

    return true;
}

static bool handle_read_A(InputSource *inputSource, va_list *args, int width, bool advance_no,
        int32_t *iostat, int32_t *chunk, bool *consumed_newline, int *arg_idx)
{
    int32_t type_code = va_arg(*args, int32_t);
    (void)type_code;
    char** str_data_ptr = va_arg(*args, char**);
    int64_t str_len = va_arg(*args, int64_t);
    (*arg_idx)++;

    char* str_data = str_data_ptr ? *str_data_ptr : NULL;
    if (str_data == NULL) {
        printf("Runtime Error: Unallocated string in formatted read\n");
        exit(1);
    }

    int read_width = (width > 0) ? width : (int)str_len;
    if (read_width < 0) read_width = 0;

    char* buffer = NULL;
    int field_len = 0;
    if (!read_field(inputSource, read_width, advance_no, iostat, chunk,
            consumed_newline, &buffer, &field_len)) {
        return false;
    }

    parse_character_from_buffer(buffer, field_len, str_data, str_len, read_width);

    free(buffer);
    return true;
}

static bool handle_read_L(InputSource *inputSource, va_list *args, int width, bool advance_no,
        int32_t *iostat, int32_t *chunk, bool *consumed_newline, int *arg_idx)
{
    int32_t type_code = va_arg(*args, int32_t);
    (void)type_code;
    int32_t* log_ptr = va_arg(*args, int32_t*);
    (*arg_idx)++;

    int read_width = (width > 0) ? width : 1;
    if (read_width < 0) read_width = 0;

    char* buffer = NULL;
    int field_len = 0;
    if (!read_field(inputSource, read_width, advance_no, iostat, chunk,
            consumed_newline, &buffer, &field_len)) {
        return false;
    }

    parse_logical_from_buffer(buffer, field_len, log_ptr);

    free(buffer);
    return true;
}

static bool handle_read_I(InputSource *inputSource, va_list *args, int width, bool advance_no,
        int32_t *iostat, int32_t *chunk, bool *consumed_newline, int *arg_idx, int blank_mode)
{
    int32_t type_code = va_arg(*args, int32_t);
    void* int_ptr = va_arg(*args, void*);
    (*arg_idx)++;

    int read_width = (width > 0) ? width : 10;
    if (read_width < 0) read_width = 0;

    char* buffer = NULL;
    int field_len = 0;
    if (!read_field(inputSource, read_width, advance_no, iostat, chunk,
            consumed_newline, &buffer, &field_len)) {
        return false;
    }

    parse_integer_from_buffer(buffer, field_len, int_ptr, type_code, blank_mode);

    free(buffer);
    return true;
}

static void parse_decimals(const fchar* fmt, int64_t fmt_len, int64_t *fmt_pos)
{
    if (*fmt_pos < fmt_len && fmt[*fmt_pos] == '.') {
        (*fmt_pos)++;
        while (*fmt_pos < fmt_len && isdigit((unsigned char)fmt[*fmt_pos])) {
            (*fmt_pos)++;
        }
    }
}

static bool handle_read_real(InputSource *inputSource, va_list *args, int width, bool advance_no,
        const fchar* fmt, int64_t fmt_len, int64_t *fmt_pos,
        int32_t *iostat, int32_t *chunk, bool *consumed_newline, int *arg_idx, int blank_mode, int scale_factor)
{
    int32_t type_code = va_arg(*args, int32_t);
    void* real_ptr = va_arg(*args, void*);
    (*arg_idx)++;
    bool is_complex = (type_code == 6 || type_code == 7);
    if (type_code == 6) type_code = 4;
    if (type_code == 7) type_code = 5;

    parse_decimals(fmt, fmt_len, fmt_pos);

    int read_width = (width > 0) ? width : 15;
    if (read_width < 0) read_width = 0;

    char* buffer = NULL;
    int field_len = 0;
    if (!read_field(inputSource, read_width, advance_no, iostat, chunk,
            consumed_newline, &buffer, &field_len)) {
        return false;
    }
    // Apply blank mode processing
    if (blank_mode == 0) {
        // BN mode: remove all blanks from the buffer
        int write_idx = 0;
        for (int read_idx = 0; read_idx < field_len; read_idx++) {
            if (buffer[read_idx] != ' ') {
                buffer[write_idx++] = buffer[read_idx];
            }
        }
        buffer[write_idx] = '\0';
        field_len = write_idx;
    } else if (blank_mode == 1) {
        // BZ mode: replace blanks with zeros
        for (int i = 0; i < field_len; i++) {
            if (buffer[i] == ' ') {
                buffer[i] = '0';
            }
        }
    }

    parse_real_from_buffer(buffer, field_len, real_ptr, type_code, scale_factor);

    free(buffer);
    if (is_complex) {
        void* imag_ptr = (type_code == 4) ? (void*)((float*)real_ptr + 1) : (void*)((double*)real_ptr + 1);
        parse_decimals(fmt, fmt_len, fmt_pos);
        buffer = NULL;
        field_len = 0;
        if (!read_field(inputSource, read_width, advance_no, iostat, chunk,
                consumed_newline, &buffer, &field_len)) {
            return false;
        }
        if (blank_mode == 0) {
            int write_idx = 0;
            for (int read_idx = 0; read_idx < field_len; read_idx++) {
                if (buffer[read_idx] != ' ') {
                    buffer[write_idx++] = buffer[read_idx];
                }
            }
            buffer[write_idx] = '\0';
            field_len = write_idx;
        } else if (blank_mode == 1) {
            for (int i = 0; i < field_len; i++) {
                if (buffer[i] == ' ') {
                    buffer[i] = '0';
                }
            }
        }
        parse_real_from_buffer(buffer, field_len, imag_ptr, type_code, scale_factor);
        free(buffer);
    }
    
    return true;
}

static void handle_read_X(InputSource *inputSource, int width, bool advance_no,
        int32_t *iostat, bool *consumed_newline)
{
    int skip = (width > 0) ? width : 1;
    for (int i = 0; i < skip; i++) {
        int c = read_character(inputSource);
        if (c == EOF) {
            if (iostat) *iostat = -1;
            break;
        }
        if (c == '\n') {
            *consumed_newline = true;
            if (advance_no) {
                if (iostat) *iostat = -2;
            }
            break;
        }
    }
}

static void handle_read_TL(InputSource *inputSource, int width)
{
    int move_left = (width > 0) ? width : 1;
    
    if (inputSource->inputMethod == INPUT_STRING) {
        if (inputSource->str.pos >= move_left) {
            inputSource->str.pos -= move_left;
        } else {
            inputSource->str.pos = 0;
        }
    } else if (inputSource->inputMethod == INPUT_FILE) {
        if (inputSource->file) {
            long current_pos = ftell(inputSource->file);
            if (current_pos >= move_left) {
                fseek(inputSource->file, -move_left, SEEK_CUR);
            } else {
                fseek(inputSource->file, 0, SEEK_SET);
            }
        }
    }
}

static void handle_read_T(InputSource *inputSource, int position)
{
    if (position < 1) position = 1;
    
    if (inputSource->inputMethod == INPUT_STRING) {
        int target_pos = position - 1;
        if (target_pos > (int)inputSource->str.len) {
            target_pos = (int)inputSource->str.len;
        }
        inputSource->str.pos = target_pos;
    } else if (inputSource->inputMethod == INPUT_FILE) {
        if (inputSource->file) {
            long target_pos = inputSource->record_start_pos + (position - 1);
            fseek(inputSource->file, target_pos, SEEK_SET);
        }
    }
}

static void handle_read_slash(InputSource *inputSource, int32_t *iostat, bool *consumed_newline)
{
    if (!*consumed_newline) {
        int c = 0;
        do {
            c = read_character(inputSource);
        } while (c != '\n' && c != EOF);
        if (c == EOF) {
            if (iostat) *iostat = -1;
            return;
        }
    }
    if (inputSource->inputMethod == INPUT_FILE && inputSource->file) {
        inputSource->record_start_pos = ftell(inputSource->file);
    }
    *consumed_newline = false;
}


static void common_formatted_read(InputSource *inputSource,
    int32_t* iostat, int32_t* chunk,
    fchar* advance, int64_t advance_length,
    fchar* fmt, int64_t fmt_len,
    int32_t no_of_args, va_list *args, bool blank_zero);

LFORTRAN_API void _lfortran_string_formatted_read(
    fchar* src_data, int64_t src_len,
    int32_t* iostat, int32_t* chunk,
    fchar* advance, int64_t advance_length,
    fchar* fmt, int64_t fmt_len,
    int32_t no_of_args, ...) {
    
    InputSource inputSource = {INPUT_STRING, .str = {src_data, src_len, 0}, .record_start_pos = 0};
    
    va_list args;
    va_start(args, no_of_args);
    
    common_formatted_read(&inputSource, iostat, chunk,
        advance, advance_length, fmt, fmt_len,
        no_of_args, &args, false);
    
    va_end(args);
}

// Type codes for _lfortran_formatted_read:
// 0 = character (followed by ptr, str_len). For strings, `ptr` is `char**`
// (pointer to the data pointer inside a string descriptor).
// 1 = logical (followed by ptr)
// 2 = int32 (followed by ptr)
// 3 = int64 (followed by ptr)
// 4 = float (followed by ptr)
// 5 = double (followed by ptr)
LFORTRAN_API void _lfortran_formatted_read(
    int32_t unit_num, int32_t* iostat, int32_t* chunk,
    fchar* advance, int64_t advance_length,
    fchar* fmt, int64_t fmt_len,
    int32_t no_of_args, ...)
{
    InputSource inputSource;
    bool unit_file_bin, blank_zero;

    if (unit_num != -1) {
        inputSource.inputMethod = INPUT_FILE;
        inputSource.file = get_file_pointer_from_unit(unit_num, &unit_file_bin,
            NULL, NULL, NULL, NULL, &blank_zero, NULL, NULL);
        if (!inputSource.file) {
            printf("No file found with given unit\n");
            exit(1);
        }
    } else {
        inputSource.inputMethod = INPUT_FILE;
        inputSource.file = stdin;
    }
    
    va_list args;
    va_start(args, no_of_args);
    
    common_formatted_read(&inputSource, iostat, chunk,
        advance, advance_length, fmt, fmt_len,
        no_of_args, &args, blank_zero);
    
    va_end(args);
}

static void process_fmt_items_read(InputSource *inputSource,
    int32_t* iostat, int32_t* chunk,
    bool advance_no,
    fchar* fmt, int64_t fmt_len,
    int32_t no_of_args, va_list *args,
    int *arg_idx, int *blank_mode, int *scale_factor,
    bool *consumed_newline)
{
    int64_t fmt_pos = 0;

    while (fmt_pos < fmt_len && *arg_idx < no_of_args) {
        while (fmt_pos < fmt_len && (fmt[fmt_pos] == ' ' || fmt[fmt_pos] == ',')) {
            fmt_pos++;
        }
        if (fmt_pos >= fmt_len || fmt[fmt_pos] == ')') break;

        int repeat_count = 0;
        while (fmt_pos < fmt_len && isdigit((unsigned char)fmt[fmt_pos])) {
            repeat_count = repeat_count * 10 + (fmt[fmt_pos] - '0');
            fmt_pos++;
        }
        if (repeat_count == 0) repeat_count = 1;

        if (fmt_pos < fmt_len && fmt[fmt_pos] == '(') {
            // Parenthesized group: N(...)
            int64_t group_start = fmt_pos + 1; // position after '('
            // Find matching ')'
            int paren_depth = 1;
            int64_t pos = fmt_pos + 1;
            while (pos < fmt_len && paren_depth > 0) {
                if (fmt[pos] == '(') paren_depth++;
                else if (fmt[pos] == ')') paren_depth--;
                pos++;
            }
            int64_t group_end = pos - 1; // position of matching ')'
            int64_t group_len = group_end - group_start;
            fmt_pos = pos; // advance past ')'

            for (int rep = 0; rep < repeat_count; rep++) {
                process_fmt_items_read(inputSource, iostat, chunk,
                    advance_no, fmt + group_start, group_len,
                    no_of_args, args, arg_idx, blank_mode,
                    scale_factor, consumed_newline);
                if (iostat && *iostat != 0) return;
                if (*arg_idx >= no_of_args) return;
            }
            continue;
        }

        char spec = toupper(fmt[fmt_pos++]);
        if (spec == 'P') {
            *scale_factor = repeat_count;
            repeat_count = 1;
        }
        
        bool is_tab_descriptor = false;
        char tab_type = ' ';
        if (spec == 'T' && fmt_pos < fmt_len) {
            char next = toupper(fmt[fmt_pos]);
            if (next == 'R' || next == 'L') {
                tab_type = next;
                fmt_pos++;
                is_tab_descriptor = true;
            }
        }
        
        int width = 0;
        if (spec != '/' && spec != ':') {
            while (fmt_pos < fmt_len && isdigit((unsigned char)fmt[fmt_pos])) {
                width = width * 10 + (fmt[fmt_pos] - '0');
                fmt_pos++;
            }
        }
        for (int rep = 0; rep < repeat_count; rep++)  {
            switch (spec) {
            case 'B':
                // Check for BN or BZ
                if (fmt_pos < fmt_len) {
                    char next = toupper(fmt[fmt_pos]);
                    if (next == 'N') {
                        *blank_mode = 0;  // BN: blank null
                        fmt_pos++;
                    } else if (next == 'Z') {
                        *blank_mode = 1;  // BZ: blank zero
                        fmt_pos++;
                    }
                }
                break;
            case 'T':
                if (is_tab_descriptor) {
                    if (tab_type == 'R') {
                        handle_read_X(inputSource, width, advance_no, iostat, consumed_newline);
                    } else if (tab_type == 'L') {
                        handle_read_TL(inputSource, width);
                    }
                } else {
                    handle_read_T(inputSource, width);
                }
                break;
            case 'A':
                if (!handle_read_A(inputSource, args, width, advance_no,
                        iostat, chunk, consumed_newline, arg_idx)) {
                    return;
                }
                break;
            case 'L':
                if (!handle_read_L(inputSource, args, width, advance_no,
                        iostat, chunk, consumed_newline, arg_idx)) {
                    return;
                }
                break;
            case 'I':
                if (!handle_read_I(inputSource, args, width, advance_no,
                        iostat, chunk, consumed_newline, arg_idx, *blank_mode)) {
                    return;
                }
                break;
            case 'F':
            case 'E':
            case 'D':
            case 'G':
                if (!handle_read_real(inputSource, args, width, advance_no,
                        fmt, fmt_len, &fmt_pos, iostat, chunk,
                        consumed_newline, arg_idx, *blank_mode, *scale_factor)) {
                    return;
                }
                break;
            case 'X':
                handle_read_X(inputSource, width, advance_no, iostat, consumed_newline);
                break;
            case '/':
                handle_read_slash(inputSource, iostat, consumed_newline);
                break;
            default:
                break;
            }

            if (iostat && *iostat != 0) break;
            if (*arg_idx >= no_of_args) break;
        }
    }
}

static void common_formatted_read(InputSource *inputSource,
    int32_t* iostat, int32_t* chunk,
    fchar* advance, int64_t advance_length,
    fchar* fmt, int64_t fmt_len,
    int32_t no_of_args, va_list *args, bool blank_zero)
{
    if (inputSource->inputMethod == INPUT_FILE && inputSource->file) {
        inputSource->record_start_pos = ftell(inputSource->file);
    }
    if (chunk) *chunk = 0;
    if (iostat) *iostat = 0;
    const bool advance_no = is_streql_NCS((char*)advance, advance_length, "no", 2);

    int64_t start_pos = 0;
    if (fmt_len > 0 && fmt[0] == '(') start_pos = 1;

    // Strip outer parentheses: find matching ')' for the opening '('
    int64_t inner_len = fmt_len - start_pos;
    if (start_pos == 1) {
        // Find the matching ')' to exclude it from inner_len
        int paren_depth = 1;
        int64_t pos = start_pos;
        while (pos < fmt_len && paren_depth > 0) {
            if (fmt[pos] == '(') paren_depth++;
            else if (fmt[pos] == ')') paren_depth--;
            if (paren_depth > 0) pos++;
        }
        inner_len = pos - start_pos;
    }

    bool consumed_newline = false;
    int arg_idx = 0;
    int blank_mode = 0;  // 0 = BN (default: blank null - ignore blanks), 1 = BZ (blank zero - treat blanks as zeros)
    if (blank_zero) {
        blank_mode = 1;
    }
    
    int scale_factor = 0;

    while (arg_idx < no_of_args && (!iostat || *iostat == 0)) {
        int args_before = arg_idx;
        process_fmt_items_read(inputSource, iostat, chunk, advance_no,
            fmt + start_pos, inner_len, no_of_args, args,
            &arg_idx, &blank_mode, &scale_factor, &consumed_newline);
        if (arg_idx > args_before && arg_idx < no_of_args && (!iostat || *iostat == 0)) {
            if (!consumed_newline) {
                int c = 0;
                do {
                    c = read_character(inputSource);
                } while (c != '\n' && c != EOF);
                if (c == EOF) {
                    if (iostat) *iostat = -1;
                    break;
                }
            }
            if (inputSource->inputMethod == INPUT_FILE && inputSource->file) {
                inputSource->record_start_pos = ftell(inputSource->file);
            }
            consumed_newline = false;
        } else {
            break;
        }
    }

    if (!advance_no && !consumed_newline && (!iostat || *iostat == 0)) {
        int c = 0;
        do {
            c = read_character(inputSource);
        } while (c != '\n' && c != EOF);
    }

}

LFORTRAN_API void _lfortran_empty_read(int32_t unit_num, int32_t* iostat) {
    if (iostat) *iostat = 0;
    if (unit_num == -1) {
        // Read from stdin
        return;
    }

    bool unit_file_bin;
    FILE* fp = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!fp) {
        fprintf(stderr, "No file found with given unit\n");
        exit(1);
    }

    if (!unit_file_bin) {
        // The contents of `c` are ignored
        char c = fgetc(fp);
        while (c != '\n' && c != EOF) {
            c = fgetc(fp);
        }

        if (feof(fp)) {
            if (iostat) *iostat = -1;
        } else if (ferror(fp)) {
            if (iostat) *iostat = 1;
        }
    }
}

LFORTRAN_API void _lfortran_file_seek(int32_t unit_num, int64_t pos, int32_t* iostat) {
    if (iostat) *iostat = 0;
    if (unit_num == -1) {
        // Cannot seek on stdin
        if (iostat) *iostat = 1;
        return;
    }

    FILE* fp = get_file_pointer_from_unit(unit_num, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
    if (!fp) {
        if (iostat) { *iostat = 1; return; }
        fprintf(stderr, "No file found with given unit number %d.\n", unit_num);
        exit(1);
    }

    // pos is 1-based in Fortran, convert to 0-based for fseek
    if (fseek(fp, pos - 1, SEEK_SET) != 0) {
        if (iostat) { *iostat = 1; return; }
        fprintf(stderr, "Error seeking to position %ld in file.\n", (long)pos);
        exit(1);
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

/**
 * @brief Checks Format match for File Write statement and OpenFile statement
 * OpenFile -> 'Unformatted' + FileWrite -> Binary Format (no format provided) => Match
 * OpenFile -> 'Formatted' + FileWrite -> Formatted Format (format provided) => Match
 * Otherwise => No Match
 */
bool is_write_and_open_format_match(bool unit_file_bin, const char* format_data){
    ASSERT(format_data)
    const bool is_openFile_formatted =  unit_file_bin == false;
    const bool is_fileWrite_formatted =  format_data[0] != '\0';
    return is_openFile_formatted == is_fileWrite_formatted;
}

LFORTRAN_API void _lfortran_file_write(int32_t unit_num, int32_t* iostat, const char* format_data, int64_t format_len, ...)
{
    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    int delim;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, &delim, NULL, NULL, NULL);

    // Check if write is allowed (action='read' sets write_access=false)
    // Only check if unit was found in table (filep != NULL); unconnected units fall through to stdout
    if (filep && !write_access) {
        if (iostat) {
            *iostat = 5003;  // Write not allowed
            return;
        } else {
            fprintf(stderr, "Runtime Error: Write operation not allowed on unit %d "
                "(opened with action='read').\n", unit_num);
            exit(1);
        }
    }

    if(!is_write_and_open_format_match(unit_file_bin, format_data)){
        if(iostat) {
            *iostat = 5001;
            return;
        } else {
            fprintf(stderr, "Runtime Error: Format mismatch between "
                "OPEN statement and WRITE statement on unit %d.\n", unit_num);
            exit(1);
        }
    }
    
    if (!filep) {
        filep = stdout;
    }
    if (unit_file_bin) { // Unformatted
        if (access_id != 2) {  // 2 = DIRECT access
            fseek(filep, 0, SEEK_END);
        }
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
        if (access_id == 0) {
            fwrite(&record_marker, sizeof(record_marker), 1, filep);
        }        
        size_t written = 0;
        // Write all data chunks
        for (int i = 0; i < count; i++) {
            written += fwrite(data[i].ptr, 1, data[i].len, filep);
        }
        // Write record marker again
        if (access_id == 0) {
            fwrite(&record_marker, sizeof(record_marker), 1, filep);
        }
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
    } else { // Formatted
        va_list args;
        va_start(args, format_len);
        char* str = va_arg(args, char*);
        int64_t str_len = va_arg(args, int64_t);

        // Detect "\b" to raise error
        if(str_len > 0 && str[0] == '\b'){
            if(iostat == NULL){
                str = str + 1;
                fprintf(stderr, "%.*s", (int)str_len, str);
                exit(1);
            } else { // Delegate error handling to the user.
                *iostat = 11;
                return;
            }
        }

        char open_delim = '\0', close_delim = '\0';
        if (delim == 1) {        // APOSTROPHE
            open_delim = close_delim = '\'';
        } else if (delim == 2) { // QUOTE
            open_delim = close_delim = '"';
        }

        // If format_data changed, we need to change the hardcoded format passed to fprintf
        if(strcmp(format_data, "%s%s") == 0){
            char* end = va_arg(args, char*);
            int64_t end_len = va_arg(args, int64_t);

            if(open_delim != '\0') {
                fprintf(filep, "%c%.*s%c%.*s",
                    open_delim, (int)str_len, str, close_delim,
                    (int)end_len, end
                );
            } else {
                fprintf(filep, "%.*s%.*s",
                    (int)str_len, str,
                    (int)end_len, end
                );
            }
        } else if (strcmp(format_data, "%s") == 0){
            if(open_delim != '\0') {
                fprintf(filep, "%c%.*s%c",
                    open_delim, (int)str_len, str, close_delim
                );
            } else {
                fprintf(filep, "%.*s", (int)str_len, str);
            }
        } else {
            fprintf(stderr,"Compiler Error : Undefined Format");
            exit(1);
        }


        if(iostat != NULL) *iostat = 0;
        va_end(args);
    }
    // Only truncate actual files, not stdout/stderr
    // This removes stale data when overwriting a file with less content
    // Do not truncate direct access files, as records may be written out of order
    if (filep != stdout && filep != stderr && access_id != 2) {
        (void)!ftruncate(fileno(filep), ftell(filep));
    }
}

LFORTRAN_API void _lfortran_string_write(char **str_holder, bool is_allocatable, bool is_deferred, int64_t* len, int32_t* iostat, const char* format,
    int64_t format_len, ...) {
    va_list args;
    va_start(args, format_len);
    char* str = "";
    int64_t str_len = 0;
    char* end_data = "";
    int64_t end_len = 0;

    if(strcmp(format, "%s%s") == 0){
        str = va_arg(args, char*);
        str_len = va_arg(args, int64_t);
        end_data = va_arg(args, char*);
        end_len = va_arg(args, int64_t); 
    } else if(strcmp(format, "%s") == 0){
        str = va_arg(args, char*);
        str_len = va_arg(args, int64_t);
    } else {
        fprintf(stderr,"Compiler Error : Undefined Format");
        exit(1);
    }

    // Detect "\b" to raise error
    if(str_len > 0 && str[0] == '\b'){
        if(iostat == NULL){
            str = str + 1;
            fprintf(stderr, "%.*s", (int)str_len, str);
            exit(1);
        } else { // Delegate error handling to the user.
            *iostat = 11;
            return;
        }
    }

    char *s = (char *) malloc(str_len * sizeof(char) + end_len * sizeof(char) + 1);

    // If format changed, we need to change the hardcoded format passed to sprintf
    if (strcmp(format, "%s"))
        sprintf(s, "%.*s", (int)str_len, str);
    else
        sprintf(s, "%.*s%.*s", (int)str_len, str, (int)end_len, end_data);

    _lfortran_strcpy(str_holder, len, is_allocatable, is_deferred, str, str_len);

    free(s);

    va_end(args);
    if(iostat != NULL) *iostat = 0;
}

LFORTRAN_API void _lfortran_string_read_i32(char *str, int64_t len, char *format, int32_t *i) {
    char *buf = (char*)malloc(len + 1);
    if (!buf) return;
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

char* remove_whitespace(char* str, int64_t* len) {
    if (!str || *len <= 0) return str;
    char* start = str;
    char* end = str + (*len - 1);
    // Trim leading spaces
    while (*len > 0 && isspace((unsigned char) *start)) {
        start++;
        (*len)--;
    }
    // Trim trailing spaces
    while (*len > 0 && isspace((unsigned char) *end)) {
        end--;
        (*len)--;
    }
    // Shift if we trimmed from the front
    if (start != str && *len > 0) {
        memmove(str, start, *len);
    }
    // Null terminate for now, but length is already known
    str[*len] = '\0';
    return str;
}

LFORTRAN_API void _lfortran_string_read_str(char *src_data, int64_t src_len, char *dest_data, int64_t dest_len) {
    _lfortran_copy_str_and_pad(
        dest_data, dest_len,
        src_data, src_len);
}

LFORTRAN_API void _lfortran_string_read_bool(char *str, int64_t len, char *format, int32_t *i) {
    char *buf = (char*)malloc(len + 1);
    if (!buf) return;
    memcpy(buf, str, len);
    buf[len] = '\0';
    sscanf(buf, format, i);
    printf("%s\n", buf);
    free(buf);
}

void lfortran_error(const char *message) {
    fprintf(stderr, "LFORTRAN ERROR: %s\n", message);
    exit(EXIT_FAILURE);
}

// TODO: add support for reading comma separated string, into `_arr` functions
// by accepting array size as an argument as well
LFORTRAN_API void _lfortran_string_read_i32_array(char *str, int64_t len, char *format, int32_t *arr) {
    (void)format; // currently unused
    const char *pos = str;
    const char *end = str + len;
    char *next = NULL;
    int64_t count = 0;
    while (pos < end) {
        // Skip whitespace and common separators
        while (pos < end && (isspace((unsigned char)*pos) || *pos == ',')) {
            pos++;
        }
        if (pos >= end) break;
        errno = 0;
        long value = strtol(pos, &next, 10);
        if (next == pos) break;
        if ((const char *)next > end) break;
        arr[count++] = (int32_t)value;
        pos = next;
    }
}

LFORTRAN_API void _lfortran_string_read_i64_array(char *str, int64_t len, char *format, int64_t *arr) {
    (void)format; 
    const char *pos = str;
    const char *end = str + len;
    char *next = NULL;
    int64_t count = 0;
    while (pos < end) {
        while (pos < end && (isspace((unsigned char)*pos) || *pos == ',')) {
            pos++;
        }
        if (pos >= end) break;
        errno = 0;
        long long value = strtoll(pos, &next, 10);
        if (next == pos) break;
        if ((const char *)next > end) break;
        arr[count++] = (int64_t)value;
        pos = next;
    }
}

LFORTRAN_API void _lfortran_string_read_f32_array(char *str, int64_t len, char *format, float *arr) {
    (void)format; 
    const char *pos = str;
    const char *end = str + len;
    char *next = NULL;
    int64_t count = 0;
    while (pos < end) {
        while (pos < end && (isspace((unsigned char)*pos) || *pos == ',')) {
            pos++;
        }
        if (pos >= end) break;
        errno = 0;
        float value = strtof(pos, &next);
        if (next == pos) break;
        if ((const char *)next > end) break;
        arr[count++] = value;
        pos = next;
    }
}

LFORTRAN_API void _lfortran_string_read_f64_array(char *str, int64_t len, char *format, double *arr) {
    (void)format; 
    const char *pos = str;
    const char *end = str + len;
    char *next = NULL;
    int64_t count = 0;
    while (pos < end) {
        while (pos < end && (isspace((unsigned char)*pos) || *pos == ',')) {
            pos++;
        }
        if (pos >= end) break;
        errno = 0;
        double value = strtod(pos, &next);
        if (next == pos) break;
        if ((const char *)next > end) break;
        arr[count++] = value;
        pos = next;
    }
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

LFORTRAN_API void _lfortran_close(int32_t unit_num, char* status, int64_t status_len, int32_t* iostat)
{
    bool unit_file_bin;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

    if (iostat) {
        *iostat = 0;
    }
    if (!filep) {
        return;
    }
    
    // Don't actually close standard units (stdin=5, stdout=6, stderr=0)
    // They are pre-connected and should remain open
    if (unit_num != 0 && unit_num != 5 && unit_num != 6) {
        if (fclose(filep) != 0) {
            if (iostat) {
                *iostat = 1;
                return;
            }
            printf("Error in closing the file!\n");
            exit(1);
        }
    }
    // TODO: Support other `status` specifiers
    char *file_name = get_file_name_from_unit(unit_num, &unit_file_bin);

    const char *scratch_file = "_lfortran_generated_file";
    const int64_t scratch_file_len = sizeof("_lfortran_generated_file") - 1; // exclude '\0'

    // file_name can be NULL for pre-connected units (stdin/stdout/stderr)
    bool is_temp_file = (file_name != NULL) &&
        (strncmp(file_name, scratch_file, scratch_file_len) == 0);

    bool delete_requested = false;
    if (status && status_len > 0) {
        // Compare to "delete" without assuming null-termination
        const char delete_str[] = "delete";
        const int64_t delete_len = sizeof(delete_str) - 1;

        if (status_len == delete_len &&
            strncmp(status, delete_str, delete_len) == 0) {
            delete_requested = true;
        }
    }

    if (delete_requested || is_temp_file) {
        if (remove(file_name) != 0) {
            if (iostat) {
                *iostat = 2;
                return;
            }
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

LFORTRAN_API void _lfortran_set_use_runtime_colors(int use_colors) {
    _lfortran_use_runtime_colors = use_colors;
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
    }
}

LFORTRAN_API int32_t _lfortran_get_command_argument_length(int n) {
    if (n >= 0 && n < _argc) {
        return strlen(_argv[n]);
    } else {
        return 0;
    }
}

LFORTRAN_API int32_t _lfortran_get_command_argument_status(int n, int arg_len, int len) {
    if (n > _argc - 1) return 42;
    else if (arg_len < len) return -1;
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
}

LFORTRAN_API int32_t _lfortran_get_command_length() {
    int32_t total_length = 0;
    for(int i=0; i<_argc; i++){
        total_length += strlen(_argv[i]);
    }
    total_length += (strlen(sep_space) * MAX((_argc - 1), 0));
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
    if (filename == NULL) {
        return NULL;
    }

    char *slash_idx_ptr = strrchr(filename, '/');
    const char *base_start = slash_idx_ptr ? (slash_idx_ptr + 1) : filename;
    const char *dot_idx_ptr = strrchr(base_start, '.');

    if (dot_idx_ptr == NULL || dot_idx_ptr == base_start) {
        return NULL;
    }

    size_t base_len = (size_t)(dot_idx_ptr - base_start);
    char *base_name = malloc(base_len + 1);
    if (base_name == NULL) {
        return NULL;
    }
    memcpy(base_name, base_start, base_len);
    base_name[base_len] = '\0';
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
    d->stack_size = 0;
    char *base_name = get_base_name(source_filename);
    if (base_name == NULL) {
        return;
    }

    char *filename = malloc(strlen(base_name) + 15);
    if (filename == NULL) {
        free(base_name);
        return;
    }
    strcpy(filename, base_name);
    strcat(filename, "_lines.dat.txt");
    int64_t fd = _lpython_open(filename, "r");
    free(base_name);
    free(filename);
    if (fd < 0) {
        return;
    }
    uint32_t size = get_file_size(fd);
    if (size == 0) {
        _lpython_close(fd);
        return;
    }
    char *file_contents = _lpython_read(fd, size);
    _lpython_close(fd);
    if (file_contents == NULL) {
        return;
    }

    char s[LCOMPILERS_MAX_STACKTRACE_LENGTH];
    bool address = true;
    uint32_t j = 0;
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
    free(file_contents);
}

char *read_line_from_file(char *filename, uint32_t line_number, int64_t *out_len) {
    if (line_number == 0) {
        *out_len = 0;
        return NULL;
    }

    FILE *fp = fopen(filename, "r");
    if (!fp) {
        *out_len = 0;
        return NULL;
    }

    char *line = NULL;
    size_t cap = 0;
    int64_t read_len = -1;
    uint32_t n = 0;
    while (n < line_number && (read_len = lfortran_getline(&line, &cap, fp)) != -1) n++;
    fclose(fp);

    if (read_len == -1) {
        free(line);
        *out_len = 0;
        return NULL;
    }

    *out_len = read_len; // length includes '\n' if present
    return line;         // caller knows length; can ignore '\0'
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

static inline void print_stacktrace_raw_addresses(struct Stacktrace *d, bool use_colors) {
    if (d->pc_size == 0) {
        return;
    }

    if (use_colors) {
        fprintf(stderr, DIM "note: debug line info unavailable; printing raw addresses\n" S_RESET);
    } else {
        fprintf(stderr, "note: debug line info unavailable; printing raw addresses\n");
    }

    for (int32_t i = (int32_t)d->pc_size - 1; i >= 0; i--) {
        if (use_colors) {
            fprintf(stderr, DIM "  0x%" PRIxPTR "\n" S_RESET, d->pc[i]);
        } else {
            fprintf(stderr, "  0x%" PRIxPTR "\n", d->pc[i]);
        }
    }
}

#endif // HAVE_RUNTIME_STACKTRACE

LFORTRAN_API void print_stacktrace_addresses(char *filename, bool use_colors) {
#ifdef HAVE_RUNTIME_STACKTRACE
    source_filename = filename;
    struct Stacktrace d = get_stacktrace_addresses();
    get_local_address(&d);
    get_local_info_dwarfdump(&d);
    if (d.stack_size == 0) {
        print_stacktrace_raw_addresses(&d, use_colors);
        return;
    }

#ifdef HAVE_LFORTRAN_MACHO
    for (int32_t i = d.local_pc_size-1; i >= 0; i--) {
#else
    for (int32_t i = d.local_pc_size-2; i >= 0; i--) {
#endif
        uint64_t index = bisection(d.addresses, d.stack_size, d.local_pc[i]);
        int64_t line_len;
        char* line = read_line_from_file(source_filename, d.line_numbers[index], &line_len);
        char* trimmed = "";
        if (line != NULL) {
            trimmed = remove_whitespace(line, &line_len);
        }
        if(use_colors) {
            fprintf(stderr, DIM "  File " S_RESET
                BOLD MAGENTA "\"%s\"" C_RESET S_RESET
#ifdef HAVE_LFORTRAN_MACHO
                DIM ", line %lld\n" S_RESET
#else
                DIM ", line %" PRIu64 "\n" S_RESET
#endif
                "    %.*s\n", source_filename, d.line_numbers[index],
                (int)line_len, trimmed);
        } else {
            fprintf(stderr, "  File \"%s\", "
#ifdef HAVE_LFORTRAN_MACHO
                "line %lld\n    %.*s\n",
#else
                "line %" PRIu64 "\n    %.*s\n",
#endif
                source_filename, d.line_numbers[index],
                (int)line_len, trimmed);
        }
        free(line);
#ifdef HAVE_LFORTRAN_MACHO
    }
#else
    }
#endif
#endif // HAVE_RUNTIME_STACKTRACE
}

// << Runtime Stacktrace << ----------------------------------------------------

LFORTRAN_API void _lfortran_get_environment_variable(fchar *name, int32_t name_len, char* receiver) {
    char* C_name = to_c_string(name , name_len); // C-Style String (Null Terminated)
    if (C_name == NULL || ! getenv(C_name)) {
        // When variable doesn't exist, leave receiver unchanged (Fortran standard)
        // For backwards compatibility when status is not checked, set to blank
        receiver[0] = '\0';
        return;
    }
    int32_t len = strlen(getenv(C_name));
    memcpy(receiver, getenv(C_name), len);
    receiver[len] = '\0';
}

LFORTRAN_API int32_t _lfortran_get_environment_variable_status(fchar *name, int32_t name_len) {
    char* C_name = to_c_string(name, name_len); // C-Style String (Null Terminated)
    if (C_name == NULL) {
        return 2; // Error: invalid name
    }
    char *value = getenv(C_name);
    if (value == NULL) {
        return 1; // Variable does not exist
    }
    return 0; // Success: variable exists
}

LFORTRAN_API int32_t _lfortran_get_length_of_environment_variable(fchar *name, int32_t name_len) {
    char* C_name = to_c_string(name, name_len); // C-Style String (Null Terminated)
    if (C_name == NULL) {
        return 0;
    } else {
        char *value = getenv(C_name);
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

// This function assumes that the length of src is at least len, and dest is at least len + 1 (1 for '\0').
static void copy_fchar_to_char(const fchar *src, int64_t len, char *dest) {
    memcpy(dest, src, len);
    dest[len] = '\0';
}

LFORTRAN_API int _lfortran_exec_command(fchar *cmd, int64_t len) {
    char *c_cmd = malloc(sizeof(char) * (len + 1));

    copy_fchar_to_char(cmd, len, c_cmd);
    _lfortran_flush(-1);

    int result = system(c_cmd);
    free(c_cmd);

    // On POSIX systems, system() returns a wait status, not the exit code directly.
    // Use WEXITSTATUS to extract the actual exit code.
#if defined(_WIN32) || defined(_WIN64) || defined(COMPILE_TO_WASM)
    return result;
#else
    if (WIFEXITED(result)) {
        return WEXITSTATUS(result);
    }
    return result;
#endif
}

// ============================================================================
// Namelist I/O Support
// ============================================================================

// Helper function to write a single namelist item value
static void write_nml_value(FILE *fp, const lfortran_nml_item_t *item, int64_t offset) {
    void *ptr = (char*)item->data + offset;
    switch (item->type) {
        case LFORTRAN_NML_INT1:
            fprintf(fp, "%d", *(int8_t*)ptr);
            break;
        case LFORTRAN_NML_INT2:
            fprintf(fp, "%d", *(int16_t*)ptr);
            break;
        case LFORTRAN_NML_INT4:
            fprintf(fp, "%d", *(int32_t*)ptr);
            break;
        case LFORTRAN_NML_INT8:
            fprintf(fp, "%" PRId64, *(int64_t*)ptr);
            break;
        case LFORTRAN_NML_REAL4:
            fprintf(fp, "%.7E", *(float*)ptr);
            break;
        case LFORTRAN_NML_REAL8:
            fprintf(fp, "%.16E", *(double*)ptr);
            break;
        case LFORTRAN_NML_LOGICAL1:
        case LFORTRAN_NML_LOGICAL2:
        case LFORTRAN_NML_LOGICAL4:
        case LFORTRAN_NML_LOGICAL8: {
            bool val = false;
            if (item->type == LFORTRAN_NML_LOGICAL1) val = *(int8_t*)ptr != 0;
            else if (item->type == LFORTRAN_NML_LOGICAL2) val = *(int16_t*)ptr != 0;
            else if (item->type == LFORTRAN_NML_LOGICAL4) val = *(int32_t*)ptr != 0;
            else if (item->type == LFORTRAN_NML_LOGICAL8) val = *(int64_t*)ptr != 0;
            fprintf(fp, "%s", val ? ".true." : ".false.");
            break;
        }
        case LFORTRAN_NML_COMPLEX4: {
            struct _lfortran_complex_32 *c = (struct _lfortran_complex_32*)ptr;
            fprintf(fp, "(%.7E,%.7E)", c->re, c->im);
            break;
        }
        case LFORTRAN_NML_COMPLEX8: {
            struct _lfortran_complex_64 *c = (struct _lfortran_complex_64*)ptr;
            fprintf(fp, "(%.16E,%.16E)", c->re, c->im);
            break;
        }
        case LFORTRAN_NML_CHAR: {
            char *str = (char*)ptr;
            fprintf(fp, "'");
            for (int64_t i = 0; i < item->elem_len; i++) {
                if (str[i] == '\'') {
                    fprintf(fp, "''");
                } else {
                    fputc(str[i], fp);
                }
            }
            fprintf(fp, "'");
            break;
        }
    }
}

// Helper to compute total array size
static int64_t compute_array_size(const lfortran_nml_item_t *item) {
    if (item->rank == 0) return 1;
    int64_t size = 1;
    for (int32_t i = 0; i < item->rank; i++) {
        size *= item->shape[i];
    }
    return size;
}

// Helper to compute element size in bytes
static int64_t get_element_size(const lfortran_nml_item_t *item) {
    switch (item->type) {
        case LFORTRAN_NML_INT1:
        case LFORTRAN_NML_LOGICAL1:
            return 1;
        case LFORTRAN_NML_INT2:
        case LFORTRAN_NML_LOGICAL2:
            return 2;
        case LFORTRAN_NML_INT4:
        case LFORTRAN_NML_LOGICAL4:
        case LFORTRAN_NML_REAL4:
            return 4;
        case LFORTRAN_NML_INT8:
        case LFORTRAN_NML_LOGICAL8:
        case LFORTRAN_NML_REAL8:
            return 8;
        case LFORTRAN_NML_COMPLEX4:
            return sizeof(struct _lfortran_complex_32);
        case LFORTRAN_NML_COMPLEX8:
            return sizeof(struct _lfortran_complex_64);
        case LFORTRAN_NML_CHAR:
            return item->elem_len;
        default:
            return 0;
    }
}

LFORTRAN_API void _lfortran_namelist_write(
    int32_t unit_num,
    int32_t *iostat,
    const lfortran_nml_group_t *group)
{
    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    int delim;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, &delim, NULL, NULL, NULL);

    if (filep && !write_access) {
        if (iostat) {
            *iostat = 5003;
            return;
        } else {
            fprintf(stderr, "Runtime Error: Write not allowed on unit %d\n", unit_num);
            exit(1);
        }
    }

    if (!filep) {
        filep = stdout;
    }

    if (unit_file_bin) {
        if (iostat) {
            *iostat = 5001;
            return;
        } else {
            fprintf(stderr, "Runtime Error: Namelist I/O requires formatted file\n");
            exit(1);
        }
    }

    // Write namelist header
    fprintf(filep, " &%s\n", group->group_name);

    // Write each item
    for (int32_t i = 0; i < group->n_items; i++) {
        const lfortran_nml_item_t *item = &group->items[i];
        fprintf(filep, "  %s=", item->name);

        if (item->rank == 0) {
            // Scalar
            write_nml_value(filep, item, 0);
        } else {
            // Array
            int64_t total_size = compute_array_size(item);
            int64_t elem_size = get_element_size(item);
            for (int64_t j = 0; j < total_size; j++) {
                if (j > 0) fprintf(filep, ",");
                write_nml_value(filep, item, j * elem_size);
            }
        }
        fprintf(filep, "\n");
    }

    // Write namelist terminator
    fprintf(filep, " /\n");

    if (iostat) *iostat = 0;
}

typedef struct {
    FILE *fp;
    const char *data;
    int64_t data_len;
    int64_t offset;
    // For character array internal files
    int64_t elem_len;   // Length of each element
    int64_t n_elems;    // Number of elements
    int64_t elem_idx;   // Current element index
    bool is_array;      // True if reading from character array
} nml_reader_t;

// Helper to read a line from a buffer (internal file)
static int64_t nml_getline_from_buffer(char **line_buf, size_t *line_len,
                                       const char *src, int64_t src_len,
                                       int64_t *offset) {
    if (*offset >= src_len) return -1;

    int64_t start = *offset;
    int64_t end = start;
    while (end < src_len && src[end] != '\n') end++;

    int64_t len = end - start;
    if ((size_t)(len + 2) > *line_len) {
        *line_len = (size_t)(len + 2);
        *line_buf = (char*)realloc(*line_buf, *line_len);
    }

    memcpy(*line_buf, src + start, (size_t)len);
    (*line_buf)[len] = '\0';

    if (end < src_len && src[end] == '\n') end++;
    *offset = end;
    return len;
}

// Helper to read a line from a character array (each element is a record)
static int64_t nml_getline_from_array(char **line_buf, size_t *line_len,
                                      const char *data, int64_t elem_len,
                                      int64_t n_elems, int64_t *elem_idx) {
    if (*elem_idx >= n_elems) return -1;

    // Get pointer to current element
    const char *elem = data + (*elem_idx * elem_len);

    // Find the actual length (trim trailing spaces/nulls)
    int64_t len = elem_len;
    while (len > 0 && (elem[len-1] == ' ' || elem[len-1] == '\0')) {
        len--;
    }

    // Allocate buffer if needed
    if ((size_t)(len + 2) > *line_len) {
        *line_len = (size_t)(len + 2);
        *line_buf = (char*)realloc(*line_buf, *line_len);
    }

    // Copy element data
    memcpy(*line_buf, elem, (size_t)len);
    (*line_buf)[len] = '\0';

    (*elem_idx)++;
    return len;
}

static int64_t nml_getline(nml_reader_t *reader, char **line_buf, size_t *line_len) {
    if (reader->fp) {
        return lfortran_getline(line_buf, line_len, reader->fp);
    }
    if (reader->is_array) {
        return nml_getline_from_array(line_buf, line_len, reader->data,
                                      reader->elem_len, reader->n_elems,
                                      &reader->elem_idx);
    }
    return nml_getline_from_buffer(line_buf, line_len, reader->data,
                                   reader->data_len, &reader->offset);
}

// Helper to skip whitespace and comments
static void skip_whitespace_nml(nml_reader_t *reader, char **line_buf, char **line_ptr,
                                size_t *line_len, int64_t *read_len) {
    while (1) {
        // Skip spaces in current line
        while (*line_ptr && **line_ptr && isspace(**line_ptr)) {
            (*line_ptr)++;
        }

        // Check for comment
        if (*line_ptr && **line_ptr == '!') {
            // Skip rest of line
            *line_ptr = NULL;
        }

        // If we need a new line, read it
        if (!*line_ptr || !**line_ptr) {
            *read_len = nml_getline(reader, line_buf, line_len);
            if (*read_len == -1) {
                *line_ptr = NULL;
                return;
            }
            *line_ptr = *line_buf;
            continue;
        }

        break;
    }
}

// Helper to read a token (word or operator)
static char* read_token_nml(nml_reader_t *reader, char **line_buf, char **line_ptr,
                            size_t *line_len, int64_t *read_len) {
    skip_whitespace_nml(reader, line_buf, line_ptr, line_len, read_len);
    if (!*line_ptr || !**line_ptr) return NULL;

    char *token = (char*)malloc(256);
    int pos = 0;

    // Check for special single-char tokens
    if (**line_ptr == '&' || **line_ptr == '$' || **line_ptr == '/' ||
        **line_ptr == '=' || **line_ptr == ',') {
        token[pos++] = **line_ptr;
        (*line_ptr)++;
        token[pos] = '\0';
        return token;
    }

    // Check for quoted string
    if (**line_ptr == '\'' || **line_ptr == '"') {
        char quote = **line_ptr;
        (*line_ptr)++;
        while (**line_ptr) {
            if (**line_ptr == quote) {
                (*line_ptr)++;
                // Check for doubled quote (escape sequence)
                if (**line_ptr == quote) {
                    // Doubled quote: add one quote and continue
                    token[pos++] = quote;
                    (*line_ptr)++;
                } else {
                    // End of string
                    break;
                }
            } else {
                token[pos++] = **line_ptr;
                (*line_ptr)++;
            }
            if (pos >= 255) break;
        }
        token[pos] = '\0';
        return token;
    }

    // Read alphanumeric token
    // Special handling for parenthesized expressions (e.g., complex numbers)
    if (**line_ptr == '(') {
        // Starts with '(' - read entire parenthesized expression like (1.5,-2.3)
        int paren_depth = 0;
        do {
            if (**line_ptr == '(') paren_depth++;
            if (**line_ptr == ')') paren_depth--;
            token[pos++] = **line_ptr;
            (*line_ptr)++;
            if (pos >= 255) break;
        } while (**line_ptr && paren_depth > 0);
    }

    // Regular token - read alphanumeric, underscore, dot, plus, minus, star
    while (**line_ptr && (isalnum(**line_ptr) || **line_ptr == '_' ||
           **line_ptr == '.' || **line_ptr == '+' || **line_ptr == '-' ||
           **line_ptr == '*' || **line_ptr == '%')) {
        token[pos++] = **line_ptr;
        (*line_ptr)++;
        if (pos >= 255) break;
    }

    // If we hit '(' after reading alphanumeric, continue reading the parenthesized part
    // This handles array subscripts like "arr(2,3)"
    if (**line_ptr == '(') {
        int paren_depth = 0;
        do {
            if (**line_ptr == '(') paren_depth++;
            if (**line_ptr == ')') paren_depth--;
            token[pos++] = **line_ptr;
            (*line_ptr)++;
            if (pos >= 255) break;
        } while (**line_ptr && paren_depth > 0);
    }

    token[pos] = '\0';
    return token;
}

// Helper to check if what follows in the buffer looks like a new variable assignment
// (identifier followed by '='). Returns true if we should stop reading values.
static bool peek_next_is_assignment(char *line_ptr) {
    if (!line_ptr || !*line_ptr) return false;
    
    char *ptr = line_ptr;
    
    // Skip whitespace
    while (*ptr && (*ptr == ' ' || *ptr == '\t')) ptr++;
    
    if (!*ptr) return false;
    
    // Check for terminator - not an assignment
    if (*ptr == '/' || *ptr == '&') return false;
    
    // Must start with alpha or underscore to be an identifier
    if (!isalpha(*ptr) && *ptr != '_') return false;
    
    // Skip the identifier
    while (*ptr && (isalnum(*ptr) || *ptr == '_' || *ptr == '%')) ptr++;
    
    // Skip optional array subscript
    if (*ptr == '(') {
        int depth = 1;
        ptr++;
        while (*ptr && depth > 0) {
            if (*ptr == '(') depth++;
            if (*ptr == ')') depth--;
            ptr++;
        }
    }
    
    // Skip whitespace
    while (*ptr && (*ptr == ' ' || *ptr == '\t')) ptr++;
    
    // Check if followed by '='
    return (*ptr == '=');
}

// Helper to convert string to lowercase
static void to_lowercase(char *str) {
    for (int i = 0; str[i]; i++) {
        str[i] = tolower(str[i]);
    }
}

// Helper to parse a value into a namelist item
static void parse_nml_value(const char *value_str, lfortran_nml_item_t *item, int64_t offset) {
    void *ptr = (char*)item->data + offset;
    switch (item->type) {
        case LFORTRAN_NML_INT1:
            *(int8_t*)ptr = (int8_t)atoi(value_str);
            break;
        case LFORTRAN_NML_INT2:
            *(int16_t*)ptr = (int16_t)atoi(value_str);
            break;
        case LFORTRAN_NML_INT4:
            *(int32_t*)ptr = (int32_t)atoi(value_str);
            break;
        case LFORTRAN_NML_INT8:
            *(int64_t*)ptr = (int64_t)atoll(value_str);
            break;
        case LFORTRAN_NML_REAL4:
            *(float*)ptr = (float)atof(value_str);
            break;
        case LFORTRAN_NML_REAL8:
            *(double*)ptr = atof(value_str);
            break;
        case LFORTRAN_NML_LOGICAL1:
        case LFORTRAN_NML_LOGICAL2:
        case LFORTRAN_NML_LOGICAL4:
        case LFORTRAN_NML_LOGICAL8: {
            char lower[32];
            strncpy(lower, value_str, 31);
            lower[31] = '\0';
            to_lowercase(lower);
            bool val = (strstr(lower, "t") != NULL || strstr(lower, "1") != NULL);
            if (item->type == LFORTRAN_NML_LOGICAL1) *(int8_t*)ptr = val ? 1 : 0;
            else if (item->type == LFORTRAN_NML_LOGICAL2) *(int16_t*)ptr = val ? 1 : 0;
            else if (item->type == LFORTRAN_NML_LOGICAL4) *(int32_t*)ptr = val ? 1 : 0;
            else if (item->type == LFORTRAN_NML_LOGICAL8) *(int64_t*)ptr = val ? 1 : 0;
            break;
        }
        case LFORTRAN_NML_COMPLEX4: {
            struct _lfortran_complex_32 *c = (struct _lfortran_complex_32*)ptr;
            sscanf(value_str, "(%f,%f)", &c->re, &c->im);
            break;
        }
        case LFORTRAN_NML_COMPLEX8: {
            struct _lfortran_complex_64 *c = (struct _lfortran_complex_64*)ptr;
            sscanf(value_str, "(%lf,%lf)", &c->re, &c->im);
            break;
        }
        case LFORTRAN_NML_CHAR: {
            char *str = (char*)ptr;
            strncpy(str, value_str, item->elem_len);
            // Pad with spaces
            for (int64_t i = strlen(value_str); i < item->elem_len; i++) {
                str[i] = ' ';
            }
            break;
        }
    }
}

// Helper to parse repeat count (e.g., "5*0.0" -> count=5, value="0.0")
// Returns 1 if repeat count found, 0 otherwise
static int parse_repeat_count(const char *token, int *count, char *value_part, size_t value_len) {
    const char *star = strchr(token, '*');
    if (!star) {
        return 0; // No repeat count
    }

    // Extract count before '*'
    *count = atoi(token);
    if (*count <= 0) {
        return 0; // Invalid count
    }

    // Extract value after '*'
    const char *value_start = star + 1;
    strncpy(value_part, value_start, value_len - 1);
    value_part[value_len - 1] = '\0';

    return 1;
}

// Helper to parse array subscript (e.g., "arr(2,3)" -> base_name="arr", indices=[2,3])
// Returns number of indices found, or 0 if no subscript
static int parse_array_subscript(const char *name_with_idx, char *base_name,
                                  int *indices, int max_indices) {
    const char *lparen = strchr(name_with_idx, '(');
    if (!lparen) {
        strcpy(base_name, name_with_idx);
        return 0; // No subscript
    }

    // Extract base name
    size_t base_len = lparen - name_with_idx;
    strncpy(base_name, name_with_idx, base_len);
    base_name[base_len] = '\0';

    // Parse indices
    const char *ptr = lparen + 1;
    int n_indices = 0;
    while (*ptr && *ptr != ')' && n_indices < max_indices) {
        indices[n_indices++] = atoi(ptr);
        // Skip to next comma or closing paren
        while (*ptr && *ptr != ',' && *ptr != ')') ptr++;
        if (*ptr == ',') ptr++;
    }

    return n_indices;
}

// Helper to calculate linear offset from multi-dimensional indices
// Returns -1 if indices are out of bounds
static int64_t calculate_array_offset(const int *indices, int n_indices,
                                       const lfortran_nml_item_t *item) {
    if (n_indices == 0 || item->rank == 0) {
        return 0;
    }

    // Validate number of indices matches rank
    if (n_indices != item->rank) {
        return -1;
    }

    // Validate each index is within bounds
    for (int i = 0; i < n_indices; i++) {
        if (indices[i] < 1 || indices[i] > item->shape[i]) {
            return -1;  // Out of bounds
        }
    }

    // Convert Fortran 1-based indices to 0-based offsets
    // Use column-major (Fortran) order: offset = i1 + i2*d1 + i3*d1*d2 + ...
    int64_t offset = indices[0] - 1;
    int64_t multiplier = 1;

    for (int i = 0; i < n_indices - 1 && i < item->rank - 1; i++) {
        multiplier *= item->shape[i];
        offset += (indices[i + 1] - 1) * multiplier;
    }

    return offset;
}

// Helper to collect derived type items (name starts with "base%")
static int collect_derived_items(const lfortran_nml_group_t *group, const char *base_name,
                                 lfortran_nml_item_t **items, int max_items) {
    char prefix[256];
    snprintf(prefix, sizeof(prefix), "%s%%", base_name);
    size_t prefix_len = strlen(prefix);
    int count = 0;
    for (int32_t i = 0; i < group->n_items; i++) {
        if (strncmp(group->items[i].name, prefix, prefix_len) == 0) {
            if (count < max_items) {
                items[count++] = &group->items[i];
            }
        }
    }
    return count;
}

typedef struct {
    lfortran_nml_item_t **items;
    int n_items;
    int item_idx;
    int64_t value_idx;
    int64_t elem_size;
    int64_t total_size;
} nml_value_iter_t;

static void nml_value_iter_init(nml_value_iter_t *it, lfortran_nml_item_t **items,
                                int n_items, int64_t value_idx, int64_t total_size) {
    it->items = items;
    it->n_items = n_items;
    it->item_idx = 0;
    it->value_idx = value_idx;
    it->elem_size = get_element_size(items[0]);
    it->total_size = total_size;
}

static bool nml_value_iter_next_item(nml_value_iter_t *it) {
    while (1) {
        it->item_idx++;
        if (it->item_idx >= it->n_items) {
            return false;
        }
        it->elem_size = get_element_size(it->items[it->item_idx]);
        it->total_size = compute_array_size(it->items[it->item_idx]);
        it->value_idx = 0;
        if (it->total_size > 0) {
            return true;
        }
    }
}

static bool nml_value_iter_advance(nml_value_iter_t *it) {
    it->value_idx++;
    if (it->value_idx < it->total_size) {
        return true;
    }
    return nml_value_iter_next_item(it);
}

// Returns: 0 = success, 1 = error, 2 = terminator found (caller should stop)
static int nml_read_values(nml_reader_t *reader, char **line_buf, char **line_ptr,
                           size_t *line_len, int64_t *read_len, nml_value_iter_t *it,
                           bool enforce_bounds, int32_t *iostat) {
    if (it->total_size == 0 && !nml_value_iter_next_item(it)) {
        return 0;
    }
    while (it->item_idx < it->n_items) {
        bool done = false;
        char *token = read_token_nml(reader, line_buf, line_ptr, line_len, read_len);

        // Check for terminator or end of data
        if (!token || strcmp(token, "/") == 0 || strcmp(token, "&") == 0) {
            if (token) free(token);
            return 2;  // Signal that terminator was found (or end of data)
        }
        if (strcmp(token, ",") == 0) {
            free(token);
            continue;
        }

        // Check for repeat count (e.g., "5*0.0")
        char value_str[256];
        int repeat_count = 1;
        if (parse_repeat_count(token, &repeat_count, value_str, sizeof(value_str))) {
            // Validate repeat count
            if (repeat_count <= 0) {
                free(token);
                free(*line_buf);
                if (iostat) {
                    *iostat = 5014;  // LFORTRAN_IOSTAT_NML_INVALID_REPEAT
                    return 1;
                } else {
                    fprintf(stderr, "Runtime Error: Invalid repeat count %d in namelist\n", repeat_count);
                    exit(1);
                }
            }
            // Check that repeat count doesn't overflow array bounds
            if (enforce_bounds && it->value_idx + repeat_count > it->total_size) {
                free(token);
                free(*line_buf);
                if (iostat) {
                    *iostat = 5015;  // LFORTRAN_IOSTAT_NML_INDEX_OUT_OF_BOUNDS
                    return 1;
                } else {
                    fprintf(stderr, "Runtime Error: Repeat count %d would exceed array bounds in namelist\n",
                            repeat_count);
                    exit(1);
                }
            }
            // Repeat count found - assign same value multiple times
            for (int r = 0; r < repeat_count && it->item_idx < it->n_items; r++) {
                parse_nml_value(value_str, it->items[it->item_idx], it->value_idx * it->elem_size);
                if (!nml_value_iter_advance(it)) {
                    done = true;
                    break;
                }
            }
        } else {
            // No repeat count - single value
            parse_nml_value(token, it->items[it->item_idx], it->value_idx * it->elem_size);
            if (!nml_value_iter_advance(it)) {
                done = true;
            }
        }

        free(token);

        // Check for comma or end
        skip_whitespace_nml(reader, line_buf, line_ptr, line_len, read_len);
        if (*line_ptr && **line_ptr == ',') {
            (*line_ptr)++;
            // After consuming the comma, check if the next thing looks like a new variable assignment
            // If so, stop reading values and let the main loop handle the new variable
            if (peek_next_is_assignment(*line_ptr)) {
                break;
            }
        } else if (*line_ptr && (**line_ptr == '/' || **line_ptr == '&')) {
            return 2;  // Found terminator
        } else {
            // No comma - check if the next thing looks like a new variable assignment
            if (peek_next_is_assignment(*line_ptr)) {
                break;
            }
        }
        if (done) {
            break;
        }
    }

    return 0;
}

static void namelist_read_impl(nml_reader_t *reader, int32_t *iostat, lfortran_nml_group_t *group) {
    char *line_buf = NULL;
    char *line_ptr = NULL;
    size_t line_len = 0;
    int64_t read_len;

    // Read and find group start
    char *token;
    bool found_group = false;
    while ((read_len = nml_getline(reader, &line_buf, &line_len)) != -1) {
        line_ptr = line_buf;
        token = read_token_nml(reader, &line_buf, &line_ptr, &line_len, &read_len);
        if (!token) continue;

        if (strcmp(token, "&") == 0 || strcmp(token, "$") == 0) {
            free(token);
            token = read_token_nml(reader, &line_buf, &line_ptr, &line_len, &read_len);
            if (token) {
                to_lowercase(token);
                if (strcmp(token, group->group_name) == 0) {
                    found_group = true;
                    free(token);
                    break;
                }
                free(token);
            }
        } else {
            free(token);
        }
    }

    if (!found_group) {
        free(line_buf);
        if (iostat) {
            *iostat = 5010;
            return;
        } else {
            fprintf(stderr, "Runtime Error: Namelist group '%s' not found\n", group->group_name);
            exit(1);
        }
    }

    // Parse name=value pairs
    while (1) {
        token = read_token_nml(reader, &line_buf, &line_ptr, &line_len, &read_len);
        if (!token) {
            free(line_buf);
            if (iostat) *iostat = 5011;
            else {
                fprintf(stderr, "Runtime Error: Unexpected end of namelist\n");
                exit(1);
            }
            return;
        }

        // Check for terminator
        if (strcmp(token, "/") == 0 ||
            (strcmp(token, "&") == 0 && (token = read_token_nml(reader, &line_buf, &line_ptr, &line_len, &read_len)) &&
             strcmp(token, "end") == 0)) {
            free(token);
            break;
        }

        // Skip comma separators (they're optional between assignments)
        if (strcmp(token, ",") == 0) {
            free(token);
            continue;
        }

        // Parse name=value (may include array subscript like "arr(2,3)")
        to_lowercase(token);
        char *name = token;

        // Parse array subscript if present
        char base_name[256];
        int indices[16];  // Support up to 16 dimensions
        int n_indices = parse_array_subscript(name, base_name, indices, 16);

        // Find matching item using base name
        lfortran_nml_item_t *item = NULL;
        lfortran_nml_item_t *derived_items[256];
        int derived_count = 0;
        for (int32_t i = 0; i < group->n_items; i++) {
            if (strcmp(base_name, group->items[i].name) == 0) {
                item = &group->items[i];
                break;
            }
        }

        if (!item) {
            derived_count = collect_derived_items(group, base_name, derived_items, 256);
        }

        if (!item && derived_count == 0) {
            free(name);
            free(line_buf);
            if (iostat) {
                *iostat = 5012;
                return;
            } else {
                fprintf(stderr, "Runtime Error: Unknown variable '%s' in namelist\n", base_name);
                exit(1);
            }
        }
        free(name);

        // Expect '='
        token = read_token_nml(reader, &line_buf, &line_ptr, &line_len, &read_len);
        if (!token || strcmp(token, "=") != 0) {
            free(token);
            free(line_buf);
            if (iostat) *iostat = 5013;
            else {
                fprintf(stderr, "Runtime Error: Expected '=' in namelist\n");
                exit(1);
            }
            return;
        }
        free(token);

        // Check for null value immediately after '=' (before reading next token)
        char *peek = line_ptr;
        while (peek && *peek && (*peek == ' ' || *peek == '\t')) {
            peek++;
        }
        bool is_null_value = (!peek || !*peek || *peek == '\n' || *peek == '\r' ||
                              *peek == '\0' || *peek == '/' || *peek == '&' || *peek == ',');

        if (is_null_value) {
            continue;
        }

        if (item) {
            // Read value(s)
            int64_t total_size = compute_array_size(item);

            // If array subscript specified, use that offset; otherwise start at 0
            int64_t value_idx = 0;
            if (n_indices > 0) {
                value_idx = calculate_array_offset(indices, n_indices, item);
                if (value_idx < 0) {
                    // Array index out of bounds
                    free(line_buf);
                    if (iostat) {
                        *iostat = 5015;  // LFORTRAN_IOSTAT_NML_INDEX_OUT_OF_BOUNDS
                        return;
                    } else {
                        fprintf(stderr, "Runtime Error: Array index out of bounds in namelist '%s'\n", base_name);
                        exit(1);
                    }
                }
                total_size = value_idx + 1;  // Only set one element
            }

            lfortran_nml_item_t *items[1] = {item};
            nml_value_iter_t it;
            nml_value_iter_init(&it, items, 1, value_idx, total_size);
            int read_result = nml_read_values(reader, &line_buf, &line_ptr, &line_len, &read_len,
                                &it, true, iostat);
            if (read_result == 1) {
                return;  // Error occurred
            } else if (read_result == 2) {
                break;  // Terminator found, exit main loop
            }
        } else if (derived_count > 0) {
            nml_value_iter_t it;
            nml_value_iter_init(&it, derived_items, derived_count, 0,
                                compute_array_size(derived_items[0]));
            int read_result = nml_read_values(reader, &line_buf, &line_ptr, &line_len, &read_len,
                                &it, false, iostat);
            if (read_result == 1) {
                return;  // Error occurred
            } else if (read_result == 2) {
                break;  // Terminator found, exit main loop
            }
        }
    }

    free(line_buf);
    if (iostat) *iostat = 0;
}

LFORTRAN_API void _lfortran_namelist_read(
    int32_t unit_num,
    int32_t *iostat,
    lfortran_nml_group_t *group)
{
    bool unit_file_bin;
    int access_id;
    bool read_access, write_access;
    FILE* filep = get_file_pointer_from_unit(unit_num, &unit_file_bin, &access_id, &read_access, &write_access, NULL, NULL, NULL, NULL);

    if (filep && !read_access) {
        if (iostat) {
            *iostat = 5002;
            return;
        } else {
            fprintf(stderr, "Runtime Error: Read not allowed on unit %d\n", unit_num);
            exit(1);
        }
    }

    if (!filep) {
        filep = stdin;
    }

    if (unit_file_bin) {
        if (iostat) {
            *iostat = 5001;
            return;
        } else {
            fprintf(stderr, "Runtime Error: Namelist I/O requires formatted file\n");
            exit(1);
        }
    }

    nml_reader_t reader = {0};
    reader.fp = filep;
    namelist_read_impl(&reader, iostat, group);
}

LFORTRAN_API void _lfortran_namelist_read_str(
    const char *data,
    int64_t data_len,
    int32_t *iostat,
    lfortran_nml_group_t *group)
{
    nml_reader_t reader = {0};
    reader.data = data;
    reader.data_len = data_len;
    reader.offset = 0;
    reader.is_array = false;
    namelist_read_impl(&reader, iostat, group);
}

LFORTRAN_API void _lfortran_namelist_read_str_array(
    const char *data,
    int64_t elem_len,
    int64_t n_elems,
    int32_t *iostat,
    lfortran_nml_group_t *group)
{
    nml_reader_t reader = {0};
    reader.data = data;
    reader.elem_len = elem_len;
    reader.n_elems = n_elems;
    reader.elem_idx = 0;
    reader.is_array = true;
    namelist_read_impl(&reader, iostat, group);
}



/*
    Section below contains code for Run-Time Type Information (RTTI)
    implementation used by LFortran.

    For general runtime functions, please add them above this section. 
    Please do not add any unrelated changes below.
*/ 


/*
    =================================================================================
    
        `clang` like implementation of Run-Time Type Information (RTTI) based on
        the standard defined by the Itanium CXX ABI.

        Reference: https://itanium-cxx-abi.github.io/cxx-abi/abi.html#rtti

    =================================================================================
*/


// Compare equality of two type-info objects.
static inline bool
is_equal(const type_info* x, const type_info* y)
{
    return x == y;
}


/*
 * Checks inheritance chain of `dynamic_type` for `dst_type`.
 *
 * `dynamic_type` is the type-info object of the  current runtime type of the selector variable in
 * `select type`.
 * `dst_type` is the type-info object of the class specified in a `class is()` block.
 *
 * Our type-info object in LLVM looks like:
 *
 *     `@_Type_Info_circle = linkonce_odr unnamed_addr constant { i8*, i8* } {
 *         i8* getelementptr inbounds ([7 x i8], [7 x i8]* @_Name_circle, i32 0, i32 0),
 *         i8* bitcast ({ i8* }* @_Type_Info_shape to i8*)
 *      }, align 8`
 *
 * The `__si_class_type_info` struct portrays this layout, with field `__base_type == NULL` for
 * type-info objects of a base class.
 *
 * The search starts from the base class of `dynamic_type`, checks if it is non-null and compares
 * the type-info objects for equality. If they match, the search returns `true`, else continues
 * until a base class is encountered or the type-info objects match.
 *
 * If we reach a base class and it's type-info object does not match `dst_type`, the search stops
 * and we return `false`.
 *
 */
static inline bool
search_dst_type(const struct __si_class_type_info* dynamic_type,
                const struct __si_class_type_info* dst_type)
{    
    // Walk up the inheritance chain
    const __si_class_type_info* base_type = (const __si_class_type_info*) dynamic_type->__base_type;
    // Check each type for equality with `dst_type` until we reach a parent type
    while (base_type != NULL) {
        if (is_equal((const type_info*) base_type, (const type_info*) dst_type)) {
            return true;
        }
        base_type = (const __si_class_type_info*) base_type->__base_type;
    }

    return false;
}


/*
 * Checks whether `dst_type` is the same class as that of `static_ptr` or a parent of it.
 *
 * The function takes a pointer to the selector variable in `select type` - `static_ptr`, extracts
 * its runtime type information - `dynamic_type` from the vtable, and checks it's inheritance chain
 * for the class/type specified in a `class is()`/`type is()` block - `dst_type`.
 *
 * For a `type is()` block, we check the exact type. For `class is()`, the inheritance chain is
 * checked.
 *
 * Our vtable in LLVM looks like:
 *
 *     `@_VTable_circle = linkonce_odr unnamed_addr constant { [3 x i8*] } {
 *          [3 x i8*] [
 *            i8* null,
 *            i8* @_Type_Info_circle,
 *            i8* bitcast (float (%circle*)* @__module_select_type_13_module_circle_area to i8*)
 *          ]
 *      }, align 8`
 *
 * `static_ptr` points to the first virtual function inside the vtable, so `vtable[-1]` is the
 * type-info pointer. Here, it points to `@__module_select_type_13_module_circle_area`, so
 * `vtable[-1]` gives us `@_Type_Info_circle`.
 *
 * Similarly `vtable[-2]` gives us `offset_to_derived` which is currently null in our
 * implementation.
 *
 * We return a pointer to the same object if the type is found, else return a null
 * pointer.
 *
 */
LFORTRAN_API void*
__lfortran_dynamic_cast(const void* static_ptr,
                        const __si_class_type_info* dst_type,
                        bool match_exact_type)
{
    void** vtable = *(void* const*) static_ptr;
    ptrdiff_t offset_to_derived = (ptrdiff_t) (intptr_t) vtable[-2];
    const void* dynamic_ptr = (const char*) static_ptr + offset_to_derived;
    const __si_class_type_info* dynamic_type = (const __si_class_type_info*) vtable[-1];

    const void* dst_ptr = NULL;

    if (is_equal((const type_info*) dynamic_type, (const type_info*) dst_type)) {
        // Types are already equal, exit early.
        dst_ptr = dynamic_ptr;
    } else if (!match_exact_type) {
        // We are handling a `class is()` block and `dst_type` is not equal to the currently
        // allocated type of `static_ptr`. Start searching the inheritance chain of `dynamic_type`
        // for `dst_type`.
        if (search_dst_type((const __si_class_type_info*) dynamic_type, dst_type)) {
            dst_ptr = dynamic_ptr;
        }
    }

    return (void*) dst_ptr;
}
