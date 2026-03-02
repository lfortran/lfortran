#ifndef LFORTRAN_INTRINSICS_H
#define LFORTRAN_INTRINSICS_H

#include <stdarg.h>
#include <complex.h>
#include <inttypes.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

struct _lfortran_complex_32 {
    float re, im;
};

struct _lfortran_complex_64 {
    double re, im;
};

typedef int8_t fchar;

#ifdef _MSC_VER
typedef _Fcomplex float_complex_t;
typedef _Dcomplex double_complex_t;
#else
typedef float _Complex float_complex_t;
typedef double _Complex double_complex_t;
#endif

#ifdef _WIN32
#define LFORTRAN_API __declspec(dllexport)
#elif defined(__linux__)
#define LFORTRAN_API __attribute__((visibility("default")))
#else
#define LFORTRAN_API /* Nothing */
#endif



#ifndef ASSERT
#define ASSERT(cond)                                                           \
    {                                                                          \
        if (!(cond)) {                                                         \
            printf("%s%s", "ASSERT failed: ", __FILE__);                       \
            printf("%s%s", "\nfunction ", __func__);                           \
            printf("%s%d%s", "(), line number ", __LINE__, " at \n");          \
            printf("%s%s", #cond, "\n");                                       \
            exit(1);                                                           \
        }                                                                      \
    }
#endif

#ifndef ASSERT_MSG
#define ASSERT_MSG(cond, fmt, msg)                                                  \
    {                                                                          \
        if (!(cond)) {                                                         \
            printf("%s%s", "ASSERT failed: ", __FILE__);                       \
            printf("%s%s", "\nfunction ", __func__);                           \
            printf("%s%d%s", "(), line number ", __LINE__, " at \n");          \
            printf("%s%s", #cond, "\n");                                       \
            printf("%s", "ERROR MESSAGE: ");                                  \
            printf(fmt, msg);                                                  \
            printf("%s", "\n");                                                \
            exit(1);                                                           \
        }                                                                      \
    }
#endif

LFORTRAN_API void _lfortran_enable_fpe_traps(int32_t trap_mask);
LFORTRAN_API double _lfortran_sum(int n, double *v);
LFORTRAN_API void _lfortran_random_number(int n, double *v);
LFORTRAN_API void _lfortran_init_random_clock();
LFORTRAN_API int _lfortran_init_random_seed(unsigned seed);
LFORTRAN_API double _lfortran_random();
LFORTRAN_API int _lfortran_randrange(int lower, int upper);
LFORTRAN_API int _lfortran_random_int(int lower, int upper);
LFORTRAN_API void _lfortran_printf(const char* format, const fchar* str, uint32_t str_len, const fchar* end, uint32_t end_len);
LFORTRAN_API char* _lcompilers_snprintf(const char* format, ...);
LFORTRAN_API void _lcompilers_print_error(const char* format, ...);
LFORTRAN_API void _lfortran_complex_add_32(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result);
LFORTRAN_API void _lfortran_complex_sub(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result);
LFORTRAN_API void _lfortran_complex_mul(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32 *result);
LFORTRAN_API void _lfortran_complex_div(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32* result);
LFORTRAN_API void _lfortran_complex_pow(struct _lfortran_complex_32* a,
        struct _lfortran_complex_32* b, struct _lfortran_complex_32* result);

LFORTRAN_API void _lfortran_complex_add_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result);
LFORTRAN_API void _lfortran_complex_sub_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result);
LFORTRAN_API void _lfortran_complex_mul_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result);
LFORTRAN_API void _lfortran_complex_div_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result);
LFORTRAN_API void _lfortran_complex_pow_64(struct _lfortran_complex_64* a,
        struct _lfortran_complex_64* b, struct _lfortran_complex_64 *result);
LFORTRAN_API void _lfortran_complex_aimag_32(struct _lfortran_complex_32 *x, float *res);
LFORTRAN_API void _lfortran_complex_aimag_64(struct _lfortran_complex_64 *x, double *res);
LFORTRAN_API float_complex_t _lfortran_csqrt(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zsqrt(double_complex_t x);
LFORTRAN_API float _lfortran_sexp(float x);
LFORTRAN_API double _lfortran_dexp(double x);
LFORTRAN_API float_complex_t _lfortran_cexp(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zexp(double_complex_t x);
LFORTRAN_API float _lfortran_slog(float x);
LFORTRAN_API double _lfortran_dlog(double x);
LFORTRAN_API bool _lfortran_sis_nan(float x);
LFORTRAN_API bool _lfortran_dis_nan(double x);
LFORTRAN_API float_complex_t _lfortran_clog(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zlog(double_complex_t x);
LFORTRAN_API float _lfortran_serf(float x);
LFORTRAN_API double _lfortran_derf(double x);
LFORTRAN_API float _lfortran_serfc(float x);
LFORTRAN_API double _lfortran_derfc(double x);
LFORTRAN_API float _lfortran_slog10(float x);
LFORTRAN_API double _lfortran_dlog10(double x);
LFORTRAN_API float _lfortran_sgamma(float x);
LFORTRAN_API double _lfortran_dgamma(double x);
LFORTRAN_API float _lfortran_slog_gamma(float x);
LFORTRAN_API double _lfortran_dlog_gamma(double x);
LFORTRAN_API float _lfortran_ssin(float x);
LFORTRAN_API double _lfortran_dsin(double x);
LFORTRAN_API float_complex_t _lfortran_csin(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zsin(double_complex_t x);
LFORTRAN_API float _lfortran_scos(float x);
LFORTRAN_API double _lfortran_dcos(double x);
LFORTRAN_API float_complex_t _lfortran_ccos(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zcos(double_complex_t x);
LFORTRAN_API float _lfortran_stan(float x);
LFORTRAN_API double _lfortran_dtan(double x);
LFORTRAN_API float_complex_t _lfortran_ctan(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_ztan(double_complex_t x);
LFORTRAN_API float _lfortran_ssinh(float x);
LFORTRAN_API double _lfortran_dsinh(double x);
LFORTRAN_API float_complex_t _lfortran_csinh(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zsinh(double_complex_t x);
LFORTRAN_API float _lfortran_scosh(float x);
LFORTRAN_API double _lfortran_dcosh(double x);
LFORTRAN_API float_complex_t _lfortran_ccosh(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zcosh(double_complex_t x);
LFORTRAN_API float _lfortran_stanh(float x);
LFORTRAN_API double _lfortran_dtanh(double x);
LFORTRAN_API float_complex_t _lfortran_ctanh(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_ztanh(double_complex_t x);
LFORTRAN_API float _lfortran_sasin(float x);
LFORTRAN_API double _lfortran_dasin(double x);
LFORTRAN_API float_complex_t _lfortran_casin(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zasin(double_complex_t x);
LFORTRAN_API float _lfortran_sacos(float x);
LFORTRAN_API double _lfortran_dacos(double x);
LFORTRAN_API float_complex_t _lfortran_cacos(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zacos(double_complex_t x);
LFORTRAN_API float _lfortran_satan(float x);
LFORTRAN_API double _lfortran_datan(double x);
LFORTRAN_API float_complex_t _lfortran_catan(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zatan(double_complex_t x);
LFORTRAN_API float _lfortran_satan2(float y, float x);
LFORTRAN_API double _lfortran_datan2(double y, double x);
LFORTRAN_API float _lfortran_sasinh(float x);
LFORTRAN_API double _lfortran_dasinh(double x);
LFORTRAN_API float_complex_t _lfortran_casinh(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zasinh(double_complex_t x);
LFORTRAN_API float _lfortran_sacosh(float x);
LFORTRAN_API double _lfortran_dacosh(double x);
LFORTRAN_API float_complex_t _lfortran_cacosh(float_complex_t x);
LFORTRAN_API double _lfortran_dfmod(double x, double y);
LFORTRAN_API double_complex_t _lfortran_zacosh(double_complex_t x);
LFORTRAN_API float _lfortran_satanh(float x);
LFORTRAN_API double _lfortran_datanh(double x);
LFORTRAN_API float_complex_t _lfortran_catanh(float_complex_t x);
LFORTRAN_API double_complex_t _lfortran_zatanh(double_complex_t x);
LFORTRAN_API float _lfortran_strunc(float x);
LFORTRAN_API double _lfortran_dtrunc(double x);
LFORTRAN_API float _lfortran_sfix(float x);
LFORTRAN_API double _lfortran_dfix(double x);
LFORTRAN_API float _lfortran_cphase(float_complex_t x);
LFORTRAN_API double _lfortran_zphase(double_complex_t x);
int str_compare(char *s1, int64_t s1_len, char *s2, int64_t s2_len);
LFORTRAN_API char* _lfortran_float_to_str8(double num);
LFORTRAN_API char* _lfortran_float_to_str4(float num);
LFORTRAN_API char* _lfortran_int_to_str1(int8_t num);
LFORTRAN_API char* _lfortran_int_to_str2(int16_t num);
LFORTRAN_API char* _lfortran_int_to_str4(int32_t num);
LFORTRAN_API char* _lfortran_int_to_str8(int64_t num);
LFORTRAN_API int32_t _lpython_bit_length1(int8_t num);
LFORTRAN_API int32_t _lpython_bit_length2(int16_t num);
LFORTRAN_API int32_t _lpython_bit_length4(int32_t num);
LFORTRAN_API int32_t _lpython_bit_length8(int64_t num);
LFORTRAN_API void _lfortran_strrepeat(char** s, int32_t n, char** dest);
LFORTRAN_API char* _lfortran_strrepeat_c(char* s, int32_t n);
LFORTRAN_API char* _lfortran_strcat(char* s1, int64_t s1_len, char* s2, int64_t s2_len);
LFORTRAN_API int64_t _lfortran_str_len(char* s);
LFORTRAN_API int _lfortran_str_ord(char** s);
LFORTRAN_API int _lfortran_str_ord_c(char* s);
LFORTRAN_API char* _lfortran_str_chr(uint8_t c);
LFORTRAN_API int _lfortran_str_to_int(char** s);
LFORTRAN_API void* _lfortran_malloc(int64_t size);
LFORTRAN_API void* _lfortran_string_malloc(int64_t length);
LFORTRAN_API void _lfortran_memset(void* s, int32_t c, int32_t size);
LFORTRAN_API int8_t* _lfortran_realloc(int8_t* ptr, int64_t size);
LFORTRAN_API int8_t* _lfortran_calloc(int32_t count, int32_t size);
LFORTRAN_API void _lfortran_free(char* ptr);
LFORTRAN_API char* _lfortran_str_item(char* s, int64_t s_len, int64_t idx);
LFORTRAN_API char* _lfortran_str_slice_fortran(char* s, int64_t start /*1-Based index*/, int64_t end);
LFORTRAN_API char* _lfortran_str_slice(char* s, int64_t s_len, int64_t idx1, int64_t idx2, int64_t step,
                        bool idx1_present, bool idx2_present);
LFORTRAN_API char* _lfortran_str_slice_assign(char* s, int64_t s_len, char *r, int64_t r_len,
         int32_t idx1, int32_t idx2, int32_t step, bool idx1_present, bool idx2_present);
LFORTRAN_API int32_t _lfortran_mvbits32(int32_t from, int32_t frompos,
                                        int32_t len, int32_t to, int32_t topos);
LFORTRAN_API int64_t _lfortran_mvbits64(int64_t from, int32_t frompos,
                                        int32_t len, int64_t to, int32_t topos);
LFORTRAN_API int32_t _lfortran_ibits32(int32_t i, int32_t pos, int32_t len);
LFORTRAN_API int64_t _lfortran_ibits64(int64_t i, int32_t pos, int32_t len);
LFORTRAN_API double _lfortran_d_cpu_time();
LFORTRAN_API float _lfortran_s_cpu_time();
LFORTRAN_API void _lfortran_i32sys_clock(
        int32_t *count, int32_t *rate, int32_t *max);
LFORTRAN_API void _lfortran_i64sys_clock(
        uint64_t *count, int64_t *rate, int64_t *max);
LFORTRAN_API void _lfortran_i64r64sys_clock(
        uint64_t *count, double *rate, int64_t *max);
LFORTRAN_API void _lfortran_date(char* result);
LFORTRAN_API void _lfortran_time(char* result);
LFORTRAN_API void _lfortran_zone(char* result);
LFORTRAN_API int32_t _lfortran_values(int32_t n);
LFORTRAN_API float _lfortran_sp_rand_num();
LFORTRAN_API double _lfortran_dp_rand_num();
LFORTRAN_API int64_t _lpython_open(char *path, char *flags);
LFORTRAN_API int64_t _lfortran_open(int32_t unit_num,
    char* f_name, int64_t f_name_len,
    char* status, int64_t status_len,
    char* form, int64_t form_len,
    char* access, int64_t access_len,
    char* iomsg, int64_t iomsg_len,
    int32_t *iostat,
    char* action, int64_t action_len,
    char* delim, int64_t delim_len,
    char* position, int64_t position_len,
    char* blank, int64_t blank_len,
    char* encoding, int64_t encoding_len,
    int32_t *recl,
    char* sign, int64_t sign_len);
LFORTRAN_API void _lfortran_flush(int32_t unit_num);
LFORTRAN_API void _lfortran_abort();
LFORTRAN_API void _lfortran_sleep(int32_t seconds);
LFORTRAN_API void _lfortran_inquire(
    const fchar* f_name_data, int64_t f_name_len,
    bool *exists, int32_t unit_num,
    bool *opened, int32_t *size, int32_t *pos,
    char *write, int64_t write_len,
    char *read, int64_t read_len,
    char *readwrite, int64_t readwrite_len,
    char *access, int64_t access_len,
    char *name, int64_t name_len,
    char *blank, int64_t blank_len,
    int32_t *recl,
    int32_t *number, bool *named,
    char *sequential, int64_t sequential_len,
    char *direct, int64_t direct_len,
    char *form, int64_t form_len,
    char *formatted, int64_t formatted_len,
    char *unformatted, int64_t unformatted_len,
    int32_t *iostat, int32_t *nextrec
);
LFORTRAN_API void _lfortran_seek_record(int32_t unit_num, int32_t rec, int32_t *iostat);
LFORTRAN_API void _lfortran_formatted_read(int32_t unit_num, int32_t* iostat, int32_t* chunk, fchar* advance, int64_t advance_length, fchar* fmt, int64_t fmt_len, int32_t no_of_args, ...);
LFORTRAN_API char* _lpython_read(int64_t fd, int64_t n);
LFORTRAN_API void _lfortran_read_int16(int16_t *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_int32(int32_t *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_int64(int64_t *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_logical(bool *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_logical(bool *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_int8(int8_t *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_int16(int16_t *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_int32(int32_t *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_int64(int64_t *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_double(double *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_float(float *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_complex_float(struct _lfortran_complex_32 *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_complex_double(struct _lfortran_complex_64 *p, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_float(float *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_double(double *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_complex_float(struct _lfortran_complex_32 *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_complex_double(struct _lfortran_complex_64 *p, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_array_char(char *p, int64_t length, int array_size, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_read_char(char **p, int64_t p_len, int32_t unit_num, int32_t *iostat);
LFORTRAN_API void _lfortran_string_write(char **str_holder, bool is_allocatable, bool is_deferred, 
        int64_t* len, int32_t* iostat, const char* format,
        int64_t format_len, ...);
LFORTRAN_API void _lfortran_file_write(int32_t unit_num, int32_t* iostat, const char* format_data, int64_t format_len, ...);
LFORTRAN_API void _lfortran_string_read_i32(char *str, int64_t len, char *format, int32_t *i, int32_t *iostat);
LFORTRAN_API void _lfortran_string_read_i32_array(char *str, int64_t len, char *format, int32_t *arr);
LFORTRAN_API void _lfortran_string_read_i64(char *str, int64_t len, char *format, int64_t *i, int32_t *iostat);
LFORTRAN_API void _lfortran_string_read_i64_array(char *str, int64_t len, char *format, int64_t *arr);
LFORTRAN_API void _lfortran_string_read_f32(char *str, int64_t len, char *format, float *f, int32_t *iostat);
LFORTRAN_API void _lfortran_string_read_f32_array(char *str, int64_t len, char *format, float *arr);
LFORTRAN_API void _lfortran_string_read_f64(char *str, int64_t len, char *format, double *f, int32_t *iostat);
LFORTRAN_API void _lfortran_string_read_f64_array(char *str, int64_t len, char *format, double *arr);
LFORTRAN_API void _lfortran_string_read_str(char *src_data, int64_t src_len, char *dest_data, int64_t dest_len);
LFORTRAN_API void _lfortran_string_read_str_array(char *str, int64_t len, char *format, char **arr);
LFORTRAN_API void _lfortran_string_read_bool(char *str, int64_t len, char *format, int32_t *i, int32_t *iostat);
LFORTRAN_API void _lfortran_string_read_c32(char *str, int64_t len, char *format, struct _lfortran_complex_32 *c, int32_t *iostat);
LFORTRAN_API void _lfortran_string_read_c64(char *str, int64_t len, char *format, struct _lfortran_complex_64 *c, int32_t *iostat);
LFORTRAN_API void _lfortran_empty_read(int32_t unit_num, int32_t* iostat);
LFORTRAN_API void _lfortran_file_seek(int32_t unit_num, int64_t pos, int32_t* iostat);
LFORTRAN_API void _lpython_close(int64_t fd);
LFORTRAN_API void _lfortran_close(int32_t unit_num, char* status, int64_t status_len, int32_t* iostat);
LFORTRAN_API int32_t _lfortran_ichar(char *c);
LFORTRAN_API int32_t _lfortran_iachar(char *c);
LFORTRAN_API void _lpython_set_argv(int32_t argc_1, char *argv_1[]);
LFORTRAN_API void _lpython_free_argv();
LFORTRAN_API void _lfortran_set_use_runtime_colors(int use_colors);
LFORTRAN_API int32_t _lpython_get_argc();
LFORTRAN_API char *_lpython_get_argv(int32_t index);
LFORTRAN_API void _lpython_call_initial_functions(int32_t argc_1, char *argv_1[]);
LFORTRAN_API void print_stacktrace_addresses(char *filename, bool use_colors);
LFORTRAN_API char *_lfortran_get_env_variable(char *name);
LFORTRAN_API void _lfortran_get_environment_variable(fchar *name, int32_t name_len, char* receiver);
LFORTRAN_API int32_t _lfortran_get_environment_variable_status(fchar *name, int32_t name_len);
LFORTRAN_API int _lfortran_exec_command(fchar *cmd, int64_t len);
LFORTRAN_API void _lfortran_get_command_command(char* receiver);
LFORTRAN_API int32_t _lfortran_get_command_length();

// Namelist I/O support
typedef enum {
    LFORTRAN_NML_INT1,
    LFORTRAN_NML_INT2,
    LFORTRAN_NML_INT4,
    LFORTRAN_NML_INT8,
    LFORTRAN_NML_REAL4,
    LFORTRAN_NML_REAL8,
    LFORTRAN_NML_LOGICAL1,
    LFORTRAN_NML_LOGICAL2,
    LFORTRAN_NML_LOGICAL4,
    LFORTRAN_NML_LOGICAL8,
    LFORTRAN_NML_COMPLEX4,
    LFORTRAN_NML_COMPLEX8,
    LFORTRAN_NML_CHAR
} lfortran_nml_type_t;

typedef struct {
    const char *name;          // lower-case, null-terminated
    lfortran_nml_type_t type;
    int32_t rank;              // 0 for scalar
    int64_t elem_len;          // for character (len), else 0
    void *data;                // scalar ptr or base address of array
    const int64_t *shape;      // rank-sized array of extents (Fortran order)
} lfortran_nml_item_t;

typedef struct {
    const char *group_name;    // lower-case, null-terminated
    int32_t n_items;
    lfortran_nml_item_t *items;
} lfortran_nml_group_t;

LFORTRAN_API void _lfortran_namelist_write(
    int32_t unit_num,
    int32_t *iostat,
    const lfortran_nml_group_t *group
);

LFORTRAN_API void _lfortran_namelist_read(
    int32_t unit_num,
    int32_t *iostat,
    lfortran_nml_group_t *group
);

LFORTRAN_API void _lfortran_namelist_read_str(
    const char *data,
    int64_t data_len,
    int32_t *iostat,
    lfortran_nml_group_t *group
);

LFORTRAN_API void _lfortran_namelist_read_str_array(
    const char *data,
    int64_t elem_len,
    int64_t n_elems,
    int32_t *iostat,
    lfortran_nml_group_t *group
);

typedef struct Span {
    const char *filename;
    uint32_t start_l, start_c;
    uint32_t last_l, last_c;
} Span;

typedef struct Label {
    bool primary;
    char *message;
    Span *spans;
    uint32_t n_spans;
} Label;

LFORTRAN_API void _lcompilers_runtime_error(Label *labels, uint32_t n_labels, const char* format, ...);
LFORTRAN_API void _lcompilers_runtime_warning(Label *labels, uint32_t n_labels, const char* format, ...);

// IOSTAT error codes for namelist operations
#define LFORTRAN_IOSTAT_NML_FORMATTED_FILE_REQUIRED 5001  // Binary file (formatted required)
#define LFORTRAN_IOSTAT_NML_READ_NOT_ALLOWED        5002  // Read access not allowed
#define LFORTRAN_IOSTAT_NML_WRITE_NOT_ALLOWED       5003  // Write access not allowed
#define LFORTRAN_IOSTAT_NML_GROUP_NOT_FOUND         5010  // Namelist group not found
#define LFORTRAN_IOSTAT_NML_UNEXPECTED_END          5011  // Unexpected end of namelist
#define LFORTRAN_IOSTAT_NML_UNKNOWN_VARIABLE        5012  // Unknown variable in namelist
#define LFORTRAN_IOSTAT_NML_EXPECTED_EQUALS         5013  // Expected '=' after variable name
#define LFORTRAN_IOSTAT_NML_INVALID_REPEAT          5014  // Invalid repeat count
#define LFORTRAN_IOSTAT_NML_INDEX_OUT_OF_BOUNDS     5015  // Array index out of bounds
#define LFORTRAN_IOSTAT_NML_TYPE_MISMATCH           5016  // Type mismatch during read
#define LFORTRAN_IOSTAT_NML_INVALID_COMPLEX         5017  // Invalid complex number format
#define LFORTRAN_IOSTAT_NML_PARSE_ERROR             5018  // General parsing error

LFORTRAN_API char* _lcompilers_string_format_fortran(const char* format, int64_t format_len, const char* serialization_string, int64_t *result_size, int32_t array_sizes_cnt, int32_t string_lengths_cnt, ...);
void lfortran_error(const char *message);


typedef struct type_info {
    char* name;  // Pointer to a null-terminated string representing the type name
    uint8_t* size;   // Size of the dynamic type
} type_info;

typedef struct __si_class_type_info {
    type_info base;                               // Inherits from type_info
    const struct __class_type_info* __base_type;  // Pointer to base class' type info
} __si_class_type_info;

static inline bool
is_equal(const struct type_info* x, const struct type_info* y);

static inline bool
search_dst_type(const struct __si_class_type_info* dynamic_type,
                const struct __si_class_type_info* dst_type);

LFORTRAN_API void*
__lfortran_dynamic_cast(const void* static_ptr,
                        const struct __si_class_type_info* dst_type,
                        bool match_exact_type);


#ifdef __cplusplus
}
#endif

#endif
