/*
 * C helpers for bindc_35: ISO_C_BINDING named constant interop
 *
 * Tests intmax_t, int_least*_t, int_fast*_t value passing.
 */
#include <stdint.h>

intmax_t c35_double_intmax(intmax_t x) { return x * 2; }

int_least8_t  c35_double_least8 (int_least8_t  x) { return (int_least8_t) (x * 2); }
int_least16_t c35_double_least16(int_least16_t x) { return (int_least16_t)(x * 2); }
int_least32_t c35_double_least32(int_least32_t x) { return (int_least32_t)(x * 2); }
int_least64_t c35_double_least64(int_least64_t x) { return x * 2; }

int_fast8_t  c35_double_fast8 (int_fast8_t  x) { return (int_fast8_t) (x * 2); }
int_fast16_t c35_double_fast16(int_fast16_t x) { return (int_fast16_t)(x * 2); }
int_fast32_t c35_double_fast32(int_fast32_t x) { return (int_fast32_t)(x * 2); }
int_fast64_t c35_double_fast64(int_fast64_t x) { return x * 2; }
