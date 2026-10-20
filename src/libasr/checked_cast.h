#ifndef LIBASR_CHECKED_CAST_H
#define LIBASR_CHECKED_CAST_H

#include <limits>
#include <type_traits>

#include <libasr/config.h>
#include <libasr/exception.h>

namespace LCompilers {

// Returns true if `value` is exactly representable in the integer type `To`.
template <typename To, typename From>
constexpr bool integer_fits(From value) {
    static_assert(std::is_integral_v<To> && !std::is_same_v<To, bool>,
        "integer_fits requires a non-bool integer target type");
    static_assert(std::is_integral_v<From> && !std::is_same_v<From, bool>,
        "integer_fits requires a non-bool integer source type");
    if constexpr (std::is_signed_v<From> && !std::is_signed_v<To>) {
        return value >= 0 && static_cast<std::make_unsigned_t<From>>(value)
            <= std::numeric_limits<To>::max();
    } else if constexpr (!std::is_signed_v<From> && std::is_signed_v<To>) {
        return value <= static_cast<std::make_unsigned_t<To>>(
            std::numeric_limits<To>::max());
    } else if constexpr (std::is_signed_v<From>) {
        return value >= std::numeric_limits<To>::min()
            && value <= std::numeric_limits<To>::max();
    } else {
        return value <= std::numeric_limits<To>::max();
    }
}

// Converts an integer to a (possibly narrower) integer type. With
// WITH_LFORTRAN_ASSERT (the Debug default), a value that does not fit raises
// an internal compiler error instead of silently truncating or changing sign.
// Otherwise this is a plain static_cast. Use integer_fits directly for values
// that must be validated in every build, such as user input.
template <typename To, typename From>
inline To checked_integer_cast(From value) {
#if defined(WITH_LFORTRAN_ASSERT)
    if (!integer_fits<To>(value)) {
        throw LCompilersException("integer value out of range in conversion");
    }
#endif
    return static_cast<To>(value);
}

// Converts an integer to the bit pattern of the integer type `To`. Values
// representable in either the signed or the unsigned type of that width are
// accepted (e.g. both -1 and 255 for an 8-bit target). As in
// checked_integer_cast, anything wider raises an internal compiler error only
// with WITH_LFORTRAN_ASSERT and is otherwise truncated by static_cast.
template <typename To, typename From>
inline To checked_bit_pattern_cast(From value) {
#if defined(WITH_LFORTRAN_ASSERT)
    if (!integer_fits<std::make_signed_t<To>>(value)
            && !integer_fits<std::make_unsigned_t<To>>(value)) {
        throw LCompilersException("integer value does not fit the target bit width");
    }
#endif
    return static_cast<To>(value);
}

} // namespace LCompilers

#endif // LIBASR_CHECKED_CAST_H
