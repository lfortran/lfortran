module lfortran_intrinsic_ieee_arithmetic
    implicit none

    type ieee_class_type
        integer :: value
    end type

    ! IEEE class constants - initialize with distinct values
    type(ieee_class_type), parameter :: ieee_signaling_nan = ieee_class_type(1)
    type(ieee_class_type), parameter :: ieee_quiet_nan = ieee_class_type(2)
    type(ieee_class_type), parameter :: ieee_negative_inf = ieee_class_type(3)
    type(ieee_class_type), parameter :: ieee_negative_normal = ieee_class_type(4)
    type(ieee_class_type), parameter :: ieee_negative_denormal = ieee_class_type(5)
    type(ieee_class_type), parameter :: ieee_negative_zero = ieee_class_type(6)
    type(ieee_class_type), parameter :: ieee_positive_zero = ieee_class_type(7)
    type(ieee_class_type), parameter :: ieee_positive_denormal = ieee_class_type(8)
    type(ieee_class_type), parameter :: ieee_positive_normal = ieee_class_type(9)
    type(ieee_class_type), parameter :: ieee_positive_inf = ieee_class_type(10)

    type ieee_round_type
        integer :: value
    end type

    ! IEEE rounding mode constants
    type(ieee_round_type), parameter :: ieee_nearest = ieee_round_type(0)
    type(ieee_round_type), parameter :: ieee_to_zero = ieee_round_type(1)
    type(ieee_round_type), parameter :: ieee_up = ieee_round_type(2)
    type(ieee_round_type), parameter :: ieee_down = ieee_round_type(3)
    type(ieee_round_type), parameter :: ieee_other = ieee_round_type(4)

    ! IEEE exception flags
    type ieee_flag_type
        integer :: value
    end type

    type(ieee_flag_type), parameter :: ieee_invalid = ieee_flag_type(1)
    type(ieee_flag_type), parameter :: ieee_overflow = ieee_flag_type(2)
    type(ieee_flag_type), parameter :: ieee_divide_by_zero = ieee_flag_type(3)
    type(ieee_flag_type), parameter :: ieee_underflow = ieee_flag_type(4)
    type(ieee_flag_type), parameter :: ieee_inexact = ieee_flag_type(5)

    ! IEEE status type (for saving/restoring exception state)
    type ieee_status_type
        private
        integer :: flags(5)
    end type

    interface ieee_class
        module procedure spieee_class, dpieee_class
    end interface

    interface ieee_value
        module procedure spieee_value, dpieee_value
    end interface

    interface ieee_is_nan
        module procedure spieee_is_nan, dpieee_is_nan
    end interface

    interface ieee_is_finite
        module procedure spieee_is_finite, dpieee_is_finite
    end interface

    interface ieee_is_negative
        module procedure spieee_is_negative, dpieee_is_negative
    end interface

    interface ieee_copy_sign
        module procedure spieee_copy_sign, dpieee_copy_sign
    end interface

    interface ieee_support_datatype
        module procedure spieee_support_datatype, dpieee_support_datatype
    end interface
    
    interface ieee_is_normal
        module procedure spieee_is_normal, dpieee_is_normal
    end interface

    interface ieee_unordered
        module procedure spieee_unordered, dpieee_unordered
    end interface

    interface ieee_logb
        module procedure spieee_logb, dpieee_logb
    end interface
    
    interface ieee_rem
        module procedure spieee_rem, dpieee_rem
    end interface

    interface ieee_support_inf
        module procedure spieee_support_inf, dpieee_support_inf
    end interface

    interface ieee_support_nan
        module procedure spieee_support_nan, dpieee_support_nan
    end interface

    ! New functions for complete IEEE support
    interface ieee_next_after
        module procedure spieee_next_after, dpieee_next_after
    end interface

    interface ieee_next_down
        module procedure spieee_next_down, dpieee_next_down
    end interface

    interface ieee_next_up
        module procedure spieee_next_up, dpieee_next_up
    end interface

    interface ieee_rint
        module procedure spieee_rint, dpieee_rint
    end interface

    interface ieee_scalb
        module procedure spieee_scalb, dpieee_scalb
    end interface

    interface ieee_int
        module procedure spieee_int, dpieee_int
    end interface

    interface ieee_real
        module procedure spieee_real_i32, dpieee_real_i32
        module procedure spieee_real_i64, dpieee_real_i64
    end interface

    interface ieee_support_denormal
        module procedure spieee_support_denormal, dpieee_support_denormal
    end interface

    interface ieee_support_divide
        module procedure spieee_support_divide, dpieee_support_divide
    end interface

    interface ieee_support_sqrt
        module procedure spieee_support_sqrt, dpieee_support_sqrt
    end interface

    interface ieee_support_standard
        module procedure spieee_support_standard, dpieee_support_standard
    end interface

    interface ieee_support_io
        module procedure spieee_support_io, dpieee_support_io
    end interface

    interface ieee_support_rounding
        module procedure spieee_support_rounding, dpieee_support_rounding
    end interface

    contains

    elemental function spieee_class(x) result(y)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        type(ieee_class_type) :: y
    end function

    elemental function dpieee_class(x) result(y)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        type(ieee_class_type) :: y
    end function

    elemental elemental function spieee_value(x, cls) result(y)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        type(ieee_class_type), intent(in) :: cls
        real(real32) :: y
    end function

    elemental function dpieee_value(x, cls) result(y)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        type(ieee_class_type), intent(in) :: cls
        real(real64) :: y
    end function

    elemental function spieee_is_nan(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        logical :: r
        interface
        pure logical function c_rsp_is_nan(x) bind(c, name="_lfortran_sis_nan")
            import :: real32
            real(real32), intent(in), value :: x
            end function
        end interface
        r = c_rsp_is_nan(x)
    end function

    elemental function dpieee_is_nan(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        interface
        pure logical function c_rdp_is_nan(x) bind(c, name="_lfortran_dis_nan")
            import :: real64
            real(real64), intent(in), value :: x
            end function
        end interface
        r = c_rdp_is_nan(x)
    end function

    elemental function spieee_is_finite(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        logical :: r
        r = x == x .and. abs(x) < huge(x)
    end function

    elemental function dpieee_is_finite(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        r = x == x .and. abs(x) < huge(x)
    end function

    elemental function spieee_is_negative(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        logical :: r
        ! Check if x is negative: includes -0.0
        ! Use sign() to check the sign bit: sign(1.0, x) returns 1.0 or -1.0 based on sign of x
        r = sign(1.0_real32, x) < 0.0_real32
    end function

    elemental function dpieee_is_negative(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        ! Check if x is negative: includes -0.0
        ! Use sign() to check the sign bit: sign(1.0, x) returns 1.0 or -1.0 based on sign of x
        r = sign(1.0_real64, x) < 0.0_real64
    end function

    elemental function spieee_copy_sign(x, y) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x, y
        real(real32) :: r
        r = abs(x) * sign(1.0_real32, y)
    end function

    elemental function dpieee_copy_sign(x, y) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x, y
        real(real64) :: r
        r = abs(x) * sign(1.0_real64, y)
    end function

    elemental function spieee_support_datatype(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        logical :: r
        r = .true.
    end function

    elemental function dpieee_support_datatype(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        r = .true.
    end function

    elemental function spieee_is_normal(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        logical :: r
        r = abs(x) >= tiny(x) .and. abs(x) <= huge(x) .and. x == x
    end function

    elemental function dpieee_is_normal(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        r = abs(x) >= tiny(x) .and. abs(x) <= huge(x) .and. x == x
    end function

    elemental function spieee_unordered(x, y) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x, y
        logical :: r
        r = x /= x .or. y /= y
    end function

    elemental function dpieee_unordered(x, y) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x, y
        logical :: r
        r = x /= x .or. y /= y
    end function

    elemental function spieee_logb(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        real(real32) :: r
        interface
            pure real(real32) function c_logbf(x) bind(c, name="logbf")
                import :: real32
                real(real32), intent(in), value :: x
            end function
        end interface
        r = c_logbf(x)
    end function

    elemental function dpieee_logb(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        real(real64) :: r
        interface
            pure real(real64) function c_logb(x) bind(c, name="logb")
                import :: real64
                real(real64), intent(in), value :: x
            end function
        end interface
        r = c_logb(x)
    end function

    elemental function spieee_rem(x, y) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x, y
        real(real32) :: r
        interface
            pure real(real32) function c_remainderf(x, y) bind(c, name="remainderf")
                import :: real32
                real(real32), intent(in), value :: x, y
            end function
        end interface
        r = c_remainderf(x, y)
    end function

    elemental function dpieee_rem(x, y) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x, y
        real(real64) :: r
        interface
            pure real(real64) function c_remainder(x, y) bind(c, name="remainder")
                import :: real64
                real(real64), intent(in), value :: x, y
            end function
        end interface
        r = c_remainder(x, y)
    end function

    elemental function spieee_support_inf(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        logical :: r
        r = .true.
    end function

    elemental function dpieee_support_inf(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        r = .true.
    end function

    elemental function spieee_support_nan(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        logical :: r
        r = .true.
    end function

    elemental function dpieee_support_nan(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        r = .true.
    end function

    ! ========================================================================
    ! New IEEE Arithmetic Functions
    ! ========================================================================

    ! IEEE_NEXT_AFTER: Next representable value in direction of y
    elemental function spieee_next_after(x, y) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x, y
        real(real32) :: r
        interface
            pure real(real32) function c_nextafterf(x, y) bind(c, name="nextafterf")
                import :: real32
                real(real32), intent(in), value :: x, y
            end function
        end interface
        r = c_nextafterf(x, y)
    end function

    elemental function dpieee_next_after(x, y) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x, y
        real(real64) :: r
        interface
            pure real(real64) function c_nextafter(x, y) bind(c, name="nextafter")
                import :: real64
                real(real64), intent(in), value :: x, y
            end function
        end interface
        r = c_nextafter(x, y)
    end function

    ! IEEE_NEXT_DOWN: Next representable value toward negative infinity
    elemental function spieee_next_down(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        real(real32) :: r
        real(real32) :: neg_inf
        interface
            pure real(real32) function c_nextafterf(x, y) bind(c, name="nextafterf")
                import :: real32
                real(real32), intent(in), value :: x, y
            end function
        end interface
        ! Generate negative infinity: -1.0 / 0.0
        neg_inf = -1.0_real32 / 0.0_real32
        r = c_nextafterf(x, neg_inf)
    end function

    elemental function dpieee_next_down(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        real(real64) :: r
        real(real64) :: neg_inf
        interface
            pure real(real64) function c_nextafter(x, y) bind(c, name="nextafter")
                import :: real64
                real(real64), intent(in), value :: x, y
            end function
        end interface
        ! Generate negative infinity: -1.0 / 0.0
        neg_inf = -1.0_real64 / 0.0_real64
        r = c_nextafter(x, neg_inf)
    end function

    ! IEEE_NEXT_UP: Next representable value toward positive infinity
    elemental function spieee_next_up(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        real(real32) :: r
        real(real32) :: pos_inf
        interface
            pure real(real32) function c_nextafterf(x, y) bind(c, name="nextafterf")
                import :: real32
                real(real32), intent(in), value :: x, y
            end function
        end interface
        ! Generate positive infinity: 1.0 / 0.0
        pos_inf = 1.0_real32 / 0.0_real32
        r = c_nextafterf(x, pos_inf)
    end function

    elemental function dpieee_next_up(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        real(real64) :: r
        real(real64) :: pos_inf
        interface
            pure real(real64) function c_nextafter(x, y) bind(c, name="nextafter")
                import :: real64
                real(real64), intent(in), value :: x, y
            end function
        end interface
        ! Generate positive infinity: 1.0 / 0.0
        pos_inf = 1.0_real64 / 0.0_real64
        r = c_nextafter(x, pos_inf)
    end function

    ! IEEE_RINT: Round to nearest integer using current rounding mode
    elemental function spieee_rint(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        real(real32) :: r
        ! TODO: Implement with rounding mode support
        ! For now, use round to nearest even (default)
        interface
        pure real(real32) function c_rintf(x) bind(c, name="rintf")
            import :: real32
            real(real32), intent(in), value :: x
            end function
        end interface
        r = c_rintf(x)
    end function

    elemental function dpieee_rint(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        real(real64) :: r
        ! TODO: Implement with rounding mode support
        interface
        pure real(real64) function c_rint(x) bind(c, name="rint")
            import :: real64
            real(real64), intent(in), value :: x
            end function
        end interface
        r = c_rint(x)
    end function

    ! IEEE_SCALB: Scale by power of radix (x * 2^i)
    elemental function spieee_scalb(x, i) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        integer, intent(in) :: i
        real(real32) :: r
        interface
        pure real(real32) function c_scalbnf(x, n) bind(c, name="scalbnf")
            import :: real32
            real(real32), intent(in), value :: x
            integer, intent(in), value :: n
            end function
        end interface
        r = c_scalbnf(x, i)
    end function

    elemental function dpieee_scalb(x, i) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        integer, intent(in) :: i
        real(real64) :: r
        interface
        pure real(real64) function c_scalbn(x, n) bind(c, name="scalbn")
            import :: real64
            real(real64), intent(in), value :: x
            integer, intent(in), value :: n
            end function
        end interface
        r = c_scalbn(x, i)
    end function

    ! IEEE_INT: Convert real to integer (Fortran 2018)
    elemental function spieee_int(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        integer :: r
        ! TODO: Accept round parameter for rounding mode
        r = nint(x)
    end function

    elemental function dpieee_int(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        integer :: r
        ! TODO: Accept round parameter for rounding mode
        r = nint(x)
    end function

    ! IEEE_REAL: Convert integer to real (Fortran 2018)
    elemental function spieee_real_i32(x, y) result(r)
        use iso_fortran_env, only: real32
        integer, intent(in) :: x
        real(real32), intent(in) :: y  ! for kind only
        real(real32) :: r
        r = real(x, kind=real32)
    end function

    elemental function dpieee_real_i32(x, y) result(r)
        use iso_fortran_env, only: real64
        integer, intent(in) :: x
        real(real64), intent(in) :: y  ! for kind only
        real(real64) :: r
        r = real(x, kind=real64)
    end function

    elemental function spieee_real_i64(x, y) result(r)
        use iso_fortran_env, only: real32, int64
        integer(int64), intent(in) :: x
        real(real32), intent(in) :: y  ! for kind only
        real(real32) :: r
        r = real(x, kind=real32)
    end function

    elemental function dpieee_real_i64(x, y) result(r)
        use iso_fortran_env, only: real64, int64
        integer(int64), intent(in) :: x
        real(real64), intent(in) :: y  ! for kind only
        real(real64) :: r
        r = real(x, kind=real64)
    end function

    ! ========================================================================
    ! IEEE Support Inquiry Functions
    ! ========================================================================

    ! IEEE_SUPPORT_DENORMAL
    pure function spieee_support_denormal(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in), optional :: x
        logical :: r
        ! Most modern hardware supports denormals
        r = .true.
    end function

    pure function dpieee_support_denormal(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    ! IEEE_SUPPORT_DIVIDE
    pure function spieee_support_divide(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    pure function dpieee_support_divide(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    ! IEEE_SUPPORT_SQRT
    pure function spieee_support_sqrt(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    pure function dpieee_support_sqrt(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    ! IEEE_SUPPORT_STANDARD
    pure function spieee_support_standard(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in), optional :: x
        logical :: r
        ! Return true if full IEEE 754 standard is supported
        r = .true.
    end function

    pure function dpieee_support_standard(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    ! IEEE_SUPPORT_IO
    pure function spieee_support_io(x) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in), optional :: x
        logical :: r
        ! Formatted I/O of IEEE values (Inf, NaN)
        r = .true.
    end function

    pure function dpieee_support_io(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    ! IEEE_SUPPORT_ROUNDING
    pure function spieee_support_rounding(round_value, x) result(r)
        use iso_fortran_env, only: real32
        type(ieee_round_type), intent(in) :: round_value
        real(real32), intent(in), optional :: x
        logical :: r
        ! TODO: Query actual hardware/system support
        ! For now, assume all rounding modes are supported
        r = .true.
    end function

    pure function dpieee_support_rounding(round_value, x) result(r)
        use iso_fortran_env, only: real64
        type(ieee_round_type), intent(in) :: round_value
        real(real64), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    ! ========================================================================
    ! Rounding Mode Functions (Subroutines)
    ! ========================================================================

    ! IEEE_GET_ROUNDING_MODE
    subroutine ieee_get_rounding_mode(round_value)
        type(ieee_round_type), intent(out) :: round_value
        ! TODO: Implement using C binding to get actual rounding mode
        ! For now, return default (round to nearest)
        round_value = ieee_nearest
    end subroutine

    ! IEEE_SET_ROUNDING_MODE
    subroutine ieee_set_rounding_mode(round_value)
        type(ieee_round_type), intent(in) :: round_value
        ! TODO: Implement using C binding to set actual rounding mode
        ! This requires calling fesetround() from C
    end subroutine

    ! ========================================================================
    ! Exception Handling Functions (Subroutines)
    ! ========================================================================

    ! IEEE_GET_FLAG
    elemental subroutine ieee_get_flag(flag, flag_value)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(out) :: flag_value
        ! TODO: Implement using C binding to get actual exception flags
        ! For now, always return false (no exceptions)
        flag_value = .false.
    end subroutine

    ! IEEE_SET_FLAG
    elemental subroutine ieee_set_flag(flag, flag_value)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(in) :: flag_value
        ! TODO: Implement using C binding to set exception flags
        ! This requires calling feclearexcept() or feraiseexcept() from C
    end subroutine

    ! IEEE_GET_HALTING_MODE
    elemental subroutine ieee_get_halting_mode(flag, halting)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(out) :: halting
        ! TODO: Implement halting mode query
        ! For now, return false (no halting on exceptions)
        halting = .false.
    end subroutine

    ! IEEE_SET_HALTING_MODE
    elemental subroutine ieee_set_halting_mode(flag, halting)
        type(ieee_flag_type), intent(in) :: flag
        logical, intent(in) :: halting
        ! TODO: Implement halting mode setting
    end subroutine

    ! IEEE_GET_STATUS
    subroutine ieee_get_status(status_value)
        type(ieee_status_type), intent(out) :: status_value
        ! TODO: Implement using C binding to save FPU status
        ! For now, initialize to default state
        status_value%flags = 0
    end subroutine

    ! IEEE_SET_STATUS
    subroutine ieee_set_status(status_value)
        type(ieee_status_type), intent(in) :: status_value
        ! TODO: Implement using C binding to restore FPU status
    end subroutine

    ! IEEE_SUPPORT_FLAG
    pure function ieee_support_flag_sp(flag, x) result(r)
        use iso_fortran_env, only: real32
        type(ieee_flag_type), intent(in) :: flag
        real(real32), intent(in), optional :: x
        logical :: r
        ! All IEEE exception flags are supported
        r = .true.
    end function

    pure function ieee_support_flag_dp(flag, x) result(r)
        use iso_fortran_env, only: real64
        type(ieee_flag_type), intent(in) :: flag
        real(real64), intent(in), optional :: x
        logical :: r
        r = .true.
    end function

    ! IEEE_SUPPORT_HALTING
    pure function ieee_support_halting(flag) result(r)
        type(ieee_flag_type), intent(in) :: flag
        logical :: r
        ! Halting on exceptions is typically supported but platform-dependent
        r = .true.
    end function

end module
