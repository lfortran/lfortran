module lfortran_intrinsic_ieee_arithmetic
    implicit none

    type ieee_class_type
    integer :: value
    end type

    type(ieee_class_type) :: ieee_negative_denormal
    type(ieee_class_type) :: ieee_negative_inf
    type(ieee_class_type) :: ieee_negative_normal
    type(ieee_class_type) :: ieee_negative_zero
    type(ieee_class_type) :: ieee_positive_denormal
    type(ieee_class_type) :: ieee_positive_inf
    type(ieee_class_type) :: ieee_positive_normal
    type(ieee_class_type) :: ieee_positive_zero
    type(ieee_class_type) :: ieee_quiet_nan
    type(ieee_class_type) :: ieee_signaling_nan

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

    elemental function spieee_value(x, cls) result(y)
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
        r = x < 0.0_real32
    end function

    elemental function dpieee_is_negative(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        logical :: r
        r = x < 0.0_real64
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
        r = floor(log(abs(x)) / log(2.0_real32))
    end function

    elemental function dpieee_logb(x) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        real(real64) :: r
        r = floor(log(abs(x)) / log(2.0_real64))
    end function

    elemental function spieee_rem(x, y) result(r)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x, y
        real(real32) :: r
        r = modulo(x, y)
    end function

    elemental function dpieee_rem(x, y) result(r)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x, y
        real(real64) :: r
        r = modulo(x, y)
    end function

end module
