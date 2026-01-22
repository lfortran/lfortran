module lfortran_intrinsic_ieee_arithmetic
    implicit none

    type ieee_class_type
    integer :: value
    end type

    type(ieee_class_type), parameter :: ieee_negative_denormal = ieee_class_type(1)
    type(ieee_class_type), parameter :: ieee_negative_inf = ieee_class_type(2)
    type(ieee_class_type), parameter :: ieee_negative_normal = ieee_class_type(3)
    type(ieee_class_type), parameter :: ieee_negative_zero = ieee_class_type(4)
    type(ieee_class_type), parameter :: ieee_positive_denormal = ieee_class_type(5)
    type(ieee_class_type), parameter :: ieee_positive_inf = ieee_class_type(6)
    type(ieee_class_type), parameter :: ieee_positive_normal = ieee_class_type(7)
    type(ieee_class_type), parameter :: ieee_positive_zero = ieee_class_type(8)
    type(ieee_class_type), parameter :: ieee_quiet_nan = ieee_class_type(9)
    type(ieee_class_type), parameter :: ieee_signaling_nan = ieee_class_type(10)

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

    contains

    elemental function spieee_class(x) result(y)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: x
        type(ieee_class_type) :: y
        
        if (x /= x) then
            y = ieee_quiet_nan
        else if (x == 0.0_real32) then
            if (sign(1.0_real32, x) < 0.0_real32) then
                y = ieee_negative_zero
            else
                y = ieee_positive_zero
            end if
        else if (abs(x) > huge(x)) then
            if (x < 0.0_real32) then
                y = ieee_negative_inf
            else
                y = ieee_positive_inf
            end if
        else if (abs(x) < tiny(x)) then
            if (x < 0.0_real32) then
                y = ieee_negative_denormal
            else
                y = ieee_positive_denormal
            end if
        else
            if (x < 0.0_real32) then
                y = ieee_negative_normal
            else
                y = ieee_positive_normal
            end if
        end if
    end function

    elemental function dpieee_class(x) result(y)
        use iso_fortran_env, only: real64
        real(real64), intent(in) :: x
        type(ieee_class_type) :: y
        
        if (x /= x) then
            y = ieee_quiet_nan
        else if (x == 0.0_real64) then
            if (sign(1.0_real64, x) < 0.0_real64) then
                y = ieee_negative_zero
            else
                y = ieee_positive_zero
            end if
        else if (abs(x) > huge(x)) then
            if (x < 0.0_real64) then
                y = ieee_negative_inf
            else
                y = ieee_positive_inf
            end if
        else if (abs(x) < tiny(x)) then
            if (x < 0.0_real64) then
                y = ieee_negative_denormal
            else
                y = ieee_positive_denormal
            end if
        else
            if (x < 0.0_real64) then
                y = ieee_negative_normal
            else
                y = ieee_positive_normal
            end if
        end if
    end function

    elemental function spieee_value(x, cls) result(y)
        use iso_fortran_env, only: real32, int32
        real(real32), intent(in) :: x
        type(ieee_class_type), intent(in) :: cls
        real(real32) :: y
        integer(int32) :: bits

        y = x
        select case(cls%value)
        case (ieee_quiet_nan%value)
            bits = int(z'7FC00000', int32)
            y = transfer(bits, y)
        case (ieee_positive_inf%value)
            bits = int(z'7F800000', int32)
            y = transfer(bits, y)
        case (ieee_negative_inf%value)
            bits = int(z'FF800000', int32)
            y = transfer(bits, y)
        case (ieee_negative_zero%value)
            bits = int(z'80000000', int32)
            y = transfer(bits, y)
        case (ieee_positive_zero%value)
            bits = int(z'00000000', int32)
            y = transfer(bits, y)
        end select
    end function

    elemental function dpieee_value(x, cls) result(y)
        use iso_fortran_env, only: real64, int64
        real(real64), intent(in) :: x
        type(ieee_class_type), intent(in) :: cls
        real(real64) :: y
        integer(int64) :: bits

        y = x
        select case(cls%value)
        case (ieee_quiet_nan%value)
            bits = int(z'7FF8000000000000', int64)
            y = transfer(bits, y)
        case (ieee_positive_inf%value)
            bits = int(z'7FF0000000000000', int64)
            y = transfer(bits, y)
        case (ieee_negative_inf%value)
            bits = int(z'FFF0000000000000', int64)
            y = transfer(bits, y)
        case (ieee_negative_zero%value)
            bits = int(z'8000000000000000', int64)
            y = transfer(bits, y)
        case (ieee_positive_zero%value)
            bits = int(z'0000000000000000', int64)
            y = transfer(bits, y)
        end select
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

end module
