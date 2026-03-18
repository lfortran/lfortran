! Test: Additional scalar types and VALUE attribute variations
!
! Covers gap items A.1-A.14:
!   - Scalar interop for all integer kinds (c_int8_t, c_int16_t, c_short,
!     c_long, c_long_long, c_intptr_t, c_size_t, c_ptrdiff_t)
!   - VALUE attribute with logical(c_bool), complex, character(c_char)
!   - Scalars passed by reference and by value for each type
module bindc_24_mod
    use iso_c_binding, only: c_int8_t, c_int16_t, c_short, c_long, &
        c_long_long, c_size_t, c_intptr_t, c_ptrdiff_t, &
        c_bool, c_float_complex, c_double_complex, c_char, &
        c_int, c_float, c_double, c_int32_t
    implicit none

    interface
        ! ---- int8 ----
        integer(c_int8_t) function c24_add_int8(a, b) &
                bind(C, name="c24_add_int8")
            import :: c_int8_t
            integer(c_int8_t), value :: a, b
        end function

        subroutine c24_double_int8(a) bind(C, name="c24_double_int8")
            import :: c_int8_t
            integer(c_int8_t), intent(inout) :: a
        end subroutine

        ! ---- int16 ----
        integer(c_int16_t) function c24_add_int16(a, b) &
                bind(C, name="c24_add_int16")
            import :: c_int16_t
            integer(c_int16_t), value :: a, b
        end function

        subroutine c24_double_int16(a) bind(C, name="c24_double_int16")
            import :: c_int16_t
            integer(c_int16_t), intent(inout) :: a
        end subroutine

        ! ---- short ----
        integer(c_short) function c24_add_short(a, b) &
                bind(C, name="c24_add_short")
            import :: c_short
            integer(c_short), value :: a, b
        end function

        ! ---- long ----
        integer(c_long) function c24_add_long(a, b) &
                bind(C, name="c24_add_long")
            import :: c_long
            integer(c_long), value :: a, b
        end function

        ! ---- long_long ----
        integer(c_long_long) function c24_add_long_long(a, b) &
                bind(C, name="c24_add_long_long")
            import :: c_long_long
            integer(c_long_long), value :: a, b
        end function

        ! ---- size_t ----
        integer(c_size_t) function c24_add_size_t(a, b) &
                bind(C, name="c24_add_size_t")
            import :: c_size_t
            integer(c_size_t), value :: a, b
        end function

        ! ---- intptr_t ----
        integer(c_intptr_t) function c24_add_intptr(a, b) &
                bind(C, name="c24_add_intptr")
            import :: c_intptr_t
            integer(c_intptr_t), value :: a, b
        end function

        ! ---- ptrdiff_t ----
        integer(c_ptrdiff_t) function c24_add_ptrdiff(a, b) &
                bind(C, name="c24_add_ptrdiff")
            import :: c_ptrdiff_t
            integer(c_ptrdiff_t), value :: a, b
        end function

        ! ---- VALUE with logical(c_bool) ----
        integer(c_int) function c24_bool_to_int(x) &
                bind(C, name="c24_bool_to_int")
            import :: c_int, c_bool
            logical(c_bool), value :: x
        end function

        ! ---- VALUE with complex(c_float_complex) ----
        real(c_float) function c24_cabs_float(z) &
                bind(C, name="c24_cabs_float")
            import :: c_float, c_float_complex
            complex(c_float_complex), value :: z
        end function

        ! ---- VALUE with complex(c_double_complex) ----
        real(c_double) function c24_cabs_double(z) &
                bind(C, name="c24_cabs_double")
            import :: c_double, c_double_complex
            complex(c_double_complex), value :: z
        end function

        ! ---- VALUE with character(c_char) ----
        integer(c_int) function c24_char_to_int(ch) &
                bind(C, name="c24_char_to_int")
            import :: c_int, c_char
            character(kind=c_char, len=1), value :: ch
        end function

        ! ---- int8/int16 arrays through descriptors ----
        integer(c_int) function c24_sum_int8_1d(a) &
                bind(C, name="c24_sum_int8_1d")
            import :: c_int, c_int8_t
            integer(c_int8_t), intent(in) :: a(:)
        end function

        integer(c_int) function c24_sum_int16_1d(a) &
                bind(C, name="c24_sum_int16_1d")
            import :: c_int, c_int16_t
            integer(c_int16_t), intent(in) :: a(:)
        end function

        ! ---- function returning complex ----
        complex(c_float_complex) function c24_make_complex(re, im) &
                bind(C, name="c24_make_complex")
            import :: c_float_complex, c_float
            real(c_float), value :: re, im
        end function

        complex(c_double_complex) function c24_make_dcomplex(re, im) &
                bind(C, name="c24_make_dcomplex")
            import :: c_double_complex, c_double
            real(c_double), value :: re, im
        end function
    end interface
end module

program bindc_24
    use bindc_24_mod
    use iso_c_binding
    implicit none

    call test_int8()
    call test_int16()
    call test_short()
    call test_long()
    call test_long_long()
    call test_size_t()
    call test_intptr()
    call test_ptrdiff()
    call test_bool_value()
    call test_complex_value()
    call test_char_value()
    call test_small_int_arrays()
    call test_complex_return()

    print *, "All bindc_24 tests passed."

contains

    subroutine test_int8()
        integer(c_int8_t) :: a, b, r, x
        a = 10_c_int8_t
        b = 20_c_int8_t
        r = c24_add_int8(a, b)
        if (r /= 30_c_int8_t) error stop "FAIL: int8 add"

        x = 7_c_int8_t
        call c24_double_int8(x)
        if (x /= 14_c_int8_t) error stop "FAIL: int8 double"
    end subroutine

    subroutine test_int16()
        integer(c_int16_t) :: a, b, r, x
        a = 1000_c_int16_t
        b = 2000_c_int16_t
        r = c24_add_int16(a, b)
        if (r /= 3000_c_int16_t) error stop "FAIL: int16 add"

        x = 123_c_int16_t
        call c24_double_int16(x)
        if (x /= 246_c_int16_t) error stop "FAIL: int16 double"
    end subroutine

    subroutine test_short()
        integer(c_short) :: r
        r = c24_add_short(100_c_short, 200_c_short)
        if (r /= 300_c_short) error stop "FAIL: short add"
    end subroutine

    subroutine test_long()
        integer(c_long) :: r
        r = c24_add_long(100000_c_long, 200000_c_long)
        if (r /= 300000_c_long) error stop "FAIL: long add"
    end subroutine

    subroutine test_long_long()
        integer(c_long_long) :: r
        r = c24_add_long_long(1000000000_c_long_long, &
                              2000000000_c_long_long)
        if (r /= 3000000000_c_long_long) error stop "FAIL: long_long add"
    end subroutine

    subroutine test_size_t()
        integer(c_size_t) :: r
        r = c24_add_size_t(100_c_size_t, 200_c_size_t)
        if (r /= 300_c_size_t) error stop "FAIL: size_t add"
    end subroutine

    subroutine test_intptr()
        integer(c_intptr_t) :: r
        r = c24_add_intptr(1000_c_intptr_t, 2000_c_intptr_t)
        if (r /= 3000_c_intptr_t) error stop "FAIL: intptr add"
    end subroutine

    subroutine test_ptrdiff()
        integer(c_ptrdiff_t) :: r
        r = c24_add_ptrdiff(500_c_ptrdiff_t, 600_c_ptrdiff_t)
        if (r /= 1100_c_ptrdiff_t) error stop "FAIL: ptrdiff add"
    end subroutine

    subroutine test_bool_value()
        logical(c_bool) :: t, f
        t = .true.
        f = .false.
        if (c24_bool_to_int(t) /= 1) error stop "FAIL: bool true"
        if (c24_bool_to_int(f) /= 0) error stop "FAIL: bool false"
    end subroutine

    subroutine test_complex_value()
        complex(c_float_complex) :: zf
        complex(c_double_complex) :: zd
        real(c_float) :: rf
        real(c_double) :: rd

        zf = (3.0, 4.0)
        rf = c24_cabs_float(zf)
        if (abs(rf - 5.0) > 1.0e-5) error stop "FAIL: cabs float"

        zd = (3.0d0, 4.0d0)
        rd = c24_cabs_double(zd)
        if (abs(rd - 5.0d0) > 1.0d-10) error stop "FAIL: cabs double"
    end subroutine

    subroutine test_char_value()
        if (c24_char_to_int('A') /= 65) error stop "FAIL: char A"
        if (c24_char_to_int('0') /= 48) error stop "FAIL: char 0"
    end subroutine

    subroutine test_small_int_arrays()
        integer(c_int8_t)  :: a8(4)
        integer(c_int16_t) :: a16(4)

        a8 = [1_c_int8_t, 2_c_int8_t, 3_c_int8_t, 4_c_int8_t]
        if (c24_sum_int8_1d(a8) /= 10) error stop "FAIL: int8 array sum"

        a16 = [100_c_int16_t, 200_c_int16_t, 300_c_int16_t, 400_c_int16_t]
        if (c24_sum_int16_1d(a16) /= 1000) error stop "FAIL: int16 array sum"
    end subroutine

    subroutine test_complex_return()
        complex(c_float_complex) :: zf
        complex(c_double_complex) :: zd

        zf = c24_make_complex(3.0, 4.0)
        if (abs(real(zf) - 3.0) > 1.0e-5) error stop "FAIL: make_complex re"
        if (abs(aimag(zf) - 4.0) > 1.0e-5) error stop "FAIL: make_complex im"

        zd = c24_make_dcomplex(5.0d0, 6.0d0)
        if (abs(real(zd) - 5.0d0) > 1.0d-10) error stop "FAIL: make_dcomplex re"
        if (abs(aimag(zd) - 6.0d0) > 1.0d-10) error stop "FAIL: make_dcomplex im"
    end subroutine

end program
