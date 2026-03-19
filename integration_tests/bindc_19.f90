! Test: Additional types through CFI descriptors (ISO_Fortran_binding.h)
!
! Covers gap items A.1-A.3, C.11:
!   - complex(c_float_complex) assumed-shape arrays
!   - complex(c_double_complex) assumed-shape arrays
!   - logical(c_bool) assumed-shape arrays
!   - CONTIGUOUS attribute on assumed-shape in BIND(C) interface
!
! Items tested in bindc_23 instead (not yet supported by LFortran LLVM):
!   - A.4: character(c_char) assumed-shape arrays via descriptor
!   - A.5: character with len > 1 through descriptor (elem_len check)
module bindc_19_mod
    use iso_c_binding, only: c_int, c_float, c_double, c_bool, &
                             c_float_complex, c_double_complex
    implicit none

    interface
        ! ---- complex(c_float_complex) arrays ----
        subroutine c19_sum_cfloat_1d(a, re, im) bind(C, name="c19_sum_cfloat_1d")
            import :: c_float_complex, c_float
            complex(c_float_complex), intent(in) :: a(:)
            real(c_float), intent(out) :: re, im
        end subroutine

        subroutine c19_sum_cfloat_2d(a, re, im) bind(C, name="c19_sum_cfloat_2d")
            import :: c_float_complex, c_float
            complex(c_float_complex), intent(in) :: a(:,:)
            real(c_float), intent(out) :: re, im
        end subroutine

        subroutine c19_scale_cfloat_1d(a) bind(C, name="c19_scale_cfloat_1d")
            import :: c_float_complex
            complex(c_float_complex), intent(inout) :: a(:)
        end subroutine

        ! ---- complex(c_double_complex) arrays ----
        subroutine c19_sum_cdouble_1d(a, re, im) bind(C, name="c19_sum_cdouble_1d")
            import :: c_double_complex, c_double
            complex(c_double_complex), intent(in) :: a(:)
            real(c_double), intent(out) :: re, im
        end subroutine

        subroutine c19_sum_cdouble_2d(a, re, im) bind(C, name="c19_sum_cdouble_2d")
            import :: c_double_complex, c_double
            complex(c_double_complex), intent(in) :: a(:,:)
            real(c_double), intent(out) :: re, im
        end subroutine

        ! ---- logical(c_bool) arrays ----
        integer(c_int) function c19_count_true_1d(a) bind(C, name="c19_count_true_1d")
            import :: c_int, c_bool
            logical(c_bool), intent(in) :: a(:)
        end function

        integer(c_int) function c19_count_true_2d(a) bind(C, name="c19_count_true_2d")
            import :: c_int, c_bool
            logical(c_bool), intent(in) :: a(:,:)
        end function

        subroutine c19_flip_bool_1d(a) bind(C, name="c19_flip_bool_1d")
            import :: c_bool
            logical(c_bool), intent(inout) :: a(:)
        end subroutine

        ! NOTE: character(kind=c_char, len=1) assumed-shape arrays and
        ! character(kind=c_char, len=4) arrays via descriptor are valid
        ! Fortran 2018 but not yet working in LFortran (see bindc_23).

        ! ---- CONTIGUOUS attribute on assumed-shape ----
        integer(c_int) function c19_sum_contiguous(a) bind(C, name="c19_sum_contiguous")
            import :: c_int
            integer(c_int), contiguous, intent(in) :: a(:)
        end function

        integer(c_int) function c19_is_contiguous_check(a) &
                bind(C, name="c19_is_contiguous_check")
            import :: c_int
            integer(c_int), contiguous, intent(in) :: a(:)
        end function
    end interface
end module

program bindc_19
    use bindc_19_mod
    use iso_c_binding, only: c_float, c_double, c_bool, c_int, &
                             c_float_complex, c_double_complex
    implicit none

    call test_complex_float()
    call test_complex_double()
    call test_logical()
    call test_contiguous()

    print *, "All bindc_19 tests passed."

contains

    subroutine test_complex_float()
        complex(c_float_complex) :: a1(3), a2(2,2)
        real(c_float) :: re, im

        a1 = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]

        call c19_sum_cfloat_1d(a1, re, im)
        if (abs(re - 9.0) > 1.0e-5) error stop "FAIL: cfloat 1d real"
        if (abs(im - 12.0) > 1.0e-5) error stop "FAIL: cfloat 1d imag"

        a2 = reshape([(1.0, 1.0), (2.0, 2.0), (3.0, 3.0), (4.0, 4.0)], [2, 2])
        call c19_sum_cfloat_2d(a2, re, im)
        if (abs(re - 10.0) > 1.0e-5) error stop "FAIL: cfloat 2d real"
        if (abs(im - 10.0) > 1.0e-5) error stop "FAIL: cfloat 2d imag"

        a1 = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
        call c19_scale_cfloat_1d(a1)
        if (abs(real(a1(1)) - 2.0) > 1.0e-5) error stop "FAIL: cfloat scale re"
        if (abs(aimag(a1(1)) - 4.0) > 1.0e-5) error stop "FAIL: cfloat scale im"
    end subroutine

    subroutine test_complex_double()
        complex(c_double_complex) :: a1(3), a2(2,2)
        real(c_double) :: re, im

        a1 = [(1.0d0, 2.0d0), (3.0d0, 4.0d0), (5.0d0, 6.0d0)]

        call c19_sum_cdouble_1d(a1, re, im)
        if (abs(re - 9.0d0) > 1.0d-10) error stop "FAIL: cdouble 1d real"
        if (abs(im - 12.0d0) > 1.0d-10) error stop "FAIL: cdouble 1d imag"

        a2 = reshape([(1.0d0, 1.0d0), (2.0d0, 2.0d0), &
                      (3.0d0, 3.0d0), (4.0d0, 4.0d0)], [2, 2])
        call c19_sum_cdouble_2d(a2, re, im)
        if (abs(re - 10.0d0) > 1.0d-10) error stop "FAIL: cdouble 2d real"
        if (abs(im - 10.0d0) > 1.0d-10) error stop "FAIL: cdouble 2d imag"
    end subroutine

    subroutine test_logical()
        logical(c_bool) :: a1(5), a2(2,3)

        a1 = [.true., .false., .true., .true., .false.]
        if (c19_count_true_1d(a1) /= 3) error stop "FAIL: bool count 1d"

        a2 = reshape([.true., .false., .true., .false., .true., .true.], [2, 3])
        if (c19_count_true_2d(a2) /= 4) error stop "FAIL: bool count 2d"

        a1 = [.true., .false., .true., .true., .false.]
        call c19_flip_bool_1d(a1)
        if (a1(1) .neqv. .false.) error stop "FAIL: flip 1"
        if (a1(2) .neqv. .true.) error stop "FAIL: flip 2"
    end subroutine

    subroutine test_contiguous()
        integer(c_int) :: arr(6)
        integer :: i

        arr = [(i, i=1,6)]

        if (c19_sum_contiguous(arr) /= 21) error stop "FAIL: contiguous sum"
        if (c19_is_contiguous_check(arr) /= 1) error stop "FAIL: contiguous check"
    end subroutine

end program
