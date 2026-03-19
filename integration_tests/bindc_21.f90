! Test: Assumed-rank extensions, optional combos, multi-argument
!
! Covers gap items D.14-D.15, E.16-E.17, I.24:
!   - Assumed-rank with intent(inout)
!   - Assumed-rank with specific types other than int32
!   - Optional assumed-rank
!   - Optional scalar
!   - Multiple descriptor arguments in a single call
!
! Items tested in bindc_23 instead (not yet supported by LFortran LLVM):
!   - D.12: Optional allocatable
!   - D.13: Optional pointer
!   - E.18: Assumed-rank scalar (rank 0 passed to dimension(..))
module bindc_21_mod
    use iso_c_binding, only: c_int, c_int32_t, c_int64_t, c_double
    implicit none

    interface
        ! ---- assumed-rank with intent(inout) ----
        subroutine c21_double_ar(a) bind(C, name="c21_double_ar")
            import :: c_int32_t
            integer(c_int32_t), intent(inout) :: a(..)
        end subroutine

        ! ---- assumed-rank with specific types ----
        integer(c_int64_t) function c21_sum_ar_i64(a) &
                bind(C, name="c21_sum_ar_i64")
            import :: c_int64_t
            integer(c_int64_t), intent(in) :: a(..)
        end function

        integer(c_int) function c21_sum_ar_dbl(a) &
                bind(C, name="c21_sum_ar_dbl")
            import :: c_int, c_double
            real(c_double), intent(in) :: a(..)
        end function

        ! ---- optional assumed-rank ----
        integer(c_int) function c21_opt_ar_present(a) &
                bind(C, name="c21_opt_ar_present")
            import :: c_int
            type(*), optional, intent(in) :: a(..)
        end function

        ! ---- optional scalar ----
        integer(c_int) function c21_opt_scalar_present(x) &
                bind(C, name="c21_opt_scalar_present")
            import :: c_int, c_int32_t
            integer(c_int32_t), optional, intent(in) :: x
        end function

        integer(c_int32_t) function c21_opt_scalar_value(x) &
                bind(C, name="c21_opt_scalar_value")
            import :: c_int32_t
            integer(c_int32_t), optional, intent(in) :: x
        end function

        ! ---- multiple descriptor arguments ----
        integer(c_int32_t) function c21_dot_product(a, b) &
                bind(C, name="c21_dot_product")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
            integer(c_int32_t), intent(in) :: b(:)
        end function

        subroutine c21_add_arrays(a, b, c) bind(C, name="c21_add_arrays")
            import :: c_int32_t
            integer(c_int32_t), intent(in) :: a(:)
            integer(c_int32_t), intent(in) :: b(:)
            integer(c_int32_t), intent(out) :: c(:)
        end subroutine
    end interface
end module

program bindc_21
    use bindc_21_mod
    use iso_c_binding, only: c_int32_t, c_int64_t, c_double, c_int
    implicit none

    call test_ar_inout()
    call test_ar_types()
    call test_opt_ar()
    call test_opt_scalar()
    call test_multi_arg()

    print *, "All bindc_21 tests passed."

contains

    subroutine test_ar_inout()
        integer(c_int32_t) :: a1(4), a2(2,3)

        a1 = [1, 2, 3, 4]
        call c21_double_ar(a1)
        if (a1(1) /= 2 .or. a1(4) /= 8) error stop "FAIL: ar inout 1d"

        a2 = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call c21_double_ar(a2)
        if (a2(1,1) /= 2 .or. a2(2,3) /= 12) error stop "FAIL: ar inout 2d"
    end subroutine

    subroutine test_ar_types()
        integer(c_int64_t) :: ai64(3)
        real(c_double)     :: ad(3)

        ai64 = [10_c_int64_t, 20_c_int64_t, 30_c_int64_t]
        if (c21_sum_ar_i64(ai64) /= 60_c_int64_t) &
            error stop "FAIL: ar i64 sum"

        ad = [1.5d0, 2.5d0, 3.0d0]
        if (c21_sum_ar_dbl(ad) /= 7) error stop "FAIL: ar dbl sum"
    end subroutine

    subroutine test_opt_ar()
        integer(c_int32_t) :: arr(4)
        arr = [1, 2, 3, 4]

        if (c21_opt_ar_present(arr) /= 1) error stop "FAIL: opt ar present"
        if (c21_opt_ar_present() /= 0) error stop "FAIL: opt ar absent"
    end subroutine

    subroutine test_opt_scalar()
        integer(c_int32_t) :: x
        x = 77

        if (c21_opt_scalar_present(x) /= 1) error stop "FAIL: opt scalar present"
        if (c21_opt_scalar_present() /= 0) error stop "FAIL: opt scalar absent"

        if (c21_opt_scalar_value(x) /= 77) error stop "FAIL: opt scalar value"
    end subroutine

    subroutine test_multi_arg()
        integer(c_int32_t) :: a(4), b(4), c(4)

        a = [1, 2, 3, 4]
        b = [10, 20, 30, 40]

        if (c21_dot_product(a, b) /= 300) error stop "FAIL: dot product"

        c = 0
        call c21_add_arrays(a, b, c)
        if (c(1) /= 11 .or. c(4) /= 44) error stop "FAIL: add arrays"
    end subroutine

end program
