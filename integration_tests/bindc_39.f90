! Test: C_LONG_DOUBLE constants, TYPE(*) scalar/assumed-size, assumed-size arrays
!
! Covers:
!   - C_LONG_DOUBLE and C_LONG_DOUBLE_COMPLEX named constants exist
!   - TYPE(*) with scalar (not assumed-rank)
!   - TYPE(*) with assumed-size dimension(*)
!   - Assumed-size integer/real arrays in BIND(C)
!   - BIND(C) on BLOCK DATA with COMMON

module bindc_39_ifaces
    use iso_c_binding
    implicit none

    interface
        ! ---- TYPE(*) scalar ----
        integer(c_int) function c39_type_star_scalar(x) bind(C)
            import :: c_int
            type(*), intent(in) :: x
        end function

        ! ---- TYPE(*) assumed-size ----
        integer(c_int) function c39_type_star_assumed_size(x, n) bind(C)
            import :: c_int
            type(*), intent(in) :: x(*)
            integer(c_int), value :: n
        end function

        ! ---- Assumed-size integer array ----
        integer(c_int) function c39_sum_assumed_size_int(arr, n) bind(C)
            import :: c_int
            integer(c_int), intent(in) :: arr(*)
            integer(c_int), value :: n
        end function

        ! ---- Assumed-size real array ----
        real(c_double) function c39_sum_assumed_size_dbl(arr, n) bind(C)
            import :: c_int, c_double
            real(c_double), intent(in) :: arr(*)
            integer(c_int), value :: n
        end function

        ! ---- BLOCK DATA common block read ----
        integer(c_int) function c39_get_common_a() bind(C)
            import :: c_int
        end function
        integer(c_int) function c39_get_common_b() bind(C)
            import :: c_int
        end function
    end interface
end module

block data bindc_39_block_data
    use iso_c_binding
    implicit none
    integer(c_int) :: bd_a, bd_b
    common /c39_bd_common/ bd_a, bd_b
    bind(C, name="c39_bd_common") :: /c39_bd_common/
    data bd_a /111/, bd_b /222/
end block data

program bindc_39
    use iso_c_binding
    use bindc_39_ifaces
    implicit none

    call test_long_double_constants()
    call test_type_star()
    call test_assumed_size_arrays()
    call test_block_data_common()

    print *, "All bindc_39 tests passed."

contains

    subroutine test_long_double_constants()
        integer :: kld, kldc
        kld = c_long_double
        kldc = c_long_double_complex
        ! Per F2018 §18.2.2: if the processor does not support a C type,
        ! the corresponding kind is negative.
        ! We just verify the constants are accessible and consistent.
        if (kld > 0) then
            ! Supported: must be a valid real kind
            if (kldc <= 0) error stop "FAIL: c_long_double supported but complex not"
        else
            ! Unsupported: must be negative
            if (kld /= -1 .and. kld /= -2 .and. kld /= -4) then
                ! Standard only says "negative", any negative is fine
                if (kld >= 0) error stop "FAIL: c_long_double not negative"
            end if
        end if
    end subroutine

    subroutine test_type_star()
        integer(c_int), target :: ival
        real(c_double), target :: dval
        integer(c_int) :: arr(5)
        integer(c_int) :: r

        ! TYPE(*) scalar: pass an integer
        ival = 42
        r = c39_type_star_scalar(ival)
        ! C just reads the first sizeof(int) bytes
        if (r /= 42) error stop "FAIL: type(*) scalar int"

        ! TYPE(*) assumed-size: pass integer array
        arr = [10, 20, 30, 40, 50]
        r = c39_type_star_assumed_size(arr, 5_c_int)
        if (r /= 150) error stop "FAIL: type(*) assumed-size int sum"
    end subroutine

    subroutine test_assumed_size_arrays()
        integer(c_int) :: iarr(4)
        real(c_double) :: darr(3)
        integer(c_int) :: ri
        real(c_double) :: rd

        iarr = [1, 2, 3, 4]
        ri = c39_sum_assumed_size_int(iarr, 4_c_int)
        if (ri /= 10) error stop "FAIL: assumed-size int sum"

        darr = [1.5_c_double, 2.5_c_double, 3.0_c_double]
        rd = c39_sum_assumed_size_dbl(darr, 3_c_int)
        if (abs(rd - 7.0_c_double) > 1.0d-10) error stop "FAIL: assumed-size dbl sum"
    end subroutine

    subroutine test_block_data_common()
        integer(c_int) :: bd_a, bd_b
        common /c39_bd_common/ bd_a, bd_b
        ! BLOCK DATA initialized these to 111, 222
        ! C reads via the bound common name
        if (c39_get_common_a() /= 111) error stop "FAIL: block data common a"
        if (c39_get_common_b() /= 222) error stop "FAIL: block data common b"
    end subroutine

end program
