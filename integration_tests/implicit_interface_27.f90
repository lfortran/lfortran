! Test that complex function return values work correctly with implicit interfaces
! when calling LFortran-compiled Fortran functions.
! This tests the ABI propagation fix: implicit interfaces should use the
! Implementation's ABI (Source) when calling internal functions, not BindC.

program test_implicit_interface_complex_return
    implicit none
    complex :: result, cdotc_test
    external cdotc_test
    complex :: x(3), y(3)

    x = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0)]
    y = [(1.0, 1.0), (1.0, 1.0), (1.0, 1.0)]

    result = cdotc_test(3, x, 1, y, 1)

    ! Expected: conjugate(x) dot y
    ! = (1-2i)(1+i) + (3-4i)(1+i) + (5-6i)(1+i)
    ! = (1-2i+i-2i^2) + (3-4i+3i-4i^2) + (5-6i+5i-6i^2)
    ! = (1-i+2) + (3-i+4) + (5-i+6)
    ! = 3-i + 7-i + 11-i
    ! = 21 - 3i

    if (abs(real(result) - 21.0) > 1.0e-5) error stop "real part wrong"
    if (abs(aimag(result) - (-3.0)) > 1.0e-5) error stop "imaginary part wrong"

    print *, "PASS: complex return via implicit interface works correctly"
end program

complex function cdotc_test(n, x, incx, y, incy)
    implicit none
    integer, intent(in) :: n, incx, incy
    complex, intent(in) :: x(*), y(*)
    integer :: i, ix, iy

    cdotc_test = (0.0, 0.0)
    ix = 1
    iy = 1
    do i = 1, n
        cdotc_test = cdotc_test + conjg(x(ix)) * y(iy)
        ix = ix + incx
        iy = iy + incy
    end do
end function
