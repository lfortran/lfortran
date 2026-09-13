! The examples from the Fortran 2023 10.1.2.3 NOTE, as executable tests.
program conditional_expr_05
    implicit none
    real :: residual, tolerance
    real :: a(3), val, x
    integer :: i
    logical :: have_val
    character(len=:), allocatable :: msg

    ! ( ABS(RESIDUAL) <= TOLERANCE ? 'ok' : 'did not converge' )
    tolerance = 1.0e-6
    residual = 1.0e-8
    msg = ( abs(residual) <= tolerance ? "ok" : "did not converge" )
    if (msg /= "ok") error stop 1
    residual = 1.0
    msg = ( abs(residual) <= tolerance ? "ok" : "did not converge" )
    if (msg /= "did not converge") error stop 2

    ! ( I>0 .AND. I<=SIZE(A) ? A(I) : PRESENT(VAL) ? VAL : 0.0 ), with a local
    ! logical standing in for PRESENT
    a = [1.0, 2.0, 3.0]
    val = 9.0
    have_val = .true.
    i = 2
    x = ( i>0 .and. i<=size(a) ? a(i) : have_val ? val : 0.0 )
    if (abs(x-2.0) > 1.0e-6) error stop 3
    i = 0
    x = ( i>0 .and. i<=size(a) ? a(i) : have_val ? val : 0.0 )
    if (abs(x-9.0) > 1.0e-6) error stop 4
    have_val = .false.
    x = ( i>0 .and. i<=size(a) ? a(i) : have_val ? val : 0.0 )
    if (abs(x-0.0) > 1.0e-6) error stop 5

    ! The same example with a real optional dummy, as written in the NOTE
    if (abs(pick(a, 2)      - 2.0) > 1.0e-6) error stop 6
    if (abs(pick(a, 0, 9.0) - 9.0) > 1.0e-6) error stop 7
    if (abs(pick(a, 0)      - 0.0) > 1.0e-6) error stop 8
contains
    real function pick(a, i, val)
        real, intent(in) :: a(:)
        integer, intent(in) :: i
        real, intent(in), optional :: val
        pick = ( i>0 .and. i<=size(a) ? a(i) : present(val) ? val : 0.0 )
    end function
end program
