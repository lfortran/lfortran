! Test: merge() with array-valued mask inside a large loop should not
! overflow the stack. Regression test for a bug where ReAlloc codegen
! placed an alloca inside the loop body instead of the entry block.
program intrinsics_428
    implicit none
    type :: t
        real, allocatable :: v(:)
    end type
    integer, parameter :: n = 300000
    type(t) :: arr(n)
    real :: h(2, n)
    integer :: j

    call random_number(h)
    do j = 1, n
        arr(j)%v = merge(1.0, 0.0, h(:,j) < 0.5)
    end do

    ! Verify the results: each arr(j)%v should have 2 elements,
    ! each being either 0.0 or 1.0.
    do j = 1, n
        if (size(arr(j)%v) /= 2) error stop
        if (arr(j)%v(1) /= 0.0 .and. arr(j)%v(1) /= 1.0) error stop
        if (arr(j)%v(2) /= 0.0 .and. arr(j)%v(2) /= 1.0) error stop
    end do
    print *, "ok"
end program
