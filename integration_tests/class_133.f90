! Test shape() intrinsic on class(*) allocatable arrays.
! Verifies that shape() works correctly on unlimited polymorphic arrays.
program class_133
    implicit none
    class(*), allocatable :: x(:)
    integer :: s(1)

    allocate(integer :: x(5))
    s = shape(x)
    if (s(1) /= 5) error stop

    deallocate(x)
    allocate(real :: x(10))
    s = shape(x)
    if (s(1) /= 10) error stop

    print *, "PASS"
end program
