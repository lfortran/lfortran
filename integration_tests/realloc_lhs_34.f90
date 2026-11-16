! Reallocation on intrinsic assignment (F2018 10.2.1.3) is on by default,
! with no compiler option: the cases of #10331.
program realloc_lhs_34
    implicit none
    integer, allocatable :: x(:), y(:), z(:)
    integer :: i

    ! An allocated array of another size is reallocated
    allocate(x(2))
    x = 7
    if (size(x) /= 2) error stop
    x = [1, 2, 3]
    if (size(x) /= 3) error stop
    if (any(x /= [1, 2, 3])) error stop

    ! A smaller one too
    x = [4]
    if (size(x) /= 1) error stop
    if (x(1) /= 4) error stop

    ! An unallocated local is allocated
    if (allocated(y)) error stop
    y = [(i*i, i = 1, 5)]
    if (.not. allocated(y)) error stop
    if (size(y) /= 5) error stop
    if (any(y /= [1, 4, 9, 16, 25])) error stop

    ! Appending to the array itself
    x = [1, 2]
    x = [x, 3]
    if (size(x) /= 3) error stop
    if (any(x /= [1, 2, 3])) error stop
    x = [0, x]
    if (any(x /= [0, 1, 2, 3])) error stop

    ! pack gives a result whose size is known only at run time
    z = pack(y, mod(y, 2) == 1)
    if (size(z) /= 3) error stop
    if (any(z /= [1, 9, 25])) error stop
    z = pack(y, y > 100)
    if (.not. allocated(z)) error stop
    if (size(z) /= 0) error stop

    ! An array expression of the array itself, of another size
    y = y(2:4) + 1
    if (size(y) /= 3) error stop
    if (any(y /= [5, 10, 17])) error stop
    print *, x, y
end program realloc_lhs_34
