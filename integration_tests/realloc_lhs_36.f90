! Reallocation on intrinsic assignment is on by default, with no compiler
! option: derived type components, rank 2 shape changes and the bounds the
! reallocated array gets (F2018 10.2.1.3 p3).
program realloc_lhs_36
    implicit none
    type :: t
        integer, allocatable :: v(:)
        real, allocatable :: m(:, :)
    end type t
    type(t) :: obj
    real, allocatable :: m(:, :)
    integer, allocatable :: x(:), y(:)
    integer :: i

    ! An allocatable component, unallocated, then of another size
    obj%v = [1, 2, 3]
    if (size(obj%v) /= 3) error stop
    obj%v = [obj%v, 4, 5]
    if (size(obj%v) /= 5) error stop
    if (any(obj%v /= [1, 2, 3, 4, 5])) error stop
    obj%m = reshape([(real(i), i = 1, 6)], [3, 2])
    if (any(shape(obj%m) /= [3, 2])) error stop

    ! Rank 2 with a change of shape, the same size included
    allocate(m(2, 2))
    m = 0
    m = reshape([(real(i), i = 1, 6)], [2, 3])
    if (any(shape(m) /= [2, 3])) error stop
    if (m(2, 3) /= 6.0) error stop
    m = transpose(m)
    if (any(shape(m) /= [3, 2])) error stop
    if (m(3, 2) /= 6.0) error stop
    m = obj%m(1:2, :)
    if (any(shape(m) /= [2, 2])) error stop
    if (m(2, 2) /= 5.0) error stop

    ! The same shape: no reallocation, the bounds are kept
    allocate(x(0:4))
    x = [1, 2, 3, 4, 5]
    if (lbound(x, 1) /= 0 .or. ubound(x, 1) /= 4) error stop
    if (x(0) /= 1) error stop

    ! Another shape: the bounds become those of the expression, which are 1
    ! for an array constructor or a section
    x = [1, 2, 3]
    if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 3) error stop

    ! and those of a whole array variable
    allocate(y(-2:2))
    y = [(i, i = -2, 2)]
    x = y
    if (lbound(x, 1) /= -2 .or. ubound(x, 1) /= 2) error stop
    if (x(-2) /= -2 .or. x(2) /= 2) error stop
    deallocate(x)
    x = y
    if (lbound(x, 1) /= -2) error stop
    x = y(-1:1)
    if (lbound(x, 1) /= 1 .or. ubound(x, 1) /= 3) error stop
    if (any(x /= [-1, 0, 1])) error stop
    print *, obj%v, m, x
end program realloc_lhs_36
