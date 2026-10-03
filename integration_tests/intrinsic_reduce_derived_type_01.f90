! reduce() on arrays of derived type with a user-defined binary operation (Fortran 2018)
program intrinsic_reduce_derived_type_01
    implicit none
    type :: pair
        integer :: a
        integer :: b
    end type pair
    type(pair) :: tmp(2, 3)
    type(pair) :: expected(2)
    integer :: i, j
    do j = 1, 3
        do i = 1, 2
            tmp(i, j) = pair(i, j)
        end do
    end do
    ! reduce along dim 2: row i -> sum of tmp(i,1)%a + tmp(i,2)%a + tmp(i,3)%a (and same for b)
    expected = reduce(tmp, add_pair, dim=2)
    if (expected(1)%a /= 3) error stop
    if (expected(1)%b /= 6) error stop
    if (expected(2)%a /= 6) error stop
    if (expected(2)%b /= 6) error stop
contains
    pure function add_pair(x, y) result(z)
        type(pair), intent(in) :: x, y
        type(pair) :: z
        z%a = x%a + y%a
        z%b = x%b + y%b
    end function add_pair
end program
