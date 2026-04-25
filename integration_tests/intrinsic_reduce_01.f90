! MRE for Fortran 2018 reduce(array, operation, dim=...)
program intrinsic_reduce_01
    implicit none
    integer :: a(2, 3)
    integer :: expected(2)
    integer :: i
    a = reshape([(i, i = 1, 6)], [2, 3])
    ! reduce along dim 2: row 1 -> 1+3+5 = 9, row 2 -> 2+4+6 = 12
    expected = reduce(a, add_i, dim=2)
    if (expected(1) /= 9) error stop
    if (expected(2) /= 12) error stop
contains
    pure integer function add_i(x, y)
        integer, intent(in) :: x, y
        add_i = x + y
    end function add_i
end program
