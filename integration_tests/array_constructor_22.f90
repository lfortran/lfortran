! An array constructor with a component taken across an array, [pts%x],
! takes the elements in order, whatever the lower bounds of pts are.
program array_constructor_22
implicit none
type :: array_constructor_22_pt
    integer :: x
    integer :: y
end type
type :: array_constructor_22_holder
    type(array_constructor_22_pt) :: p(0:2)
end type
type(array_constructor_22_pt) :: pts(0:2), grid(-1:1, 2:3)
type(array_constructor_22_pt), allocatable :: pa(:)
type(array_constructor_22_holder) :: h
integer :: w(3), w6(6), i, j

pts%x = [4, 5, 6]
pts%y = [7, 8, 9]
w = [pts%x]
if (any(w /= [4, 5, 6])) error stop 1
w6 = [pts%x, pts%y]
if (any(w6 /= [4, 5, 6, 7, 8, 9])) error stop 2

do j = 2, 3
    do i = -1, 1
        grid(i, j)%x = 10*(i + 2) + j
    end do
end do
w6 = [grid%x]
if (any(w6 /= [12, 22, 32, 13, 23, 33])) error stop 3

h%p%x = [1, 2, 3]
w = [h%p%x]
if (any(w /= [1, 2, 3])) error stop 4

allocate(pa(-3:-1))
pa%x = [7, 8, 9]
w = [pa%x]
if (any(w /= [7, 8, 9])) error stop 5

call check_dummy(pts)
print *, "OK"

contains

subroutine check_dummy(d)
    type(array_constructor_22_pt), intent(in) :: d(0:)
    integer :: v(3)
    v = [d%x]
    if (any(v /= [4, 5, 6])) error stop 6
end subroutine

end program
