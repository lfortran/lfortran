program elemental_07

integer, parameter :: n = 10
integer :: i

type :: vector
    integer :: elements(-5:4)
end type

type(vector) :: a, b, c
do i = -5, 4
    a%elements(i) = i
    b%elements(i) = i
end do
c%elements = add_vector(a%elements, b%elements)
print *, c%elements
do i = -5, 4
    if (c%elements(i) /= 2*i) error stop
end do

contains

elemental function add_vector(a, b) result(c)
    integer, intent(in) :: a, b
    integer :: c
    c = a + b
end function

end program
