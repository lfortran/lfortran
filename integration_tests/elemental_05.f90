program elemental_05

integer, parameter :: n = 10
integer :: i

type :: vector
    integer :: elements(n)
end type

type(vector) :: a, b, c
do i = 1, n
    a%elements(i) = i
    b%elements(i) = i
end do
c%elements = add_vector(a%elements, b%elements)
print *, c%elements
do i = 1, n
    if (c%elements(i) /= 2*i) error stop
end do

contains

elemental function add_vector(a, b) result(c)
    integer, intent(in) :: a, b
    integer :: c
    c = a + b
end function

end program
