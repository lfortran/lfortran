program elemental_06

integer, parameter :: n = 10
integer :: i

type :: vector
    integer :: elements(0:n-1)
end type

type(vector) :: a, b, c
do i = 0, n-1
    a%elements(i) = i
    b%elements(i) = i
end do
c%elements = add_vector(a%elements, b%elements)
print *, c%elements

contains

elemental function add_vector(a, b) result(c)
    integer, intent(in) :: a, b
    integer :: c
    c = a + b
end function

end program
