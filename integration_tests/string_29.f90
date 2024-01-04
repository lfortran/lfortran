function c2s(n, x) result(y)
integer, intent(in) :: n
character, intent(in) :: x(:)
character(:), allocatable :: y
integer :: i
allocate(character(n) :: y)
do i = 1, n
    y(i:i) = x(i)
end do
end function

program string_29
character(len=1) :: s(3)
integer :: i
s(1) = "a"
s(2) = "b"
s(3) = "c"
i = fortran_string(3, s)
print *, i
if (i /= 3) error stop

contains

integer function fortran_string(n,s) result(r) bind(c)
integer :: n
character(len=1), intent(in) :: s(:)
interface
    function c2s(n, x) result(y)
    integer, intent(in) :: n
    character, intent(in) :: x(:)
    character(:), allocatable :: y
    end function
end interface
print *, c2s(n,s)
if (c2s(n,s) /= "abc") error stop
r = len(c2s(n,s))
end function

end program
