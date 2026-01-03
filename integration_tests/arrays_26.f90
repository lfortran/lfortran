program arrays_26
type :: my_struct
    integer :: i
end type
type(my_struct), allocatable :: s(:)
allocate(s(1))
s(1)%i = 5
if (s(1)%i /= 5) error stop
end program
